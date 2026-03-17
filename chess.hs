{- HLINT ignore "Use list comprehension" -}
{- HLINT ignore "Use join" -}
module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  print "Start"

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Eq, Show)

data PieceColor = White | Black deriving (Eq, Show)

data Piece = Piece {pieceColor :: PieceColor, pieceType :: PieceType} deriving (Eq, Show)

data GameStatus = Normal | Check | CheckMate | StaleMate | Draw deriving (Eq, Show)

type Square = (Char, Int)

data CastlingRights = WhiteKingSide | WhiteQueenSide | BlackQueenSide | BlackKingSide deriving (Eq, Show)

data Move = Move
  { from :: Square,
    to :: Square,
    promotion :: Maybe PieceType,
    isCapture :: Bool,
    isEnPassant :: Bool
  }
  deriving (Eq, Show)

type BoardState = ([(Square, Maybe Piece)], PieceColor, [CastlingRights], Maybe Square) -- for 3-fold repetition

data Board = Board
  { squares :: [(Square, Maybe Piece)], -- should switch to map for complexity reduction
    toMove :: PieceColor,
    status :: GameStatus,
    history :: [Move],
    castling :: [CastlingRights],
    enPassantSq :: Maybe Square,
    moveCount :: Float,
    movesWithoutPawnMoveOrCapture :: Float,
    historyStates :: [BoardState]
  }
  deriving (Eq, Show)

initChessBoard :: Board
initChessBoard =
  Board
    ([getInitialPiece (f, r) | r <- [1 .. 8], f <- ['a' .. 'h']])
    White
    Normal
    []
    [WhiteKingSide, WhiteQueenSide, BlackQueenSide, BlackKingSide]
    Nothing
    0
    0
    []
  where
    getInitialPiece :: Square -> (Square, Maybe Piece)
    getInitialPiece sq = case sq of
      ('a', 1) -> (sq, Just (Piece White Rook))
      ('b', 1) -> (sq, Just (Piece White Knight))
      ('c', 1) -> (sq, Just (Piece White Bishop))
      ('d', 1) -> (sq, Just (Piece White Queen))
      ('e', 1) -> (sq, Just (Piece White King))
      ('f', 1) -> (sq, Just (Piece White Bishop))
      ('g', 1) -> (sq, Just (Piece White Knight))
      ('h', 1) -> (sq, Just (Piece White Rook))
      (f, 2) -> (sq, Just (Piece White Pawn)) -- White pawns
      ('a', 8) -> (sq, Just (Piece Black Rook))
      ('b', 8) -> (sq, Just (Piece Black Knight))
      ('c', 8) -> (sq, Just (Piece Black Bishop))
      ('d', 8) -> (sq, Just (Piece Black Queen))
      ('e', 8) -> (sq, Just (Piece Black King))
      ('f', 8) -> (sq, Just (Piece Black Bishop))
      ('g', 8) -> (sq, Just (Piece Black Knight))
      ('h', 8) -> (sq, Just (Piece Black Rook))
      (f, 7) -> (sq, Just (Piece Black Pawn)) -- Black pawns
      _ -> (sq, Nothing) -- Empty squares

getKingLocation :: Board -> PieceColor -> Square
getKingLocation b color = case find isMatchingPiece (squares b) of
  Just (sq, _) -> sq
  Nothing -> error (show color ++ " king missing from board.")
  where
    isMatchingPiece :: (Square, Maybe Piece) -> Bool
    isMatchingPiece (_, Just (Piece c King)) = c == color
    isMatchingPiece _ = False

pieceAt :: Board -> Char -> Int -> Maybe Piece
pieceAt b l n = case lookup (l, n) (squares b) of
  Just (Just p) -> Just p
  _ -> Nothing

squareEmpty :: Board -> Char -> Int -> Bool
squareEmpty b l n = n >= 1 && n <= 8 && l >= 'a' && l <= 'h' && isNothing (pieceAt b l n)

moveFile :: Char -> Int -> Maybe Char
moveFile f offset =
  let newF = chr (ord f + offset)
   in if newF >= 'a' && newF <= 'h' then Just newF else Nothing

moveRank :: Int -> Int -> Maybe Int
moveRank r offset =
  let newR = r + offset
   in if newR >= 1 && newR <= 8 then Just newR else Nothing

isSquareUnderAttackFromColor :: Board -> Square -> PieceColor -> Bool 
isSquareUnderAttackFromColor b sq@(l, n) col =
  anyPawnAttacking || anyKnightAttacking || anyRookAttacking || anyBishopAttacking || anyKingAttacking
  where
    isEnemyPiece pType f' r' = pieceAt b f' r' == Just (Piece col pType)

    moveSq df dr = do
      f' <- moveFile l df
      r' <- moveRank n dr
      return (f', r')

    hasEnemy pType (df, dr) = case moveSq df dr of
      Just (f', r') -> isEnemyPiece pType f' r'
      Nothing -> False

    checkDirection (df, dr) targets (currF, currR) =
      case (moveFile currF df, moveRank currR dr) of
        (Just f', Just r') ->
          case pieceAt b f' r' of
            Nothing -> checkDirection (df, dr) targets (f', r')
            Just (Piece c pType) -> c == col && pType `elem` targets
        _ -> False

    pDir = if col == White then -1 else 1
    anyPawnAttacking = any (hasEnemy Pawn) [(-1, pDir), (1, pDir)]

    knightDirs =[(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
    anyKnightAttacking = any (hasEnemy Knight) knightDirs

    kingDirs =[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    anyKingAttacking = any (hasEnemy King) kingDirs

    rookDirs =[(0, 1), (0, -1), (1, 0), (-1, 0)]
    anyRookAttacking = any (\dir -> checkDirection dir [Rook, Queen] sq) rookDirs

    bishopDirs =[(-1, -1), (1, -1), (1, 1), (-1, 1)]
    anyBishopAttacking = any (\dir -> checkDirection dir [Bishop, Queen] sq) bishopDirs

isCheckForColor :: Board -> PieceColor -> Bool
isCheckForColor b col = isSquareUnderAttackFromColor b (getKingLocation b col) (if col == White then Black else White)


getMovesInDir :: Board -> (Int, Int) -> PieceColor -> PieceColor -> Square -> Square -> [Move] 
getMovesInDir board (df, dr) ownColor enemyColor startSq (currF, currR) =
  case (moveFile currF df, moveRank currR dr) of
    (Just nextF, Just nextR) ->
      case pieceAt board nextF nextR of
        Nothing ->
          Move startSq (nextF, nextR) Nothing False False
            : getMovesInDir board (df, dr) ownColor enemyColor startSq (nextF, nextR)
        Just (Piece col _) ->[Move startSq (nextF, nextR) Nothing True False | col == enemyColor]
    _ ->[]


getPseudoPossibleMovesForSquare :: Board -> Square -> [Move] 
getPseudoPossibleMovesForSquare b sq@(l, n) = case pieceAt b l n of
  Nothing ->[]
  Just p@(Piece c pt) -> case pt of
    Pawn   -> pawnMoves c
    Knight -> knightMoves c
    Bishop -> bishopMoves c
    Rook   -> rookMoves c
    Queen  -> queenMoves c
    King   -> kingMoves c
  where
    enemyColor c = if c == White then Black else White

    moveSq df dr = do
      f' <- moveFile l df
      r' <- moveRank n dr
      return (f', r')

    isEnemyAt c f' r' = case pieceAt b f' r' of
      Just (Piece col _) -> col == enemyColor c
      _ -> False

    classifyMove c (f', r') = case pieceAt b f' r' of
      Nothing -> Just (Move sq (f', r') Nothing False False)
      Just (Piece col _) ->
        if col == enemyColor c
          then Just (Move sq (f', r') Nothing True False)
          else Nothing

    mkPawnMoves c toSq@(_, rank) capt isEnPassant
      | rank == 8 || rank == 1 =[Move sq toSq (Just pt) capt isEnPassant | pt <- [Queen, Rook, Bishop, Knight]]
      | otherwise =[Move sq toSq Nothing capt isEnPassant]

    pawnMoves c =
      let forward = if c == White then 1 else -1
          startRank = if c == White then 2 else 7
          oneStep = n + forward
          twoStep = n + (forward * 2)

          canStep1 = squareEmpty b l oneStep
          canStep2 = n == startRank && canStep1 && squareEmpty b l twoStep

          steps = (if canStep1 then mkPawnMoves c (l, oneStep) False False else[])
               ++ (if canStep2 then mkPawnMoves c (l, twoStep) False False else [])

          captureDirs = catMaybes[moveFile l (-1), moveFile l 1]
          captures =
            concat[ mkPawnMoves c (f', oneStep) True isEnPassant
              | f' <- captureDirs
              , let isEnPassant = enPassantSq b == Just (f', oneStep)
              , isEnemyAt c f' oneStep || isEnPassant
              ]
       in steps ++ captures

    knightMoves c =
      let dirs =[(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
       in mapMaybe (classifyMove c) (mapMaybe (uncurry moveSq) dirs)

    kingMoves c =
      let dirs =[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
          normalMoves = mapMaybe (classifyMove c) (mapMaybe (uncurry moveSq) dirs)

          queenSideEmpty = if c == White then[('b', 1), ('c', 1), ('d', 1)] else[('b', 8), ('c', 8), ('d', 8)]
          kingSideEmpty  = if c == White then[('f', 1), ('g', 1)] else [('f', 8), ('g', 8)]

          queenSideSafe = if c == White then[('e', 1), ('d', 1), ('c', 1)] else[('e', 8), ('d', 8), ('c', 8)]
          kingSideSafe  = if c == White then[('e', 1), ('f', 1), ('g', 1)] else[('e', 8), ('f', 8), ('g', 8)]

          kingSideTo  = if c == White then ('g', 1) else ('g', 8)
          queenSideTo = if c == White then ('c', 1) else ('c', 8)

          isSafe sqs = not (any (\sq' -> isSquareUnderAttackFromColor b sq' (enemyColor c)) sqs)
          isEmpty sqs = all (uncurry (squareEmpty b)) sqs

          qSideRight = if c == White then WhiteQueenSide else BlackQueenSide
          kSideRight = if c == White then WhiteKingSide else BlackKingSide

          castleQ =[Move sq queenSideTo Nothing False False | qSideRight `elem` castling b, isEmpty queenSideEmpty, isSafe queenSideSafe]
          castleK =[Move sq kingSideTo Nothing False False | kSideRight `elem` castling b, isEmpty kingSideEmpty, isSafe kingSideSafe]

       in normalMoves ++ castleQ ++ castleK

    bishopMoves c = concat[getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 1), (-1, -1), (-1, 1), (1, -1)]]
    rookMoves c   = concat[getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 0), (0, 1), (-1, 0), (0, -1)]]
    queenMoves c  = concat[getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)]]

updateBoardSimple :: Board -> Move -> Board
updateBoardSimple board@(Board squares toMove status history castling enPassant moveCount counter statesHistory) move@(Move from to promotion captureBool enPassantBool) =
  let isC = isCastling board move
      (rFrom, rTo) =
        if isC
          then case to of
            ('g', 1) -> (('h', 1), ('f', 1))
            ('c', 1) -> (('a', 1), ('d', 1))
            ('g', 8) -> (('h', 8), ('f', 8))
            ('c', 8) -> (('a', 8), ('d', 8))
            _ -> (('a', 1), ('a', 1)) -- Fallback???
          else (('a', 1), ('a', 1))

      fromPiece = lookup from squares >>= id
      movingPiece = case promotion of
        Nothing -> fromPiece
        Just pt -> Just (Piece toMove pt)
      newCounter = if captureBool || fromPiece == Just (Piece toMove Pawn) then 0 else 0.5 + counter
      updateSquare (sq, p)
        | sq == from = (sq, Nothing)
        | sq == to = (sq, movingPiece)
        | enPassantBool && sq == (fst to, snd from) = (sq, Nothing)
        | isC && sq == rFrom = (sq, Nothing) -- Remove Rook from old square
        | isC && sq == rTo = (sq, lookup rFrom squares >>= id) -- Add Rook to new square
        | otherwise = (sq, p)

      newSquares = map updateSquare squares
      newToMove = if toMove == White then Black else White
   in Board newSquares newToMove Normal history castling enPassant moveCount newCounter statesHistory

isMoveLegal :: Board -> Move -> Bool
isMoveLegal board move = not (isCheckForColor (updateBoardSimple board move) (toMove board))

getAllPseudoLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllPseudoLegalMovesForColor b color = concat [getPseudoPossibleMovesForSquare b sq | (sq, Just p) <- squares b, pieceColor p == color]

getAllLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllLegalMovesForColor b col = filter (isMoveLegal b) (getAllPseudoLegalMovesForColor b col)

isCheckmate :: Board -> Bool
isCheckmate b = isCheckForColor b (toMove b) && null (getAllLegalMovesForColor b (toMove b))

countPieceAndColor :: [(Square, Maybe Piece)] -> PieceColor -> PieceType -> Int
countPieceAndColor sqs col refType = 
  length $ filter isMatch sqs
  where isMatch (_, Just (Piece c pt)) = c == col && pt == refType
        isMatch _ = False

existsPiece ::[(Square, Maybe Piece)] -> PieceType -> Bool
existsPiece sqs refType = any isMatch sqs
  where isMatch (_, Just (Piece _ pt)) = pt == refType
        isMatch _ = False

isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial (Board squares _ _ _ _ _ _ _ _) =
  not
    ( existsPiece squares Queen
        || existsPiece squares Rook
        || existsPiece squares Pawn
        || countPieceAndColor squares Black Bishop > 0 && countPieceAndColor squares Black Knight > 0
        || countPieceAndColor squares White Bishop > 0 && countPieceAndColor squares White Knight > 0
        || countPieceAndColor squares White Bishop > 0 && countPieceAndColor squares Black Knight > 0-- according to FIDE King + Bishop vs King + Knight is technically not a dead position
        || countPieceAndColor squares Black Bishop > 0 && countPieceAndColor squares White Knight > 0
        || countPieceAndColor squares Black Knight >= 2
        || countPieceAndColor squares White Knight >= 2
        || bishopsOnDifferentColors squares
    )
  where
    -- A square is "light" if the sum of its file's ASCII value and its rank is odd
    isLightSquare :: Square -> Bool
    isLightSquare (f, r) = (ord f + r) `mod` 2 /= 0

    bishopsOnDifferentColors :: [(Square, Maybe Piece)] -> Bool -- Checks if there are bishops on both light and dark squares
    bishopsOnDifferentColors sqs =
      let bSquares = [sq | (sq, Just (Piece _ Bishop)) <- sqs]
          hasLight = any isLightSquare bSquares
          hasDark  = any (not . isLightSquare) bSquares
      in hasLight && hasDark

is3FoldRepetition :: Board -> Bool
is3FoldRepetition b = 
  let currentState = (squares b, toMove b, castling b, enPassantSq b)
      occurrences = length (filter (== currentState) (historyStates b))
  in occurrences >= 2 

isDraw :: Board -> Bool
isDraw b = movesWithoutPawnMoveOrCapture b >= 75 || isInsufficientMaterial b || is3FoldRepetition b

isStalemate :: Board -> Bool
isStalemate b = not (isCheckForColor b (toMove b)) && null (getAllLegalMovesForColor b (toMove b))

isCastling :: Board -> Move -> Bool
isCastling b (Move ('e', 1) ('g', 1) _ _ _) = getKingLocation b White == ('e', 1)
isCastling b (Move ('e', 1) ('c', 1) _ _ _) = getKingLocation b White == ('e', 1)
isCastling b (Move ('e', 8) ('g', 8) _ _ _) = getKingLocation b Black == ('e', 8)
isCastling b (Move ('e', 8) ('c', 8) _ _ _) = getKingLocation b Black == ('e', 8)
isCastling _ _ = False

updateCastlingRights :: [CastlingRights] -> Move -> [CastlingRights]
updateCastlingRights rights (Move from to _ _ _) = filter keep rights
  where
    keep WhiteKingSide = from /= ('e', 1) && from /= ('h', 1) && to /= ('h', 1)
    keep WhiteQueenSide = from /= ('e', 1) && from /= ('a', 1) && to /= ('a', 1)
    keep BlackKingSide = from /= ('e', 8) && from /= ('h', 8) && to /= ('h', 8)
    keep BlackQueenSide = from /= ('e', 8) && from /= ('a', 8) && to /= ('a', 8)

createNewBoard :: Board -> Move -> [(Square, Maybe Piece)] -> Maybe Square -> Maybe Piece -> Board
createNewBoard (Board _ toMove _ history castling _ moveCount counter historyStates) move newSquares newEnPassant movingPiece =
  let newCastling = updateCastlingRights castling move
      newToMove = if toMove == White then Black else White
      newHistory = move : history

      -- Check if the moved piece was a Pawn
      isPawnMove = case movingPiece of
        Just (Piece _ Pawn) -> True
        _ -> False

      newCounter = if isCapture move || isPawnMove then 0 else counter + 0.5

      currentState = (newSquares, newToMove, newCastling, newEnPassant)
      newHistoryStates = if newCounter == 0 then[] else currentState : historyStates

      nextBoardNoStatus = Board newSquares newToMove Normal newHistory newCastling newEnPassant moveCount newCounter newHistoryStates
      
      -- (Optimized status checks - see performance section below)
      inCheck = isCheckForColor nextBoardNoStatus newToMove
      legalMoves = getAllLegalMovesForColor nextBoardNoStatus newToMove
      noLegalMoves = null legalMoves

      newStatus
        | inCheck && noLegalMoves = CheckMate
        | not inCheck && noLegalMoves = StaleMate
        | inCheck = Check
        | isDraw nextBoardNoStatus = Draw
        | otherwise = Normal

   in Board newSquares newToMove newStatus newHistory newCastling newEnPassant (moveCount + 0.5) newCounter newHistoryStates

updateBoard :: Board -> Move -> Board
updateBoard board@(Board squares toMove _ _ _ _ _ counter historyStates) move@(Move from to promotion _ enPassantBool)
  | isCastling board move =
      let (rFrom, rTo) = case to of
            ('g', 1) -> (('h', 1), ('f', 1))
            ('c', 1) -> (('a', 1), ('d', 1))
            ('g', 8) -> (('h', 8), ('f', 8))
            ('c', 8) -> (('a', 8), ('d', 8))
            _ -> error "Invalid castling move"
          
          updateSquare (s, p) 
            | s == from || s == rFrom = (s, Nothing)
            | s == to                 = (s, Just (Piece toMove King))
            | s == rTo                = (s, Just (Piece toMove Rook))
            | otherwise               = (s, p)
          
          newSquares = map updateSquare squares
       in createNewBoard board move newSquares Nothing (Just (Piece toMove King))
  | enPassantBool =
      let capturedPawnSq = (fst to, snd from)
          updateSquare (s, p) 
            | s == from           = (s, Nothing)
            | s == to             = (s, Just (Piece toMove Pawn))
            | s == capturedPawnSq = (s, Nothing)
            | otherwise           = (s, p)
          
          newSquares = map updateSquare squares
       in createNewBoard board move newSquares Nothing (Just (Piece toMove Pawn))
  | otherwise =
      let fromPiece = lookup from squares >>= id
          movingPiece = case promotion of
            Nothing -> fromPiece
            Just pt -> Just (Piece toMove pt)
          
          updateSquare (s, p)
            | s == from = (s, Nothing)
            | s == to   = (s, movingPiece)
            | otherwise = (s, p)
          
          newSquares = map updateSquare squares
          newEnPassant = case movingPiece of
            Just (Piece c Pawn) ->
              if abs (snd from - snd to) == 2
                then Just (fst from, if c == White then snd to - 1 else snd to + 1)
                else Nothing
            _ -> Nothing
       in createNewBoard board move newSquares newEnPassant movingPiece

simulateGame :: Board -> Board -- TODO_6 proper implementation of game handler that can later be integrated with gui
simulateGame b =
  let moves = getAllLegalMovesForColor b (toMove b)
   in case moves of
        [] -> b
        m : xs ->
          if status b == Normal || status b == Check
            then simulateGame (updateBoard b m)
            else b
