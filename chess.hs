{- HLINT ignore "Use list comprehension" -}
{- HLINT ignore "Use join" -}
module Main where

import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

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

type BoardState = (Map Square Piece, PieceColor,[CastlingRights], Maybe Square) -- for 3-fold repetition

data Board = Board
  { squares :: Map Square Piece, -- Switched to a Map for complexity reduction
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
    (M.fromList [(sq, p) | r <-[1 .. 8], f <- ['a' .. 'h'], let sq = (f, r), Just p <- [getInitialPiece sq]])
    White
    Normal
    [] [WhiteKingSide, WhiteQueenSide, BlackQueenSide, BlackKingSide]
    Nothing
    0
    0 []
  where
    getInitialPiece :: Square -> Maybe Piece
    getInitialPiece sq = case sq of
      ('a', 1) -> Just (Piece White Rook)
      ('b', 1) -> Just (Piece White Knight)
      ('c', 1) -> Just (Piece White Bishop)
      ('d', 1) -> Just (Piece White Queen)
      ('e', 1) -> Just (Piece White King)
      ('f', 1) -> Just (Piece White Bishop)
      ('g', 1) -> Just (Piece White Knight)
      ('h', 1) -> Just (Piece White Rook)
      (_, 2)   -> Just (Piece White Pawn) -- White pawns
      ('a', 8) -> Just (Piece Black Rook)
      ('b', 8) -> Just (Piece Black Knight)
      ('c', 8) -> Just (Piece Black Bishop)
      ('d', 8) -> Just (Piece Black Queen)
      ('e', 8) -> Just (Piece Black King)
      ('f', 8) -> Just (Piece Black Bishop)
      ('g', 8) -> Just (Piece Black Knight)
      ('h', 8) -> Just (Piece Black Rook)
      (_, 7)   -> Just (Piece Black Pawn) -- Black pawns
      _        -> Nothing -- Empty squares

getKingLocation :: Board -> PieceColor -> Square
getKingLocation b color =
  case[sq | (sq, p) <- M.toList (squares b), p == Piece color King] of
    (sq:_) -> sq
    []     -> error (show color ++ " king missing from board.")

pieceAt :: Board -> Char -> Int -> Maybe Piece
pieceAt b l n = M.lookup (l, n) (squares b)

squareEmpty :: Board -> Char -> Int -> Bool
squareEmpty b l n = n >= 1 && n <= 8 && l >= 'a' && l <= 'h' && not (M.member (l, n) (squares b))

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
      | rank == 8 || rank == 1 = [Move sq toSq (Just pt) capt isEnPassant | pt <-[Queen, Rook, Bishop, Knight]]
      | otherwise = [Move sq toSq Nothing capt isEnPassant]

    pawnMoves c =
      let forward = if c == White then 1 else -1
          startRank = if c == White then 2 else 7
          oneStep = n + forward
          twoStep = n + (forward * 2)

          canStep1 = squareEmpty b l oneStep
          canStep2 = n == startRank && canStep1 && squareEmpty b l twoStep

          steps = (if canStep1 then mkPawnMoves c (l, oneStep) False False else[])
               ++ (if canStep2 then mkPawnMoves c (l, twoStep) False False else[])

          captureDirs = catMaybes [moveFile l (-1), moveFile l 1]
          captures =
            concat [ mkPawnMoves c (f', oneStep) True isEnPassant
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

          queenSideEmpty = if c == White then [('b', 1), ('c', 1), ('d', 1)] else [('b', 8), ('c', 8), ('d', 8)]
          kingSideEmpty  = if c == White then [('f', 1), ('g', 1)] else[('f', 8), ('g', 8)]

          queenSideSafe = if c == White then[('e', 1), ('d', 1), ('c', 1)] else[('e', 8), ('d', 8), ('c', 8)]
          kingSideSafe  = if c == White then [('e', 1), ('f', 1), ('g', 1)] else[('e', 8), ('f', 8), ('g', 8)]

          kingSideTo  = if c == White then ('g', 1) else ('g', 8)
          queenSideTo = if c == White then ('c', 1) else ('c', 8)

          isSafe sqs = not (any (\sq' -> isSquareUnderAttackFromColor b sq' (enemyColor c)) sqs)

          qSideRight = if c == White then WhiteQueenSide else BlackQueenSide
          kSideRight = if c == White then WhiteKingSide else BlackKingSide

          castleQ =[Move sq queenSideTo Nothing False False | qSideRight `elem` castling b, isEmpty queenSideEmpty, isSafe queenSideSafe]
          castleK =[Move sq kingSideTo Nothing False False | kSideRight `elem` castling b, isEmpty kingSideEmpty, isSafe kingSideSafe]
          isEmpty = all (uncurry (squareEmpty b))

       in normalMoves ++ castleQ ++ castleK

    bishopMoves c = concat [getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 1), (-1, -1), (-1, 1), (1, -1)]]
    rookMoves c   = concat [getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 0), (0, 1), (-1, 0), (0, -1)]]
    queenMoves c  = concat [getMovesInDir b dir c (enemyColor c) sq sq | dir <-[(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)]]

updateBoardSimple :: Board -> Move -> Board
updateBoardSimple board@(Board sqs toMv status history castling enPass moveCount counter statesHistory) move@(Move f t promotion captureBool enPassantBool) =
  let isC = isCastling board move
      (rFrom, rTo) =
        if isC
          then case t of
            ('g', 1) -> (('h', 1), ('f', 1))
            ('c', 1) -> (('a', 1), ('d', 1))
            ('g', 8) -> (('h', 8), ('f', 8))
            ('c', 8) -> (('a', 8), ('d', 8))
            _ -> (('a', 1), ('a', 1)) -- Fallback
          else (('a', 1), ('a', 1))

      fromPiece = M.lookup f sqs
      movingPiece = case promotion of
        Nothing -> fromPiece
        Just pt -> Just (Piece toMv pt)
      newCounter = if captureBool || fromPiece == Just (Piece toMv Pawn) then 0 else 0.5 + counter

      -- Map operations heavily drop complexity here
      sqs1 = M.delete f sqs
      sqs2 = case movingPiece of
               Just p -> M.insert t p sqs1
               Nothing -> sqs1
      sqs3 = if enPassantBool
             then M.delete (fst t, snd f) sqs2
             else sqs2
      sqs4 = if isC
             then let rPiece = M.lookup rFrom sqs
                  in case rPiece of
                       Just rp -> M.insert rTo rp (M.delete rFrom sqs3)
                       Nothing -> M.delete rFrom sqs3
             else sqs3

      newSquares = sqs4
      newToMove = if toMv == White then Black else White
   in Board newSquares newToMove Normal history castling enPass moveCount newCounter statesHistory

isMoveLegal :: Board -> Move -> Bool
isMoveLegal board move = not (isCheckForColor (updateBoardSimple board move) (toMove board))

getAllPseudoLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllPseudoLegalMovesForColor b color = concat [getPseudoPossibleMovesForSquare b sq | (sq, p) <- M.toList (squares b), pieceColor p == color]

getAllLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllLegalMovesForColor b col = filter (isMoveLegal b) (getAllPseudoLegalMovesForColor b col)

isCheckmate :: Board -> Bool
isCheckmate b = isCheckForColor b (toMove b) && null (getAllLegalMovesForColor b (toMove b))

countPieceAndColor :: Map Square Piece -> PieceColor -> PieceType -> Int
countPieceAndColor sqs col refType =
  M.size $ M.filter (\(Piece c pt) -> c == col && pt == refType) sqs

existsPiece :: Map Square Piece -> PieceType -> Bool
existsPiece sqs refType =
  not $ M.null $ M.filter (\(Piece _ pt) -> pt == refType) sqs

isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial (Board sqs _ _ _ _ _ _ _ _) =
  not (existsPiece sqs Queen || existsPiece sqs Rook || existsPiece sqs Pawn)
  && not (hasMatingMaterial White || hasMatingMaterial Black)
  where
    hasMatingMaterial :: PieceColor -> Bool
    hasMatingMaterial col =
      let knights = countPieceAndColor sqs col Knight
          bishops = countPieceAndColor sqs col Bishop
      in knights >= 3
         || (bishops >= 1 && knights >= 1)                 -- Bishop + Knight
         || (bishops >= 2 && hasOppositeColorBishops col)  -- two bishops on different colours

    hasOppositeColorBishops :: PieceColor -> Bool
    hasOppositeColorBishops col =
      let bSquares = [sq | (sq, Piece c Bishop) <- M.toList sqs, c == col]
      in length bSquares >= 2
         && any isLightSquare bSquares
         && not (all isLightSquare bSquares)

    isLightSquare :: Square -> Bool
    isLightSquare (f, r) = odd (ord f + r)

is3FoldRepetition :: Board -> Bool
is3FoldRepetition b =
  let currentState = (squares b, toMove b, castling b, enPassantSq b)
      occurrences = length (filter (== currentState) (historyStates b))
  in occurrences >= 3

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

updateCastlingRights :: [CastlingRights] -> Move ->[CastlingRights]
updateCastlingRights rights (Move f t _ _ _) = filter keep rights
  where
    keep WhiteKingSide = f /= ('e', 1) && f /= ('h', 1) && t /= ('h', 1)
    keep WhiteQueenSide = f /= ('e', 1) && f /= ('a', 1) && t /= ('a', 1)
    keep BlackKingSide = f /= ('e', 8) && f /= ('h', 8) && t /= ('h', 8)
    keep BlackQueenSide = f /= ('e', 8) && f /= ('a', 8) && t /= ('a', 8)

createNewBoard :: Board -> Move -> Map Square Piece -> Maybe Square -> Maybe Piece -> Board
createNewBoard (Board _ toMv _ history castling _ moveCount counter historyStates) move newSquares newEnPassant movingPiece =
  let newCastling = updateCastlingRights castling move
      newToMove = if toMv == White then Black else White
      newHistory = move : history
      -- Check if the moved piece was a Pawn OR if this was a promotion (promotion = pawn move)
      isPawnMove = case movingPiece of
        Just (Piece _ Pawn) -> True
        _                   -> False
      isPromotion = isJust (promotion move)
      newCounter = if isCapture move || isPawnMove || isPromotion
                   then 0
                   else counter + 0.5
      currentState = (newSquares, newToMove, newCastling, newEnPassant)
      newHistoryStates = if newCounter == 0 then [] else currentState : historyStates
      nextBoardNoStatus = Board newSquares newToMove Normal newHistory newCastling newEnPassant moveCount newCounter newHistoryStates

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
updateBoard board@(Board sqs toMv _ _ _ _ _ counter historyStates) move@(Move f t promotion _ enPassantBool)
  | isCastling board move =
      let (rFrom, rTo) = case t of
            ('g', 1) -> (('h', 1), ('f', 1))
            ('c', 1) -> (('a', 1), ('d', 1))
            ('g', 8) -> (('h', 8), ('f', 8))
            ('c', 8) -> (('a', 8), ('d', 8))
            _ -> error "Invalid castling move"

          sqs1 = M.delete f sqs
          sqs2 = M.delete rFrom sqs1
          sqs3 = M.insert t (Piece toMv King) sqs2
          newSquares = M.insert rTo (Piece toMv Rook) sqs3

       in createNewBoard board move newSquares Nothing (Just (Piece toMv King))
  | enPassantBool =
      let capturedPawnSq = (fst t, snd f)
          sqs1 = M.delete f sqs
          sqs2 = M.delete capturedPawnSq sqs1
          newSquares = M.insert t (Piece toMv Pawn) sqs2
       in createNewBoard board move newSquares Nothing (Just (Piece toMv Pawn))
  | otherwise =
      let fromPiece = M.lookup f sqs
          movingPiece = case promotion of
            Nothing -> fromPiece
            Just pt -> Just (Piece toMv pt)

          sqs1 = M.delete f sqs
          newSquares = case movingPiece of
            Just p  -> M.insert t p sqs1
            Nothing -> sqs1

          newEnPassant = case movingPiece of
            Just (Piece c Pawn) ->
              if abs (snd f - snd t) == 2
                then Just (fst f, if c == White then snd t - 1 else snd t + 1)
                else Nothing
            _ -> Nothing
       in createNewBoard board move newSquares newEnPassant movingPiece

simulateGame :: Board -> Board
simulateGame b =
  let moves = getAllLegalMovesForColor b (toMove b)
   in case moves of
