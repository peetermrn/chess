{- HLINT ignore "Use list comprehension" -}
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

data Board = Board
  { squares :: [(Square, Maybe Piece)],
    toMove :: PieceColor,
    status :: GameStatus,
    history :: [Move],
    castling :: [CastlingRights],
    enPassantSq :: Maybe Square,
    moveCount :: Float,
    movesWithoutPawnMoveOrCapture :: Float
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

isSquareUnderAttackFromColor :: Board -> Square -> PieceColor -> Bool -- TODO refactor for readability
isSquareUnderAttackFromColor b sq col = anyPawnAttacking || anyKnightAttacking || anyRookAttacking || anyBishopAttacking || anyKingAttacking
  where
    (l, n) = sq
    isEnemyPiece pType f' r' = case pieceAt b f' r' of
      Just (Piece c p') -> c == col && p' == pType
      _ -> False

    checkDirection (df, dr) targets currF currN =
      case (moveFile currF df, moveRank currN dr) of
        (Just nextF, Just nextR) ->
          case pieceAt b nextF nextR of
            Nothing -> checkDirection (df, dr) targets nextF nextR
            Just (Piece c pType) -> c == col && elem pType targets
        _ -> False

    anyPawnAttacking =
      let pawnRank = if col == White then n - 1 else n + 1
          pawnFiles = [moveFile l (-1), moveFile l 1]
       in any
            ( \mf -> case mf of
                Just f' -> isEnemyPiece Pawn f' pawnRank
                _ -> False
            )
            pawnFiles
    anyKnightAttacking =
      any
        ( \(df, dr) -> case (moveFile l df, moveRank n dr) of
            (Just f', Just r') -> isEnemyPiece Knight f' r'
            _ -> False
        )
        [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
    anyRookAttacking = any (\dir -> checkDirection dir [Rook, Queen] l n) [(0, 1), (0, -1), (1, 0), (-1, 0)]
    anyBishopAttacking = any (\dir -> checkDirection dir [Bishop, Queen] l n) [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    anyKingAttacking =
      let offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
       in any
            ( \(df, dr) -> case (moveFile l df, moveRank n dr) of
                (Just f', Just r') -> isEnemyPiece King f' r'
                _ -> False
            )
            offsets

isCheckForColor :: Board -> PieceColor -> Bool
isCheckForColor b col = isSquareUnderAttackFromColor b (getKingLocation b col) (if col == White then Black else White)

increaseDir :: (Int, Int) -> (Int, Int)
increaseDir (f, r) = (nextF, nextR)
  where
    nextF | f < 0 = f - 1 | f > 0 = f + 1 | otherwise = f
    nextR | r < 0 = r - 1 | r > 0 = r + 1 | otherwise = r

getInDir :: Board -> (Int, Int) -> PieceColor -> PieceColor -> Square -> [Move] -- TODO refactor to remove this as checkDirection does the same thing essentially
getInDir board (df, dr) ownColor enemyColor (f, r) = case (moveFile f df, moveRank r dr) of
  (Just nextF, Just nextR) ->
    case pieceAt board nextF nextR of
      Nothing ->
        Move (f, r) (nextF, nextR) Nothing False False
          : getInDir board (increaseDir (df, dr)) ownColor enemyColor (f, r)
      Just (Piece col pType) ->
        ([Move (f, r) (nextF, nextR) Nothing True False | col == enemyColor])
  _ -> []

getPseudoPossibleMovesForSquare :: Board -> Square -> [Move] -- TODO refactor for readability
getPseudoPossibleMovesForSquare b sq = case pieceAt b l n of
  Nothing -> []
  Just (Piece c Pawn) -> pawnMoves
  Just (Piece c Knight) -> knightMoves
  Just (Piece c King) -> kingMoves
  Just (Piece c Bishop) -> bishopMoves
  Just (Piece c Rook) -> rookMoves
  Just (Piece c Queen) -> queenMoves
  where
    (l, n) = sq
    color = case pieceAt b l n of
      Just (Piece c _) -> c
      Nothing -> toMove b
    enemyColor = if color == White then Black else White
    forward = if color == White then 1 else -1
    startRank = if color == White then 2 else 7

    isEnemyAt f' r' = case pieceAt b f' r' of
      Just (Piece col _) -> col == enemyColor
      _ -> False

    mkPawnMoves :: Square -> Square -> Bool -> Bool -> [Move]
    mkPawnMoves fromSq toSq@(_, rank) capt isEnPassant
      | rank == 8 || rank == 1 = [Move fromSq toSq (Just pt) capt isEnPassant | pt <- [Queen, Rook, Bishop, Knight]]
      | otherwise = [Move fromSq toSq Nothing capt isEnPassant]

    classifyMove (f', r') = case pieceAt b f' r' of
      Nothing -> Just (Move sq (f', r') Nothing False False)
      Just (Piece col _) ->
        if col /= color
          then Just (Move sq (f', r') Nothing True False)
          else Nothing
    pawnMoves =
      let oneStep = n + forward
          twoStep = n + (forward * 2)

          canStep1 = squareEmpty b l oneStep
          canStep2 = n == startRank && canStep1 && squareEmpty b l twoStep

          steps = [mkPawnMoves sq (l, oneStep) False False | canStep1] ++ [mkPawnMoves sq (l, twoStep) False False | canStep2]

          captureFiles = catMaybes [moveFile l (-1), moveFile l 1]
          captures =
            [ mkPawnMoves sq (f', oneStep) True (enPassantSq b == Just (f', oneStep))
              | f' <- captureFiles,
                isEnemyAt f' oneStep || enPassantSq b == Just (f', oneStep)
            ]
       in concat steps ++ concat captures
    knightMoves =
      let potentialSquares =
            [ (f', r')
              | (df, dr) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)],
                Just f' <- [moveFile l df],
                Just r' <- [moveRank n dr]
            ]
       in mapMaybe classifyMove potentialSquares
    kingMoves =
      let potentialSquares =
            [ (f', r')
              | (df, dr) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)],
                Just f' <- [moveFile l df],
                Just r' <- [moveRank n dr]
            ]
          queenSideEmptySquares = if color == White then [('b', 1), ('c', 1), ('d', 1)] else [('b', 8), ('c', 8), ('d', 8)]
          kingSideEmptySquares = if color == White then [('f', 1), ('g', 1)] else [('f', 8), ('g', 8)]

          queenSideSafeSquares = if color == White then [('e', 1), ('d', 1), ('c', 1)] else [('e', 8), ('d', 8), ('c', 8)]
          kingSideSafeSquares = if color == White then [('e', 1), ('f', 1), ('g', 1)] else [('e', 8), ('f', 8), ('g', 8)]

          kingSideToSquare = if color == White then ('g', 1) else ('g', 8)
          queenSideToSquare = if color == White then ('c', 1) else ('c', 8)

          areSquaresSafe sqs = not (any (\sq' -> isSquareUnderAttackFromColor b sq' enemyColor) sqs)
          areSquaresEmpty = all (uncurry (squareEmpty b))

          qSideRight = if color == White then WhiteQueenSide else BlackQueenSide
          kSideRight = if color == White then WhiteKingSide else BlackKingSide

          queenSideCastleMoves =
            if elem qSideRight (castling b)
              && areSquaresEmpty queenSideEmptySquares
              && areSquaresSafe queenSideSafeSquares
              then [Move sq queenSideToSquare Nothing False False]
              else []

          kingSideCastleMoves =
            if elem kSideRight (castling b)
              && areSquaresEmpty kingSideEmptySquares
              && areSquaresSafe kingSideSafeSquares
              then [Move sq kingSideToSquare Nothing False False]
              else []
       in mapMaybe classifyMove potentialSquares ++ queenSideCastleMoves ++ kingSideCastleMoves
    bishopMoves =
      let dirs = [(1, 1), (-1, -1), (-1, 1), (1, -1)]
       in concat [getInDir b x color enemyColor sq | x <- dirs]
    rookMoves =
      let dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
       in concat [getInDir b x color enemyColor sq | x <- dirs]
    queenMoves =
      let dirs = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)]
       in concat [getInDir b x color enemyColor sq | x <- dirs]

updateBoardSimple :: Board -> Move -> Board
updateBoardSimple board@(Board squares toMove status history castling enPassant moveCount counter) move@(Move from to promotion captureBool enPassantBool) =
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
      newCounter = if captureBool || movingPiece == Just (Piece toMove Pawn) then 0 else 0.5 + counter
      updateSquare (sq, p)
        | sq == from = (sq, Nothing)
        | sq == to = (sq, movingPiece)
        | enPassantBool && sq == (fst to, snd from) = (sq, Nothing)
        | isC && sq == rFrom = (sq, Nothing) -- Remove Rook from old square
        | isC && sq == rTo = (sq, lookup rFrom squares >>= id) -- Add Rook to new square
        | otherwise = (sq, p)

      newSquares = map updateSquare squares
      newToMove = if toMove == White then Black else White
   in Board newSquares newToMove Normal history castling enPassant moveCount newCounter

isMoveLegal :: Board -> Move -> Bool
isMoveLegal board move = not (isCheckForColor (updateBoardSimple board move) (toMove board))

getAllPseudoLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllPseudoLegalMovesForColor b color = concat [getPseudoPossibleMovesForSquare b sq | (sq, Just p) <- squares b, pieceColor p == color]

getAllLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllLegalMovesForColor b col = filter (isMoveLegal b) (getAllPseudoLegalMovesForColor b col)

isCheckmate :: Board -> Bool
isCheckmate b = isCheckForColor b (toMove b) && null (getAllLegalMovesForColor b (toMove b))

countPieceAndColor :: [(Square, Maybe Piece)] -> PieceColor -> PieceType -> Int -> Int
countPieceAndColor [] _ _ count = count
countPieceAndColor ((_, Just (Piece color pType)) : xs) col refType count =
  if color == col && pType == refType
    then countPieceAndColor xs col refType (count + 1)
    else countPieceAndColor xs col refType count

existsPieceAndColor :: [(Square, Maybe Piece)] -> PieceColor -> PieceType -> Bool
existsPieceAndColor [] _ _ = False
existsPieceAndColor ((_, Just (Piece color pType)) : xs) col refType =
  (color == col && pType == refType) || existsPieceAndColor xs col refType

existsPiece :: [(Square, Maybe Piece)] -> PieceType -> Bool
existsPiece [] _ = False
existsPiece ((_, Just (Piece color pType)) : xs) refType =
  (pType == refType) || existsPiece xs refType
existsPiece ((_, Nothing) : xs) refType =
  existsPiece xs refType

isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial (Board squares _ _ _ _ _ _ _) =
  not
    ( existsPiece squares Queen
        || existsPiece squares Rook
        || existsPiece squares Pawn
        || existsPieceAndColor squares Black Bishop && existsPieceAndColor squares Black Knight
        || existsPieceAndColor squares White Bishop && existsPieceAndColor squares White Knight
        || countPieceAndColor squares Black Knight 0 >= 2
        || countPieceAndColor squares White Knight 0 >= 2
        || countPieceAndColor squares Black Bishop 0 >= 2 -- TODO: check colors as well 
        || countPieceAndColor squares White Bishop 0 >= 2
    )

is3FoldRepetition :: Board -> Bool
is3FoldRepetition b = False -- TODO implement

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

createNewBoard :: Board -> Move -> [(Square, Maybe Piece)] -> Maybe Square -> Board
createNewBoard (Board _ toMove _ history castling _ moveCount counter) move newSquares newEnPassant =
  let newCastling = updateCastlingRights castling move
      newToMove = if toMove == White then Black else White

      newHistory = move : history

      nextBoardNoStatus = Board newSquares newToMove Normal newHistory newCastling newEnPassant moveCount counter
      newStatus
        | isCheckmate nextBoardNoStatus = CheckMate
        | isStalemate nextBoardNoStatus = StaleMate
        | isCheckForColor nextBoardNoStatus newToMove = Check
        | isDraw nextBoardNoStatus = Draw
        | otherwise = Normal

      newCounter = if isCapture move || uncurry (pieceAt nextBoardNoStatus) (to move) == Just (Piece toMove Pawn) then 0 else counter + 0.5
   in Board newSquares newToMove newStatus newHistory newCastling newEnPassant (moveCount + 0.5) (newCounter)

updateBoard :: Board -> Move -> Board
updateBoard board@(Board squares toMove _ _ _ _ _ counter) move@(Move from to promotion _ enPassantBool)
  | isCastling board move =
      let (rFrom, rTo) = case to of
            ('g', 1) -> (('h', 1), ('f', 1))
            ('c', 1) -> (('a', 1), ('d', 1))
            ('g', 8) -> (('h', 8), ('f', 8))
            ('c', 8) -> (('a', 8), ('d', 8))
            _ -> error "Invalid castling move"
          newSquares =
            map
              ( \(s, p) ->
                  ( if (s == from) || (s == rFrom) -- TODO refactor for readability
                      then (s, Nothing)
                      else
                        ( if s == to
                            then (s, Just (Piece toMove King))
                            else
                              if s == rTo
                                then (s, Just (Piece toMove Rook))
                                else (s, p)
                        )
                  )
              )
              squares
       in createNewBoard board move newSquares Nothing
  | enPassantBool =
      let capturedPawnSq = (fst to, snd from)
          newSquares =
            map
              ( \(s, p) ->
                  if s == from
                    then (s, Nothing)
                    else
                      if s == to
                        then (s, Just (Piece toMove Pawn))
                        else
                          if s == capturedPawnSq
                            then (s, Nothing)
                            else (s, p)
              )
              squares
       in createNewBoard board move newSquares Nothing
  | otherwise =
      let fromPiece = lookup from squares >>= id
          movingPiece = case promotion of
            Nothing -> fromPiece
            Just pt -> Just (Piece toMove pt)
          newSquares =
            map
              ( \(s, p) ->
                  if s == from
                    then (s, Nothing)
                    else
                      if s == to
                        then (s, movingPiece)
                        else (s, p)
              )
              squares
          newEnPassant = case movingPiece of
            Just (Piece c Pawn) ->
              if abs (snd from - snd to) == 2
                then Just (fst from, if c == White then snd to - 1 else snd to + 1)
                else Nothing
            _ -> Nothing
       in createNewBoard board move newSquares newEnPassant

simulateGame :: Board -> Board -- TODO proper implementation of game handler that can later be integrated with gui
simulateGame b =
  let moves = getAllLegalMovesForColor b (toMove b)
   in case moves of
        [] -> b
        m : xs ->
          if status b == Normal || status b == Check
            then simulateGame (updateBoard b m)
            else b
