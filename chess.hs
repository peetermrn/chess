module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  print "Start"

data PieceType
  = Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King
  deriving (Eq, Show)

data PieceColor = White | Black
  deriving (Eq, Show)

data Piece = Piece
  { pieceColor :: PieceColor,
    pieceType :: PieceType
  }
  deriving (Eq, Show)

data GameStatus
  = Normal
  | Check
  | CheckMate
  | StaleMate
  deriving (Eq, Show)

type Square = (Char, Int)

data Board = Board
  { squares :: [(Square, Maybe Piece)],
    toMove :: PieceColor,
    status :: GameStatus
  }
  deriving (Eq, Show)

allSquares :: [Square]
allSquares = [(f, r) | r <- [1 .. 8], f <- ['a' .. 'h']]

initChessBoard :: Board
initChessBoard =
  Board (map getInitialPiece allSquares) White Normal
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

data Move = Move
  { from :: Square,
    to :: Square,
    promotion :: Maybe PieceType,
    isCapture :: Bool
  }
  deriving (Eq, Show)

data CastlingRights = WhiteKingSide | WhiteQueenSide | BlackQueenSide | BlackKingSide

data Game = Game
  { board :: Board,
    history :: [Move],
    lastMove :: Maybe Move,
    castling :: [CastlingRights],
    enPassantSq :: Maybe Square,
    moveCount :: Int
  }

initializeGame :: Game
initializeGame =
  Game
    { board = initChessBoard,
      history = [],
      lastMove = Nothing,
      castling = [WhiteKingSide, WhiteQueenSide, BlackQueenSide, BlackKingSide],
      enPassantSq = Nothing,
      moveCount = 0
    }

getKingLocation :: Board -> PieceColor -> Square
getKingLocation b color =
  case find isMatchingPiece (squares b) of
    Just (sq, _) -> sq
    Nothing -> error (show color ++ " king missing from board.")
  where
    isMatchingPiece :: (Square, Maybe Piece) -> Bool
    isMatchingPiece (_, Just (Piece c King)) = c == color
    isMatchingPiece _ = False

pieceAt :: Board -> Char -> Int -> Maybe Piece
pieceAt b l n =
  case lookup (l, n) (squares b) of
    Just (Just p) -> Just p
    _ -> Nothing

squareEmpty :: Board -> Char -> Int -> Bool
squareEmpty b l n =
  n >= 1 && n <= 8 && l >= 'a' && l <= 'h' && isNothing (pieceAt b l n)

mkPawnMoves :: Square -> Square -> Bool -> [Move]
mkPawnMoves fromSq toSq@(_, rank) capt
  | rank == 8 || rank == 1 = [Move fromSq toSq (Just pt) capt | pt <- [Queen, Rook, Bishop, Knight]]
  | otherwise = [Move fromSq toSq Nothing capt]

moveFile :: Char -> Int -> Maybe Char
moveFile f offset =
  let newF = chr (ord f + offset)
   in if newF >= 'a' && newF <= 'h' then Just newF else Nothing

moveRank :: Int -> Int -> Maybe Int
moveRank r offset =
  let newR = r + offset
   in if newR >= 1 && newR <= 8 then Just newR else Nothing

isCheckForColor :: Board -> PieceColor -> Bool
isCheckForColor b col =
  anyPawnAttacking || anyKnightAttacking || anyRookAttacking || anyBishopAttacking || anyKingAttacking
  where
    kingLoc = getKingLocation b col
    (l, n) = kingLoc
    enemyColor = if col == White then Black else White

    isEnemyPiece pType f' r' = case pieceAt b f' r' of
      Just (Piece col p') -> col == enemyColor && p' == pType
      _ -> False

    checkDirection (df, dr) targets currF currN =
      case (moveFile currF df, moveRank currN dr) of
        (Just nextF, Just nextR) ->
          case pieceAt b nextF nextR of
            Nothing -> checkDirection (df, dr) targets nextF nextR
            Just (Piece col pType) -> col == enemyColor && elem pType targets
        _ -> False

    anyPawnAttacking =
      let pawnRank = if col == White then n + 1 else n - 1
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

increaseDir :: (Int, Int) -> (Int, Int)
increaseDir (f, r) =
  (nextF, nextR)
  where
    nextF | f < 0 = f - 1 | f > 0 = f + 1 | otherwise = f
    nextR | r < 0 = r - 1 | r > 0 = r + 1 | otherwise = r

getInDir :: Board -> (Int, Int) -> PieceColor -> PieceColor -> Square -> [Move]
getInDir board (df, dr) ownColor enemyColor (f, r) =
  case (moveFile f df, moveRank r dr) of
    (Just nextF, Just nextR) ->
      case pieceAt board nextF nextR of
        Nothing ->
          Move (f, r) (nextF, nextR) Nothing False
            : getInDir board (increaseDir (df, dr)) ownColor enemyColor (f, r)
        Just (Piece col pType) ->
          ([Move (f, r) (nextF, nextR) Nothing True | col == enemyColor])
    _ -> []

getPseudoPossibleMovesForSquare :: Board -> Square -> [Move]
getPseudoPossibleMovesForSquare b sq =
  case pieceAt b l n of
    Nothing -> []
    Just (Piece color Pawn) -> pawnMoves
    Just (Piece color Knight) -> knightMoves
    Just (Piece color King) -> kingMoves
    Just (Piece color Bishop) -> bishopMoves
    Just (Piece color Rook) -> rookMoves
    Just (Piece color Queen) -> queenMoves
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

    classifyMove (f', r') =
      case pieceAt b f' r' of
        Nothing -> Just (Move sq (f', r') Nothing False)
        Just (Piece col _) ->
          if col /= color
            then Just (Move sq (f', r') Nothing True)
            else Nothing
    pawnMoves =
      let oneStep = n + forward
          twoStep = n + (forward * 2)

          canStep1 = squareEmpty b l oneStep
          canStep2 = n == startRank && canStep1 && squareEmpty b l twoStep

          steps = [mkPawnMoves sq (l, oneStep) False | canStep1] ++ [mkPawnMoves sq (l, twoStep) False | canStep2]

          captureFiles = catMaybes [moveFile l (-1), moveFile l 1]
          captures = [mkPawnMoves sq (f', oneStep) True | f' <- captureFiles, isEnemyAt f' oneStep]
       in -- TODO En Passant !!!!!!!
          -- would check if (f', oneStep) == enPassantSq game

          concat steps ++ concat captures
    knightMoves =
      let offsets = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
          potentialSquares =
            [ (f', r')
              | (df, dr) <- offsets,
                Just f' <- [moveFile l df],
                Just r' <- [moveRank n dr]
            ]
       in mapMaybe classifyMove potentialSquares
    kingMoves =
      let offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
          -- TODO CASTLING!!
          potentialSquares =
            [ (f', r')
              | (df, dr) <- offsets,
                Just f' <- [moveFile l df],
                Just r' <- [moveRank n dr]
            ]
       in mapMaybe classifyMove potentialSquares
    bishopMoves =
      let dirs = [(1, 1), (-1, -1), (-1, 1), (1, -1)]
          moves = [getInDir b x color enemyColor sq | x <- dirs]
       in concat moves
    rookMoves =
      let dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
          moves = [getInDir b x color enemyColor sq | x <- dirs]
       in concat moves

    queenMoves =
      let dirs = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)]
          moves = [getInDir b x color enemyColor sq | x <- dirs]
       in concat moves

updateBoardSimple :: Board -> Move -> Board -- no status change
updateBoardSimple (Board squares toMove _) (Move from to promotion _) =
  let newSquares = map updateSquare squares
      updateSquare (sq, p)
        | sq == from = (sq, Nothing)
        | sq == to = (sq, movingPiece)
        | otherwise = (sq, p)
      fromPiece = lookup from squares >>= id
      movingPiece = case promotion of
        Nothing -> fromPiece
        Just pt -> Just (Piece toMove pt)

      newToMove = if toMove == White then Black else White
   in Board newSquares newToMove Normal

isMoveLegal :: Board -> Move -> Bool
isMoveLegal board move = not (isCheckForColor (updateBoardSimple board move) (toMove board))

getAllPseudoLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllPseudoLegalMovesForColor b color =
  concat [getPseudoPossibleMovesForSquare b sq | (sq, Just p) <- squares b, pieceColor p == color]

getAllLegalMovesForColor :: Board -> PieceColor -> [Move]
getAllLegalMovesForColor b col = filter (isMoveLegal b) (getAllPseudoLegalMovesForColor b col)

isCheckmate :: Board -> Bool
isCheckmate b = isCheckForColor b (toMove b) && null (getAllLegalMovesForColor b (toMove b))

isStalemate :: Board -> Bool
isStalemate b = not (isCheckForColor b (toMove b)) && null (getAllLegalMovesForColor b (toMove b))

updateBoard :: Board -> Move -> Board
updateBoard (Board squares toMove _) (Move from to promotion _) =
  let newSquares = map updateSquare squares
      updateSquare (sq, p)
        | sq == from = (sq, Nothing) -- Piece left this square
        | sq == to = (sq, movingPiece) -- Piece arrived here
        | otherwise = (sq, p) -- No change
      fromPiece = lookup from squares >>= id
      movingPiece = case promotion of
        Nothing -> fromPiece
        Just pt -> Just (Piece toMove pt)

      newToMove = if toMove == White then Black else White

      nextBoardNoStatus = Board newSquares newToMove Normal

      newStatus
        | isCheckmate nextBoardNoStatus = CheckMate
        | isStalemate nextBoardNoStatus = StaleMate
        | isCheckForColor nextBoardNoStatus newToMove = Check
        | otherwise = Normal
   in Board newSquares newToMove newStatus

-- TODO change pretty much everything to work with the Game type not just board type to allow for easier castling/en passant logic
