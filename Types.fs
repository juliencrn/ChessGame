module Types

type RankPosition = RankPosition of int
type FilePosition = FilePosition of int

type CellPosition =
    { rank: RankPosition // row
      file: FilePosition } // columns

type Camp =
    | White
    | Black

type ChessPiece =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type Piece = { camp: Camp; kind: ChessPiece }

type CellState =
    | Occupied of Piece
    | Empty

type Cell =
    { position: CellPosition
      state: CellState }

type GameStatus =
    | InProgress of Camp
    | Win of Camp

// TODO: list list VS 2d array?
type GameState =
    { board: Cell list list
      status: GameStatus }
