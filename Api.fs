module ChessGame.Api

open ChessGame.Common

// --- Input data ---

type PickPieceInput = { fromPosition: Position }

type MovePieceInput =
    { toPosition: Position
      pickedPiece: PickedPiece }

type Command<'T> =
    { data: 'T
      board: Board
      camp: Camp
      captured: Piece list }

type PickPieceCommand = Command<PickPieceInput>
type MovePieceCommand = Command<MovePieceInput>

// --- Public API ---

type GameError = GameError of string

type InitializeGame = unit -> GameState

type PickPiece = PickPieceCommand -> Result<GameState, GameError>

type MovePiece = MovePieceCommand -> Result<GameState, GameError>

// --- Helpers ---
