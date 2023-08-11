module ChessGame.MovePiece.Impl

open ChessGame.Common
open ChessGame.Api
open ChessGame.Helper.Adapters

// --- Sub steps ---

let validateMove (board: Board) (validPositions: Position list) (toPosition: Position) =
    if List.contains toPosition validPositions then
        Ok board
    else
        Error(GameError "This move is not allowed")

// TODO: Keep a list of Captured Pieces
let applyMoveOnBoard (fromPosition: Position) (toPosition: Position) (piece: Piece) (board: Board) =
    let newBoard =
        board
        |> List.map (fun cell ->
            if cell.position = fromPosition then
                { cell with state = Empty }
            elif cell.position = toPosition then
                { cell with state = (Occupied piece) }
            else
                cell)

    Ok newBoard

let toGameState (camp: Camp) (board: Board) =
    let status = PickingPiece(Camp.getAdverse camp)

    Ok { board = board; status = status }

// --- Workflow ---

// TODO: Check end-of-game
let movePiece: MovePiece =
    fun
        { board = board
          camp = camp
          data = { toPosition = toPosition
                   pickedPiece = { validPositions = validPositions
                                   piece = piece
                                   position = fromPosition } } } ->
        validateMove board validPositions toPosition
        |> Result.bind (applyMoveOnBoard fromPosition toPosition piece)
        |> Result.bind (toGameState camp)
