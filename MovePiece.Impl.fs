module ChessGame.MovePiece.Impl

open ChessGame.Common
open ChessGame.Api

// --- Sub steps ---

let validateLegalMove (board: Board) (legalMoves: Position list) (toPosition: Position) =
    if List.contains toPosition legalMoves then
        Ok board
    else
        Error(GameError "Not allowed move")

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
                   pickedPiece = { legalMoves = legalMoves
                                   piece = piece
                                   position = fromPosition } } } ->

        validateLegalMove board legalMoves toPosition
        |> Result.bind (applyMoveOnBoard fromPosition toPosition piece)
        |> Result.bind (toGameState camp)
