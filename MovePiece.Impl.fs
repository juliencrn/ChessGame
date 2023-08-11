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

let applyMoveOnBoard (fromPosition: Position) (toPosition: Position) (piece: Piece) (board: Board) =
    let captured =
        board
        |> List.tryFind (fun c -> c.position = toPosition)
        |> Option.bind (fun c ->
            match c.state with
            | Empty -> None
            | Occupied piece -> Some piece)

    let newBoard =
        board
        |> List.map (fun cell ->
            if cell.position = fromPosition then
                { cell with state = Empty }
            elif cell.position = toPosition then
                { cell with state = (Occupied piece) }
            else
                cell)

    Ok(newBoard, captured)

let toGameState (captured: Piece list) (camp: Camp) (changed: (Board * Piece option)) =
    let status = PickingPiece(Camp.getAdverse camp)
    let (board, capturedOption) = changed

    let captured' =
        match capturedOption with
        | Some capturedPiece -> (capturedPiece :: captured)
        | None -> captured

    Ok
        { board = board
          status = status
          captured = captured' }

// --- Workflow ---

// TODO: Check end-of-game
let movePiece: MovePiece =
    fun
        { board = board
          camp = camp
          captured = captured
          data = { toPosition = toPosition
                   pickedPiece = { validPositions = validPositions
                                   piece = piece
                                   position = fromPosition } } } ->
        validateMove board validPositions toPosition
        |> Result.bind (applyMoveOnBoard fromPosition toPosition piece)
        |> Result.bind (toGameState captured camp)
