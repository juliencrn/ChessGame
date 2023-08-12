module ChessGame.MovePiece.Impl

open ChessGame.Common
open ChessGame.Api
open ChessGame.Helper.Adapters
open ChessGame.Helper.ValidPositions

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

// --- EndOfGame's helpers ---

let getPieceByCamp (board: Board) (camp: Camp) =
    board
    |> List.map (fun c ->
        match c.state with
        | Empty -> None
        | Occupied piece ->
            match piece.camp = camp with
            | true -> Some(piece, c.position)
            | false -> None)
    |> filterNonePositions



let getKingPosition (board: Board) (camp: Camp) =
    board
    |> List.tryFind (fun c ->
        match c.state with
        | Empty -> false
        | Occupied piece -> piece.camp = camp && piece.kind = King)
    |> Option.bind (fun cell ->
        let piece = Piece.create camp King
        Some(cell.position))

let isKingSafe (board: Board) (camp: Camp) (kingPosition: Position) : bool =
    let adverseValidMoves =
        getPieceByCamp board (Camp.getAdverse camp)
        |> List.map (fun (piece, position) -> getValidPositionsFn board camp piece position)
        |> List.concat
        |> List.distinct

    not (List.contains kingPosition adverseValidMoves)

// --- Saving strategies ---

// 1) move the King
let canTheKingBeSavedByMovingIt (board: Board) (camp: Camp) (kingPosition: Position) : bool =
    let saveTheKingByMovingItPositions: Position list =
        getKingValidPositions board camp kingPosition
        |> List.filter (fun p -> isKingSafe board camp p)

    not (List.isEmpty saveTheKingByMovingItPositions)

// 2) capturing the menace (and become safe)
let canTheKingBeSavedByCapturingTheMenace (gameState: GameState) (camp: Camp) (kingPosition: Position) : bool =
    let dangerousPieces =
        getPieceByCamp gameState.board (Camp.getAdverse camp)
        |> List.map (fun (piece, position) ->
            (piece, position, getValidPositionsFn gameState.board camp piece position))
        |> List.filter (fun (piece, position, validMoves) -> List.contains kingPosition validMoves)
        |> List.map (fun (piece, position, _) -> (piece, position))

    match List.length dangerousPieces with
    // if only one attacking piece, try to capture it
    | 1 ->
        let (piece, position) = dangerousPieces[0]

        // take all my piece and check if any of them can capture the target
        let possibleMovesFromMyPieces =
            getPieceByCamp gameState.board camp
            |> List.map (fun (piece, position) -> getValidPositionsFn gameState.board camp piece position)
            |> List.concat
            |> List.distinct

        let hasCapturingMove = List.contains position possibleMovesFromMyPieces

        hasCapturingMove
    | _ -> false


// 3) block the menage with one of my pieces (and become safe)
// If my piece's validMoves are crossing danger zone, try the move, create an updated board copy and re-check isKingSafe
let canTheKingBeSavedByBlockingTheAttacker (gameState: GameState) (camp: Camp) (kingPosition: Position) : bool =
    let dangerCells =
        getPieceByCamp gameState.board (Camp.getAdverse camp)
        |> List.map (fun (piece, position) -> getValidPositionsFn gameState.board camp piece position)
        |> List.concat
        |> List.distinct

    let myPossibleMoves =
        getPieceByCamp gameState.board camp
        |> List.filter (fun (piece, _) -> piece.kind <> King)
        |> List.map (fun (piece, position) ->
            let validMoves = getValidPositionsFn gameState.board camp piece position
            (piece, position, validMoves))

    let matchingMovesByPieces =
        myPossibleMoves
        |> List.map (fun (piece, position, validMoves) ->
            // For each piece
            let matching = validMoves |> List.filter (fun pos -> List.contains pos dangerCells)

            (piece, position, matching))
        |> List.filter (fun (_, _, moves) -> List.length moves > 0)

    let canTheKingBeSavedByBlockingTheAttacker =
        let validSavingMove =
            matchingMovesByPieces
            |> List.filter (fun (piece, position, moves) ->
                // For each piece: simulate the move by creating a tmp new board
                let hasAnySavingMoves =
                    moves
                    |> List.tryFind (fun newPos ->
                        let moveResult =
                            validateMove
                                gameState.board
                                (getValidPositionsFn gameState.board camp piece newPos)
                                newPos
                            |> Result.bind (applyMoveOnBoard position newPos piece)

                        let canSave =
                            match moveResult with
                            | Ok(simulatedBoard, _) -> isKingSafe simulatedBoard camp kingPosition
                            | Error _ -> false

                        canSave)
                    |> Option.isSome

                hasAnySavingMoves)

        List.length validSavingMove > 0

    canTheKingBeSavedByBlockingTheAttacker

/// Check end of game
/// 3 ways to save the king (else is checkmate)
/// - move the King (somewhere safe)
/// - capturing the menace (and become safe)
/// - block the menage with one of my pieces (and become safe)
let evaluateEndOfGame (camp: Camp) (gameState: GameState) : Result<GameState, GameError> =
    let currentPlayerLooses (gameState: GameState) : Result<GameState, GameError> =
        Ok
            { gameState with
                status = Win(Camp.getAdverse camp) }

    match getKingPosition gameState.board camp with
    | None -> currentPlayerLooses gameState
    | Some kingPosition ->
        match isKingSafe gameState.board camp kingPosition with
        // King is safe, continue game
        | true -> Ok gameState
        // King is in trouble, check if it's checkmate
        | false ->
            let canTheKingBeSaved =
                (canTheKingBeSavedByMovingIt gameState.board camp kingPosition)
                || (canTheKingBeSavedByCapturingTheMenace gameState camp kingPosition)
                || (canTheKingBeSavedByBlockingTheAttacker gameState camp kingPosition)

            match canTheKingBeSaved with
            | true -> Ok gameState
            | false -> currentPlayerLooses gameState


// --- Workflow ---

// TODO: When Pawn reach the adverse last line, it can be changed to any lost piece
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
        |> Result.bind (evaluateEndOfGame camp)
