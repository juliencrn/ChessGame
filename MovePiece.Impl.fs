module ChessGame.MovePiece.Impl

open ChessGame.Common
open ChessGame.Api
open ChessGame.Helper.LegalMoves
open ChessGame.Helper.Adapters

// --- life cycle, list of states ---

type MoveInput =
    { fromPosition: Position
      toPosition: Position }

type TryMovePiece = GameState -> MoveInput -> Result<GameState, GameError>

let getCamp (gameState: GameState) =
    match gameState.status with
    | Win camp ->
        let errMessage = sprintf "Game is already won by %s" (camp.ToString())
        Error(GameError errMessage)
    | InProgress camp -> Ok camp

let findCell (gameState: GameState) (position: Position) =
    let optionToResult' = optionToResult (GameError "Piece not found")

    gameState.board
    |> List.tryFind (fun cell -> cell.position = position)
    |> optionToResult'

let pickMyPiece (gameState: GameState) (position: Position) : Result<Piece, GameError> =
    result {
        let! camp = getCamp gameState

        let! piece =
            findCell gameState position
            |> Result.bind (fun cell ->
                match cell.state with
                | Empty -> Error(GameError "There is no piece at this location")
                | Occupied piece -> Ok piece)
            |> Result.bind (fun piece ->
                match piece.camp = camp with
                | false -> Error(GameError "You cannot move your adverser's pieces")
                | true -> Ok piece)

        return piece
    }



// findCell gameState position
// |> Result.bind
// Error(GameError "Unimplemented")

// let findPiece (gameState: GameState) (position: Position) (camp: Camp) : Result<Piece, GameError> =
//     let optionToResult' = optionToResult (GameError "Piece not found")

//     gameState.board
//     |> List.tryFind (fun cell -> cell.position = position)
//     |> optionToResult'
//     |> Result.bind (fun cell ->
//         match cell.state with
//         | Empty -> Error(GameError "There is no piece at this location")
//         | Occupied piece -> Ok piece)
//     |> Result.bind (fun piece ->
//         match piece.camp = camp with
//         | false -> Error(GameError "You cannot move your adverser's pieces")
//         | true -> Ok piece)

let getLegalMoves (position: Position) (piece: Piece) = getLegalMoveFn piece position

let isLegalMove (position: Position) (legalMoves: Position list) : bool = legalMoves |> List.contains position


// Note, move a piece depends on this piece;
// for the Knight, it's a direct move
// for the Rook, we'll go forward one case at the time and stop when the cell isn't empty

let getAdverseCamp (camp: Camp) : Camp =
    match camp with
    | White -> Black
    | Black -> White

// let moveKnight
//     (gameState: GameState)
//     (fromPosition: Position)
//     (pieceToMove: Piece)
//     (toPosition: Position)
//     : Result<GameState, GameError> =
// is legal move
// let piece = findPiece gameState fromPosition

// result {
//     let! piece =
//     let! camp = getCamp gameState
// }

// is Occupied
// // none -> ok
// // Occupied
// // // By me -> Error
// // // By other, eat it
// let optionToResult' = optionToResult (GameError "Destination cell not found")

// Do not extract this fn, no check is performed, only replace (unsafe)
// let flip (fromPosition: Position) (piece: Piece) (toPosition: Position) (cell: Cell) : Cell =
//     if cell.position = fromPosition then
//         { cell with state = Empty }
//     elif cell.position = toPosition then
//         { cell with state = (Occupied piece) }
//     else
//         cell

// let preparedFlip = flip fromPosition pieceToMove toPosition

// let destination =
//     gameState.board
//     |> List.tryFind (fun cell -> cell.position = toPosition)
//     |> optionToResult'



// let board : Result<Cell list, GameError> =
//     gameState.board
//     |> List.map (fun cell ->
//         match cell.state with
//         | Empty -> Ok({ cell with state = (Occupied piece) }) // ok move
//         | Occupied piece ->
//             if piece.camp = pieceToMove.camp then
//                 Error(GameError "You cannot eat your own piece")
//             else
//                 Ok(gameState.board |> List.map (fun cell -> preparedFlip cell)))
//     |> transformResultsToListResult

// board
// |> Result.bind (fun cells ->
//     Ok(
//         { board = cells
//           state = InProgress(getAdverseCamp White) }
//     ))

// match board with
// | Ok cells -> ({ board = cells; state = InProgress (getAdverseCamp White) })
// | Error err -> Error err

// gameState.board
// |>

// |> Result.bind (fun cell ->
//     match cell.state with
//     | Empty -> Ok (
//             gameState.board
//             |> List.map (fun cell -> preparedFlip cell)
//         ) // ok move
//     | Occupied piece ->
//         if piece.camp = pieceToMove.camp then
//             Error(GameError "You cannot eat your own piece")
//         else
//             Ok (
//                 gameState.board
//                 |> List.map (fun cell -> preparedFlip cell)
//             )
// |> Result.bind

// // TODO Check end-of-game

// // Update board
// { boa }




// --- Impl ---
let tryMovePiece: TryMovePiece =
    fun
        gameState
        { fromPosition = fromPosition
          toPosition = toPosition } ->


        result {
            let! camp = getCamp gameState
            let! piece = pickMyPiece gameState fromPosition
            let legalMoves = getLegalMoves fromPosition piece

            // TODO: Add logic (no empty cell by example)
            let updatedBoard =
                gameState.board
                |> List.map (fun cell ->
                    if cell.position = fromPosition then
                        { cell with state = Empty }
                    elif cell.position = toPosition then
                        { cell with state = (Occupied piece) }
                    else
                        cell)

            // TODO: Check end-of-game
            let newCamp = getAdverseCamp camp
            let updatedStatus = InProgress newCamp

            let updatedGameStatus =
                { board = updatedBoard
                  status = updatedStatus }

            return updatedGameStatus
        }


// partial application
// let getLegalMove' = getLegalMoves fromPosition
// let findPiece' = findPiece gameState
// let isLegalMoveForToPosition = isLegalMove toPosition

// let validateIsLegalMove (legalMoves: Position list) =
//     predicateToPassThrough (GameError "This move isn't authorized") isLegalMoveForToPosition

// let legalMoves =
// gameState
// |> getCamp
// |> Result.bind (findPiece' fromPosition)
// |> Result.bind (fun piece -> Ok(getLegalMove' piece))
// |> Result.bind (fun legalMoves ->
//     match isLegalMove toPosition legalMoves with
//     | true -> true
//     | false -> false)


// if is a legal move
// try to move it by doing it reccursively (to be blocked if case is occupied or ...)
// this will generate a new board each time
// or throw an error

// let rec execMove








// return updated gameState

// Error(GameError "Not implemented")

// TODO: How to avoid too-deep nesting? -> Split into smaller functions and compose
// let pickPiece: PickPiece =
//     fun state position ->
//         match state.status with
//         | Win _ -> Error(MakeMoveError "Game terminated")
//         | InProgress camp ->

//             // TODO: This will Throw Exception anything is found
//             let cell = state.board |> List.find (fun c -> c.position = position)

//             match cell.state with
//             | Empty -> Error(MakeMoveError "Price not found")
//             | Occupied piece ->
//                 if piece.camp = camp then
//                     Ok(
//                         { piece = piece
//                           position = cell.position }
//                     )
//                 else
//                     Error(MakeMoveError "Not your piece")

//     let pieces = gameState.board |> List.filter (fun cell ->
//         match cell.state with
//         | Empty -> false
//         | Occupied piece -> piece.camp = camp
//     )

//     let piece = { camp = White; kind = Knight }
//     let pickedPiece = { position = position; piece = piece }
// Ok pickedPiece
// Error (MakeMoveError "-")



// / Input validation
// type PiecePicked = { position: Position; piece: Piece }

// / Checks is the move is Legal and move and return it (board update)
// type PieceMoved = PieceMoved of GameState

// / Step state stages
// type MovingState =
// | MoveInput of MoveInput
// | PiecePicked of PiecePicked
// | PieceMoved of PieceMoved

// --- Internal steps ---

// validate selected piece
// type PickPiece = GameState -> MoveInput -> Result<PiecePicked, GameError>

// validate move
// type MoveInput = { from: PiecePicked; moveTo: Position }

// type MovePiece = GameState -> MoveInput -> Result<PieceMoved, GameError>

// validate end-of-game
// generate next available piece
// return updated GameState

// type UpdateGameState = GameState -> Result<GameStateUpdated, GameError>




// type MovePieceInput = { from: Position; moveTo: Position }

// type GameError =
//     | OutofBoard
//     | GameAlreadyEnded
//     | NoPieceLeftToMove

// type MovePieceWorkflow = GameState -> MovePieceInput -> Result<GameState, GameError>

// let getAvailableWhitePieces (gameState: GameState): Result<Cell list, GameError> =
//     match gameState.status with
//         | Win _ -> Error GameAlreadyEnded
//         | InProgress camp ->
//             let pieces = gameState.board |> List.filter (fun cell ->
//                 match cell.state with
//                 | Empty -> false
//                 | Occupied piece -> piece.camp = camp
//             )

//             match (NonEmptyList.create pieces) with
//             | Error _ -> Error NoPieceLeftToMove
//             | Ok pieces' -> Ok (NonEmptyList.value pieces')

// let movePiece: MovePieceWorkflow =
//     fun gameState ->
//         fun { from = from; moveTo = moveTo } ->
//             // validate selected piece using getAvailablePiecesForPlayerX






//             // Validate move
//             // Check if game is won
//             // Update gameState
//             // Generate next available piece to move
//             // Return updates gameState with next available piece for the pther player
//             Error OutofBoard
