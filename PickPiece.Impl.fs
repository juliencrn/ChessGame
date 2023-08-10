module ChessGame.PickPiece.Impl

open ChessGame.Common
open ChessGame.Api
open ChessGame.Helper.LegalMoves
open ChessGame.Helper.Adapters

// --- Sub steps ---

let findCell (board: Board) (position: Position) =
    let optionToResultWithError = optionToResult (GameError "Piece not found")

    board
    |> List.tryFind (fun cell -> cell.position = position)
    |> optionToResultWithError

let validatePieceExists cell =
    match cell.state with
    | Empty -> Error(GameError "There is no piece at this location")
    | Occupied piece -> Ok piece

let validatePieceCamp camp (piece: Piece) =
    match piece.camp = camp with
    | false -> Error(GameError "You cannot move your adverser's pieces")
    | true -> Ok piece

let addLegalMoves position piece =
    let legalMoves = getLegalMoveFn piece position

    let pickedPiece: PickedPiece =
        { piece = piece
          position = position
          legalMoves = legalMoves }

    Ok pickedPiece

let toGameState board camp pickedPiece =
    let status = MovingPiece pickedPiece

    Ok { board = board; status = status }

// --- Workflow ---


// TODO: During legalMoves computation, check if the cell is empty,
//       | Empty -> Ok
//       | Occupied by adverse -> Ok once
//       | Occupied by me -> Error "Cannot capture your own piece"

// TODO: If legalMoves is Empty, we cannot move the piece and should return an Error
// TODO: Pawn could move twice when it is its first move
let pickPiece: PickPiece =
    fun
        { board = board
          camp = camp
          data = { fromPosition = fromPosition } } ->

        findCell board fromPosition
        |> Result.bind validatePieceExists
        |> Result.bind (validatePieceCamp camp)
        |> Result.bind (addLegalMoves fromPosition)
        |> Result.bind (toGameState board camp)
