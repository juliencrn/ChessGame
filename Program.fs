open ChessGame.Gui
open ChessGame.Common
open ChessGame.Api
open ChessGame.InitializeGame.Impl
open ChessGame.MovePiece.Impl
open ChessGame.PickPiece.Impl

let getFromPositon () =
    printf "Piece to move: "
    askUserPosition ()

let getToPositon () =
    printf "To postion: "
    askUserPosition ()

let rec recTryPickPiece board camp captured =
    let fromPosition = getFromPositon ()

    let pickPieceCommand =
        { board = board
          camp = camp
          captured = captured
          data = { fromPosition = fromPosition } }

    match pickPiece pickPieceCommand with
    | Error(GameError errMessage) ->
        printfn "Error: %s" errMessage
        recTryPickPiece board camp captured
    | Ok newGameState -> newGameState

let rec recTryMovePiece board captured (pickedPiece: PickedPiece) =
    let toPosition = getToPositon ()

    let movePieceCommand =
        { board = board
          camp = pickedPiece.piece.camp
          captured = captured
          data =
            { toPosition = toPosition
              pickedPiece = pickedPiece } }

    let result = movePiece movePieceCommand

    match result with
    | Error(GameError errMessage) ->
        printfn "Error: %s" errMessage
        recTryMovePiece board captured pickedPiece
    | Ok newGameState -> newGameState

let rec gameLoop (gameState: GameState) =
    drawGame gameState

    match gameState.status with
    | Win camp -> printfn "Checkmate. %s player win! " (camp.ToString())
    | PickingPiece camp ->
        let newGameState = recTryPickPiece gameState.board camp gameState.captured
        gameLoop newGameState
    | MovingPiece pickedPiece ->
        let newGameState = recTryMovePiece gameState.board gameState.captured pickedPiece
        gameLoop newGameState


[<EntryPoint>]
let main argv =
    let initialGameState = initializeGame ()
    gameLoop initialGameState
    0
