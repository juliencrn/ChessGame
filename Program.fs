open App.Gui
open Domain.Types
open Domain.Api
open Domain.InitializeGame
open Domain.TryMovePiece

let getUserInput () =
    printf "Piece to move: "
    let fromPosition = askUserPosition ()

    printf "To position: "
    let toPosition = askUserPosition ()

    { fromPosition = fromPosition
      toPosition = toPosition }

let rec askMoveUntilOk gameState =
    let moveInput = getUserInput ()
    let moveResult = tryMovePiece gameState moveInput

    match moveResult with
    | Error(GameError errMessage) ->
        printfn "Error: %s" errMessage
        askMoveUntilOk gameState
    | Ok newGameState -> newGameState

let rec gameLoop (gameState: GameState) =
    drawGame gameState

    match gameState.status with
    | Win camp -> printfn "Checkmate. %s player win! " (camp.ToString())
    | InProgress camp -> gameLoop (askMoveUntilOk gameState)


[<EntryPoint>]
let main argv =
    let initialGameState = initializeGame ()
    gameLoop initialGameState
    0
