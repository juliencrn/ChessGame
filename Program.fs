open App.Gui
open Domain.Types
open Domain.Api
open Domain.InitializeGame
open Domain.TryMovePiece
open System.Collections.Generic

type Input = Input of string
type DrawBoard = GameState -> unit

let createResult x = Ok x

type Xy = { x: int; y: int }

let exp x y =
    result {
        let! a = createResult x
        let! b = createResult y

        return { x = a; y = b }
    }


[<EntryPoint>]
let main argv =
    let mutable gameState = initializeGame ()
    let mutable continueLooping = true

    while continueLooping do
        // Draw CLI board
        drawBoard gameState

        // Ask the user to select a piece to move
        printfn "Type the position of the piece you want to move"
        let fromPosition = askUserPosition ()

        printfn "Type the destination position"
        let toPosition = askUserPosition ()

        let moveInput =
            { fromPosition = fromPosition
              toPosition = toPosition }

        let result = tryMovePiece gameState moveInput

        match result with
        | Ok updatedGameStatus ->
            match updatedGameStatus.status with
            | Win camp ->
                printfn "Checkmate. %s player win! " (camp.ToString())
                continueLooping <- false
            | InProgress camp ->
                printfn "Move succedeed!"
                printfn "It is now %s's move." (camp.ToString())
                gameState <- updatedGameStatus
        | Error(GameError err) ->
            printfn "Move encounted an Error: %s" err
            printfn "Please, re-try"

    // Dependency injection
    // let tryMovePiece' = tryMovePiece gameState

    // ()

    // // TryMovePiece workflow
    // let result = composePositions fromPosition toPosition |> Result.bind tryMovePiece'

    // match result with
    // | Ok updatedState ->
    //     gameState <- updatedState

    //     // End of game
    //     match gameState.status with
    //     | InProgress _ -> ()
    //     | Win player ->
    //         let playerStr =
    //             match player with
    //             | White -> "White"
    //             | Black -> "Black"

    //         printfn "%s player win!" playerStr
    //         continueLooping <- false
    //         ()

    //     ()
    // | Error(GameError err) -> printfn "Err %s" err

    0
