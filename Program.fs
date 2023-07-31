open Functions
open Gui
open Newtonsoft.Json


[<EntryPoint>]
let main argv =

    // show the empty chess board
    let game = initializeGame ()

    // Print json game data
    let json = JsonConvert.SerializeObject(game)
    printfn "%s" json

    // Draw CLI board
    drawBoard game

    // Ask the user which piece to move (by coor) (type by Camp)
    // Calc and return all legal moves for that piece

    // ask the user for an x,y coor where we will place a piece

    // Print the chess board
    0
