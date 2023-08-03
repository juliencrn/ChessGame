module Gui

open Domain.Types
open Domain.Constants

let drawBoard (gameState: GameState) =
    let getPieceSymbol (piece: Piece) =
        match piece.kind with
        | King -> "K"
        | Queen -> "Q"
        | Rook -> "R"
        | Bishop -> "B"
        | Knight -> "N"
        | Pawn -> "P"

    let drawCell (cell: Cell) =
        match cell.state with
        | Occupied piece -> getPieceSymbol piece
        | Empty -> "-"

    let drawBoardSeparator () =
        printfn "   +---+---+---+---+---+---+---+---+"

    let drawRankLetterUnits () =
        printfn "     a   b   c   d   e   f   g   h"

    let drawRow (rankIndex: int) (cells: Cell list) =
        let rank = 8 - rankIndex
        printf " %d " rank

        let cellSymbols = List.map drawCell cells
        cellSymbols |> List.iter (fun symbol -> printf "| %s " symbol)

        printf "| %d" rank
        printfn ""
        drawBoardSeparator ()

    let to2DCellBoard (cells: Cell list) : Cell list list =
        RANKS
        |> List.map (fun rank -> cells |> List.filter (fun cell -> cell.position.rank = rank))



    printfn ""
    drawRankLetterUnits ()
    drawBoardSeparator ()

    gameState.board |> to2DCellBoard |> List.iteri drawRow

    drawRankLetterUnits ()

    printfn ""
    printfn "Game Status: %A" gameState.status
    printfn ""
