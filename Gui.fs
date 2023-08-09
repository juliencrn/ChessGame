module ChessGame.Gui

open System
open ChessGame.Helper.Adapters
open ChessGame.Common
open ChessGame.Api

let parsePosition (input: string) : Result<Position, GameError> =
    let column = input[0] |> Char.ToLower |> string
    let rowStr = input[1..]

    match Int32.TryParse(rowStr) with
    | true, row ->
        let rankResult = Rank.create row
        let fileResult = File.create column

        match combineResults rankResult fileResult with
        | Ok(rank, file) -> Ok({ rank = rank; file = file })
        | Error err -> Error(GameError err)

    | _ -> Error(GameError "Cannot parse input. Format like `a4` or `f3`")

let rec askUserPosition () = // : UnvalidatedPosition =
    let input = Console.ReadLine()

    match parsePosition input with
    | Ok position -> position
    | Error err ->
        printfn "Invalid input: %s" (err.ToString())
        askUserPosition ()


let drawBoard (board: Board) =
    let getPieceSymbol (piece: Piece) =
        match piece.camp, piece.kind with
        | Black, King -> "♚"
        | White, King -> "♔"
        | Black, Queen -> "♛"
        | White, Queen -> "♕"
        | Black, Rook -> "♜"
        | White, Rook -> "♖"
        | Black, Bishop -> "♝"
        | White, Bishop -> "♗"
        | Black, Knight -> "♞"
        | White, Knight -> "♘"
        | Black, Pawn -> "♟︎"
        | White, Pawn -> "♙"

    let drawCell (cell: Cell) =
        match cell.state with
        | Occupied piece -> getPieceSymbol piece
        | Empty -> " "

    let drawBoardTopSeparator () =
        printfn "   ┌───┬───┬───┬───┬───┬───┬───┬───┐"

    let drawBoardMiddleSeparator () =
        printfn "   ├───┼───┼───┼───┼───┼───┼───┼───┤"

    let drawBoardBottomSeparator () =
        printfn "   └───┴───┴───┴───┴───┴───┴───┴───┘"

    let drawRankLetterUnits () =
        printfn "     a   b   c   d   e   f   g   h"

    let drawRow (rankIndex: int) (cells: Cell list) =
        let rank = 8 - rankIndex
        printf " %d " rank

        let cellSymbols = List.map drawCell cells
        cellSymbols |> List.iter (fun symbol -> printf "│ %s " symbol)

        printf "│ %d" rank
        printfn ""

        match rank = 1 with
        | false -> drawBoardMiddleSeparator ()
        | true -> drawBoardBottomSeparator ()

    let to2DCellBoard (cells: Cell list) : Cell list list =
        Rank.getAll ()
        |> List.map (fun rank -> cells |> List.filter (fun cell -> cell.position.rank = rank))

    drawRankLetterUnits ()
    drawBoardTopSeparator ()
    board |> to2DCellBoard |> List.iteri drawRow
    drawRankLetterUnits ()


let drawStatus (gameStatus: GameStatus) =
    match gameStatus with
    | Win camp -> printfn "Checkmate. %s player win! " (camp.ToString())
    | InProgress camp -> printfn "Turn: %s" (camp.ToString())


let drawGame ({ board = board; status = status }: GameState) : unit =
    Console.Clear()
    drawBoard board
    drawStatus status
    ()
