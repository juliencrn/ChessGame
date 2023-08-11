module ChessGame.InitializeGame.Impl

open ChessGame.Common
open ChessGame.Api

let defaultGrid: List<List<Option<(Camp * PieceKind)>>> =
    [ [ Some(Black, Rook)
        Some(Black, Bishop)
        Some(Black, Knight)
        Some(Black, Queen)
        Some(Black, King)
        Some(Black, Knight)
        Some(Black, Bishop)
        Some(Black, Rook) ]
      [ Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn)
        Some(Black, Pawn) ]
      [ None; None; None; None; None; None; None; None ]
      [ None; None; None; None; None; None; None; None ]
      [ None; None; None; None; None; None; None; None ]
      [ None; None; None; None; None; None; None; None ]
      [ Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn)
        Some(White, Pawn) ]
      [ Some(White, Rook)
        Some(White, Bishop)
        Some(White, Knight)
        Some(White, Queen)
        Some(White, King)
        Some(White, Knight)
        Some(White, Bishop)
        Some(White, Rook) ] ]

let gridToBoard (grid: List<List<Option<(Camp * PieceKind)>>>) : Board =
    let numberToLetter (num: int) : string =
        if num >= 1 && num <= 26 then
            let letter = char (num + int 'a' - 1)
            string letter
        else
            failwith "Invalid number"

    let unwrapResult result =
        match result with
        | Ok x -> x
        | Error err -> failwith err

    grid
    |> List.rev
    |> List.mapi (fun i col ->
        let rank = unwrapResult (Rank.create (i + 1))

        col
        |> List.mapi (fun i o ->
            let file = unwrapResult (File.create (numberToLetter (i + 1)))
            let position = Position.create file rank

            match o with
            | None -> Cell.createEmpty position
            | Some(camp, kind) -> Cell.createOccupied position (Piece.create camp kind)))
    |> List.concat

let initializeGame: InitializeGame =
    fun () ->
        let board = gridToBoard defaultGrid
        let status = PickingPiece White

        { board = board
          status = status
          captured = [] }
