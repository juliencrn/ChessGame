module ChessGame.InitializeGame.Impl

open ChessGame.Common
open ChessGame.Api

let initializeGame: InitializeGame =
    fun () ->
        // Row helpers
        // ===========

        let createEmptyRow (rank: Rank) : Cell list =
            File.getAll ()
            |> List.map (fun file -> Cell.createEmpty (Position.create file rank))

        let createPawnRow (rank: Rank) (camp: Camp) : Cell list =
            File.getAll ()
            |> List.map (fun file ->
                let position = Position.create file rank
                let piece = Piece.create camp Pawn

                Cell.createOccupied position piece)

        let createFirstRow (rank: Rank) (camp: Camp) : Cell list =
            let create (kind: PieceKind) (file: File) =
                let position = Position.create file rank
                let piece = Piece.create camp kind

                Cell.createOccupied position piece

            File.getAll ()
            |> List.map (fun file ->
                match file with
                | (FileA | FileH) -> create Rook file
                | (FileB | FileG) -> create Bishop file
                | (FileC | FileF) -> create Knight file
                | FileD -> create Queen file
                | FileE -> create King file)

        // Build board
        // ===========

        let board: Board =
            [ createFirstRow Rank8 Black
              createPawnRow Rank7 Black
              createEmptyRow Rank6
              createEmptyRow Rank5
              createEmptyRow Rank4
              createEmptyRow Rank3
              createPawnRow Rank2 White
              createFirstRow Rank1 White ]
            |> List.concat

        let status = PickingPiece White

        { board = board; status = status }
