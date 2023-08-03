module InitializeGame

open Domain.Types
open Domain.Constants

let initializeGame () : GameState =
    // Cell helpers
    // ============

    // Improve with Builder pattern?
    let createCell (state: CellState) (rank: Rank) (file: File) : Cell =
        { position = { rank = rank; file = file }
          state = state }

    let createEmptyCell = createCell Empty

    let createPieceCell (camp: Camp) (kind: PieceKind) =
        createCell (Occupied { camp = camp; kind = kind })

    // Row helpers
    // ===========

    let createEmptyRow (rank: Rank) : Cell list =
        FILES |> List.map (fun file -> createEmptyCell rank file)

    let createPawnRow (rank: Rank) (camp: Camp) : Cell list =
        FILES |> List.map (fun file -> createPieceCell camp Pawn rank file)

    let createFirstRow (rank: Rank) (camp: Camp) : Cell list =
        let create (kind: PieceKind) (file: File) = createPieceCell camp kind rank file

        FILES
        |> List.map (fun file ->
            match file with
            | (FileA | FileH) -> create Rook file
            | (FileB | FileG) -> create Bishop file
            | (FileC | FileF) -> create Knight file
            | FileD -> create Queen file
            | FileE -> create King file)

    // Build board
    // ===========

    let cells: Cell list list =
        [ createFirstRow Rank8 Black
          createPawnRow Rank7 Black
          createEmptyRow Rank6
          createEmptyRow Rank5
          createEmptyRow Rank4
          createEmptyRow Rank3
          createPawnRow Rank2 White
          createFirstRow Rank1 White ]

    { board = List.concat cells
      status = InProgress White }
