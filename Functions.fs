module Functions

open Types

let initializeGame () : GameState =
    let FILES = [ 1; 2; 3; 4; 5; 6; 7; 8 ]

    // Cell helpers
    // ============

    // Improve with Builder pattern?
    let createCell (state: CellState) (rank: RankPosition) (file: FilePosition) : Cell =
        { position = { rank = rank; file = file }
          state = state }

    let createEmptyCell = createCell Empty

    let createPieceCell (camp: Camp) (kind: ChessPiece) =
        createCell (Occupied { camp = camp; kind = kind })

    // Row helpers
    // ===========

    let createEmptyRow (rank: RankPosition) : Cell list =
        FILES |> List.map (fun x -> createEmptyCell rank (FilePosition x))

    let createPawnRow (rank: RankPosition) (camp: Camp) : Cell list =
        FILES |> List.map (fun x -> createPieceCell camp Pawn rank (FilePosition x))

    let createFirstRow (rank: RankPosition) (camp: Camp) : Cell list =
        let create (kind: ChessPiece) (file: FilePosition) = createPieceCell camp kind rank file

        [ create Rook (FilePosition 1)
          create Bishop (FilePosition 2)
          create Knight (FilePosition 3)
          create Queen (FilePosition 4)
          create King (FilePosition 5)
          create Knight (FilePosition 6)
          create Bishop (FilePosition 7)
          create Rook (FilePosition 8) ]

    // Build board
    // ===========

    let cells: Cell list list =
        [ createFirstRow (RankPosition 8) Black
          createPawnRow (RankPosition 7) Black
          createEmptyRow (RankPosition 6)
          createEmptyRow (RankPosition 5)
          createEmptyRow (RankPosition 4)
          createEmptyRow (RankPosition 3)
          createPawnRow (RankPosition 2) White
          createFirstRow (RankPosition 1) White ]

    { board = cells
      status = InProgress White }
