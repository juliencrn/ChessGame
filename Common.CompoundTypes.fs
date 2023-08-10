// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace ChessGame.Common

open ChessGame.Common

// ===============================
// Compound types
// ===============================

type Position = { file: File; rank: Rank }

type Piece = { camp: Camp; kind: PieceKind }

type CellState =
    | Occupied of Piece
    | Empty

type Cell =
    { position: Position; state: CellState }

type PickedPiece =
    { piece: Piece
      position: Position
      legalMoves: Position list }

type GameStatus =
    | PickingPiece of Camp
    | MovingPiece of PickedPiece
    | Win of Camp

type Board = Cell list

type GameState = { board: Board; status: GameStatus }


// ===============================
// Reusable helpers for these types
// ===============================

module Position =
    let create file rank = { file = file; rank = rank }

    let getTopPosition position =
        match position.rank with
        | Rank8 -> Error "Out of board" // No rank above Rank8
        | Rank7 -> Ok { position with rank = Rank8 }
        | Rank6 -> Ok { position with rank = Rank7 }
        | Rank5 -> Ok { position with rank = Rank6 }
        | Rank4 -> Ok { position with rank = Rank5 }
        | Rank3 -> Ok { position with rank = Rank4 }
        | Rank2 -> Ok { position with rank = Rank3 }
        | Rank1 -> Ok { position with rank = Rank2 }

    let getDownPosition position =
        match position.rank with
        | Rank1 -> Error "Out of board" // No rank below Rank1
        | Rank2 -> Ok { position with rank = Rank1 }
        | Rank3 -> Ok { position with rank = Rank2 }
        | Rank4 -> Ok { position with rank = Rank3 }
        | Rank5 -> Ok { position with rank = Rank4 }
        | Rank6 -> Ok { position with rank = Rank5 }
        | Rank7 -> Ok { position with rank = Rank6 }
        | Rank8 -> Ok { position with rank = Rank7 }

    let getLeftPosition position =
        match position.file with
        | FileA -> Error "Out of board" // No file to the left of FileA
        | FileB -> Ok { position with file = FileA }
        | FileC -> Ok { position with file = FileB }
        | FileD -> Ok { position with file = FileC }
        | FileE -> Ok { position with file = FileD }
        | FileF -> Ok { position with file = FileE }
        | FileG -> Ok { position with file = FileF }
        | FileH -> Ok { position with file = FileG }

    let getRightPosition position =
        match position.file with
        | FileH -> Error "Out of board" // No file to the right of FileH
        | FileG -> Ok { position with file = FileH }
        | FileF -> Ok { position with file = FileG }
        | FileE -> Ok { position with file = FileF }
        | FileD -> Ok { position with file = FileE }
        | FileC -> Ok { position with file = FileD }
        | FileB -> Ok { position with file = FileC }
        | FileA -> Ok { position with file = FileB }

    let private composeMoves rankFn fileFn position =
        result {
            let! { rank = rank } = rankFn position
            let! { file = file } = fileFn position

            return { rank = rank; file = file }
        }

    let getTopLeftPosition = composeMoves getTopPosition getLeftPosition

    let getTopRightPosition = composeMoves getTopPosition getRightPosition

    let getDownLeftPosition = composeMoves getDownPosition getLeftPosition

    let getDownRightPosition = composeMoves getDownPosition getRightPosition


module Piece =
    let create camp kind = { camp = camp; kind = kind }

module Cell =
    let create position state = { position = position; state = state }

    let createEmpty position = create position Empty

    let createOccupied position piece = create position (Occupied piece)

// module Board =

// module GameState =
