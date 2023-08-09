// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace ChessGame.Common

// ===============================
// Primitive simple types
// ===============================

type Rank = // rows
    | Rank8
    | Rank7
    | Rank6
    | Rank5
    | Rank4
    | Rank3
    | Rank2
    | Rank1

type File = // lines
    | FileA
    | FileB
    | FileC
    | FileD
    | FileE
    | FileF
    | FileG
    | FileH

type Camp =
    | White
    | Black

type PieceKind =
    | King // roi
    | Queen // rene
    | Rook // tour
    | Bishop // fou
    | Knight // cavalier
    | Pawn // pion

// ===============================
// Reusable constructors and getters for constrained types
// ===============================

module Rank =
    let create (int: int) =
        match int with
        | 1 -> Ok Rank1
        | 2 -> Ok Rank2
        | 3 -> Ok Rank3
        | 4 -> Ok Rank4
        | 5 -> Ok Rank5
        | 6 -> Ok Rank6
        | 7 -> Ok Rank7
        | 8 -> Ok Rank8
        | _ -> Error "Invalid rank, should be between 1 and 8 (included)"

    let getAll () =
        [ Rank8; Rank7; Rank6; Rank5; Rank4; Rank3; Rank2; Rank1 ]

module File =
    let create (str: string) =
        match str with
        | "a" -> Ok FileA
        | "b" -> Ok FileB
        | "c" -> Ok FileC
        | "d" -> Ok FileD
        | "e" -> Ok FileE
        | "f" -> Ok FileF
        | "g" -> Ok FileG
        | "h" -> Ok FileH
        | _ -> Error "Invalid column, should be between `a` and `h` (included)"

    let getAll () =
        [ FileA; FileB; FileC; FileD; FileE; FileF; FileG; FileH ]

module Camp =
    let getAdverse (camp: Camp) =
        match camp with
        | White -> Black
        | Black -> White
