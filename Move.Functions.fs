module Move.Functions

open Domain.Types

type MoveError = | OutofBoard

type Move = Position -> Result<Position, MoveError>

let moveTop: Move =
    fun position ->
        match position.rank with
        | Rank8 -> Error OutofBoard // No rank above Rank8
        | Rank7 -> Ok { position with rank = Rank8 }
        | Rank6 -> Ok { position with rank = Rank7 }
        | Rank5 -> Ok { position with rank = Rank6 }
        | Rank4 -> Ok { position with rank = Rank5 }
        | Rank3 -> Ok { position with rank = Rank4 }
        | Rank2 -> Ok { position with rank = Rank3 }
        | Rank1 -> Ok { position with rank = Rank2 }

let moveDown: Move =
    fun position ->
        match position.rank with
        | Rank1 -> Error OutofBoard // No rank below Rank1
        | Rank2 -> Ok { position with rank = Rank1 }
        | Rank3 -> Ok { position with rank = Rank2 }
        | Rank4 -> Ok { position with rank = Rank3 }
        | Rank5 -> Ok { position with rank = Rank4 }
        | Rank6 -> Ok { position with rank = Rank5 }
        | Rank7 -> Ok { position with rank = Rank6 }
        | Rank8 -> Ok { position with rank = Rank7 }

let moveLeft: Move =
    fun position ->
        match position.file with
        | FileA -> Error OutofBoard // No file to the left of FileA
        | FileB -> Ok { position with file = FileA }
        | FileC -> Ok { position with file = FileB }
        | FileD -> Ok { position with file = FileC }
        | FileE -> Ok { position with file = FileD }
        | FileF -> Ok { position with file = FileE }
        | FileG -> Ok { position with file = FileF }
        | FileH -> Ok { position with file = FileG }

let moveRight: Move =
    fun position ->
        match position.file with
        | FileH -> Error OutofBoard // No file to the right of FileH
        | FileG -> Ok { position with file = FileH }
        | FileF -> Ok { position with file = FileG }
        | FileE -> Ok { position with file = FileF }
        | FileD -> Ok { position with file = FileE }
        | FileC -> Ok { position with file = FileD }
        | FileB -> Ok { position with file = FileC }
        | FileA -> Ok { position with file = FileB }

// TODO: Could you use `bind` to adapt Position -> Result?
let tryMove
    (newRankPosition: Result<Position, MoveError>)
    (newFilePosition: Result<Position, MoveError>)
    : Result<Position, MoveError> =
    match newRankPosition, newFilePosition with
    | Ok rankPos, Ok filePos ->
        Ok
            { rank = rankPos.rank
              file = filePos.file }
    | _, _ -> Error OutofBoard

let composeMoves (rankFn: Move) (fileFn: Move) : Move =
    fun position -> tryMove (rankFn position) (fileFn position)

let moveTopLeft: Move = composeMoves moveTop moveLeft

let moveTopRight: Move = composeMoves moveTop moveRight

let moveDownLeft: Move = composeMoves moveDown moveLeft

let moveDownRight: Move = composeMoves moveDown moveRight
