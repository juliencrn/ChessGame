module Domain.Api

open Domain.Types

// --- Input data, DTOs ---

type UnvalidatedRank = UnvalidatedRank of int
type UnvalidatedFile = UnvalidatedFile of string

type UnvalidatedPosition =
    { rank: UnvalidatedRank
      file: UnvalidatedFile }

type UnvalidatedMoveInput =
    { fromPosition: UnvalidatedPosition
      toPosition: UnvalidatedPosition }


// --- Public API ---

type InitializeGame = unit -> GameState

type GameError = GameError of string
// type TryMovePiece = GameState -> UnvalidatedMoveInput -> Result<GameState, GameError>
