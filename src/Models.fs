module Models

open ColumnName
open Evaluator

type Range = { TopLeft: Position; BottomRight: Position }

type Event =
  | UpdateValue of Position * string
  | StartEdit of Position
  | CancelEdit
  | Select of Range

type EditorState = 
  | Nothing
  | Editor of Position
  | Selection of Range

type SpreadsheetState =
  { Rows : int[]
    Active : Position option
    Range: Range option
    Cols : Column[]
    Cells : Map<Position, string> }

type Movement =
    | MoveTo of Position
    | Invalid

type Direction = Up | Down | Left | Right


