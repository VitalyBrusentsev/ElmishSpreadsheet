module Models

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
  { Rows : int list
    Active : Position option
    Range: Range option
    Cols : ColumnName list
    Cells : Map<Position, string> }

type Movement =
    | MoveTo of Position
    | Invalid

type Direction = Up | Down | Left | Right


