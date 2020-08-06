module Models

open Cell

type Range = { TopLeft: Position; BottomRight: Position }

type Event =
  | UpdateValue of Position * string
  | StartEdit of Position
  | CancelEdit
  | Select of Range
  | SelectColumn of Column
  | SelectRow of int

type EditorState = 
  | Nothing
  | Active of Position
  | Selection of Range

type SpreadsheetModel =
  { Rows : int[]
    Editor: EditorState
    Cols : Column[]
    Cells : Map<Position, string> }

type Movement =
    | MoveTo of Position
    | Invalid


