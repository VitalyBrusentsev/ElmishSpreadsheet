module Spreadsheet

open Elmish
open Elmish.React

open Cell
open Models

// ----------------------------------------------------------------------------
// EVENT HANDLING
// ----------------------------------------------------------------------------

let update msg state =
  match msg with
  | StartEdit(pos) ->
    { state with Editor = Active pos }

  | CancelEdit ->
    { state with Editor = Nothing }

  | UpdateValue(pos, value) ->
    let newCells =
      if value = ""
          then Map.remove pos state.Cells
          else Map.add pos value state.Cells
    { state with Cells = newCells }
  | Select range -> 
    { state with Editor = Selection range }
  | SelectColumn column ->
    let range = { Start = { Column = column; Row = 1 }
                  End = { Column = column; Row = state.Rows |> Array.length } }
    { state with Editor = Selection range}
  | SelectRow row ->
    let range = { Start = { Column = 1 |> Column.ofIndex; Row = row }
                  End = { Column = state.Cols |> Array.length |> Column.ofIndex; Row =row } }
    { state with Editor = Selection range}
  | ChangeSelection range ->
    { state with Editor = ChangingSelection range }

open Views

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = [|'A' .. 'K'|] |> Array.map Column.ofChar
    Rows = [|1 .. 15|]
    Editor = Selection { 
                         Start = { Column = Column.ofChar 'B'; Row = 2 }
                         End = { Column = Column.ofChar 'D' ; Row = 5 } }
    Cells = Map.empty }

open Fable.Elmish.ElmishToReact

let private program = Program.mkSimple initial update view
let externalisedProgram =
  Externalised.externalise program

let private spreadSheetReactComponent = elmishToReact externalisedProgram

ReactToWebComponents.register "spreadsheet-component" spreadSheetReactComponent