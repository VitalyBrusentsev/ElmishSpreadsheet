module Views

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

open Cell
open Evaluator
open Models
open Keyboard


let getPosition ({ Column = col; Row = row }) (direction: Direction) cols rows =
  match direction with
  | Up -> if row = 1 then None else Some (col, row - 1)
  | Down -> if row = rows then None else Some (col, row + 1)
  | Left -> Column.tryPrev col |> Option.map (fun col1 -> (col1, row))
  | Right -> Column.tryNext col |> Option.map (fun col1 -> col1, row)

let getMovement (model: SpreadsheetModel) (direction: Direction) : Movement =
  match model.Editor with
  | Active position ->
    match (getPosition position direction (model.Cols |> Array.length) (model.Rows |> Array.length)) with
    | Some (col, row) when Array.contains col model.Cols && Array.contains row model.Rows ->
        MoveTo {Column = col; Row = row }
    | _ -> Invalid
  | Nothing | Selection _ | ChangingSelection _ -> Invalid

let getKeyPressEvent (model: SpreadsheetModel) trigger ke =
  match ke with
  | Enter ->
    match getMovement model Down with
    | Invalid -> trigger <| CancelEdit
    | MoveTo pos -> trigger <| StartEdit pos

  | Escape -> trigger <| CancelEdit

  | Tab -> 
    match getMovement model Right with
    | Invalid -> trigger <| CancelEdit
    | MoveTo pos -> trigger <| StartEdit pos

  | Arrow direction ->
    match getMovement model direction with
    | Invalid -> ()
    | MoveTo pos -> trigger <| StartEdit pos
  | _ -> ()

let renderEditor (trigger:Event -> unit) pos state value =
  td [ Class "editor"] [
    input [
      AutoFocus true
      OnKeyDown (getKeyPressEvent state trigger)
      OnInput (fun e -> trigger (UpdateValue (pos, e.target?value )))
      Ref (fun element ->
        if not (isNull element) then
          element?focus()
      )
      Value value ]
  ]

let prettyPos pos = (pos.Column |> Column.pretty) + string pos.Row

let pretty range = 
  (range.Start |> prettyPos) + ":" + (range.End |> prettyPos)

let minmax a b =
  if a < b then (a, b) else (b, a)

let inRange range { Column = x; Row = y} = 
  let (x1, x2) = minmax range.Start.Column range.End.Column
  let (y1, y2) = minmax range.Start.Row range.End.Row
  x <=x2 && x >= x1 && y <= y2 && y >= y1

let getCellClasses state pos = [
  match state.Editor with
  | Selection range | ChangingSelection range ->
    let (x1, x2) = minmax range.Start.Column range.End.Column
    let (y1, y2) = minmax range.Start.Row range.End.Row
    if pos.Column <=x2 && pos.Column >= x1 then
      if pos.Row <= y2 && pos.Row >= y1 then
        yield "selectedInRange", true
        if x1 = pos.Column then yield "firstColumn", true
        if y1 = pos.Row then yield "firstRow", true
        if x2 = pos.Column then yield "lastColumn", true
        if y2 = pos.Row then yield "lastRow", true
      if pos.Row = y2 + 1 then
        yield "pastRow", true
  | _ -> ()

]

let onMouseMove trigger pos state (e: Browser.Types.MouseEvent) = 
  if e.buttons = 1.0 then
    let start = 
      match state.Editor with 
      | Nothing | Active _ | Selection _ -> pos
      | ChangingSelection range -> range.Start
    e.preventDefault()
    trigger (ChangeSelection { Start = start; End = pos })
  else
    match state.Editor with
    | ChangingSelection range -> trigger (Select range)
    | _ -> ()

let renderView trigger (pos: Position) state (value:option<_>) =
  td
    [ classList (getCellClasses state pos)
      OnClick (fun _ -> trigger (Select {Start = pos; End = pos}))
      OnDoubleClick (fun _ -> trigger(StartEdit(pos)) ) 
      OnMouseMove (onMouseMove trigger pos state) ]
    [ str (value |> Option.defaultValue "#ERR") ]

let renderCell trigger pos state =
  let value = Map.tryFind pos state.Cells
  match state.Editor with
  | Active coord when coord = pos ->
    renderEditor trigger pos state (Option.defaultValue "" value)
  | _ ->
    let value =
      match value with
      | Some value ->
          parse value |> Option.bind (evaluate Set.empty state.Cells) |> Option.map string
      | _ -> Some ""
    renderView trigger pos state value

let view state trigger =
#if DEBUG
  let trigger event =
    printfn "Event: %A" event
    trigger event
#endif

  let colHeader h =
    th [ OnClick (fun _ -> SelectColumn h |> trigger) ] 
       [ h |> Column.pretty |> str ]

  let rowHeader row =
    th [ OnClick (fun _ -> SelectRow row |> trigger)] [ row |> string |> str ]

  let cells n =
    [ yield rowHeader n
      for col in state.Cols -> renderCell trigger { Column = col; Row = n } state ]

  let renderFormula pos =
    let value = Map.tryFind pos state.Cells
    value |> Option.defaultValue "" |> str

  div [] [
    div[][
      span[] [
        match state.Editor with
        | Active p -> p |> prettyPos |> str
        | Selection r | ChangingSelection r -> r.Start |> prettyPos |> str
        | _ -> ()
      ]
      str " : "
      span[] [
        match state.Editor with
        | Active p -> renderFormula p
        | Selection r when r.Start = r.End -> renderFormula r.Start
        | _ -> ()
      ]
    ]
    table [] [
      thead [] [
        tr [] [
          yield td [] []
          for col in state.Cols -> colHeader col ]
      ]
      tbody [] [
        for row in state.Rows -> tr [] (cells row) ]
    ]

#if DEBUG
    div [] [
      pre [] [ sprintf "%A" state |> str ]
    ]
#endif

  ]