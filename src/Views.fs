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
  | Nothing | Selection _ -> Invalid

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

let prettyPos pos = sprintf "%s%d" (pos.Column |> Column.pretty) pos.Row

let pretty range = 
  sprintf "%s:%s" (range.TopLeft |> prettyPos) (range.BottomRight |> prettyPos)

let inRange range { Column = x; Row = y} = 
  let { Column = x1; Row = y1 } = range.TopLeft
  let { Column = x2; Row = y2 } = range.BottomRight
  x <=x2 && x >= x1 && y <= y2 && y >= y1

let mapPosToStyles editor pos (value: string option) = 
  let activeBorderColor = "#1a73e8"
  let activeBackgroundColor = "rgba(23, 102, 202, 0.2)"
  match editor with
  | Selection range when inRange range pos -> [
    if value.IsSome then yield BackgroundColor activeBackgroundColor
    // if range.TopLeft |> fst = fst pos then yield BorderLeftColor activeBorderColor
    // if range.TopLeft |> snd = snd pos then yield BorderTopColor activeBorderColor
    // if range.BottomRight |> fst = fst pos then yield BorderRightColor activeBorderColor
    // if range.BottomRight |> snd = snd pos then yield BorderBottomColor activeBorderColor 
    ]
  | _ -> []

let onMouseMove trigger pos state (e: Browser.Types.MouseEvent) = 
  if e.buttons = 1.0 then
    let start = 
      match state.Editor with 
      | Nothing | Active _ -> pos
      | Selection range -> range.TopLeft
    e.preventDefault()
    trigger (Select { TopLeft = start; BottomRight = pos })

let renderView trigger (pos: Position) state (value:option<_>) =
  td
    [ Style [
        if value.IsNone then yield Background "#ffb0b0"
        yield! mapPosToStyles state.Editor pos value ]
      OnClick (fun _ -> trigger (Select {TopLeft = pos; BottomRight = pos}))
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

#if DEBUG
let renderDebugPane state =
  div[][
    pre[][
      sprintf "%A" state |> str
    ]
  ]
#endif

let view state trigger =
#if DEBUG
  let trigger event =
    printfn "Event: %A" event
    trigger event
#endif
  let empty = td [] []
  let colHeader h = 
    th [ OnClick (fun _ -> SelectColumn h |> trigger) ] 
       [ h |> Column.pretty |> str ]
  let headers = state.Cols |> Array.map colHeader
  let headers = [| yield empty; yield! headers |]

  let rowHeader row =
    th [ OnClick (fun _ -> SelectRow row |> trigger)] [ row |> string |> str]

  let cells n =
    let cells = state.Cols |> Array.map (fun h -> renderCell trigger { Column = h; Row = n } state)
    [| yield rowHeader n
       yield! cells |]
  let rows = state.Rows |> Array.map (fun r -> tr [] (cells r))

  div [] [
    table [] [
      tr [] headers
      tbody [] rows
    ]

#if DEBUG
    renderDebugPane state
#endif

  ]