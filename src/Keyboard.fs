module Keyboard

type Direction = Up | Down | Left | Right

let keyMatch expected result (ke:Browser.Types.KeyboardEvent) = 
  if int ke.keyCode = expected then 
    Some result 
  else 
    None

let (|Escape|_|) e = e |> keyMatch 27 Escape
let (|Tab|_|) e = e |> keyMatch 9 Tab
let (|Enter|_|) e = e |> keyMatch 13 Enter
let (|Arrow|_|) (e: Browser.Types.KeyboardEvent) =
  if e.ctrlKey || e.shiftKey then 
    None
  else 
    match int e.keyCode with
    | 37 -> Arrow Left |> Some
    | 38 -> Arrow Up |> Some
    | 39 -> Arrow Right |> Some
    | 40 -> Arrow Down |> Some
    | _ -> None
