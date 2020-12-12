module Keyboard

type Direction = Up | Down | Left | Right

let keyMatch expected result (ke:Browser.Types.KeyboardEvent) = 
  if ke.key = expected then 
    Some result 
  else 
    None

let (|Escape|_|) e = e |> keyMatch "Escape" Escape
let (|Tab|_|) e = e |> keyMatch "Tab" Tab
let (|Enter|_|) e = e |> keyMatch "Enter" Enter
let (|Arrow|_|) (e: Browser.Types.KeyboardEvent) =
  if e.ctrlKey || e.shiftKey then 
    None
  else 
    match e.key with
    | "ArrowLeft" -> Arrow Left |> Some
    | "ArrowUp" -> Arrow Up |> Some
    | "ArrowRight" -> Arrow Right |> Some
    | "ArrowDown" -> Arrow Down |> Some
    | _ -> None
