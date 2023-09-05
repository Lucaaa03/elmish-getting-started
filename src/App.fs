module App

open Elmish
open Elmish.React
open Feliz

// type State =
//     { Count: int }

// type Msg =
//     | Increment
//     | Decrement

// let init() =
//     { Count = 0 }

// let update (msg: Msg) (state: State): State =
//     match msg with
//     | Increment ->
//         { state with Count = state.Count + 1 }

//     | Decrement ->
//         { state with Count = state.Count - 1 }

// let render (state: State) (dispatch: Msg -> unit) =
//   let headerText =
//     if state.Count % 2 = 0
//     then "Count is even"
//     else "Count is odd"

//   let oddOrEvenMessage =
//     if state.Count >= 0
//     then Html.h1 headerText
//     else Html.none

//   Html.div [
//     Html.button [
//       prop.onClick (fun _ -> dispatch Increment)
//       prop.classes [ "button"; "is-primary" ]
//       prop.style [ style.width 40 ]
//       prop.text "+"
//     ]

//     Html.h1 state.Count

//     Html.button [
//       prop.onClick (fun _ -> dispatch Decrement)
//       prop.classes [ "button"; "is-danger" ]
//       prop.style [ style.width 40 ]
//       prop.text "-"
//     ]

//     oddOrEvenMessage
//   ]

// Program.mkSimple init update render
// |> Program.withReactSynchronous "elmish-app"
// |> Program.run

type Validated<'t> =
  { Raw: string
    Parsed: Option<'t> }

module Validated =
  let createEmpty() : Validated<_> =
    { Raw = ""; Parsed = None }

  let success raw value : Validated<_> =
    { Raw = raw; Parsed = Some value }

  let failure raw : Validated<_> =
    { Raw = raw; Parsed = None }

type State = { NumberInput : Validated<int> }

type Msg =
  | SetNumberInput of Validated<int>

let init() = { NumberInput = Validated.createEmpty() }

let update msg state =
  match msg with
  | SetNumberInput numberInput ->
    { state with NumberInput = numberInput }

let tryParseInt (input: string) : Validated<int>=
  try Validated.success input (int input)
  with | _ -> Validated.failure input

let validatedTextColor validated =
  match validated.Parsed with
  | Some _ -> color.green
  | None -> color.red

let render state dispatch =
  Html.div [
    Html.input [
      prop.className "input"
      prop.style [ style.width 500; style.borderRadius 0; style.padding 20 ]
      prop.onChange (tryParseInt >> SetNumberInput >> dispatch)
    ]
    Html.span [
      prop.text state.NumberInput.Raw
      prop.className "tag"
      prop.style [ style.fontSize 18; style.color (validatedTextColor state.NumberInput) ]
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
