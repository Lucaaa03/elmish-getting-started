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

// type Validated<'t> =
//   { Raw: string
//     Parsed: Option<'t> }

// module Validated =
//   let createEmpty() : Validated<_> =
//     { Raw = ""; Parsed = None }

//   let success raw value : Validated<_> =
//     { Raw = raw; Parsed = Some value }

//   let failure raw : Validated<_> =
//     { Raw = raw; Parsed = None }

// type State = {
//   NumberInput : Validated<int>
//   Capitalized : bool
// }

// type Msg =
//   | SetNumberInput of Validated<int>
//   | SetCapitalized of bool

// let init() = {
//   NumberInput = Validated.createEmpty()
//   Capitalized = false
// }

// let update msg state =
//   match msg with
//   | SetNumberInput numberInput ->
//     { state with NumberInput = numberInput }

//   | SetCapitalized value ->
//     { state with Capitalized = value }

// let tryParseInt (input: string) : Validated<int>=
//   try Validated.success input (int input)
//   with | _ -> Validated.failure input

// let validatedTextColor validated =
//   match validated.Parsed with
//   | Some _ -> color.green
//   | None -> color.red

// let render state dispatch =
//   Html.div [
//     Html.input [
//       prop.className "input"
//       prop.style [ style.width 500; style.borderRadius 0; style.padding 20 ]
//       prop.onChange (tryParseInt >> SetNumberInput >> dispatch)
//     ]

//     Html.div [
//       Html.label [
//         prop.htmlFor "checkbox"
//         prop.text "Capitalized"
//       ]

//       Html.input [
//         prop.style [ style.marginLeft 5 ]
//         prop.id "checkbox"
//         prop.type'.checkbox
//         prop.isChecked state.Capitalized
//         prop.onChange (SetCapitalized >> dispatch)
//       ]
//     ]

//     Html.span [
//       prop.text (
//         if state.Capitalized
//         then state.NumberInput.Raw.ToUpper()
//         else state.NumberInput.Raw
//       )
//       prop.className "tag"
//       prop.style [ style.fontSize 18; style.color (validatedTextColor state.NumberInput) ]
//     ]
//   ]

type State = {
  TodoList: string list
  NewTodo: string
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo

let init() = {
  TodoList = []
  NewTodo = ""
}

let update msg state =
  match msg with
  | SetNewTodo todoText -> { state with NewTodo = todoText }
  | AddNewTodo when state.NewTodo = "" -> state
  | AddNewTodo -> { state with NewTodo = ""; TodoList = List.append state.TodoList [ state.NewTodo ] }

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish Todo List"
  ]

let inputField state dispatch =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded" ]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault state.NewTodo
            prop.onChange (SetNewTodo >> dispatch)
          ]
        ]
      ]

      Html.div [
        prop.className "control"
        prop.children [
          Html.button [
            prop.classes [ "button"; "is-medium"; "is-primary" ]
            prop.text "Add"
            prop.onClick (fun _ -> dispatch AddNewTodo)
            prop.children [
              Html.i [
                prop.classes [ "fa"; "fa-plus" ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]

let todoList state dispatch =
  Html.ul [
    prop.children [
      for todo in state.TodoList ->
        Html.li [
          prop.classes [ "box"; "subtitle" ]
          prop.text todo
        ]
    ]
  ]

let render state dispatch =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
