module App.Todo

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input

type Todo = {Id: int; Completed: bool; Title: string}
module Todo =
  let id = Optic.lens (fun t -> t.Id) (fun v t -> {t with Id = v})
  let completed =
    Optic.lens (fun t -> t.Completed) (fun v t -> {t with Completed = v})
  let title = Optic.lens (fun t -> t.Title) (fun v t -> {t with Title = v})

type Filter = All | Active | Completed
module Filter =
  let predicate: Filter -> Todo -> bool = function
    | All -> fun _ -> true
    | Active -> fun t -> not t.Completed
    | Completed -> fun t -> t.Completed

type State =
  { NewTodo: string
    Todos: IROL<Todo>
    Filter: Filter
    Editing: option<int> }
module State =
  let todos = Optic.lens (fun t -> t.Todos) (fun v t -> {t with Todos = v})
  let newTodo =
    Optic.lens (fun t -> t.NewTodo) (fun v t -> {t with NewTodo = v})
  let filter = Optic.lens (fun t -> t.Filter) (fun v t -> {t with Filter = v})
  let editing =
    Optic.lens (fun t -> t.Editing) (fun v t -> {t with Editing = v})

module Todos =
  let completed = Optic.elemsT << Todo.completed

[<EntryPoint; STAThread>]
let main _ =
  let state = Atom.create {
    NewTodo = ""
    Todos = [|
      {Id = 1; Completed = true; Title = "Be functional!"}
      {Id = 2; Completed = true; Title = "Be reactive!"}
      {Id = 3; Completed = false; Title = "Write cool apps!"}
    |]
    Filter = All
    Editing = None
  }

  let newTodo = Atom.view State.newTodo state
  let todos = Atom.view State.todos state
  let filter = Atom.view State.filter state
  let editing = Atom.view State.editing state

  let allDone =
    todos |> Atom.view (Optic.foldLens (Optic.forall id) Todos.completed)

  let numLeft =
    UI.lift1 (Optic.count (Todos.completed << Optic.whereP not)) todos

  let empty = UI.lift1 (fun (xs: IROL<_>) -> xs.Count = 0) todos

  let filterButton value =
    Button (Content = sprintf "%A" value) |> UI.bind [
      UI.onClick <| Atom.setAct filter value
    ]

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Todo",
        Width = 300.0,
        Height = 300.0,
        Content = (
          StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
            UI.children [
              DockPanel () |> UI.bind [
                UI.children [
                  CheckBox () |> UI.bind [
                    UI.isChecked allDone
                    UI.isEnabled <| UI.lift1 not empty
                  ]
                  TextBox () |> UI.bind [
                    UI.text newTodo
                    UI.onEnter <| fun textBox ->
                      let title = textBox.Text
                      if title <> "" then
                        Atom.modify todos
                          <| fun todos ->
                               let id =
                                 if todos.Count = 0 then 0
                                 else todos.[todos.Count-1].Id + 1
                               Optic.set Optic.appendL
                                 [{Todo.Id = id
                                   Todo.Completed = false
                                   Todo.Title = title}]
                                 todos
                        Atom.set newTodo ""
                  ]
                ]
              ]
              StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
                todos
                 |> Atom.view
                      (UI.lift1
                       <| fun filter ->
                            Optic.rewriteI
                              (Optic.over Optic.arrayI
                                (Array.sortBy (Optic.view Todo.id)))
                               << Optic.filterL (Filter.predicate filter)
                       <| filter)
                 |> Atom.mapByKey (Optic.view Todo.id) (fun id todo ->
                    let editing = Atom.view (Optic.isOrI None (Some id)) editing
                    DockPanel () |> UI.bind [
                      UI.children [
                        CheckBox () |> UI.bind [
                          UI.isChecked <| Atom.view Todo.completed todo
                          UI.dock Dock.Left
                        ]
                        Button (Content = "Remove") |> UI.bind [
                          UI.onClick <| Atom.removeAct todo
                          UI.dock Dock.Right
                        ]
                        TextBox () |> UI.bind [
                          UI.text <| Atom.view Todo.title todo
                          UI.onLostFocus <| Atom.setAct editing false
                          UI.onEnter <| fun _ -> Keyboard.ClearFocus()
                          UI.onMouseDoubleClick <| Atom.setAct editing true
                          UI.isReadOnly <| UI.lift1 not editing
                        ]
                      ]
                    ])
                |> UI.children
              ]
              StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [
                empty.IfElse([], [
                  Label () |> UI.bind [
                    UI.lift1 (fun n ->
                        sprintf "%d item%s left" n (if n = 1 then "" else "s"))
                      numLeft
                     |> UI.content
                  ]
                  filterButton All
                  filterButton Active
                  filterButton Completed
                ]) |> UI.children
              ]
            ]
          ]
        )
      )
    )
  )
