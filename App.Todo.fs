module App.Todo

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Reactive.Linq
open System.Windows
open System.Windows.Controls
open System.Windows.Input

module Todo =
  type t = {Id: int; Completed: bool; Title: string}
  let id = Optic.lens (fun t -> t.Id) (fun v t -> {t with Id = v})
  let completed =
    Optic.lens (fun t -> t.Completed) (fun v t -> {t with Completed = v})
  let title = Optic.lens (fun t -> t.Title) (fun v t -> {t with Title = v})

module Filter =
  type t = All | Active | Completed
  let predicate: t -> Todo.t -> bool = function
    | All -> fun _ -> true
    | Active -> fun t -> not t.Completed
    | Completed -> fun t -> t.Completed

module State =
  type t = {NewTodo: string
            Todos: IROL<Todo.t>
            Filter: Filter.t
            Editing: option<int>}
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
  let state = Atom.create<State.t> {
    NewTodo = ""
    Todos = [|
      {Id = 1; Completed = false; Title = "Be functional!"}
      {Id = 2; Completed = false; Title = "Be reactive!"}
      {Id = 3; Completed = false; Title = "Write cool apps!"}
    |]
    Filter = Filter.All
    Editing = None
  }

  let newTodo = Atom.view State.newTodo state
  let todos = Atom.view State.todos state
  let filter = Atom.view State.filter state
  let editing = Atom.view State.editing state

  let allDone =
    todos
    |> Atom.view (Optic.foldLens (Optic.forall id) Todos.completed)

  let numLeft =
    todos
     .Select(Optic.fold 0 (fun s b -> if b then s else s+1) Todos.completed)
     .AsProperty()

  let empty = todos.Select(fun xs -> xs.Count = 0).AsProperty()

  let filterButton value =
    Button (Content = sprintf "%A" value) |> UI.bind [|
      UI.onClick <| fun _ -> Atom.set filter value
    |]

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Todo",
        Content = (
          StackPanel (Orientation = Orientation.Vertical) |> UI.bind [|
            UI.children [|
              DockPanel () |> UI.bind [|
                UI.children [|
                  CheckBox () |> UI.bind [|
                    UI.isChecked allDone
                    UI.isEnabled (empty.Select not)
                  |]
                  TextBox () |> UI.bind [|
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
                                 [|{Todo.Id = id
                                    Todo.Completed = false
                                    Todo.Title = title}|]
                                 todos
                        Atom.set newTodo ""
                  |]
                |]
              |]
              StackPanel (Orientation = Orientation.Vertical) |> UI.bind [|
                state
                |> Atom.view (filter.Select(Func<_, _>(fun f ->
                    State.todos
                    << Optic.rewriteI
                        (Optic.over Optic.arrayI
                          (Array.sortBy (Optic.view Todo.id)))
                    << Optic.filterL (Filter.predicate f))))
                |> Atom.mapByKey (Optic.view Todo.id) (fun id todo ->
                    DockPanel () |> UI.bind [|
                      UI.children [|
                        CheckBox () |> UI.bind [|
                          UI.isChecked (Atom.view Todo.completed todo)
                          UI.dock Dock.Left
                        |]
                        Button (Content = "Remove") |> UI.bind [|
                          UI.onClick <| fun _ -> Atom.remove todo
                          UI.dock Dock.Right
                        |]
                        TextBox () |> UI.bind [|
                          UI.text (Atom.view Todo.title todo)
                          UI.onLostFocus <| fun _ -> Atom.set editing None
                          UI.onEnter <| fun _ -> Keyboard.ClearFocus()
                          UI.onMouseDoubleClick <| fun _ ->
                            Atom.set editing <| Some id
                          UI.isReadOnly (editing.Select(function
                            | None -> true
                            | Some id' -> id <> id'))
                        |]
                      |]
                    |])
                |> UI.children
              |]
              StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [|
                UI.children (empty.Select(fun empty ->
                  if empty then [||] else [|
                    Label () |> UI.bind [|
                      UI.content (numLeft.Select(fun n ->
                        sprintf "%d item%s left" n (if n = 1 then "" else "s")
                        |> box))
                    |]
                    filterButton Filter.All
                    filterButton Filter.Active
                    filterButton Filter.Completed
                  |]))
              |]
            |]
          |]
        )
      )
    )
  )
