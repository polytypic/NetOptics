module App.Login

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Reactive.Subjects
open System.Windows
open System.Windows.Controls

type Credentials = {Username: string; Password: string}
module Credentials =
  let username =
    Optic.lens (fun t -> t.Username) (fun v t -> {t with Username = v})
  let password =
    Optic.lens (fun t -> t.Password) (fun v t -> {t with Password = v})
  let empty = {Username = ""; Password = ""}

module Api =
  let login =
    Prop.value
     >> Stream.delay (TimeSpan.FromSeconds 2.0)
     >> Stream.map ((=) {Username = "simon"; Password = "letmein"})

module Model =
  type t = {
      Credentials: IAtom<Credentials>
      HasEmptyCredentials: IObs<bool>
      LoginPressed: Subject<unit>
      LoggedIn: IObs<bool>
      LoginInProgress: IObs<bool>
      InputEnabled: IObs<bool>
      LoginEnabled: IObs<bool>
    }

  let create (credentials: IAtom<_>) =
    let loginPressed = new Subject<unit>()
    let hasEmptyCredentials =
      Prop.map (fun c -> c.Username = "" || c.Password = "") credentials
    let loginResult =
      credentials
       |> Stream.latestWhen loginPressed
       |> Stream.switchMap Api.login
    let loggedIn = loginResult |> Stream.startWith false |> Stream.toProp
    let loginInProgress =
      [ loginPressed |> Stream.map (fun _ -> true)
        loginResult |> Stream.map (fun _ -> false) ]
       |> Stream.merge |> Stream.startWith false |> Stream.toProp
    let inputDisabled = Prop.map2 (||) loggedIn loginInProgress
    let loginDisabled = Prop.map2 (||) hasEmptyCredentials inputDisabled
    {
      Credentials = credentials
      HasEmptyCredentials = hasEmptyCredentials
      LoginPressed = loginPressed
      LoggedIn = loggedIn
      LoginInProgress = loginInProgress
      InputEnabled = Prop.map not inputDisabled
      LoginEnabled = Prop.map not loginDisabled
    }

let loginView (model: Model.t) =
  UI.elem StackPanel [
    UI.orientation Orientation.Vertical
    UI.children [
      UI.elem TextBox [
        UI.isEnabled model.InputEnabled
        Atom.view Credentials.username model.Credentials |> UI.text
      ]
      UI.elem PasswordBox [
        UI.isEnabled model.InputEnabled
        UI.password <| Atom.view Credentials.password model.Credentials
        UI.onEnter <| fun _ -> model.LoginPressed.OnNext ()
      ]
      UI.elem StackPanel [
        UI.orientation Orientation.Horizontal
        UI.children [
          UI.elem Button [
            UI.content "Login"
            UI.isEnabled model.LoginEnabled
            UI.onClick <| fun _ -> model.LoginPressed.OnNext ()
          ]
        ]
      ]
    ]
  ]

let loggedInView = UI.elem TextBlock [UI.text "👍😄👍"]

[<EntryPoint; STAThread>]
let main _ =
  let model = Model.create <| Atom.create Credentials.empty

  UI.run <| Application (
    MainWindow = UI.show (
      UI.window Window [
        UI.title "Login"
        UI.width 300.0
        UI.height 300.0
        model.LoggedIn
         |> Prop.ifElse (Prop.value loggedInView) (Prop.value (loginView model))
         |> UI.content
      ]
    )
  )
