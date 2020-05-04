module App.Login

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Reactive.Linq
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
  let private pass = {Username = "simon"; Password = "letmein"}
  let login credentials =
    Observable.Return(())
      .Delay(TimeSpan.FromSeconds 2.0)
      .Select(fun _ -> credentials = pass)

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
      UI.lift1 (fun c -> c.Username = "" || c.Password = "") credentials
    let loginResult =
      loginPressed
        .SelectMany(fun _ -> credentials.Take(1))
        .SelectMany(Api.login)
    let loggedIn = loginResult.StartWith(false).AsProperty()
    let loginInProgress =
      Observable.Merge(
        loginPressed.Select(fun _ -> true),
        loginResult.Select(fun _ -> false)
      ).StartWith(false).AsProperty()
    let inputDisabled = UI.lift2 (||) loggedIn loginInProgress
    let loginDisabled = UI.lift2 (||) hasEmptyCredentials inputDisabled
    {
      Credentials = credentials
      HasEmptyCredentials = hasEmptyCredentials
      LoginPressed = loginPressed
      LoggedIn = loggedIn
      LoginInProgress = loginInProgress
      InputEnabled = UI.lift1 not inputDisabled
      LoginEnabled = UI.lift1 not loginDisabled
    }

let loginView (model: Model.t) =
  StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
    UI.children [
      TextBox () |> UI.bind [
        UI.isEnabled model.InputEnabled
        UI.text <| Atom.view Credentials.username model.Credentials
      ]
      PasswordBox () |> UI.bind [
        UI.isEnabled model.InputEnabled
        UI.password <| Atom.view Credentials.password model.Credentials
        UI.onEnter <| fun _ -> model.LoginPressed.OnNext ()
      ]
      StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [
        UI.children [
          Button (Content = "Login") |> UI.bind [
            UI.isEnabled model.LoginEnabled
            UI.onClick <| fun _ -> model.LoginPressed.OnNext ()
          ]
        ]
      ]
    ]
  ]

let loggedInView () =
  TextBlock (Text = "👍😄👍") |> UI.bind []

[<EntryPoint; STAThread>]
let main _ =
  let model = Model.create <| Atom.create Credentials.empty

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Login",
        Width = 300.0,
        Height = 300.0,
        Content = (
          ContentControl () |> UI.bind [
            model.LoggedIn.IfElse(loggedInView (), loginView model)
             |> UI.content
          ]
        )
      )
    )
  )
