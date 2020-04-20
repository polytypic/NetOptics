module App.Contacts

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls

module Contact =
  type t = {Name: string; Phone: string}
  let name = Optic.lens (fun t -> t.Name) (fun v t -> {t with Name = v})
  let phone = Optic.lens (fun t -> t.Phone) (fun v t -> {t with Phone = v})

let contactView contact =
  StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [|
    UI.children [|
      Button (Content = "Remove") |> UI.bind [|
        UI.onClick <| fun _ -> Atom.remove contact
      |]
      TextBox (Width = 100.0) |> UI.bind [|
        UI.text (Atom.view Contact.name contact)
      |]
      TextBox (Width = 100.0) |> UI.bind [|
        UI.text (Atom.view Contact.phone contact)
      |]
    |]
  |]

let contactsView contacts =
  StackPanel (Orientation = Orientation.Vertical) |> UI.bind [|
    UI.children [|
      StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [|
        UI.children [|
          Button (Content = "Add") |> UI.bind [|
            UI.onClick <| fun _ ->
              Atom.setAt
                Optic.appendL
                contacts
                [|{Contact.Name = ""; Contact.Phone = ""}|]
          |]
        |]
      |]
      StackPanel (Orientation = Orientation.Vertical) |> UI.bind [|
        UI.children (Atom.map contactView contacts)
      |]
    |]
  |]

[<EntryPoint; STAThread>]
let main _ =
  let state = Atom.create<IROL<Contact.t>> [|
    {Name = "Would"; Phone = "you"}
    {Name = "like"; Phone = "to know?"}
  |]

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Contacts",
        Width = 300.0,
        Height = 300.0,
        Content = contactsView state
      )
    )
  )
