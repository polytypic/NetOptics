module App.Contacts

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Reactive.Linq
open System.Windows
open System.Windows.Controls

type Contact = {Name: string; Phone: string}
module Contact =
  let name = Optic.lens (fun t -> t.Name) (fun v t -> {t with Name = v})
  let phone = Optic.lens (fun t -> t.Phone) (fun v t -> {t with Phone = v})
  let empty = {Name = ""; Phone = ""}

let textBox text = TextBox (Width = 100.0) |> UI.bind [ UI.text text ]

let contactView contact =
  StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [
    UI.children [
      Button (Content = "Remove") |> UI.bind [
        UI.onClick <| Atom.removeAct contact
      ]
      Atom.view Contact.name contact |> textBox
      Atom.view Contact.phone contact |> textBox
    ]
  ]

let contactsView contacts =
  StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
    UI.children [
      StackPanel (Orientation = Orientation.Horizontal) |> UI.bind [
        UI.children [
          Button (Content = "Add") |> UI.bind [
            UI.onClick <| Atom.setAtAct Optic.appendL contacts [Contact.empty]
          ]
        ]
      ]
      StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
        Atom.map contactView contacts |> UI.children
      ]
    ]
  ]

let countDownButton label value =
  Button () |> UI.bind [
    UI.onClick <| Atom.modifyAct value ((+) -1)
    UI.isEnabled (value.Select((<>) 0))
    UI.content (value.Select(sprintf"%s (%d)" label))
  ]

let historyView history =
  DockPanel () |> UI.bind [
    UI.children [
      countDownButton "Undo" <| Atom.view History.undoIndex history
      countDownButton "Redo" <| Atom.view History.redoIndex history
      Slider (Minimum = 0.0, SmallChange = 1.0) |> UI.bind [
        UI.maximum (history.Select(History.indexMax >> float))
        UI.value <| Atom.view (History.index << Optic.truncateI) history
      ]
    ]
  ]

[<EntryPoint; STAThread>]
let main _ =
  let state =
    Atom.create << History.init<IROL<_>> id
     <| [|{Name = "Would"; Phone = "you"}
          {Name = "like"; Phone = "to know?"}|]

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Contacts",
        Width = 300.0,
        Height = 300.0,
        Content = (
          StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
            UI.children [
              historyView state
              contactsView (Atom.view History.present state)
            ]
          ]
        )
      )
    )
  )
