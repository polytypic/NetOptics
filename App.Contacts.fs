module App.Contacts

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls

type Contact = {Name: string; Phone: string}
module Contact =
  let name = Optic.lens (fun t -> t.Name) (fun v t -> {t with Name = v})
  let phone = Optic.lens (fun t -> t.Phone) (fun v t -> {t with Phone = v})
  let empty = {Name = ""; Phone = ""}

let textBox (text: IAtom<_>) = UI.elem TextBox [UI.width 100.0; UI.text text]

let contactView contact =
  UI.elem StackPanel [
    UI.orientation Orientation.Horizontal
    UI.children [
      UI.elem Button [UI.content "Remove"; UI.onClick <| Atom.removeAct contact]
      Atom.view Contact.name contact |> textBox
      Atom.view Contact.phone contact |> textBox
    ]
  ]

let contactsView contacts =
  UI.elem StackPanel [
    UI.orientation Orientation.Vertical
    UI.children [
      UI.elem StackPanel [
        UI.orientation Orientation.Horizontal
        UI.children [
          UI.elem Button [
            UI.content "Add"
            UI.onClick <| Atom.setAtAct Optic.appendL contacts [Contact.empty]
          ]
        ]
      ]
      UI.elem StackPanel [
        UI.orientation Orientation.Vertical
        Atom.map contactView contacts |> UI.children
      ]
    ]
  ]

let countDownButton label value =
  UI.elem Button [
    UI.onClick <| Atom.modifyAct value ((+) -1)
    UI.isEnabled <| Prop.map ((<>) 0) value
    Prop.map (sprintf"%s (%d)" label) value |> UI.content
  ]

let historyView history =
  UI.elem DockPanel [
    UI.children [
      countDownButton "Undo" <| Atom.view History.undoIndex history
      countDownButton "Redo" <| Atom.view History.redoIndex history
      UI.elem Slider [
        UI.smallChange 1.0
        UI.minimum 0.0
        UI.maximum (Prop.map (History.indexMax >> float) history)
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
      UI.window Window [
        UI.title "Contacts"
        UI.width 300.0
        UI.height 300.0
        UI.content (
          UI.elem StackPanel [
            UI.orientation Orientation.Vertical
            UI.children [
              historyView state
              contactsView (Atom.view History.present state)
            ]
          ]
        )
      ]
    )
  )
