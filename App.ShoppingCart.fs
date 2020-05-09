module App.ShoppingCart

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls

let stackElems orientation (children: IROL<_>) =
  UI.elem StackPanel [
    UI.orientation orientation
    UI.children children
  ]

let counterElem count =
  stackElems Orientation.Horizontal [
    UI.elem Button [
      UI.content "-"
      UI.onClick (Atom.modifyAct count ((+) -1))
    ]
    UI.elem Label [UI.content (UI.lift1 string count)]
    UI.elem Button [
      UI.content "+"
      UI.onClick (Atom.modifyAct count ((+) +1))
    ]
  ]

let removableElem removable =
  UI.elem Button [
    UI.content "🗑️"
    UI.onClick (Atom.removeAct removable)
  ]

let removableCounterElem count =
  stackElems Orientation.Horizontal [
    removableElem count
    counterElem count
  ]

type CartItem = {Id: int; Count: int}
module CartItem =
  let id t = t.Id
  let count =
    Optic.lens (fun t -> t.Count) (fun v t -> {t with Count = v})
    << Optic.removeEqL 0 << Optic.rewriteI (max 0)
  let byId Id =
    Optic.findL (id >> (=) Id) << Optic.toDefaultI {Id=Id; Count=0}

type InventoryItem = {Id: int; Name: string; Price: float}

let cartItemElem inventory id cartItem =
  let inventoryItem = Map.find id inventory
  stackElems Orientation.Horizontal [
    removableCounterElem (Atom.view CartItem.count cartItem)
    UI.elem Label [UI.content inventoryItem.Name]
  ]

let cartElem inventory cart =
  let total =
    cart
     |> UI.lift1 (
          Seq.sumBy <| fun (item: CartItem) ->
            let info = Map.find item.Id inventory
            info.Price * float item.Count
        )
  stackElems Orientation.Vertical [
    UI.elem Label [UI.content "Shopping Cart"]
    UI.elem StackPanel [
      UI.orientation Orientation.Vertical
      cart
       |> Atom.mapByKey CartItem.id (cartItemElem inventory)
       |> UI.children
    ]
    UI.elem Label [
      UI.content (UI.lift1 (sprintf "Total: %.2f") total)
    ]
  ]

let inventoryItemElem cart (inventoryItem: InventoryItem) =
  stackElems Orientation.Horizontal [
    removableCounterElem
     <| Atom.view
          (CartItem.byId inventoryItem.Id << CartItem.count)
          cart
    UI.elem Label [UI.content inventoryItem.Name]
    UI.elem Label [UI.content inventoryItem.Price]
  ]

let inventoryElem cart inventory =
  stackElems Orientation.Vertical [
    UI.elem Label [UI.content "Inventory"]
    inventory
     |> Map.toArray
     |> Array.map (snd >> inventoryItemElem cart)
     |> stackElems Orientation.Vertical
  ]

[<EntryPoint; STAThread>]
let main _ =
  let cart = Atom.create<IROL<_>> [||]
  let inventory =
    Map.ofArray << Array.map (fun t -> (t.Id, t)) <| [|
      {Id = 1; Price = 1.0; Name = "Toilet paper"}
      {Id = 2; Price = 2.5; Name = "Bread"}
      {Id = 3; Price = 2.0; Name = "Butter"}
      {Id = 4; Price = 3.0; Name = "Milk"}
      {Id = 5; Price = 2.5; Name = "Coffee"}
      {Id = 6; Price = 1.5; Name = "Cheese"}
    |]

  UI.run <| Application (
    MainWindow = UI.show (
      UI.window Window [
        UI.title "Shopping Cart"
        UI.width 400.0
        UI.height 300.0
        UI.content (
          stackElems Orientation.Horizontal [
            inventoryElem cart inventory
            cartElem inventory cart
          ]
        )
      ]
    )
  )
