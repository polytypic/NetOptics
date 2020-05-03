module App.ShoppingCart

open DeclarativeWPF
open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls

let stackElems orientation (children: IROL<_>) =
  StackPanel (Orientation = orientation) |> UI.bind [UI.children children]

let counterElem count =
  stackElems Orientation.Horizontal [
    Button (Content = "-") |> UI.bind [
      UI.onClick <| Atom.modifyAct count ((+) -1)
    ]
    Label () |> UI.bind [UI.content (UI.lift1 string count)]
    Button (Content = "+") |> UI.bind [
      UI.onClick <| Atom.modifyAct count ((+) +1)
    ]
  ]

let removableElem removable =
  Button (Content = "🗑️") |> UI.bind [UI.onClick (Atom.removeAct removable)]

type CartItem = {Id: int; Count: int}
module CartItem =
  let id t = t.Id
  let count = Optic.lens (fun t -> t.Count) (fun v t -> {t with Count = v})
  let removableCount = count << Optic.removeEqL 0 << Optic.rewriteI (max 0)
  let byId Id = Optic.findL (id >> (=) Id) << Optic.defaultsI {Id=Id; Count=0}

type InventoryItem = {Id: int; Name: string; Price: float}

let itemControlsElem cartItem =
  stackElems Orientation.Horizontal [
    removableElem cartItem
    counterElem <| Atom.view CartItem.removableCount cartItem
  ]

let cartItemElem inventory id cartItem =
  let inventoryItem = Map.find id inventory
  stackElems Orientation.Horizontal [
    itemControlsElem cartItem
    Label (Content = inventoryItem.Name)
  ]

let cartElem inventory cart =
  let total =
    cart
     |> UI.lift1 (
          Seq.sumBy (fun (item: CartItem) ->
            let info = Map.find item.Id inventory
            info.Price * float item.Count))
  stackElems Orientation.Vertical [
    Label (Content = "Shopping Cart")
    StackPanel (Orientation = Orientation.Vertical) |> UI.bind [
      cart
       |> Atom.mapByKey CartItem.id (cartItemElem inventory)
       |> UI.children
    ]
    Label () |> UI.bind [
      UI.content (UI.lift1 (sprintf "Total: %.2f") total)
    ]
  ]

let inventoryItemElem cart (inventoryItem: InventoryItem) =
  stackElems Orientation.Horizontal [
    itemControlsElem <| Atom.view (CartItem.byId inventoryItem.Id) cart
    Label (Content = inventoryItem.Name)
    Label (Content = inventoryItem.Price)
  ]

let inventoryElem cart inventory =
  stackElems Orientation.Vertical [
    Label (Content = "Inventory")
    inventory
     |> Map.toArray
     |> Array.map (snd >> inventoryItemElem cart)
     |> stackElems Orientation.Vertical
  ]

[<EntryPoint; STAThread>]
let main _ =
  let cart = Atom.create<IROL<_>> [||]
  let inventory = Map.ofArray << Array.map (fun t -> (t.Id, t)) <| [|
    {Id = 1; Price = 1.0; Name = "Toilet paper"}
    {Id = 2; Price = 2.5; Name = "Bread"}
    {Id = 3; Price = 2.0; Name = "Butter"}
    {Id = 4; Price = 3.0; Name = "Milk"}
    {Id = 5; Price = 2.5; Name = "Coffee"}
    {Id = 6; Price = 1.5; Name = "Cheese"}
  |]

  UI.run <| Application (
    MainWindow = UI.show (
      Window (
        Title = "Shopping Cart",
        Width = 400.0,
        Height = 300.0,
        Content = (
          stackElems Orientation.Horizontal [
            inventoryElem cart inventory
            cartElem inventory cart
          ]
        )
      )
    )
  )
