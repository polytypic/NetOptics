module NetOptics.Test

type Node<'k, 'v> =
  { key: 'k
    value: 'v
    smaller: BST<'k, 'v>
    greater: BST<'k, 'v> }
and BST<'k, 'v> = option<Node<'k, 'v>>

module Node =
  let keyL p = Optic.lens (fun r -> r.key) (fun v r -> {r with key = v}) p
  let valueL p = Optic.lens (fun r -> r.value) (fun v r -> {r with value = v}) p
  let smallerL p =
    Optic.lens (fun r -> r.smaller) (fun v r -> {r with smaller = v}) p
  let greaterL p =
    Optic.lens (fun r -> r.greater) (fun v r -> {r with greater = v}) p

module BST =
  open Node

  let rec valuesT p =
       Optic.optionP
    << (smallerL << valuesT
        |> Optic.andAlso valueL
        |> Optic.andAlso (greaterL << valuesT))
    <| p

  let rec nodeL key =
    Optic.choose <| function
      | None -> Optic.idI
      | Some n ->
        if     key < n.key then Optic.optionP << smallerL << nodeL key
        elif n.key <   key then Optic.optionP << greaterL << nodeL key
        else Optic.idI

  let valueP key = nodeL key << Optic.optionP << valueL

let [<EntryPoint>] main _ =
  printfn "%A" <| Optic.view (Optic.fstL << Optic.sndL) (("101", 42), true)
  printfn "%A" <| Optic.canView (Optic.atP 5) [|3; 1; 4|]
  printfn "%A" <| Optic.over (Optic.elemsT << Optic.fstL) (~-) [|(3, "a"); (1, "b"); (4, "c")|]
  printfn "%A" <| Optic.fold 0 (+) Optic.elemsT [|3; 1; 4|]
  printfn "%A" <| Optic.remove (Optic.atP 1) [|3; 1; 4|]
  printfn "%A"
  <| Optic.remove (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3))
       [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|]
  printfn "%A"
  <| Optic.collect (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3))
       [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|]
  printfn "%A"
  <| Optic.disperse (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3)) [|-9;-1;-1|]
       [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|]
  printfn "%A"
  <| Optic.over (Optic.partsOf (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3))) (Optic.view Optic.revI)
       [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|]
  printfn "%A" <| Optic.review Optic.revI [|1;2;3|]
  printfn "%A" <| Optic.review (Optic.splitI '-' << Optic.revI << Optic.invertI (Optic.splitI '-')) "this-it-is"
  0
