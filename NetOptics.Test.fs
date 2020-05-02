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
       Optic.someP
    << (smallerL << valuesT
        |> Optic.andAlso valueL
        |> Optic.andAlso (greaterL << valuesT))
    <| p

  let rec nodeL key =
    Optic.choose <| function
      | None -> Optic.idI
      | Some n ->
        if     key < n.key then Optic.someP << smallerL << nodeL key
        elif n.key <   key then Optic.someP << greaterL << nodeL key
        else Optic.idI

  let valueP key = nodeL key << Optic.someP << valueL

let [<EntryPoint>] main _ =
  let mutable passed = 0
  let mutable failed = 0
  let testEq actual expected =
    if actual <> expected
    then failed <- failed + 1; printfn "Expected %A, but got %A" expected actual
    else passed <- passed + 1
  testEq
   <| Optic.view (Optic.fstL << Optic.sndL) (("101", 42), true)
   <| 42
  testEq
   <| Optic.canView (Optic.atP 5) [|3; 1; 4|]
   <| false
  testEq
   <| Optic.fold 0 (+) Optic.elemsT [|3; 1; 4|]
   <| 8
  testEq
   <| Optic.over (Optic.elemsT << Optic.fstL) (~-)
        [|(3, "a"); (1, "b"); (4, "c")|]
   <| upcast [|(-3, "a"); (-1, "b"); (-4, "c")|]
  testEq
   <| Optic.remove (Optic.atP 1) [|3; 1; 4|]
   <| upcast [|3; 4|]
  testEq
   <| Optic.remove (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3))
        [|(1, 3); (1, 1); (1, 4); (1, 1); (1, 5); (1, 9); (1, 2)|]
   <| upcast [|(1, 3); (1, 4); (1, 5); (1, 9)|]
  testEq
   <| Optic.collect
        (Optic.elemsT << Optic.sndL << Optic.whereP (fun x -> x < 3))
        [|(1, 3); (1, 1); (1, 4); (1, 1); (1, 5); (1, 9); (1, 2)|]
   <| upcast [|1; 1; 2|]
  testEq
   <| Optic.disperse
        (Optic.elemsT
          << Optic.sndL
          << Optic.whereP (fun x -> x < 3))
        [|-9; -1; -1|]
        [|(1, 3); (1, 1); (1, 4); (1, 1); (1, 5); (1, 9); (1, 2)|]
   <| upcast [|(1, 3); (1, -9); (1, 4); (1, -1); (1, 5); (1, 9); (1, -1)|]
  testEq
   <| Optic.over
        (Optic.partsOf
          (Optic.elemsT
            << Optic.sndL
            << Optic.whereP (fun x -> x < 3)))
        (Optic.view Optic.revI)
        [|(1, 3); (1, 1); (1, 4); (1, 1); (1, 5); (1, 9); (1, 2)|]
   <| upcast [|(1, 3); (1, 2); (1, 4); (1, 1); (1, 5); (1, 9); (1, 1)|]
  testEq
   <| Optic.review Optic.revI [|1;2;3|]
   <| upcast [|3; 2; 1|]
  testEq
   <| Optic.review
        (Optic.splitI '-'
         << Optic.revI
         << Optic.invertI (Optic.splitI '-'))
        "this-it-is"
   <| "is-it-this"
  if failed <> 0
  then printfn "%d PASSED, %d FAILED" passed failed; 1
  else printfn "%d PASSED" passed; 0
