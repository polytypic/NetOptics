open NetOptics

let [<EntryPoint>] main args =
  printfn "%A" <| view (fstL << sndL) (("101", 42), true)
  printfn "%A" <| canView (atP 5) (upcast [|3; 1; 4|])
  printfn "%A" <| over (elemsT << fstL) (~-) (upcast [|(3, "a"); (1, "b"); (4, "c")|])
  printfn "%A" <| fold elemsT (+) 0 (upcast [|3; 1; 4|])
  printfn "%A" <| remove (atP 1) (upcast [|3; 1; 4|])
  printfn "%A"
  <| remove (elemsT << sndL << whereP (fun x -> x < 3))
       (upcast [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|])
  printfn "%A"
  <| collect (elemsT << sndL << whereP (fun x -> x < 3))
       (upcast [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|])
  printfn "%A"
  <| disperse (elemsT << sndL << whereP (fun x -> x < 3)) [|-9;-1;-1|]
       (upcast [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|])
  printfn "%A"
  <| over (partsOf (elemsT << sndL << whereP (fun x -> x < 3))) (view revI)
       (upcast [|(1,3);(1,1);(1,4);(1,1);(1,5);(1,9);(1,2)|])
  printfn "%A" <| review revI (upcast [|1;2;3|])
  printfn "%A" <| review (splitI '-' << revI << invertI (splitI '-')) "this-it-is"
  0
