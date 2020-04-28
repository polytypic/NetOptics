#r "bin/Debug/NetOptics.dll"

open NetOptics.Optic

view fstL ("basics", 101)
set (fstL << sndL) "Hello" (("basics", true), 101)

collect (elemsT << sndL) [(1, "a"); (2, "b")]

over (elemsT << fstL) (fun x -> x + 1) [(1, "a"); (2, "b")]

over (partsOf (elemsT << sndL) << arrayI) Array.rev [(1, "a"); (2, "b")]
