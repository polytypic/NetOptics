namespace DeclarativeWPF

open NetAtom
open NetOptics
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Input
open System.Reactive.Linq
open System.Runtime.CompilerServices
open System.Collections.Generic

type UI<'T> = Pure of (unit -> 'T)

module UI =
  let isVisibleProperties = ConditionalWeakTable<UIElement, IObs<bool>>()

  let instantiate (Pure ui) = ui ()

  let physicalEqualityComparer<'T when 'T : not struct> =
    {new IEqualityComparer<'T> with
      member t.GetHashCode x = LanguagePrimitives.PhysicalHash x
      member t.Equals (lhs, rhs) = LanguagePrimitives.PhysicalEquality lhs rhs}

  let childrenWithCache (c: UIElementCollection) =
    let cache = Dictionary<_, _>(physicalEqualityComparer)
    fun (pes: #IROL<UI<UIElement>>) ->
      c.Clear () // TODO: More incremental update of children
      let counts = Dictionary<_, _>(cache.Count, physicalEqualityComparer)
      for pe in pes do
        let mutable count = 0
        counts.TryGetValue (pe, &count) |> ignore
        let mutable elems = Unchecked.defaultof<_>
        if not (cache.TryGetValue (pe, &elems)) then
          elems <- ResizeArray<_>(1)
          cache.Add (pe, elems)
        let elem =
          if elems.Count <= count then
            elems.Add (instantiate pe)
          elems.[count]
        counts.[pe] <- count + 1
        c.Add elem |> ignore
      let toRemove = ResizeArray<_>(cache.Count)
      for kv in cache do
        let mutable count = 0
        counts.TryGetValue (kv.Key, &count) |> ignore
        if count = 0 then
          toRemove.Add kv.Key
        elif count < kv.Value.Count then
          kv.Value.RemoveRange (count, kv.Value.Count - count)
      for k in toRemove do
        cache.Remove k |> ignore

type [<Extension; Sealed>] UI =
  static member observeOnDispatcher (xO: IObs<_>) =
    xO.ObserveOnDispatcher()

  static member isVisible (e: UIElement) =
    UI.isVisibleProperties.GetValue(e, fun e ->
      e.IsVisibleChanged |> Prop.map (fun _ -> e.IsVisible))

  static member wrap (cast: 'E -> 'R) (constructor: unit -> 'E) =
    fun (bindings: #IROL<'E -> unit>) -> Pure <| fun () ->
      let element = constructor ()
      for binding in bindings do
        binding element
      cast element
  static member elem (constructor: unit -> 'E when 'E :> UIElement) =
    fun (bindings: #IROL<'E -> unit>) ->
      UI.wrap (fun x -> x :> UIElement) constructor bindings
  static member window (constructor: unit -> 'E when 'E :> Window) =
    fun (bindings: #IROL<'E -> unit>) ->
      UI.wrap (fun x -> x :> Window) constructor bindings

  static member subscribeVisible (c: #UIElement)
                                 (o: IObs<'x>)
                                 (action: 'x -> unit) =
    UI.isVisible c
     |> Stream.ifElse o Stream.empty<_>
     |> UI.observeOnDispatcher
     |> Stream.subscribe action
     |> ignore

  static member bindAtom initial
                         (c: #UIElement)
                         (o: IObs<_>)
                         (v: IAtom<'V>)
                         (get: unit -> 'V)
                         (set: 'V -> unit) =
    let mutable setting = initial
    UI.subscribeVisible c o <| fun _ ->
      if setting then setting <- false else Atom.set v (get ())
    UI.subscribeVisible c v <| fun v ->
      if get () <> v then setting <- true; set v

  static member children (pes: #IROL<UI<UIElement>>) = fun (c: #Panel) ->
    let c = c.Children
    c.Clear ()
    for pe in pes do
      c.Add (UI.instantiate pe) |> ignore

  static member children (pes: IObs<IROL<UI<UIElement>>>) = fun (c: #Panel) ->
    UI.subscribeVisible c pes <| UI.childrenWithCache c.Children

  static member children (pes: IObs<UI<UIElement>[]>) = fun (c: #Panel) ->
    UI.subscribeVisible c pes <| UI.childrenWithCache c.Children

  static member children (pes: IObs<list<UI<UIElement>>>) = fun (c: #Panel) ->
    UI.subscribeVisible c pes <| UI.childrenWithCache c.Children

  static member orientation v = fun (c: #StackPanel) -> c.Orientation <- v

  static member title t = fun (c: #Window) -> c.Title <- t

  static member width v = fun (c: #FrameworkElement) -> c.Width <- v
  static member height v = fun (c: #FrameworkElement) -> c.Height <- v

  static member content (v: obj) = fun (c: #ContentControl) -> c.Content <- v

  static member content (o: IObs<#obj>) = fun (c: #ContentControl) ->
    UI.subscribeVisible c o <| fun v -> c.Content <- v

  static member content (pe: UI<UIElement>) = fun (c: #ContentControl) ->
    c.Content <- UI.instantiate pe

  static member content (o: IObs<UI<UIElement>>) = fun (c: #ContentControl) ->
    UI.subscribeVisible c o <| fun pe -> c.Content <- UI.instantiate pe

  static member isEnabled (o: IObs<bool>) = fun (c: #UIElement) ->
    UI.subscribeVisible c o <| fun v -> c.IsEnabled <- v

  static member isChecked v = fun (c: #ToggleButton) ->
    UI.bindAtom false c (Stream.merge [|c.Checked; c.Unchecked|]) v
     <| fun _ -> c.IsChecked.Value
     <| fun v -> c.IsChecked <- Nullable<bool> v

  static member isReadOnly (o: IObs<bool>) = fun (c: #TextBoxBase) ->
    UI.subscribeVisible c o <| fun v -> c.IsReadOnly <- v

  static member onEnter action = fun (c: #UIElement) ->
    let enter =
      c.KeyDown
       |> Stream.filter (fun e -> e.Key = Key.Enter)
       |> Stream.map (fun (e: KeyEventArgs) -> e.Handled <- true)
    UI.subscribeVisible c enter <| fun _ -> action c

  static member maximum v = fun (c: #RangeBase) -> c.Maximum <- v
  static member maximum v = fun (c: #RangeBase) ->
    UI.subscribeVisible c v <| fun v -> c.Maximum <- v

  static member minimum v = fun (c: #RangeBase) -> c.Minimum <- v
  static member minimum v = fun (c: #RangeBase) ->
    UI.subscribeVisible c v <| fun v -> c.Maximum <- v

  static member smallChange v = fun (c: #RangeBase) -> c.SmallChange <- v

  static member value v = fun (c: #RangeBase) ->
    UI.bindAtom true c c.ValueChanged v
     <| fun _ -> c.Value
     <| fun v -> c.Value <- v

  static member text v = fun (c: #TextBox) ->
    UI.bindAtom true c c.TextChanged v
     <| fun _ -> c.Text
     <| fun v -> c.Text <- v

  static member text v = fun (c: #TextBlock) -> c.Text <- v

  static member text (v: IObs<string>) = fun (c: #TextBlock) ->
    UI.subscribeVisible c v <| fun v -> c.Text <- v

  static member password v = fun (c: PasswordBox) ->
    UI.bindAtom true c c.PasswordChanged v
     <| fun _ -> c.Password
     <| fun v -> c.Password <- v

  static member onLostFocus action = fun (c: #UIElement) ->
    UI.subscribeVisible c c.LostFocus <| fun _ -> action c

  static member onClick action = fun (c: #ButtonBase) ->
    UI.subscribeVisible c c.Click <| fun _ -> action c

  static member onMouseDoubleClick action = fun (c: #Control) ->
    UI.subscribeVisible c c.MouseDoubleClick <| fun _ -> action c

  static member dock d = fun (c: #UIElement) -> DockPanel.SetDock(c, d)

  static member show (pw: UI<Window>) =
    let w = UI.instantiate pw
    w.Show ()
    w

  static member run (a: Application) = a.Run ()
