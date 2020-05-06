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

type UI<'T> = Pure of (unit -> 'T)

type ExtAttribute = ExtensionAttribute

module UI =
  let isVisibleProperties = ConditionalWeakTable<UIElement, IObs<bool>>()

type [<Extension; Sealed>] UI =
  [<Ext>] static member AsProperty (o: IObs<_>) =
            o.DistinctUntilChanged().Replay(1).RefCount()

  [<Ext>] static member Cond (cond: IObs<_>, onT: IObs<_>, onF: IObs<_>) =
            cond.Select(fun c -> if c then onT else onF).Switch()

  [<Ext>] static member IfElse (cond: IObs<_>, onT: IObs<'T>, onF: IObs<'T>) =
            cond.Cond(onT, onF).AsProperty()
  [<Ext>] static member IfElse (cond: IObs<_>, onT:      'T , onF: IObs<'T>) =
            cond.IfElse(Observable.Return(onT), onF)
  [<Ext>] static member IfElse (cond: IObs<_>, onT: IObs<'T>, onF:      'T ) =
            cond.IfElse(onT, Observable.Return(onF))
  [<Ext>] static member IfElse (cond: IObs<_>, onT:      'T , onF:      'T ) =
            cond.IfElse(Observable.Return(onT), Observable.Return(onF))

  static member lift1 (fn: 'S1 -> 'T) = fun (x1: #IObs<'S1>) ->
    x1.Select(fn).AsProperty()

  static member lift2 (fn: 'S1 -> 'S2 -> 'T) =
    fun (x1: #IObs<'S1>) (x2: #IObs<'S2>) ->
      Observable.CombineLatest(x1, x2, fn).AsProperty()

  static member isVisible (e: UIElement) =
    UI.isVisibleProperties.GetValue(e, fun e ->
      e.IsVisibleChanged.Select(fun _ -> e.IsVisible).AsProperty())

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

  static member instantiate (Pure ui) = ui ()

  static member subscribeVisible (c: #UIElement)
                                 (o: IObs<'x>)
                                 (action: 'x -> unit) =
    (UI.isVisible c)
      .Cond(o, Observable.Empty<_>())
      .ObserveOnDispatcher()
      .Subscribe(action) |> ignore

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

  static member children (es: #IROL<UI<UIElement>>) = fun (c: #Panel) ->
    let c = c.Children
    c.Clear ()
    for e in es do
      c.Add (UI.instantiate e) |> ignore

  static member children (es: IObs<IROL<UI<UIElement>>>) =
    fun (c: #Panel) -> UI.subscribeVisible c es <| fun es -> UI.children es c

  static member children (es: IObs<UI<UIElement>[]>) = fun (c: #Panel) ->
    UI.subscribeVisible c es <| fun es -> UI.children es c

  static member children (es: IObs<list<UI<UIElement>>>) = fun (c: #Panel) ->
    UI.subscribeVisible c es <| fun es -> UI.children es c

  static member orientation v = fun (c: #StackPanel) -> c.Orientation <- v

  static member title t = fun (c: #Window) -> c.Title <- t

  static member width v = fun (c: #FrameworkElement) -> c.Width <- v
  static member height v = fun (c: #FrameworkElement) -> c.Height <- v

  static member content (v: obj) = fun (c: #ContentControl) -> c.Content <- v

  static member content (o: IObs<#obj>) = fun (c: #ContentControl) ->
    UI.subscribeVisible c o <| fun v -> c.Content <- v

  static member content (Pure v: UI<UIElement>) = fun (c: #ContentControl) ->
    c.Content <- v ()

  static member content (o: IObs<UI<UIElement>>) = fun (c: #ContentControl) ->
    UI.subscribeVisible c o <| fun (Pure v) -> c.Content <- v ()

  static member isEnabled (o: IObs<bool>) = fun (c: #UIElement) ->
    UI.subscribeVisible c o <| fun v -> c.IsEnabled <- v

  static member isChecked v = fun (c: #ToggleButton) ->
    UI.bindAtom false c (c.Checked.Merge(c.Unchecked)) v
     <| fun _ -> c.IsChecked.Value
     <| fun v -> c.IsChecked <- Nullable<bool> v

  static member isReadOnly (o: IObs<bool>) = fun (c: #TextBoxBase) ->
    UI.subscribeVisible c o <| fun v -> c.IsReadOnly <- v

  static member onEnter action = fun (c: #UIElement) ->
    let enter =
      c.KeyDown.Where(fun e -> e.Key = Key.Enter)
        .Select(fun e -> e.Handled <- true)
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

  static member show (Pure w: UI<Window>) =
    let w = w()
    w.Show ()
    w

  static member run (a: Application) = a.Run ()
