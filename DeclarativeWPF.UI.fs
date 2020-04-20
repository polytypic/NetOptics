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

module UI =
  let isVisibleProperties = ConditionalWeakTable<UIElement, IObservable<bool>>()

[<Extension>]
type [<Sealed>] UI =
  [<Extension>]
  static member AsProperty(o: IObs<_>) =
    o.DistinctUntilChanged().Replay(1).RefCount()

  static member isVisible (e: UIElement) =
    UI.isVisibleProperties.GetValue(e, fun e ->
      e.IsVisibleChanged.Select(fun _ -> e.IsVisible).AsProperty())

  static member bind (bs: seq<'c -> unit> when 'c :> UIElement) = fun (c: 'c) ->
    for b in bs do
      b c
    c :> UIElement

  static member subscribeVisible (o: IObs<'x>)
                                 (action: 'x -> unit)
                                 (c: #UIElement) =
    (UI.isVisible c)
      .Select(fun b -> if b then o else Observable.Empty<_>())
      .Switch()
      .ObserveOnDispatcher()
      .Subscribe(action) |> ignore

  static member bindAtom (set: 'c -> 'v -> unit)
                         (o: 'c -> IObs<'v>)
                         (v: IAtom<_>)
                         (c: #UIElement) =
    UI.subscribeVisible v (set c) c
    UI.subscribeVisible (o c) (Atom.set v) c

  static member children (es: #IROL<UIElement>) = fun (c: #Panel) ->
    let c = c.Children
    c.Clear ()
    for e in es do
      c.Add e |> ignore

  static member children (es: IObs<IROL<UIElement>>) =
    fun (c: #Panel) -> UI.subscribeVisible es (fun es -> UI.children es c) c

  static member children (es: IObs<UIElement[]>) = fun (c: #Panel) ->
    UI.subscribeVisible es (fun es -> UI.children es c) c

  static member content (o: IObs<obj>) = fun (c: #ContentControl) ->
    UI.subscribeVisible o (fun v -> c.Content <- v) c

  static member isEnabled (o: IObs<bool>) = fun (c: #UIElement) ->
    UI.subscribeVisible o (fun v -> c.IsEnabled <- v) c

  static member isChecked v = fun (c: #ToggleButton) ->
    UI.bindAtom <| fun (c: ToggleButton) t -> c.IsChecked <- Nullable<bool> t
                <| fun c -> c.Click.Select(fun _ -> c.IsChecked.Value)
                <| v
                <| (c :> ToggleButton)

  static member isReadOnly (o: IObs<bool>) = fun (c: #TextBoxBase) ->
    UI.subscribeVisible o (fun v -> c.IsReadOnly <- v) c

  static member onEnter action = fun (c: #UIElement) ->
    let enter =
      c.KeyDown.Where(fun e -> e.Key = Key.Enter)
        .Select(fun e -> e.Handled <- true)
    UI.subscribeVisible enter (fun _ -> action c) c

  static member text v = fun (c: #TextBox) ->
    UI.bindAtom <| fun (c: TextBox) t -> c.Text <- t
                <| fun c -> c.TextChanged.Select(fun _ -> c.Text)
                <| v
                <| (c :> TextBox)

  static member text (v: IObs<string>) = fun (c: #TextBlock) ->
    UI.subscribeVisible v (fun v -> c.Text <- v) c

  static member onLostFocus action = fun (c: #UIElement) ->
    UI.subscribeVisible c.LostFocus (fun _ -> action c) c

  static member onClick action = fun (c: #ButtonBase) ->
    UI.subscribeVisible c.Click (fun _ -> action c) c

  static member onMouseDoubleClick action = fun (c: #Control) ->
    UI.subscribeVisible c.MouseDoubleClick (fun _ -> action c) c

  static member dock d = fun (c: #UIElement) -> DockPanel.SetDock(c, d)

  static member show (w: Window) = w.Show (); w

  static member run (a: Application) = a.Run ()
