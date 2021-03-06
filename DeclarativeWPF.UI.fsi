namespace DeclarativeWPF

open NetAtom
open NetOptics
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Runtime.CompilerServices

type UI<'T>

type ExtAttribute = ExtensionAttribute

type [<Ext; Sealed>] UI =
  [<Ext>] static member AsProperty: IObs<'T> -> IObs<'T>

  [<Ext>] static member IfElse: IObs<bool> * IObs<'T> * IObs<'T> -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> *       'T * IObs<'T> -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> * IObs<'T> *      'T  -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> *      'T  *      'T  -> IObs<'T>

  static member lift1: ('S1 -> 'T) -> (#IObs<'S1> -> IObs<'T>)
  static member lift2: ('S1 -> 'S2 -> 'T)
                    -> (#IObs<'S1> -> #IObs<'S2> -> IObs<'T>)

  static member elem: constructor: (unit -> 'E)
                   -> (#IROL<'E -> unit> -> UI<UIElement>) when 'E :> UIElement

  static member window: constructor: (unit -> 'W)
                     -> (#IROL<'W -> unit> -> UI<Window>) when 'W :> Window

  static member children:     #IROL<UI<UIElement>  >  -> (#Panel -> unit)
  static member children: IObs<IROL<UI<UIElement>  >> -> (#Panel -> unit)
  static member children: IObs<     UI<UIElement>[]>  -> (#Panel -> unit)
  static member children: IObs<list<UI<UIElement>> >  -> (#Panel -> unit)

  static member orientation: Orientation -> (#StackPanel -> unit)

  static member title: string -> (#Window -> unit)

  static member width:  float -> (#FrameworkElement -> unit)
  static member height: float -> (#FrameworkElement -> unit)

  static member content:       obj  -> (#ContentControl -> unit)
  static member content: IObs<#obj> -> (#ContentControl -> unit)

  static member content:      UI<UIElement>  -> (#ContentControl -> unit)
  static member content: IObs<UI<UIElement>> -> (#ContentControl -> unit)
  
  static member isEnabled: IObs<bool> -> (#UIElement -> unit)
  
  static member isChecked: IAtom<bool> -> (#ToggleButton -> unit)

  static member isReadOnly: IObs<bool> -> (#TextBoxBase -> unit)

  static member maximum:      float  -> (#RangeBase -> unit)
  static member maximum: IObs<float> -> (#RangeBase -> unit)
  static member minimum:      float  -> (#RangeBase -> unit)
  static member minimum: IObs<float> -> (#RangeBase -> unit)

  static member smallChange: float -> (#RangeBase -> unit)

  static member value: IAtom<float> -> (#RangeBase -> unit)

  static member text: IAtom<string> -> (#TextBox -> unit)

  static member text:      string  -> (#TextBlock -> unit)
  static member text: IObs<string> -> (#TextBlock -> unit)

  static member password: IAtom<string> -> (PasswordBox -> unit)

  static member onEnter: ('E -> unit) -> ('E -> unit) when 'E :> UIElement

  static member onLostFocus: ('E -> unit) -> ('E -> unit) when 'E :> UIElement

  static member onClick: ('E -> unit) -> ('E -> unit) when 'E :> ButtonBase

  static member onMouseDoubleClick: ('E -> unit)
                                 -> ('E -> unit) when 'E :> Control

  static member dock: Dock -> (#UIElement -> unit)

  static member show: UI<Window> -> Window

  static member run: Application -> int
