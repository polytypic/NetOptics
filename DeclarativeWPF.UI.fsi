namespace DeclarativeWPF

open NetAtom
open NetOptics
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Runtime.CompilerServices

type ExtAttribute = ExtensionAttribute

type [<Ext; Sealed>] UI =
  [<Ext>] static member AsProperty: IObs<'T> -> IObs<'T>

  [<Ext>] static member IfElse: IObs<bool> * IObs<'T> * IObs<'T> -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> *       'T * IObs<'T> -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> * IObs<'T> *      'T  -> IObs<'T>
  [<Ext>] static member IfElse: IObs<bool> *      'T  *      'T  -> IObs<'T>

  static member bind: seq<'E -> unit> -> ('E -> UIElement) when 'E :> UIElement

  static member children:     #IROL<UIElement  >  -> (#Panel -> unit)
  static member children: IObs<IROL<UIElement  >> -> (#Panel -> unit)
  static member children: IObs<     UIElement[]>  -> (#Panel -> unit)
  static member children: IObs<list<UIElement> >  -> (#Panel -> unit)

  static member content: IObs<#obj> -> (#ContentControl -> unit)
  
  static member isEnabled: IObs<bool> -> (#UIElement -> unit)
  
  static member isChecked: IAtom<bool> -> (#ToggleButton -> unit)

  static member isReadOnly: IObs<bool> -> (#TextBoxBase -> unit)

  static member maximum: IObs<float> -> (#RangeBase -> unit)
  static member minimum: IObs<float> -> (#RangeBase -> unit)
  static member value: IAtom<float> -> (#RangeBase -> unit)

  static member text: IAtom<string> -> (#TextBox -> unit)
  static member text: IObs<string> -> (#TextBlock -> unit)

  static member password: IAtom<string> -> (PasswordBox -> unit)

  static member onEnter: ('E -> unit) -> ('E -> unit) when 'E :> UIElement

  static member onLostFocus: ('E -> unit) -> ('E -> unit) when 'E :> UIElement

  static member onClick: ('E -> unit) -> ('E -> unit) when 'E :> ButtonBase

  static member onMouseDoubleClick: ('E -> unit)
                                 -> ('E -> unit) when 'E :> Control

  static member dock: Dock -> (#UIElement -> unit)

  static member show: Window -> Window

  static member run: Application -> int
