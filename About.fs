/// You can use modules in Avalonia.FuncUI in the same way you would do
/// in [Elmish ](https://elmish.github.io/elmish/)
namespace Lab1

module About =
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL

    module View =

        let headerView =
            lazy
                (StackPanel.create [ StackPanel.dock Dock.Top
                                     StackPanel.verticalAlignment VerticalAlignment.Top
                                     StackPanel.children [ TextBlock.create [ TextBlock.classes [ "title" ]
                                                                              TextBlock.fontSize 24.0
                                                                              TextBlock.text
                                                                                  "Виконано Андращуком Едуардом, варіант 2" ]
                                                           TextBlock.create [ TextBlock.classes [ "subtitle" ]
                                                                              TextBlock.fontSize 18.0
                                                                              TextBlock.text
                                                                                  ("Доступні операції:\n"
                                                                                   + "+, -, *, / (бінарні операції);\n"
                                                                                   + "mod, dіv;\n"
                                                                                   + "+, - (унарні операції);\n"
                                                                                   + "іnc, dec") ] ] ])

        let view =
            lazy
                (DockPanel.create [ DockPanel.horizontalAlignment HorizontalAlignment.Center
                                    DockPanel.verticalAlignment VerticalAlignment.Top
                                    DockPanel.margin (0.0, 20.0, 0.0, 0.0)
                                    DockPanel.children [ headerView.Value ] ])
