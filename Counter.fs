namespace Lab1

module Counter =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type Row = int
    type Col = int
    type Msg = Msg of Row * Col * string

    type State =
        { Table: string list list
          Coords: (Row * Col) }

    let init =
        { Table = [ for _ in 1 .. 10 -> [ for _ in 1 .. 10 -> "" ] ]
          Coords = (1, 1) }



    let updateCell row col (str: string) table =
        [ for i in 0 .. List.length table - 1 ->
            [ for j in 0 .. List.length (table.Item(0)) - 1 -> if i = row && j = col then str else table.Item(i).Item(j) ] ]

    let update (msg: Msg) (state: State): State =
        match msg with
        | Msg (row, col, str) ->
            { state with
                  Table = updateCell row col str state.Table }

    let generateTable (state: State) =
        StackPanel.create [ StackPanel.dock Dock.Top
                            StackPanel.children
                                (List.concat [ for i in 1 .. state.Table.Length ->
                                                   [ for j in 1 .. state.Table.Item(0).Length ->
                                                       TextBlock.create [ TextBlock.dock Dock.Top
                                                                          TextBlock.fontSize 48.0
                                                                          TextBlock.verticalAlignment
                                                                              VerticalAlignment.Center
                                                                          TextBlock.horizontalAlignment
                                                                              HorizontalAlignment.Center
                                                                          TextBlock.text (string state.Table) ] ] ]) ]

    let view (state: State) (dispatch) =
        DockPanel.create [ DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                    StackPanel.margin 5.0
                                                                    StackPanel.spacing 5.0
                                                                    StackPanel.children [ TextBox.create [ TextBox.watermark
                                                                                                               "I'm a placeholder"
                                                                                                           TextBox.verticalAlignment
                                                                                                               VerticalAlignment.Center
                                                                                                           TextBox.horizontalAlignment
                                                                                                               HorizontalAlignment.Center
                                                                                                           TextBox.text
                                                                                                               (string
                                                                                                                   (state.Table.Item(1)
                                                                                                                         .Item(1))) ]
                                                                                          Border.create [ Border.borderThickness
                                                                                                              2.0
                                                                                                          Border.child
                                                                                                              (StackPanel.create [ StackPanel.children [ (TextBlock.create [ TextBlock.text
                                                                                                                                                                                 (string
                                                                                                                                                                                     (state.Table.Item(1)
                                                                                                                                                                                           .Item(1))) ]) ] ]) ]
                                                                                          Button.create [ Button.onClick (fun _ ->
                                                                                                              dispatch
                                                                                                                  (Msg
                                                                                                                      (1,
                                                                                                                       1,
                                                                                                                       "abc")))
                                                                                                          Button.content
                                                                                                              "1 1"
                                                                                                          Button.classes [ "plus" ] ] ] ]

                                                TextBlock.create [ TextBlock.dock Dock.Top
                                                                   TextBlock.fontSize 48.0
                                                                   TextBlock.verticalAlignment VerticalAlignment.Center
                                                                   TextBlock.horizontalAlignment
                                                                       HorizontalAlignment.Center
                                                                   TextBlock.text (string state.Table) ] ] ]
