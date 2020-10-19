namespace Lab1


module Counter =
    open Avalonia.FuncUI.Types
    open Avalonia.Controls
    open Avalonia.Media
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type Row = int
    type Col = int

    type Msg =
        | UpdateCellMsg
        | AddRowMsg
        | AddColumnMsg
        | RemoveRowMsg
        | RemoveColumnMsg
        | CursorMsg of Row * Col
        | CurrentCellDataMsg of string

    type State =
        { Table: string list list
          Cursor: (Row * Col)
          CurrentCellData: string }

    let init =
        { Table = [ for _ in 0 .. 10 -> [ for _ in 0 .. 10 -> "" ] ]
          Cursor = (2, 2)
          CurrentCellData = "" }


    let parseExpression = id

    let updateCell (row, col) state =
        [ for i in 0 .. state.Table.Length - 1 ->
            [ for j in 0 .. state.Table.Item(0).Length - 1 ->
                if i = row && j = col then state.CurrentCellData else state.Table.Item(i).Item(j) ] ]

    let genetateCell row col state dispatch =
        Button.create [ Button.verticalAlignment VerticalAlignment.Center
                        Button.horizontalAlignment HorizontalAlignment.Left
                        Button.width 75.0
                        Button.height 30.0
                        // Button.opacity (if state.Cursor = (row, col) then 11.0 else 10.0)
                        Button.foreground "black"
                        Button.background "white"
                        Button.borderBrush (if state.Cursor = (row, col) then "green" else "black")
                        Button.borderThickness (if state.Cursor = (row, col) then 2.0 else 1.5)
                        // Button.onPointerEnter (fun args ->
                        //     ((args.Source) :?> Button).BorderBrush <- Immutable.ImmutableSolidColorBrush
                        //                                                   ((Color.Parse "red"), 1.0))
                        // Button.onPointerLeave (fun args ->
                        //     ((args.Source) :?> Button).BorderBrush <- Immutable.ImmutableSolidColorBrush
                        //                                                   ((Color.Parse
                        //                                                       (if state.Cursor = (row, col) then
                        //                                                           "green"
                        //                                                        else
                        //                                                            "black")),
                        //                                                    1.0))
                        // Button.fontWeight
                        // Button.padding (left = 5.0, top = 5.0, right = 5.0, bottom = 5.0)
                        // Button.margin 1.0
                        Button.content (parseExpression (state.Table.Item(row).Item(col)))
                        Button.onClick (fun _ -> dispatch (CursorMsg(row, col))) ]

    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateCellMsg ->
            { state with
                  Table = updateCell (state.Cursor) state }
        | CursorMsg (row, col) -> { state with Cursor = (row, col) }
        | CurrentCellDataMsg str -> { state with CurrentCellData = str }
        | AddColumnMsg ->
            { state with
                  Table =
                      state.Table
                      @ [ [ for _ in 0 .. state.Table.Item(0).Length - 1 -> "" ] ] }
        | AddRowMsg ->
            { state with
                  Table = List.map (fun row -> row @ [ "" ]) state.Table }
        | RemoveColumnMsg ->
            { state with
                  Table = [ for i in 0 .. state.Table.Length - 2 -> state.Table.Item(i) ] }
        | RemoveRowMsg ->
            { state with
                  Table =
                      [ for i in 0 .. state.Table.Length - 1 ->
                          [ for j in 0 .. state.Table.Item(0).Length - 2 -> state.Table.Item(i).Item(j) ] ] }

    let generateInputBox state dispatch =
        UniformGrid.create [ UniformGrid.columns 2
                             UniformGrid.children [ TextBox.create [ TextBox.watermark "I'm a placeholder"
                                                                     TextBox.width 400.0
                                                                     TextBox.verticalAlignment VerticalAlignment.Center
                                                                     TextBox.horizontalAlignment
                                                                         HorizontalAlignment.Left
                                                                     TextBox.onTextChanged
                                                                         (CurrentCellDataMsg >> dispatch)
                                                                     TextBox.text
                                                                         (string
                                                                             (state.Table.Item(fst (state.Cursor))
                                                                                   .Item(snd (state.Cursor)))) ]
                                                    Button.create [ Button.onClick (fun _ -> (UpdateCellMsg |> dispatch))
                                                                    Button.content (string "Set cell value")
                                                                    Button.borderBrush "black"
                                                                    //   Button.classes [ "plus" ]
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (AddRowMsg |> dispatch))
                                                                    Button.content (string "Add new row")
                                                                    Button.borderBrush "black"
                                                                    //   Button.classes [ "plus" ]
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (AddColumnMsg |> dispatch))
                                                                    Button.content (string "Add new column")
                                                                    Button.borderBrush "black"
                                                                    //   Button.classes [ "plus" ]
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (RemoveRowMsg |> dispatch))
                                                                    Button.content (string "Remove last row")
                                                                    Button.borderBrush "black"
                                                                    //   Button.classes [ "plus" ]
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ ->
                                                                        (RemoveColumnMsg |> dispatch))
                                                                    Button.content (string "Remove last column")
                                                                    Button.borderBrush "black"
                                                                    //   Button.classes [ "plus" ]
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ] ] ]

    let generateTable state dispatch =
        UniformGrid.create [ UniformGrid.columns state.Table.Length
                             UniformGrid.rows (state.Table.Item(0).Length)
                             UniformGrid.horizontalAlignment HorizontalAlignment.Left
                             UniformGrid.children
                                 (List.concat [ for i in 0 .. state.Table.Length - 1 ->
                                                    [ for j in 0 .. (state.Table.Item(0)).Length - 1 ->
                                                        genetateCell i j state dispatch ] ]) ]

    let view state dispatch =
        DockPanel.create [ DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                    // StackPanel.margin 5.0
                                                                    // StackPanel.spacing 5.0
                                                                    StackPanel.children [ generateInputBox
                                                                                              state
                                                                                              dispatch
                                                                                          generateTable state dispatch ] ] ] ]
