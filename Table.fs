namespace Lab1

module Table =
    type Row = int
    type Col = int

    type State =
        { Grid: string list list
          Cursor: (Row * Col)
          CurrentCellData: string }

    type Msg =
        | UpdateCellMsg
        | AddRowMsg
        | AddColumnMsg
        | RemoveRowMsg
        | RemoveColumnMsg
        | ChangeCursorMsg of Row * Col
        | CurrentCellDataMsg of string
        | NewGridMsg of State

    let init: State =
        { Grid = [ for _ in 0 .. 10 -> [ for _ in 0 .. 10 -> "" ] ]
          Cursor = (2, 2)
          CurrentCellData = "" }

    module Utils =
        open FParsec
        open System.Text.RegularExpressions
        open Avalonia.Controls
        open Avalonia.Media
        open Avalonia.Layout

        let mainWindow () =
            (Avalonia.Application.Current.ApplicationLifetime :?> ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime).MainWindow

        let commonWindow txt =
            let window = Window()
            window.VerticalContentAlignment <- VerticalAlignment.Center
            window.HorizontalContentAlignment <- HorizontalAlignment.Center
            window.Height <- 100.0
            window.Width <- 200.0
            window.BorderBrush <- Immutable.ImmutableSolidColorBrush((Color.Parse "black"), 1.0)
            window.Content <- txt
            window

        let parseExpression str state =
            let parseCursor (r, c) =
                new string([| char (r + int 'A' - 1); char c |])

            let rec parseStr s =
                let cellReplacer = Regex @"[A-Z]\d+"
                let matches = cellReplacer.Matches s

                let matchVals (matchColl: MatchCollection) = [ for m in matchColl -> m.Value ]
                let cToI c = int c + 1 - int 'A'

                let parseCell (col :: row) =
                    (row |> Array.ofList |> System.String |> int, cToI col)

                let isRec checkingString =
                    let getCell (r, c) = state.Grid.Item(r).Item(c)

                    let rec helper n chS =
                        if n >= state.Grid.Length * state.Grid.Item(0).Length then
                            true
                        else if cellReplacer.IsMatch chS then
                            List.exists
                                (helper (n + 1))
                                (chS
                                 |> cellReplacer.Matches
                                 |> matchVals
                                 |> List.map (Seq.toList >> parseCell >> getCell))
                        else
                            false

                    helper 0 checkingString

                let isNotRec = not (isRec s)

                match cellReplacer.IsMatch s && isNotRec with
                | false -> if s.Contains("/ (0)") || s.Contains("/ 0") then "#ERROR" else s
                | true ->
                    let replacers =
                        matches
                        |> (fun mColl ->
                            [ for m in mColl ->
                                m.Value
                                |> Seq.toList
                                |> parseCell
                                |> (fun (r, c) -> (m.Value, "(" + state.Grid.Item(r).Item(c) + ")")) ])

                    List.fold (fun (st: string) (subst: string, replacer: string) -> st.Replace(subst, replacer)) s
                        replacers
                    |> parseStr

            let strWs s = pstring s .>> spaces

            let opp =
                new OperatorPrecedenceParser<int, unit, unit>()

            let expr = opp.ExpressionParser

            let term =
                (pint32 .>> spaces)
                <|> between (strWs "(") (strWs ")") expr

            opp.TermParser <- term

            let unaryPlusOperator = PrefixOperator("+", spaces, 1, true, id)

            let unaryMinusOperator =
                PrefixOperator("-", spaces, 1, false, (fun x -> 0 - x))

            let powOperator =
                InfixOperator("^", spaces, 2, Associativity.Right, (fun x y -> int (float x ** float y)))

            let incOperator =
                PrefixOperator("inc", spaces, 2, false, (fun x -> x + 1))

            let decOperator =
                PrefixOperator("dec", spaces, 2, false, (fun x -> x - 1))

            let divOperator =
                InfixOperator("div", spaces, 2, Associativity.Left, (/))

            let modOperator =
                InfixOperator("mod", spaces, 2, Associativity.Left, (%))

            let binaryPlusOperator =
                InfixOperator("+", spaces, 3, Associativity.Left, (+))

            let binaryMinusOperator =
                InfixOperator("-", spaces, 3, Associativity.Left, (-))

            let productOperator =
                InfixOperator("*", spaces, 4, Associativity.Left, (fun x y -> x * y))

            let divideOperator =
                InfixOperator("/", spaces, 4, Associativity.Left, (/))

            opp.AddOperator unaryPlusOperator
            opp.AddOperator unaryMinusOperator
            opp.AddOperator powOperator
            opp.AddOperator incOperator
            opp.AddOperator decOperator
            opp.AddOperator divOperator
            opp.AddOperator modOperator
            opp.AddOperator binaryPlusOperator
            opp.AddOperator binaryMinusOperator
            opp.AddOperator productOperator
            opp.AddOperator divideOperator

            match run expr (str |> parseStr) with
            | Success (a, _, _) -> a |> sprintf "%i"
            | Failure (_) -> if str = "" then "" else "#ERROR"

    module Controller =
        open Utils

        let updateCell state =
            let (row, col) = state.Cursor
            [ for i in 0 .. state.Grid.Length - 1 ->
                [ for j in 0 .. state.Grid.Item(0).Length - 1 ->
                    if i = row && j = col then state.CurrentCellData else state.Grid.Item(i).Item(j) ] ]

        let update (msg: Msg) (state: State) =
            match msg with
            | UpdateCellMsg -> { state with Grid = updateCell state }
            | ChangeCursorMsg (row, col) -> { state with Cursor = (row, col) }
            | CurrentCellDataMsg str -> { state with CurrentCellData = str }
            | AddRowMsg ->
                { state with
                      Grid =
                          state.Grid
                          @ [ [ for _ in 0 .. state.Grid.Item(0).Length - 1 -> "" ] ] }
            | AddColumnMsg ->
                { state with
                      Grid = List.map (fun row -> "" :: row) state.Grid }
            | RemoveRowMsg ->
                if state.Grid
                   |> List.last
                   |> List.exists (fun el -> el <> "") then
                    commonWindow "Last row isn't empty"
                    |> (fun w -> w.Show())
                    state
                else
                    { state with
                          Grid = [ for i in 0 .. state.Grid.Length - 2 -> state.Grid.Item(i) ] }
            | RemoveColumnMsg ->
                if [ for row in state.Grid -> row |> List.last ]
                   |> List.exists (fun el -> el <> "") then
                    commonWindow "Last column isn't empty"
                    |> (fun w -> w.Show())
                    state
                else
                    { state with
                          Grid =
                              [ for i in 0 .. state.Grid.Length - 1 ->
                                  [ for j in 0 .. state.Grid.Item(0).Length - 2 -> state.Grid.Item(i).Item(j) ] ] }

            | NewGridMsg st -> st


    module View =
        open Utils
        open Avalonia.Controls
        open Avalonia.Media
        open Avalonia.Controls.Primitives
        open Avalonia.FuncUI.DSL
        open Avalonia.Layout

        let controlButton isDefault (content: string) fn =
            Button.create [ Button.content content
                            Button.borderBrush "black"
                            Button.isDefault isDefault
                            Button.horizontalAlignment HorizontalAlignment.Left
                            Button.onClick fn ]

        let generateInputBox state dispatch =
            UniformGrid.create [ UniformGrid.rows 3
                                 UniformGrid.children [ TextBox.create [ TextBox.watermark (string state.Cursor)
                                                                         TextBox.width 400.0
                                                                         TextBox.verticalAlignment
                                                                             VerticalAlignment.Center
                                                                         TextBox.horizontalAlignment
                                                                             HorizontalAlignment.Left
                                                                         TextBox.onTextChanged
                                                                             (CurrentCellDataMsg >> dispatch)
                                                                         TextBox.text
                                                                             (string
                                                                                 (state.Grid.Item(fst (state.Cursor))
                                                                                       .Item(snd (state.Cursor)))) ]
                                                        controlButton true "Set cell value" (fun _ ->
                                                            (UpdateCellMsg |> dispatch))
                                                        controlButton false "Add new row" (fun _ ->
                                                            (AddRowMsg |> dispatch))
                                                        controlButton false "Add new column" (fun _ ->
                                                            (AddColumnMsg |> dispatch))
                                                        controlButton false "Remove last row" (fun _ ->
                                                            (RemoveRowMsg |> dispatch))
                                                        controlButton false "Remove last column" (fun _ ->
                                                            (RemoveColumnMsg |> dispatch)) ] ]

        let genetateCell row col state dispatch =
            let isZeroRow = row = 0
            let isZeroCol = col = 0
            let isZeroRowOrCol = isZeroRow || isZeroCol
            let isZeroRowAndCol = isZeroRow && isZeroCol

            ViewBox.create [ Viewbox.stretch Stretch.Fill
                             Grid.row row
                             Grid.column col
                             Viewbox.child
                                 (Button.create [ Button.verticalAlignment VerticalAlignment.Center
                                                  Button.horizontalAlignment HorizontalAlignment.Left
                                                  Button.width
                                                      (1000.0
                                                       * 0.995
                                                       / (state.Grid.Item(0) |> List.length |> float))
                                                  Button.height
                                                      (1000.0
                                                       * 0.995
                                                       * (4.0 / 15.0)
                                                       / (state.Grid |> List.length |> float))
                                                  Button.foreground "black"
                                                  Button.background (if isZeroRowOrCol then "gray" else "white")
                                                  Button.borderBrush
                                                      (if state.Cursor = (row, col) then "green" else "black")
                                                  Button.borderThickness
                                                      (if state.Cursor = (row, col) then 1.5 else 0.5)
                                                  Button.content
                                                      (if isZeroRowAndCol
                                                       then ""
                                                       else if isZeroRow
                                                       then ((col - 1 + (int 'A')) |> char |> string)
                                                       else if isZeroCol
                                                       then (row |> string)
                                                       else (parseExpression (state.Grid.Item(row).Item(col)) state))
                                                  Button.onClick
                                                      (if isZeroRowOrCol
                                                       then ignore
                                                       else (fun _ -> (row, col) |> ChangeCursorMsg |> dispatch)) ]) ]

        let generateGrid state dispatch =
            ViewBox.create [ Viewbox.stretch Stretch.UniformToFill
                             Viewbox.child
                                 (Grid.create [ Grid.width 1000.0
                                                Grid.height (1000.0 * 4.0 / 15.0 + 30.0)
                                                Grid.rowDefinitions
                                                    ("1.0*"
                                                     + String.replicate (state.Grid.Length - 1) ",1.0*")
                                                Grid.columnDefinitions
                                                    ("1.0*"
                                                     + String.replicate (state.Grid.Item(0).Length - 1) ",1.0*")
                                                Grid.background "black"
                                                Grid.horizontalAlignment HorizontalAlignment.Left
                                                Grid.horizontalScrollBarVisibility ScrollBarVisibility.Visible
                                                Grid.verticalScrollBarVisibility ScrollBarVisibility.Visible
                                                Grid.children
                                                    (List.concat [ for i in 0 .. state.Grid.Length - 1 ->
                                                                       [ for j in 0 .. (state.Grid.Item(0)).Length - 1 ->
                                                                           genetateCell i j state dispatch ] ]) ]) ]

        let view state dispatch =
            DockPanel.create [ DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                        StackPanel.background "white"
                                                                        StackPanel.orientation Orientation.Vertical
                                                                        StackPanel.children [ generateInputBox
                                                                                                  state
                                                                                                  dispatch
                                                                                              generateGrid
                                                                                                  state
                                                                                                  dispatch ] ] ] ]
