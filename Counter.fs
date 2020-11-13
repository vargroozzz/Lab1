namespace Lab1

open Avalonia.Controls.ApplicationLifetimes
open Avalonia
open Microsoft.FSharp.Core
open FParsec
open System.Text.RegularExpressions
open System.Text.Json
open System.Text.Json.Serialization
open System
open System.IO
open System.Threading.Tasks


module Counter =
    open Avalonia.FuncUI.Types
    open Avalonia.Controls
    open Avalonia.Media
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type Row = int
    type Col = int

    type StateCrutch =
        { Table: string array array
          Cursor: (Row * Col)
          CurrentCellData: string }

    type State =
        { Table: string list list
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
        | NewTableMsg of State
        | SaveAndQuitMsg

    let crutchToState (st: StateCrutch) =
        { Table = st.Table |> Array.toList |> List.map Array.toList
          Cursor = st.Cursor
          CurrentCellData = st.CurrentCellData }


    let init: State =
        { Table = [ for _ in 0 .. 10 -> [ for _ in 0 .. 10 -> "" ] ]
          Cursor = (2, 2)
          CurrentCellData = "" }

    let mainWindow () =
        (Application.Current.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime).MainWindow

    let commonWindow txt =
        let window = Window()
        window.VerticalContentAlignment <- VerticalAlignment.Center
        window.HorizontalContentAlignment <- HorizontalAlignment.Center
        window.Height <- 100.0
        window.Width <- 200.0
        window.BorderBrush <- Immutable.ImmutableSolidColorBrush((Color.Parse "black"), 1.0)
        // window.Title <- txt
        window.Content <- txt
        // Button.create [ Button.onClick (fun _ -> window.Hide())
        //                 Button.content "Ok"
        //                 Button.borderBrush "black"
        //                 Button.horizontalAlignment HorizontalAlignment.Left ]
        window

    // let saveIntoFile state =
    //     let savesDirectory = Directory.GetCurrentDirectory() + "/saves"
    //     let fileName n = if Directory.Exists(Directory.GetCurrentDirectory() + "/saves") then

    type Assoc = Associativity


    let parseExpression str state =
        let parseCursor (r, c) =
            new string([| char (r + int 'A' - 1); char c |])

        let rec parseStr s =
            let cellReplacer = Regex @"[A-Z]\d+"
            let matches = cellReplacer.Matches s

            let matchVals (matchColl: MatchCollection) = [ for m in matchColl -> m.Value ]
            let cToI c = int c + 1 - int 'A'

            let parseCell (col :: row) =
                (row |> Array.ofList |> String |> int, cToI col)

            let isRec checkingString =
                let getCell (r, c) = state.Table.Item(r).Item(c)

                let rec helper n chS =
                    if n
                       >= state.Table.Length
                       * state.Table.Item(0).Length then
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
                            |> (fun (r, c) -> (m.Value, "(" + state.Table.Item(r).Item(c) + ")")) ])

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


        opp.AddOperator(PrefixOperator("+", spaces, 1, true, id))
        opp.AddOperator(PrefixOperator("-", spaces, 1, false, (fun x -> 0 - x)))
        opp.AddOperator(InfixOperator("^", spaces, 2, Assoc.Right, (fun x y -> int (float x ** float y))))
        opp.AddOperator(PrefixOperator("inc", spaces, 2, false, (fun x -> x + 1)))
        opp.AddOperator(PrefixOperator("dec", spaces, 2, false, (fun x -> x - 1)))
        opp.AddOperator(InfixOperator("div", spaces, 2, Assoc.Left, (/)))
        opp.AddOperator(InfixOperator("mod", spaces, 2, Assoc.Left, (%)))
        opp.AddOperator(InfixOperator("+", spaces, 3, Assoc.Left, (+)))
        opp.AddOperator(InfixOperator("-", spaces, 3, Assoc.Left, (-)))
        opp.AddOperator(InfixOperator("*", spaces, 4, Assoc.Left, (fun x y -> x * y)))
        opp.AddOperator(InfixOperator("/", spaces, 4, Assoc.Left, (/)))

        match run expr (str |> parseStr) with
        | Success (a, b, c) -> a |> sprintf "%i"
        | Failure (a, b, c) -> if str = "" then "" else "#ERROR"

    let updateCell state =
        let (row, col) = state.Cursor
        [ for i in 0 .. state.Table.Length - 1 ->
            [ for j in 0 .. state.Table.Item(0).Length - 1 ->
                if i = row && j = col then state.CurrentCellData else state.Table.Item(i).Item(j) ] ]

    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateCellMsg -> { state with Table = updateCell state }
        | ChangeCursorMsg (row, col) -> { state with Cursor = (row, col) }
        | CurrentCellDataMsg str -> { state with CurrentCellData = str }
        | AddRowMsg ->
            { state with
                  Table =
                      state.Table
                      @ [ [ for _ in 0 .. state.Table.Item(0).Length - 1 -> "" ] ] }
        | AddColumnMsg ->
            { state with
                  Table = List.map (fun row -> "" :: row) state.Table }
        | RemoveRowMsg ->
            if state.Table
               |> List.last
               |> List.exists (fun el -> el <> "") then
                commonWindow "Last row isn't empty"
                |> (fun w -> w.Show())
                state
            else
                { state with
                      Table = [ for i in 0 .. state.Table.Length - 2 -> state.Table.Item(i) ] }
        | RemoveColumnMsg ->
            if [ for row in state.Table -> row |> List.last ]
               |> List.exists (fun el -> el <> "") then
                commonWindow "Last column isn't empty"
                |> (fun w -> w.Show())
                state
            else
                { state with
                      Table =
                          [ for i in 0 .. state.Table.Length - 1 ->
                              [ for j in 0 .. state.Table.Item(0).Length - 2 -> state.Table.Item(i).Item(j) ] ] }
        | SaveAndQuitMsg ->

            // let save = OpenFileDialog()
            // let save = SaveFileDialog()
            // save.InitialFileName <- "testdb.json"
            // let filter = FileDialogFilter()
            // filter.Name <- "json"
            // filter.Extensions <- (Collections.Generic.List<string> [ "json" ])
            // save.Filters.Add(filter)
            // save.Directory <- Directory.GetCurrentDirectory()
            // save.Title <- "Choose file to save"

            // let fileToSave =
            //     (async {
            //         printfn "breakpoint"
            //         let! files = save.ShowAsync(mainWindow ()) |> Async.AwaitTask
            //         return files
            //      }
            //      |> Async.RunSynchronously)

            // let disp =
            //     Async.StartWithContinuations
            //         (async { disp.InvokeAsync((fun () -> callback.Invoke(context))) }, ignore, error, cancel, ct)

            // let fileToSaveTask =
            //     printfn "breakpoint 1"
            //     save.ShowAsync(mainWindow ())

            // printfn "breakpoint 2"

            // Async.StartWithContinuations
            //     ((fileToSaveTask |> Async.AwaitTask),
            //      (fun filepath -> File.WriteAllText(filepath, "myJSON")),
            //      (fun _ -> printfn "exception"),
            //      (fun _ -> printfn "cancellation"))
            // printfn "breakpoint 3"
            // Threading.Thread.Sleep(2000)
            // fileToSaveTask.CreationOptions <- TaskCreationOptions.AttachedToParent
            // fileToSaveTask.Start()
            // fileToSaveTask.Wait()
            // let fileToSave = fileToSaveTask().Result

            // let fileToSave = fileToSaveTask |> Async.AwaitTask
            // printfn "%s" fileToSave

            // File.WriteAllText(fileToSave, state.ToString())
            // File.WriteAllText(fileToSave, "myJSON")


            let files =
                (DirectoryInfo(Directory.GetCurrentDirectory())).GetFiles("*.json")
                |> Array.toList
                |> List.map (fun fi -> fi.Name)

            let createFile (path: string) =
                Button.create [ Button.width 600.0
                                Button.height (300.0 / (files.Length |> float))
                                TextBox.verticalAlignment VerticalAlignment.Center
                                TextBox.horizontalAlignment HorizontalAlignment.Center
                                Button.content path
                                Button.onClick (fun _ ->
                                    File.WriteAllText(path, state.ToString())
                                    mainWindow().Close())

                                 ]

            let window = Window()
            window.VerticalContentAlignment <- VerticalAlignment.Center
            window.HorizontalContentAlignment <- HorizontalAlignment.Center
            window.Width <- 800.0
            window.Height <- 400.0

            window.BorderBrush <- Immutable.ImmutableSolidColorBrush((Color.Parse "black"), 1.0)
            window.Content <-
                ViewBox.create [ Viewbox.stretch Stretch.UniformToFill
                                 Viewbox.child
                                     (UniformGrid.create [ UniformGrid.width 600.0
                                                           UniformGrid.height 300.0
                                                           UniformGrid.rows files.Length
                                                           UniformGrid.columns 1
                                                           // Grid.background "black"
                                                           UniformGrid.horizontalAlignment HorizontalAlignment.Left
                                                           // Grid.horizontalScrollBarVisibility ScrollBarVisibility.Visible
                                                           // Grid.verticalScrollBarVisibility ScrollBarVisibility.Visible
                                                           UniformGrid.children [ for i in 1 .. files.Length ->
                                                                                      createFile (files.Item(i - 1)) ] ]) ]
            //    (List.map createFile files) |> Helpers.generalize
            // Button.create [ Button.onClick (fun _ -> window.Hide())
            //                 Button.content "Ok"
            //                 Button.borderBrush "black"
            //                 Button.horizontalAlignment HorizontalAlignment.Left ]
            window.Show()

            // mainWindow().Close()

            state
        | NewTableMsg st -> st



    let generateInputBox state dispatch =
        UniformGrid.create [ UniformGrid.rows 3
                             UniformGrid.children [ TextBox.create [ TextBox.watermark (string state.Cursor)
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
                                                                    Button.content "Set cell value"
                                                                    Button.borderBrush "black"
                                                                    Button.isDefault true
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (AddRowMsg |> dispatch))
                                                                    Button.content "Add new row"
                                                                    Button.borderBrush "black"
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (AddColumnMsg |> dispatch))
                                                                    Button.content "Add new column"
                                                                    Button.borderBrush "black"
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ -> (RemoveRowMsg |> dispatch))
                                                                    Button.content "Remove last row"
                                                                    Button.borderBrush "black"
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    Button.create [ Button.onClick (fun _ ->
                                                                        (RemoveColumnMsg |> dispatch))
                                                                    Button.content "Remove last column"
                                                                    Button.borderBrush "black"
                                                                    Button.horizontalAlignment HorizontalAlignment.Left ]
                                                    // Button.create [ Button.onClick (fun _ ->
                                                    //                     (commonWindow "a" |> (fun w -> w.Show())))
                                                    //                 // Button.onClick (fun _ ->
                                                    //                 //     (commonWindow "a"
                                                    //                 //      |> (fun w ->
                                                    //                 //          w.ShowDialog
                                                    //                 //              ((Application.Current.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime).MainWindow)
                                                    //                 //          |> ignore)))
                                                    //                 Button.content "Window test"
                                                    //                 Button.borderBrush "black"
                                                    //                 //   Button.classes [ "plus" ]
                                                    //                 Button.horizontalAlignment HorizontalAlignment.Left ]
                                                     ] ]

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
                                                   / (state.Table.Item(0) |> List.length |> float))
                                              Button.height
                                                  (1000.0
                                                   * 0.995
                                                   * (4.0 / 15.0)
                                                   / (state.Table |> List.length |> float))
                                              Button.foreground "black"
                                              Button.background (if isZeroRowOrCol then "gray" else "white")
                                              Button.borderBrush
                                                  (if state.Cursor = (row, col) then "green" else "black")
                                              Button.borderThickness (if state.Cursor = (row, col) then 1.5 else 0.5)
                                              Button.content
                                                  (if isZeroRowAndCol
                                                   then ""
                                                   else if isZeroRow
                                                   then ((col - 1 + (int 'A')) |> char |> string)
                                                   else if isZeroCol
                                                   then (row |> string)
                                                   else (parseExpression (state.Table.Item(row).Item(col)) state))
                                              Button.onClick
                                                  (if isZeroRowOrCol
                                                   then ignore
                                                   else (fun _ -> (row, col) |> ChangeCursorMsg |> dispatch)) ]) ]

    let generateTable state dispatch =
        ViewBox.create [ Viewbox.stretch Stretch.UniformToFill
                         Viewbox.child
                             (Grid.create [ Grid.width 1000.0
                                            Grid.height (1000.0 * 4.0 / 15.0 + 30.0)
                                            Grid.rowDefinitions
                                                ("1.0*"
                                                 + String.replicate (state.Table.Length - 1) ",1.0*")
                                            Grid.columnDefinitions
                                                ("1.0*"
                                                 + String.replicate (state.Table.Item(0).Length - 1) ",1.0*")
                                            Grid.background "black"
                                            Grid.horizontalAlignment HorizontalAlignment.Left
                                            Grid.horizontalScrollBarVisibility ScrollBarVisibility.Visible
                                            Grid.verticalScrollBarVisibility ScrollBarVisibility.Visible
                                            Grid.children
                                                (List.concat [ for i in 0 .. state.Table.Length - 1 ->
                                                                   [ for j in 0 .. (state.Table.Item(0)).Length - 1 ->
                                                                       genetateCell i j state dispatch ] ]) ]) ]

    let view state dispatch =
        DockPanel.create [ DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                    StackPanel.background "white"
                                                                    StackPanel.orientation Orientation.Vertical
                                                                    StackPanel.children [ generateInputBox
                                                                                              state
                                                                                              dispatch
                                                                                          generateTable state dispatch ] ] ] ]
