namespace Lab1

module Shell =
    open Elmish

    type State =
        /// store the child state in your main state
        { TableState: Table.State
          SaveFileState: SaveFile.State }

    type Msg =
        | TableMsg of Table.Msg
        | SaveFileMsg of SaveFile.Msg

    type StateCrutch =
        { Grid: string array array
          Cursor: (Table.Row * Table.Col)
          CurrentCellData: string }

    let init =
        let tableState = Table.init
        let saveFileState = SaveFile.init
        { TableState = tableState
          SaveFileState = saveFileState },
        Cmd.none

    module Utils =
        let crutchToState (st: StateCrutch): Table.State =
            { Grid = st.Grid |> Array.toList |> List.map Array.toList
              Cursor = st.Cursor
              CurrentCellData = st.CurrentCellData }

    module Controller =
        open System.IO
        open System.Text.Json

        let update (msg: Msg) (state: State): State * Cmd<_> =
            match msg with
            | TableMsg tablemsg ->
                let tableMsg =
                    Table.Controller.update tablemsg state.TableState

                { state with TableState = tableMsg }, Cmd.none
            | SaveFileMsg savefilemsg ->
                match savefilemsg with
                | SaveFile.LoadMsg ->
                    let jsonNewState =
                        File.ReadAllText state.SaveFileState.ChosenFile

                    let newState =
                        JsonSerializer.Deserialize<StateCrutch> jsonNewState

                    let tableMsg =
                        Table.Controller.update
                            (newState
                             |> Utils.crutchToState
                             |> Table.NewGridMsg)
                            state.TableState

                    { state with TableState = tableMsg }, Cmd.none
                | SaveFile.SaveMsg ->
                    File.WriteAllText(state.SaveFileState.ChosenFile, JsonSerializer.Serialize state.TableState)
                    { state with
                          SaveFileState =
                              { state.SaveFileState with
                                    Files =
                                        if List.contains state.SaveFileState.ChosenFile state.SaveFileState.Files then
                                            state.SaveFileState.Files
                                        else
                                            state.SaveFileState.Files
                                            @ [ state.SaveFileState.ChosenFile ] } },
                    Cmd.none
                | _ ->
                    let saveFileMsg =
                        SaveFile.Controller.update savefilemsg state.SaveFileState

                    { state with
                          SaveFileState = saveFileMsg },
                    Cmd.none

    module View =
        open Avalonia.Controls
        open Avalonia.FuncUI.DSL

        let view (state: State) (dispatch) =
            DockPanel.create [ DockPanel.children [ TabControl.create [ TabControl.tabStripPlacement Dock.Top
                                                                        TabControl.viewItems [ TabItem.create [ TabItem.header
                                                                                                                    "Table"
                                                                                                                TabItem.content
                                                                                                                    (Table.View.view
                                                                                                                        state.TableState
                                                                                                                         (TableMsg
                                                                                                                          >> dispatch)) ]
                                                                                               TabItem.create [ TabItem.header
                                                                                                                    "About"
                                                                                                                TabItem.content
                                                                                                                    (About.View.view.Value) ]
                                                                                               TabItem.create [ TabItem.header
                                                                                                                    "File"
                                                                                                                TabItem.content
                                                                                                                    (SaveFile.View.view
                                                                                                                        state.SaveFileState
                                                                                                                         (SaveFileMsg
                                                                                                                          >> dispatch)) ] ] ] ] ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia

    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- "My Excel"
            base.Width <- 1200.0
            base.Height <- 800.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            Program.mkProgram (fun () -> init) Controller.update View.view
            |> Program.withHost this
            |> Program.run
