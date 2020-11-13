namespace Lab1

open System.IO

/// This is the main module of your application
/// here you handle all of your child pages as well as their
/// messages and their updates, useful to update multiple parts
/// of your application, Please refer to the `view` function
/// to see how to handle different kinds of "*child*" controls
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish
    open System.Text.Json

    type State =
        /// store the child state in your main state
        { AboutState: About.State
          CounterState: Counter.State
          SaveFileState: SaveFile.State }

    type Msg =
        | AboutMsg of About.Msg
        | CounterMsg of Counter.Msg
        | SaveFileMsg of SaveFile.Msg

    let init =
        let aboutState, aboutCmd = About.init
        let counterState = Counter.init
        let saveFileState = SaveFile.init
        { AboutState = aboutState
          CounterState = counterState
          SaveFileState = saveFileState },
        /// If your children controls don't emit any commands
        /// in the init function, you can just return Cmd.none
        /// otherwise, you can use a batch operation on all of them
        /// you can add more init commands as you need
        Cmd.batch [ aboutCmd ]

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | AboutMsg bpmsg ->
            let aboutState, cmd = About.update bpmsg state.AboutState
            { state with AboutState = aboutState },
            /// map the message to the kind of message
            /// your child control needs to handle
            Cmd.map AboutMsg cmd
        | CounterMsg countermsg ->
            let counterMsg =
                Counter.update countermsg state.CounterState

            { state with CounterState = counterMsg },
            /// map the message to the kind of message
            /// your child control needs to handle
            Cmd.none
        | SaveFileMsg savefilemsg ->
            match savefilemsg with
            | SaveFile.LoadMsg ->
                let jsonNewState =
                    File.ReadAllText state.SaveFileState.ChosenFile

                let newState =
                    JsonSerializer.Deserialize<Counter.StateCrutch> jsonNewState

                let counterMsg =
                    Counter.update
                        (newState
                         |> Counter.crutchToState
                         |> Counter.NewTableMsg)
                        state.CounterState

                { state with CounterState = counterMsg }, Cmd.none
            | SaveFile.SaveMsg ->
                File.WriteAllText(state.SaveFileState.ChosenFile, JsonSerializer.Serialize state.CounterState)
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
                    SaveFile.update savefilemsg state.SaveFileState

                { state with
                      SaveFileState = saveFileMsg },
                Cmd.none

    let view (state: State) (dispatch) =
        DockPanel.create [ DockPanel.children [ TabControl.create [ TabControl.tabStripPlacement Dock.Top
                                                                    TabControl.viewItems [ TabItem.create [ TabItem.header
                                                                                                                "Table"
                                                                                                            TabItem.content
                                                                                                                (Counter.view
                                                                                                                    state.CounterState
                                                                                                                     (CounterMsg
                                                                                                                      >> dispatch)) ]
                                                                                           TabItem.create [ TabItem.header
                                                                                                                "About"
                                                                                                            TabItem.content
                                                                                                                (About.view
                                                                                                                    state.AboutState
                                                                                                                     (AboutMsg
                                                                                                                      >> dispatch)) ]
                                                                                           TabItem.create [ TabItem.header
                                                                                                                "File"
                                                                                                            TabItem.content
                                                                                                                (SaveFile.view
                                                                                                                    state.SaveFileState
                                                                                                                     (SaveFileMsg
                                                                                                                      >> dispatch)) ] ] ] ] ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- "My Excel"
            base.Width <- 1200.0
            base.Height <- 800.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0
            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
