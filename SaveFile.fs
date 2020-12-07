/// You can use modules in Avalonia.FuncUI in the same way you would do
/// in [Elmish ](https://elmish.github.io/elmish/)
namespace Lab1

module SaveFile =
    open System.IO

    type State =
        { ChosenFile: string
          Files: string list }

    type Msg =
        | ChooseFileMsg of string
        | SaveMsg
        | LoadMsg
        | QuitMsg

    let init =
        { ChosenFile = ""
          Files =
              (DirectoryInfo(Directory.GetCurrentDirectory())).GetFiles("*.json")
              |> Array.toList
              |> List.map (fun fi -> fi.Name) }

    module Controller =
        let update (msg: Msg) (state: State) =
            match msg with
            | ChooseFileMsg path -> { state with ChosenFile = path }
            | QuitMsg ->
                Table.Utils.mainWindow().Close()
                state
            | _ -> state

    module View =
        open Avalonia.FuncUI.DSL
        open Avalonia.Controls
        open Avalonia.Controls.Primitives
        open Avalonia.Layout

        let chosenFileBoxView state dispatch =
            TextBox.create [ TextBox.text state.ChosenFile
                             TextBox.background "white"
                             TextBox.foreground "black"
                             TextBox.fontSize 20.0
                             TextBox.width 300.0
                             TextBox.height 30.0
                             TextBox.verticalAlignment VerticalAlignment.Center
                             TextBox.horizontalAlignment HorizontalAlignment.Center
                             TextBox.onTextChanged (ChooseFileMsg >> dispatch) ]

        let controlButton (content: string) fn =
            Button.create [ Button.content content
                            Button.background "white"
                            Button.foreground "black"
                            Button.fontSize 20.0
                            Button.verticalAlignment VerticalAlignment.Center
                            Button.horizontalAlignment HorizontalAlignment.Center
                            Button.width 300.0
                            Button.height 50.0
                            Button.onClick fn ]

        let controlButtons dispatch =
            UniformGrid.create [ UniformGrid.width 1000.0
                                 UniformGrid.height 300.0
                                 UniformGrid.rows 1
                                 UniformGrid.columns 3
                                 UniformGrid.horizontalAlignment HorizontalAlignment.Center
                                 UniformGrid.children [ controlButton "Save" (fun _ -> SaveMsg |> dispatch)
                                                        controlButton "Load(Don't saves current data!)" (fun _ ->
                                                            LoadMsg |> dispatch)
                                                        controlButton "Quit Without Save(CAREFUL!)" (fun _ ->
                                                            QuitMsg |> dispatch) ] ]

        let createFileButton (path: string) state dispatch =
            Button.create [ Button.width 300.0
                            Button.height (300.0 / (state.Files.Length |> float) - 2.0)
                            TextBox.verticalAlignment VerticalAlignment.Center
                            TextBox.horizontalAlignment HorizontalAlignment.Center
                            Button.content path
                            Button.onClick (fun _ -> path |> ChooseFileMsg |> dispatch) ]

        let filesToSaveToView state dispatch =
            UniformGrid.create [ UniformGrid.width 600.0
                                 UniformGrid.height 300.0
                                 UniformGrid.rows state.Files.Length
                                 UniformGrid.columns 1
                                 UniformGrid.horizontalAlignment HorizontalAlignment.Center
                                 UniformGrid.children [ for i in 1 .. state.Files.Length ->
                                                            (createFileButton (state.Files.Item(i - 1)) state dispatch) ] ]

        let view state (dispatch: Msg -> unit) =
            DockPanel.create [ DockPanel.horizontalAlignment HorizontalAlignment.Center
                               DockPanel.verticalAlignment VerticalAlignment.Top
                               DockPanel.margin (0.0, 20.0, 0.0, 0.0)
                               DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                        StackPanel.width 1000.0
                                                                        StackPanel.height 400.0
                                                                        StackPanel.children [ chosenFileBoxView
                                                                                                  state
                                                                                                  dispatch
                                                                                              controlButtons dispatch
                                                                                              filesToSaveToView
                                                                                                  state
                                                                                                  dispatch ] ] ] ]
