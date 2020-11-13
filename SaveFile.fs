/// You can use modules in Avalonia.FuncUI in the same way you would do
/// in [Elmish ](https://elmish.github.io/elmish/)
namespace Lab1

open System.IO
open Avalonia.Media
open Avalonia.Controls.ApplicationLifetimes

open Microsoft.FSharp.Core
open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization



module SaveFile =
    open Avalonia
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.Media
    open Avalonia.Layout

    open FSharp.Data

    open System.Diagnostics
    open System.Runtime.InteropServices

    type State =
        { ChosenFile: string
          Files: string list }

    let init =
        { ChosenFile = ""
          Files =
              (DirectoryInfo(Directory.GetCurrentDirectory())).GetFiles("*.json")
              |> Array.toList
              |> List.map (fun fi -> fi.Name) }

    type Msg =
        | ChooseFileMsg of string
        | SaveMsg
        | LoadMsg
        | QuitMsg



    // type State = { noop: bool }
    // let mutable files =
    //     (DirectoryInfo(Directory.GetCurrentDirectory())).GetFiles("*.json")
    //     |> Array.toList
    //     |> List.map (fun fi -> fi.Name)

    let createFileButton (path: string) state dispatch =
        Button.create [ Button.width 300.0
                        Button.height (300.0 / (state.Files.Length |> float) - 2.0)
                        TextBox.verticalAlignment VerticalAlignment.Center
                        TextBox.horizontalAlignment HorizontalAlignment.Center
                        Button.content path
                        Button.onClick (fun _ -> path |> ChooseFileMsg |> dispatch) ]


    let gridToJson grid =
        grid.ToString().Replace("Table", "\"Table\"").Replace("Cursor", ",\"Cursor\"")
            .Replace("CurrentCellData", ",\"CurrentCellData\"").Replace("(", "\"(").Replace(")", ")\"")
            .Replace(";", ",").Replace("=", ":")



    let update (msg: Msg) (state: State) =
        match msg with
        | ChooseFileMsg path -> { state with ChosenFile = path }
        // | SaveMsg table ->
        //     File.WriteAllText(state.ChosenFile, JsonSerializer.Serialize table)
        //     { state with
        //           Files =
        //               if List.contains state.ChosenFile state.Files
        //               then state.Files
        //               else state.ChosenFile :: state.Files }
        | QuitMsg ->
            Counter.mainWindow().Close()
            state
        | _ -> state





    // Button.create [ Button.onClick (fun _ -> (SaveAndQuitMsg |> dispatch))
    //                 Button.content "Quit with save"
    //                 Button.borderBrush "black" ]


    let view state (dispatch: Msg -> unit) =
        DockPanel.create [ DockPanel.horizontalAlignment HorizontalAlignment.Center
                           DockPanel.verticalAlignment VerticalAlignment.Top
                           DockPanel.margin (0.0, 20.0, 0.0, 0.0)
                           DockPanel.children [ StackPanel.create [ StackPanel.dock Dock.Top
                                                                    StackPanel.width 1000.0
                                                                    StackPanel.height 400.0
                                                                    StackPanel.children [ TextBlock.create [ TextBlock.text
                                                                                                                 state.ChosenFile
                                                                                                             TextBlock.width
                                                                                                                 300.0
                                                                                                             TextBlock.height
                                                                                                                 30.0 ]
                                                                                          TextBox.create [ TextBox.text
                                                                                                               state.ChosenFile
                                                                                                           TextBox.background
                                                                                                               "white"
                                                                                                           TextBox.foreground
                                                                                                               "black"
                                                                                                           TextBox.fontSize
                                                                                                               20.0
                                                                                                           TextBox.width
                                                                                                               300.0
                                                                                                           TextBox.height
                                                                                                               30.0
                                                                                                           TextBox.verticalAlignment
                                                                                                               VerticalAlignment.Center
                                                                                                           TextBox.horizontalAlignment
                                                                                                               HorizontalAlignment.Center
                                                                                                           TextBox.onTextChanged
                                                                                                               (ChooseFileMsg
                                                                                                                >> dispatch) ]
                                                                                          UniformGrid.create [ UniformGrid.width
                                                                                                                   1000.0
                                                                                                               UniformGrid.height
                                                                                                                   300.0
                                                                                                               UniformGrid.rows
                                                                                                                   1
                                                                                                               UniformGrid.columns
                                                                                                                   3
                                                                                                               UniformGrid.horizontalAlignment
                                                                                                                   HorizontalAlignment.Center
                                                                                                               UniformGrid.children [ Button.create [ Button.content
                                                                                                                                                          "Save"
                                                                                                                                                      Button.background
                                                                                                                                                          "white"
                                                                                                                                                      Button.foreground
                                                                                                                                                          "black"
                                                                                                                                                      Button.fontSize
                                                                                                                                                          20.0
                                                                                                                                                      Button.verticalAlignment
                                                                                                                                                          VerticalAlignment.Center
                                                                                                                                                      Button.horizontalAlignment
                                                                                                                                                          HorizontalAlignment.Center
                                                                                                                                                      Button.width
                                                                                                                                                          300.0
                                                                                                                                                      Button.height
                                                                                                                                                          50.0
                                                                                                                                                      Button.onClick (fun _ ->
                                                                                                                                                          SaveMsg
                                                                                                                                                          |> dispatch) ]
                                                                                                                                      Button.create [ Button.content
                                                                                                                                                          "Load"
                                                                                                                                                      Button.background
                                                                                                                                                          "white"
                                                                                                                                                      Button.foreground
                                                                                                                                                          "black"
                                                                                                                                                      Button.fontSize
                                                                                                                                                          20.0
                                                                                                                                                      Button.verticalAlignment
                                                                                                                                                          VerticalAlignment.Center
                                                                                                                                                      Button.horizontalAlignment
                                                                                                                                                          HorizontalAlignment.Center
                                                                                                                                                      Button.width
                                                                                                                                                          300.0
                                                                                                                                                      Button.height
                                                                                                                                                          50.0
                                                                                                                                                      Button.onClick (fun _ ->
                                                                                                                                                          LoadMsg
                                                                                                                                                          |> dispatch) ]
                                                                                                                                      Button.create [ Button.content
                                                                                                                                                          "Quit Without Save(CAREFUL!)"
                                                                                                                                                      Button.background
                                                                                                                                                          "white"
                                                                                                                                                      Button.foreground
                                                                                                                                                          "black"
                                                                                                                                                      Button.fontSize
                                                                                                                                                          20.0
                                                                                                                                                      Button.verticalAlignment
                                                                                                                                                          VerticalAlignment.Center
                                                                                                                                                      Button.horizontalAlignment
                                                                                                                                                          HorizontalAlignment.Center
                                                                                                                                                      Button.width
                                                                                                                                                          300.0
                                                                                                                                                      Button.height
                                                                                                                                                          50.0
                                                                                                                                                      Button.onClick (fun _ ->
                                                                                                                                                          QuitMsg
                                                                                                                                                          |> dispatch) ] ] ]




                                                                                          //    Viewbox.stretch
                                                                                          //        Stretch.UniformToFill
                                                                                          UniformGrid.create [ UniformGrid.width
                                                                                                                   600.0
                                                                                                               UniformGrid.height
                                                                                                                   300.0
                                                                                                               UniformGrid.rows
                                                                                                                   state.Files.Length
                                                                                                               UniformGrid.columns
                                                                                                                   1
                                                                                                               UniformGrid.horizontalAlignment
                                                                                                                   HorizontalAlignment.Center
                                                                                                               // Grid.horizontalScrollBarVisibility ScrollBarVisibility.Visible
                                                                                                               // Grid.verticalScrollBarVisibility ScrollBarVisibility.Visible
                                                                                                               UniformGrid.children [ for i in 1 .. state.Files.Length ->
                                                                                                                                          (createFileButton
                                                                                                                                              (state.Files.Item
                                                                                                                                                  (i
                                                                                                                                                   - 1))
                                                                                                                                               state
                                                                                                                                               dispatch) ] ] ] ] ] ]
