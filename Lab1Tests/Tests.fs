module Tests

open Xunit
open Xunit.Abstractions
open Lab1

type Tests(output: ITestOutputHelper) =
    let initState: Table.State =
        { Grid = [ for _ in 0 .. 10 -> [ for _ in 0 .. 10 -> "" ] ]
          Cursor = (2, 2)
          CurrentCellData = "" }

    let setCellValue cell value (state: Table.State) =
        { state with
              Grid =
                  Table.Controller.updateCell
                      { state with
                            Cursor = cell
                            CurrentCellData = value } }

    let write result =
        output.WriteLine(sprintf "The actual result was %O" result)

    [<Fact>]
    let ``3 + 5 = 8`` () =
        let result =
            Table.Utils.parseExpression "3 + 5" initState

        write result
        Assert.Equal("8", result)

    [<Fact>]
    let ``A1(3) + 5 = 8`` () =
        let result =
            Table.Utils.parseExpression "A1 + 5" (setCellValue (1, 1) "3" initState)

        write result
        Assert.Equal("8", result)

    [<Fact>]
    let ``5 / 3 = 1`` () =
        let result =
            Table.Utils.parseExpression "5 / 3" initState

        write result
        Assert.Equal("1", result)

    [<Fact>]
    let ``5 * 3 = 15`` () =
        let result =
            Table.Utils.parseExpression "5 * 3" initState

        write result
        Assert.Equal("15", result)
