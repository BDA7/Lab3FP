module Tests

open System
open System.IO
open Xunit
open Xunit.Abstractions
open Functions
open Reader

type LabTests(output: ITestOutputHelper) =

    let points =
        [ (1., 2.); (3., 4.); (5., 6.); (7., 8.); (9., 10.); (11., 12.); (13., 14.) ]

    [<Fact>]
    let ``Test Linear`` () =
        let f = chooseF "1" points
        assert (2. = f 1)
        assert (5.5 = f 4.5)
        assert (6.7 = f 5.7)

    [<Fact>]
    let ``Test logarithm`` () =
        let f = chooseF "2" points
        assert (0.4406432356126949 = f 1.02)
        assert (4.503721892587115 = f 2.5)
        assert (7.364782591706943 = f 4.7)

    [<Fact>]
    let ``Test segment`` () =
        let f = chooseF "3" points
        assert (10.0 = f 7.8)
        assert (4.0 = f 1.023)
        assert (6.0 = f 4.6)

    [<Theory>]
    [<InlineData("/file.txt")>]
    let ``Test i/o`` (path: string) =
        Console.SetIn(File.OpenText(__SOURCE_DIRECTORY__ + path))
        let writer = new StringWriter()
        Console.SetOut writer
        handleInputForOne false "1" "" 20 
        output.WriteLine(writer.ToString())