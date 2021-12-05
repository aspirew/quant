module Lecture1.Tests

open System
open NUnit.Framework
open Lecture1

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.TupleFunctions() =
        let tuple = (1, 2, 3)
        Assert.AreEqual(1, fst tuple)
        Assert.AreEqual(2, mid tuple)
        Assert.AreEqual(3, lst tuple)

    [<Test>]
    member this.Factorial() =
        Assert.AreEqual(120L, factorial 5L)
        Assert.AreEqual(479001600L, factorial 12L)
        Assert.AreEqual(1L, factorial -5L)

    [<Test>]
    member this.BinomialCoefficient() =
        Assert.AreEqual(10L, binomialCoefficient 5L 2L)
        Assert.AreEqual(1140L, binomialCoefficient 20L 3L)
        Assert.AreEqual(0L, binomialCoefficient 2L 5L)
        Assert.AreEqual(1L, binomialCoefficient 5L 5L)
        Assert.AreEqual(0L, binomialCoefficient 0L 5L)
        Assert.AreEqual(1L, binomialCoefficient 5L 0L)

    [<Test>]
    member this.pascalTriangle() =
        Assert.AreEqual([1], pascalTriangle 0L)
        Assert.AreEqual([1; 1], pascalTriangle 1L)
        Assert.AreEqual([1; 2; 1], pascalTriangle 2L)
        Assert.AreEqual([1; 5; 10; 10; 5; 1], pascalTriangle 5L)

    [<Test>]
    member this.getInfo() =
        let result = [
            { StudentName="Staszek"; GPA=3.12; Faculty = IT };
            { StudentName="Monika"; GPA=4.68; Faculty = IT };
        ]
        let rec compare xl yl = 
            match xl, yl with 
            | [], [] -> true
            | x::xs, y::ys -> x = y && compare xs ys
            | _ -> false

        Assert.IsTrue(compare result (getInfo examples IT))

    [<Test>]
    member this.getFacultyAvg() =
        Assert.AreEqual(3.1, getFacultyAvg examples PPT)

    [<Test>]
    member this.getListWithoutWord() =
        let example = ["ala"; "ma"; "kota"]
        Assert.AreEqual(["ala"; "kota"], getListWithoutWord "ma" example)

    [<Test>]
    member this.idxList() =
        let example = ["ala"; "ma"; "kota"]
        Assert.AreEqual([(1, "ala"); (2, "ma"); (3, "kota")], idxList 1 example)



