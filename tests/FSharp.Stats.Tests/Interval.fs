module IntervalTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Interval
open TestExtensions

[<Tests>]
let intervalTests =
    //apply tests also to Seq.range

    testList
        "Intervals"
        [

          testCase
              "create"
              (fun _ ->

                  let expected = Interval.Closed(-5., 5.)
                  let actual = Interval.Closed(-5., 5.)
                  Expect.equal expected actual "Instantiation of Interval.Closed is incorrect"

              //let expectedError() = Intervals.create 5. -5. |> ignore
              //Expect.throws expectedError "Interval maximum must be greater than minimum"
              )

          testCase
              "ofSeq"
              (fun _ ->
                  let expected = Interval.Closed(-5., 5.)
                  let actual =
                      Interval.ofSeq
                          [ 3.
                            -0.
                            -5.
                            0.
                            0.
                            5. ]
                  Expect.equal actual expected "Wrong interval was extracted from sequence"

                  let expectedInt = Interval.Closed(5, 5)
                  let actualInt =
                      Interval.ofSeq
                          [ 5
                            5
                            5
                            5
                            5 ]
                  Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

                  let nanCase () =
                      Interval.ofSeq
                          [ 3.
                            nan
                            -0.
                            -5.
                            0.
                            0.
                            5. ]
                      |> ignore
                  Expect.throws nanCase "collections containing nan should fail to return a valid interval"

                  let expectedInf = Interval.Closed(-5., infinity)
                  let actualInf =
                      Interval.ofSeq
                          [ 3.
                            infinity
                            -0.
                            -5.
                            infinity
                            0.
                            0.
                            5. ]
                  Expect.equal actualInf expectedInf "infinity should be upper margin of interval"

                  let expectedInfNeg = Interval.Closed(-infinity, 5.)
                  let actualInfNeg =
                      Interval.ofSeq
                          [ 3.
                            -infinity
                            -0.
                            -5.
                            0.
                            0.
                            -infinity
                            5. ]
                  Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"

                  let expectedInfs = Interval.Closed(-infinity, infinity)
                  let actualInfs =
                      Interval.ofSeq
                          [ 3.
                            infinity
                            -0.
                            -5.
                            0.
                            0.
                            -infinity
                            5. ]
                  Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"

                  let expectedEmpty = Interval.Empty
                  let actualEmpty = Interval.ofSeq []
                  Expect.equal actualEmpty expectedEmpty "Interval should be empty"

                  let expectedStr = Interval.Closed("aavbsd", "z")
                  let actualStr =
                      Interval.ofSeq
                          [ "asd"
                            "bcd"
                            "aavbsd"
                            "z" ]
                  Expect.equal actualStr expectedStr "Interval of strings is incorrect"

                  let expectedChar = Interval.CreateClosed<char>('f', 'r')
                  let actualChar =
                      Interval.ofSeq
                          [ 'g'
                            'f'
                            'q'
                            'q'
                            'r' ]
                  Expect.equal actualChar expectedChar "Interval of chars is incorrect"
              )

          testCase
              "ofSeqBy"
              (fun _ ->
                  let expected = Interval.CreateClosed<int * float>((3, -5.), (6, 5.))
                  let actual =
                      Interval.ofSeqBy
                          snd
                          [ 0, 3.
                            1, 5
                            2, -0.
                            3, -5.
                            4, 0.
                            5, 0.
                            6, 5. ]
                  Expect.equal actual expected "Wrong interval was extracted from indexed sequence"

                  let expectedInt = Interval.CreateClosed<int * int>((0, 5), (4, 5))
                  let actualInt =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 5
                                5
                                5
                                5
                                5 ])
                  Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

                  let nanCase () =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 3.
                                nan
                                -0.
                                -5.
                                0.
                                0.
                                5. ])
                      |> ignore
                  Expect.throws nanCase "collections containing nan should fail to return a valid interval"

                  let expectedInf = Interval.CreateClosed<int * float>((3, -5.), (4, infinity))
                  let actualInf =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 3.
                                infinity
                                -0.
                                -5.
                                infinity
                                0.
                                0.
                                5. ])
                  Expect.equal actualInf expectedInf "infinity should be upper margin of interval"

                  let expectedInfNeg = Interval.CreateClosed<int * float>((1, -infinity), (7, 5.))
                  let actualInfNeg =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 3.
                                -infinity
                                -0.
                                -5.
                                0.
                                0.
                                -infinity
                                5. ])
                  Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"

                  let expectedInfs = Interval.CreateClosed<int * float>((6, -infinity), (1, infinity))
                  let actualInfs =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 3.
                                infinity
                                -0.
                                -5.
                                0.
                                0.
                                -infinity
                                5. ])
                  Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"

                  let expectedEmpty = Interval.Empty
                  let actualEmpty = Interval.ofSeqBy snd []
                  Expect.equal actualEmpty expectedEmpty "Interval should be empty"

                  let expectedStr = Interval.CreateClosed<int * string>((2, "a"), (4, "zz"))
                  let actualStr =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ "asd"
                                "bcd"
                                "a"
                                "z"
                                "zz"
                                "aavbsd" ])
                  Expect.equal actualStr expectedStr "Interval of strings is incorrect"

                  let expectedChar = Interval.CreateClosed<int * char>((1, 'f'), (5, 'r'))
                  let actualChar =
                      Interval.ofSeqBy
                          snd
                          (List.indexed
                              [ 'g'
                                'f'
                                'q'
                                'q'
                                'r'
                                'r' ])
                  Expect.equal actualChar expectedChar "Interval of chars is incorrect"
              )

          testCase
              "values"
              (fun _ ->
                  let expected = Interval.CreateClosed<float>(-5., 5.) |> Interval.values
                  let actual = (-5., 5.)
                  Expect.equal actual expected "Interval value assessment is incorrect"

                  let expectedEmpty () =
                      Interval.Empty |> Interval.values |> ignore
                  Expect.throws expectedEmpty "Empty intervals cannot have values"
              )

          testCase
              "getStart"
              (fun _ ->
                  let expected = Interval.CreateClosed<float>(-5., 5.) |> Interval.getStart
                  let actual = -5.
                  Expect.equal actual expected "Interval minimum assessment is incorrect"

                  let expectedEmpty () =
                      Interval.Empty |> Interval.getStart |> ignore
                  Expect.throws expectedEmpty "Empty intervals cannot have starts"
              )

          testCase
              "getEnd"
              (fun _ ->
                  let actual = Interval.CreateClosed<float>(-5., 5.) |> Interval.getEnd
                  let expected = 5.
                  Expect.equal actual expected "Interval maximum assessment is incorrect"

                  let expectedEmpty () =
                      Interval.Empty |> Interval.getEnd |> ignore
                  Expect.throws expectedEmpty "Empty intervals cannot have ends"
              )

          testCase
              "getSize"
              (fun _ ->
                  let actual = Interval.CreateClosed<float>(-5., 5.5) |> Interval.getSize
                  let expected = 10.5
                  Expect.equal actual expected "Interval size calculation is incorrect"

                  let expectedEmpty () =
                      Interval.Empty |> Interval.getSize |> ignore
                  Expect.throws expectedEmpty "Empty intervals cannot have have a size"
              )

          testCase
              "getSizeBy"
              (fun _ ->
                  let actual =
                      Interval.CreateClosed<string * float>(("a", -5.), ("b", 5.5))
                      |> Interval.getSizeBy snd
                  let expected = 10.5
                  Expect.equal actual expected "Interval size calculation is incorrect"

                  let expectedEmpty () =
                      Interval.Empty |> Interval.getSizeBy id |> ignore
                  Expect.throws expectedEmpty "Empty intervals cannot have a size"
              )

          testCase
              "trySize"
              (fun _ ->
                  let actual = Interval.CreateClosed<float>(-5., 5.5) |> Interval.trySize
                  let expected = Some 10.5
                  Expect.equal actual expected "Size of interval is incorrect"

                  let expectedEmpty = Interval.Empty |> Interval.trySize
                  Expect.equal expectedEmpty None "Empty intervals have no size"
              )

          testCase
              "add"
              (fun _ ->
                  let actual =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(0., 3.)
                      Interval.add i1 i2
                  let expected = Interval.CreateClosed<float>(-5., 8.5)
                  Expect.equal actual expected "Interval addition is incorrect"

                  let actualCE =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.Empty
                      Interval.add i1 i2
                  let expectedCE = Interval.CreateClosed<float>(-5., 5.5)
                  Expect.equal actualCE expectedCE "Interval addition of Empty intervals is incorrect"

                  let actualEC =
                      let i1 = Interval.Empty
                      let i2 = Interval.CreateClosed<float>(0., 3.)
                      Interval.add i1 i2
                  let expectedEC = Interval.CreateClosed<float>(0., 3.)
                  Expect.equal actualEC expectedEC "Interval addition of Empty intervals is incorrect"

                  let actualEE =
                      let i1 = Interval.Empty
                      let i2 = Interval.Empty
                      Interval.add i1 i2
                  let expectedEE = Interval.Empty
                  Expect.equal actualEE expectedEE "Interval addition of Empty intervals is incorrect"
              )

          testCase
              "subtract"
              (fun _ ->
                  let actual =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.subtract i1 i2
                  let expected = Interval.CreateClosed<float>(-5., 8.5)
                  Expect.equal actual expected "Interval subtraction is incorrect"

                  let actualCE =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.Empty
                      Interval.subtract i1 i2
                  let expectedCE = Interval.CreateClosed<float>(-5., 5.5)
                  Expect.equal actualCE expectedCE "Interval subtraction of Empty intervals is incorrect"

                  let actualEC =
                      let i1 = Interval.Empty
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.subtract i1 i2
                  let expectedEC = Interval.CreateClosed<float>(-3., 0.)
                  Expect.equal actualEC expectedEC "Interval subtraction of Empty intervals is incorrect"

                  let actualEE =
                      let i1 = Interval.Empty
                      let i2 = Interval.Empty
                      Interval.subtract i1 i2
                  let expectedEE = Interval.Empty
                  Expect.equal actualEE expectedEE "Interval subtraction of Empty intervals is incorrect"
              )

          // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
          testCase
              "isIntersection"
              (fun _ ->
                  let actual =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.isIntersection i1 i2
                  let expected = true
                  Expect.equal actual expected "Intervals do intersect"

                  let actualFalse =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-infinity, -6.)
                      Interval.isIntersection i1 i2
                  let expectedFalse = false
                  Expect.equal actualFalse expectedFalse "Intervals do not intersect"

                  let actualFalse2 =
                      let i1 = Interval.RightOpen(3, 5)
                      let i2 = Interval.LeftOpen(5, 9)
                      Interval.isIntersection i1 i2
                  Expect.equal actualFalse2 false "Intervals (3,5) and (5,9) do not intersect"

                  let actualCE =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.Empty
                      Interval.isIntersection i1 i2
                  let expectedCE = false
                  Expect.equal actualCE expectedCE "Intervals do not intersect"

                  let actualEC =
                      let i1 = Interval.Empty
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.isIntersection i1 i2
                  let expectedEC = false
                  Expect.equal actualEC expectedEC "Empty intervals do not intersect"

                  let actualEE =
                      let i1 = Interval.Empty
                      let i2 = Interval.Empty
                      Interval.isIntersection i1 i2
                  let expectedEE = true
                  Expect.equal actualEE expectedEE "Empty intervals do intersect"

                  let actualStr =
                      let i1 = Interval.CreateClosed<string>("a", "d")
                      let i2 = Interval.CreateClosed<string>("de", "e")
                      Interval.isIntersection i1 i2
                  let expectedStr = false
                  Expect.equal actualStr expectedStr "String intervals do not intersect"

                  let actualStrT =
                      let i1 = Interval.CreateClosed<string>("a", "d")
                      let i2 = Interval.CreateClosed<string>("d", "e")
                      Interval.isIntersection i1 i2
                  let expectedStrT = true
                  Expect.equal actualStrT expectedStrT "String intervals do intersect"


                  let i1 = Interval.CreateOpen<float>(1., 2.)
                  let i2 = Interval.CreateOpen<float>(2., 3.)
                  let i3 = Interval.CreateClosed<float>(2., 3.)
                  let i4 = Interval.CreateClosed<float>(3., 4.)
                  let i5 = Interval.CreateLeftOpen<float>(2., 3.)
                  let i6 = Interval.CreateLeftOpen<float>(1., 2.)
                  let i7 = Interval.CreateRightOpen<float>(2., 3.)
                  let i8 = Interval.CreateRightOpen<float>(3., 4.)

                  Expect.equal (Interval.isIntersection i1 i1) true "(i1 i1) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i1 i2) false "(i1 i2) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i1 i3) false "(i1 i3) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i1 i4) false "(i1 i4) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i1 i5) false "(i1 i5) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i1 i6) true "(i1 i6) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i1 i7) false "(i1 i7) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i1 i8) false "(i1 i8) Intervals do not intersect"

                  Expect.equal (Interval.isIntersection i2 i1) false "(i2 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i2 i2) true "(i2 i2) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i2 i3) true "(i2 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i2 i4) false "(i2 i4) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i2 i5) true "(i2 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i2 i6) false "(i2 i6) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i2 i7) true "(i2 i7) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i2 i8) false "(i2 i8) Intervals do not intersect"

                  Expect.equal (Interval.isIntersection i3 i1) false "(i3 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i3 i2) true "(i3 i2) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i3) true "(i3 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i4) true "(i3 i4) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i5) true "(i3 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i6) true "(i3 i6) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i7) true "(i3 i7) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i3 i8) true "(i3 i8) Intervals do intersect"

                  Expect.equal (Interval.isIntersection i4 i1) false "(i4 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i4 i2) false "(i4 i2) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i4 i3) true "(i4 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i4 i4) true "(i4 i4) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i4 i5) true "(i4 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i4 i6) false "(i4 i6) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i4 i7) false "(i4 i7) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i4 i8) true "(i4 i8) Intervals do intersect"

                  Expect.equal (Interval.isIntersection i5 i1) false "(i5 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i5 i2) true "(i5 i2) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i5 i3) true "(i5 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i5 i4) true "(i5 i4) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i5 i5) true "(i5 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i5 i6) false "(i5 i6) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i5 i7) true "(i5 i7) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i5 i8) true "(i5 i8) Intervals do intersect"

                  Expect.equal (Interval.isIntersection i6 i1) true "(i6 i1) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i6 i2) false "(i6 i2) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i6 i3) true "(i6 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i6 i4) false "(i6 i4) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i6 i5) false "(i6 i5) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i6 i6) true "(i6 i6) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i6 i7) true "(i6 i7) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i6 i8) false "(i6 i8) Intervals do not intersect"

                  Expect.equal (Interval.isIntersection i7 i1) false "(i7 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i7 i2) true "(i7 i2) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i7 i3) true "(i7 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i7 i4) false "(i7 i4) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i7 i5) true "(i7 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i7 i6) true "(i7 i6) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i7 i7) true "(i7 i7) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i7 i8) false "(i7 i8) Intervals do not intersect"

                  Expect.equal (Interval.isIntersection i8 i1) false "(i8 i1) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i8 i2) false "(i8 i2) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i8 i3) true "(i8 i3) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i8 i4) true "(i8 i4) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i8 i5) true "(i8 i5) Intervals do intersect"
                  Expect.equal (Interval.isIntersection i8 i6) false "(i8 i6) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i8 i7) false "(i8 i7) Intervals do not intersect"
                  Expect.equal (Interval.isIntersection i8 i8) true "(i8 i8) Intervals do intersect"
              )


          // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
          testCase
              "intersect"
              (fun _ ->
                  let actual =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.intersect i1 i2
                  let expected = (Interval.CreateClosed<float>(-3., 0.))
                  Expect.equal actual expected "Interval intersect is calculated incorrectly"

                  let actualFalse =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-infinity, -6.)
                      Interval.intersect i1 i2
                  let expectedFalse = Interval.Empty
                  Expect.equal actualFalse expectedFalse "Interval intersect is calculated incorrectly"

                  let actual2 =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.CreateClosed<float>(-infinity, 2.)
                      Interval.intersect i1 i2
                  let expected2 = Interval.CreateClosed<float>(-5., 2.)
                  Expect.equal actual2 expected2 "Intervals do intersect"

                  let actualCE =
                      let i1 = Interval.CreateClosed<float>(-5., 5.5)
                      let i2 = Interval.Empty
                      Interval.intersect i1 i2
                  let expectedCE = Interval.Empty
                  Expect.equal actualCE expectedCE "Intervals do not intersect"

                  let actualEC =
                      let i1 = Interval.Empty
                      let i2 = Interval.CreateClosed<float>(-3., 0.)
                      Interval.intersect i1 i2
                  let expectedEC = Interval.Empty
                  Expect.equal actualEC expectedEC "Empty intervals do not intersect"

                  let actualEE =
                      let i1 = Interval.Empty
                      let i2 = Interval.Empty
                      Interval.intersect i1 i2
                  let expectedEE = Interval.Empty
                  Expect.equal actualEE expectedEE "Empty intervals do intersect"

                  let actualStr =
                      let i1 = Interval.CreateClosed<string>("a", "d")
                      let i2 = Interval.CreateClosed<string>("de", "e")
                      Interval.intersect i1 i2
                  let expectedStr = Interval.Empty
                  Expect.equal actualStr expectedStr "String intervals do not intersect"

                  let actualStrT =
                      let i1 = Interval.CreateClosed<string>("a", "d")
                      let i2 = Interval.CreateClosed<string>("d", "e")
                      Interval.intersect i1 i2
                  let expectedStrT = Interval.CreateClosed<string>("d", "d")
                  Expect.equal actualStrT expectedStrT "String intervals do intersect"

                  let actualCO1 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateOpen<int>(1, 4)
                      Interval.intersect i1 i2
                  let expectedCO1 = Interval.CreateRightOpen<int>(3, 4)
                  Expect.equal actualCO1 expectedCO1 "Interval intersect is calculated incorrectly"

                  let actualCO2 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateOpen<int>(4, 7)
                      Interval.intersect i1 i2
                  let expectedCO2 = Interval.CreateLeftOpen<int>(4, 6)
                  Expect.equal actualCO2 expectedCO2 "Interval intersect is calculated incorrectly"

                  let actualCO3 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateOpen<int>(4, 5)
                      Interval.intersect i1 i2
                  let expectedCO3 = Interval.CreateOpen<int>(4, 5)
                  Expect.equal actualCO3 expectedCO3 "Interval intersect is calculated incorrectly"

                  let actualCO4 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateOpen<int>(1, 7)
                      Interval.intersect i1 i2
                  let expectedCO4 = Interval.CreateClosed<int>(3, 6)
                  Expect.equal actualCO4 expectedCO4 "Interval intersect is calculated incorrectly"

                  let actualCO5 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateOpen<int>(1, 3)
                      Interval.intersect i1 i2
                  let expectedCO5 = Interval.Empty
                  Expect.equal actualCO5 expectedCO5 "Interval intersect is calculated incorrectly"

                  let actualCLO1 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateLeftOpen<int>(1, 4)
                      Interval.intersect i1 i2
                  let expectedCLO1 = Interval.CreateClosed<int>(3, 4)
                  Expect.equal actualCLO1 expectedCLO1 "Interval intersect is calculated incorrectly"

                  let actualCLO2 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateLeftOpen<int>(4, 7)
                      Interval.intersect i1 i2
                  let expectedCLO2 = Interval.CreateLeftOpen<int>(4, 6)
                  Expect.equal actualCLO2 expectedCLO2 "Interval intersect is calculated incorrectly"

                  let actualCLO3 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateLeftOpen<int>(4, 5)
                      Interval.intersect i1 i2
                  let expectedCLO3 = Interval.CreateLeftOpen<int>(4, 5)
                  Expect.equal actualCLO3 expectedCLO3 "Interval intersect is calculated incorrectly"

                  let actualCLO4 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateLeftOpen<int>(1, 7)
                      Interval.intersect i1 i2
                  let expectedCLO4 = Interval.CreateClosed<int>(3, 6)
                  Expect.equal actualCLO4 expectedCLO4 "Interval intersect is calculated incorrectly"

                  let actualCLO5 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateLeftOpen<int>(1, 3)
                      Interval.intersect i1 i2
                  let expectedCLO5 = Interval.CreateClosed<int>(3, 3)
                  Expect.equal actualCLO5 expectedCLO5 "Interval intersect is calculated incorrectly"

                  let actualCRO1 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 4)
                      Interval.intersect i1 i2
                  let expectedCRO1 = Interval.CreateRightOpen<int>(3, 4)
                  Expect.equal actualCRO1 expectedCRO1 "Interval intersect is calculated incorrectly"

                  let actualCRO2 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(4, 7)
                      Interval.intersect i1 i2
                  let expectedCRO2 = Interval.CreateClosed<int>(4, 6)
                  Expect.equal actualCRO2 expectedCRO2 "Interval intersect is calculated incorrectly"

                  let actualCRO3 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(4, 5)
                      Interval.intersect i1 i2
                  let expectedCRO3 = Interval.CreateRightOpen<int>(4, 5)
                  Expect.equal actualCRO3 expectedCRO3 "Interval intersect is calculated incorrectly"

                  let actualCRO4 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 7)
                      Interval.intersect i1 i2
                  let expectedCRO4 = Interval.CreateClosed<int>(3, 6)
                  Expect.equal actualCRO4 expectedCRO4 "Interval intersect is calculated incorrectly"

                  let actualCRO5 =
                      let i1 = Interval.CreateClosed<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 3)
                      Interval.intersect i1 i2
                  let expectedCRO5 = Interval.Empty
                  Expect.equal actualCRO5 expectedCRO5 "Interval intersect is calculated incorrectly"

                  let actualROLO1 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 4)
                      Interval.intersect i1 i2
                  let expectedROLO1 = Interval.CreateOpen<int>(3, 4)
                  Expect.equal actualROLO1 expectedROLO1 "Interval intersect is calculated incorrectly"

                  let actualROLO2 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(4, 7)
                      Interval.intersect i1 i2
                  let expectedROLO2 = Interval.CreateClosed<int>(4, 6)
                  Expect.equal actualROLO2 expectedROLO2 "Interval intersect is calculated incorrectly"

                  let actualROLO3 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(4, 5)
                      Interval.intersect i1 i2
                  let expectedROLO3 = Interval.CreateRightOpen<int>(4, 5)
                  Expect.equal actualROLO3 expectedROLO3 "Interval intersect is calculated incorrectly"

                  let actualROLO4 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 7)
                      Interval.intersect i1 i2
                  let expectedROLO4 = Interval.CreateLeftOpen<int>(3, 6)
                  Expect.equal actualROLO4 expectedROLO4 "Interval intersect is calculated incorrectly"

                  let actualROLO5 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(1, 3)
                      Interval.intersect i1 i2
                  let expectedROLO5 = Interval.Empty
                  Expect.equal actualROLO5 expectedROLO5 "Interval intersect is calculated incorrectly"

                  let actualROLO6 =
                      let i1 = Interval.CreateLeftOpen<int>(3, 6)
                      let i2 = Interval.CreateRightOpen<int>(6, 9)
                      Interval.intersect i1 i2
                  let expectedROLO6 = Interval.CreateClosed<int>(6, 6)
                  Expect.equal actualROLO6 expectedROLO6 "Interval intersect is calculated incorrectly"


              )

          testCase
              "liesInInterval"
              (fun _ ->
                  // Test Closed interval [2.0, 5.0]
                  let closed = Interval.CreateClosed(2.0, 5.0)
                  Expect.isTrue (closed.liesInInterval 2.0) "2.0 should be in [2.0, 5.0]"
                  Expect.isTrue (closed.liesInInterval 3.5) "3.5 should be in [2.0, 5.0]"
                  Expect.isTrue (closed.liesInInterval 5.0) "5.0 should be in [2.0, 5.0]"
                  Expect.isFalse (closed.liesInInterval 1.9) "1.9 should not be in [2.0, 5.0]"
                  Expect.isFalse (closed.liesInInterval 5.1) "5.1 should not be in [2.0, 5.0]"

                  // Test Open interval (2.0, 5.0)
                  let opn = Interval.CreateOpen(2.0, 5.0)
                  Expect.isFalse (opn.liesInInterval 2.0) "2.0 should not be in (2.0, 5.0)"
                  Expect.isTrue (opn.liesInInterval 3.5) "3.5 should be in (2.0, 5.0)"
                  Expect.isFalse (opn.liesInInterval 5.0) "5.0 should not be in (2.0, 5.0)"
                  Expect.isFalse (opn.liesInInterval 1.9) "1.9 should not be in (2.0, 5.0)"

                  // Test LeftOpen interval (2.0, 5.0]
                  let leftOpen = Interval.CreateLeftOpen(2.0, 5.0)
                  Expect.isFalse (leftOpen.liesInInterval 2.0) "2.0 should not be in (2.0, 5.0]"
                  Expect.isTrue (leftOpen.liesInInterval 3.5) "3.5 should be in (2.0, 5.0]"
                  Expect.isTrue (leftOpen.liesInInterval 5.0) "5.0 should be in (2.0, 5.0]"

                  // Test RightOpen interval [2.0, 5.0)
                  let rightOpen = Interval.CreateRightOpen(2.0, 5.0)
                  Expect.isTrue (rightOpen.liesInInterval 2.0) "2.0 should be in [2.0, 5.0)"
                  Expect.isTrue (rightOpen.liesInInterval 3.5) "3.5 should be in [2.0, 5.0)"
                  Expect.isFalse (rightOpen.liesInInterval 5.0) "5.0 should not be in [2.0, 5.0)"

                  // Test Empty interval
                  Expect.isFalse (Interval.Empty.liesInInterval 3.0) "No value should be in Empty interval"
              )

          testCase
              "TryStart/TryEnd/TryToTuple"
              (fun _ ->
                  // Test Closed interval
                  let closed = Interval.CreateClosed(2.0, 5.0)
                  Expect.equal closed.TryStart (Some 2.0) "TryStart should return Some 2.0"
                  Expect.equal closed.TryEnd (Some 5.0) "TryEnd should return Some 5.0"
                  Expect.equal closed.TryToTuple (Some(2.0, 5.0)) "TryToTuple should return Some (2.0, 5.0)"

                  // Test Open interval
                  let opn = Interval.CreateOpen(1.0, 10.0)
                  Expect.equal opn.TryStart (Some 1.0) "TryStart should return Some 1.0"
                  Expect.equal opn.TryEnd (Some 10.0) "TryEnd should return Some 10.0"
                  Expect.equal opn.TryToTuple (Some(1.0, 10.0)) "TryToTuple should return Some (1.0, 10.0)"

                  // Test LeftOpen interval
                  let leftOpen = Interval.CreateLeftOpen(-5.0, 3.0)
                  Expect.equal leftOpen.TryStart (Some -5.0) "TryStart should return Some -5.0"
                  Expect.equal leftOpen.TryEnd (Some 3.0) "TryEnd should return Some 3.0"

                  // Test RightOpen interval
                  let rightOpen = Interval.CreateRightOpen(0.0, 1.0)
                  Expect.equal rightOpen.TryStart (Some 0.0) "TryStart should return Some 0.0"
                  Expect.equal rightOpen.TryEnd (Some 1.0) "TryEnd should return Some 1.0"

                  // Test Empty interval
                  Expect.equal Interval.Empty.TryStart None "TryStart should return None for Empty"
                  Expect.equal Interval.Empty.TryEnd None "TryEnd should return None for Empty"
                  Expect.equal Interval.Empty.TryToTuple None "TryToTuple should return None for Empty"
              )

          testCase
              "createClosedOfSize"
              (fun _ ->
                  let interval = Interval.createClosedOfSize 2.0 5.0
                  Expect.equal interval (Interval.CreateClosed(2.0, 7.0)) "Should create [2.0, 7.0]"

                  let interval2 = Interval.createClosedOfSize 0.0 10.0
                  Expect.equal interval2 (Interval.CreateClosed(0.0, 10.0)) "Should create [0.0, 10.0]"

                  let interval3 = Interval.createClosedOfSize -5.0 3.0
                  Expect.equal interval3 (Interval.CreateClosed(-5.0, -2.0)) "Should create [-5.0, -2.0]"
              )

          testCase
              "createOpenOfSize"
              (fun _ ->
                  let interval = Interval.createOpenOfSize 2.0 5.0
                  Expect.equal interval (Interval.CreateOpen(2.0, 7.0)) "Should create (2.0, 7.0)"

                  let interval2 = Interval.createOpenOfSize 0.0 10.0
                  Expect.equal interval2 (Interval.CreateOpen(0.0, 10.0)) "Should create (0.0, 10.0)"

                  // Zero size should return Empty
                  let intervalZero = Interval.createOpenOfSize 5.0 0.0
                  Expect.equal intervalZero Interval.Empty "Should create Empty for zero size"
              )

          testCase
              "createLeftOpenOfSize"
              (fun _ ->
                  let interval = Interval.createLeftOpenOfSize 2.0 5.0
                  Expect.equal interval (Interval.CreateLeftOpen(2.0, 7.0)) "Should create (2.0, 7.0]"

                  let interval2 = Interval.createLeftOpenOfSize 0.0 10.0
                  Expect.equal interval2 (Interval.CreateLeftOpen(0.0, 10.0)) "Should create (0.0, 10.0]"

                  // Zero size should return Empty
                  let intervalZero = Interval.createLeftOpenOfSize 5.0 0.0
                  Expect.equal intervalZero Interval.Empty "Should create Empty for zero size"
              )

          testCase
              "createRightOpenOfSize"
              (fun _ ->
                  let interval = Interval.createRightOpenOfSize 2.0 5.0
                  Expect.equal interval (Interval.CreateRightOpen(2.0, 7.0)) "Should create [2.0, 7.0)"

                  let interval2 = Interval.createRightOpenOfSize 0.0 10.0
                  Expect.equal interval2 (Interval.CreateRightOpen(0.0, 10.0)) "Should create [0.0, 10.0)"

                  // Zero size should return Empty
                  let intervalZero = Interval.createRightOpenOfSize 5.0 0.0
                  Expect.equal intervalZero Interval.Empty "Should create Empty for zero size"
              )

          testCase
              "getValueAt"
              (fun _ ->
                  // Test Closed interval [0.0, 10.0]
                  let interval = Interval.CreateClosed(0.0, 10.0)
                  Expect.floatClose Accuracy.high (Interval.getValueAt 0.0 interval) 0.0 "Value at 0.0 should be 0.0"
                  Expect.floatClose Accuracy.high (Interval.getValueAt 0.5 interval) 5.0 "Value at 0.5 should be 5.0"
                  Expect.floatClose Accuracy.high (Interval.getValueAt 1.0 interval) 10.0 "Value at 1.0 should be 10.0"
                  Expect.floatClose Accuracy.high (Interval.getValueAt 0.25 interval) 2.5 "Value at 0.25 should be 2.5"
                  Expect.floatClose Accuracy.high (Interval.getValueAt 0.75 interval) 7.5 "Value at 0.75 should be 7.5"

                  // Test extrapolation outside interval
                  Expect.floatClose Accuracy.high (Interval.getValueAt 1.5 interval) 15.0 "Value at 1.5 should be 15.0"
                  Expect.floatClose
                      Accuracy.high
                      (Interval.getValueAt -0.5 interval)
                      -5.0
                      "Value at -0.5 should be -5.0"

                  // Test with negative interval [-10.0, -5.0]
                  let interval2 = Interval.CreateClosed(-10.0, -5.0)
                  Expect.floatClose
                      Accuracy.high
                      (Interval.getValueAt 0.5 interval2)
                      -7.5
                      "Value at 0.5 should be -7.5"

                  // Test Empty interval (should return NaN)
                  let emptyResult = Interval.getValueAt 0.5 Interval.Empty
                  Expect.isTrue (System.Double.IsNaN emptyResult) "Empty interval should return NaN"
              )

          testCase
              "ToString"
              (fun _ ->
                  let closed = Interval.CreateClosed(2.0, 5.0)
                  Expect.equal (closed.ToString()) "[2.0,5.0]" "Closed interval ToString incorrect"

                  let opn = Interval.CreateOpen(2.0, 5.0)
                  Expect.equal (opn.ToString()) "(2.0,5.0)" "Open interval ToString incorrect"

                  let leftOpen = Interval.CreateLeftOpen(2.0, 5.0)
                  Expect.equal (leftOpen.ToString()) "(2.0,5.0]" "LeftOpen interval ToString incorrect"

                  let rightOpen = Interval.CreateRightOpen(2.0, 5.0)
                  Expect.equal (rightOpen.ToString()) "[2.0,5.0)" "RightOpen interval ToString incorrect"

                  let empty: Interval<float> = Interval.Empty
                  Expect.equal (empty.ToString()) "[empty]" "Empty interval ToString incorrect"
              )



          ]
