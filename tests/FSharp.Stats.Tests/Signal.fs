module SignalTests


open Expecto
open System
open System.Numerics
open FSharp.Stats
open FSharp.Stats.Signal
open FSharp.Stats.Signal.Padding.Discrete
open Signal.Outliers
open TestExtensions

[<Tests>]
let outlierTests =
    let ls =
        [ -1.4
          -1.4
          -1.3
          -7.9
          9.4
          -1.5
          5.0
          7.0
          1.1
          1.6 ]
    let m = List.mean ls //1.06

    let dataRow =
        [ [ 20.
            11. ]
          [ 22.
            29. ]
          [ 12.
            27. ]
          [ 13.
            15. ]
          [ 19.
            23. ]
          [ 28.
            18. ]
          [ 16.
            30. ]
          [ 25.
            24. ]
          [ 14.
            21. ]
          [ 17.
            26. ] ]
        |> matrix

    let dataColumn =
        [ [ 20.
            22.
            12.
            13.
            19.
            28.
            16.
            25.
            14.
            17. ]
          [ 11.
            29.
            27.
            15.
            23.
            18.
            30.
            24.
            21.
            26. ] ]
        |> matrix


    let compareIntervals a b (str: string) =
        Expect.floatClose Accuracy.high (Interval.getStart a) (Interval.getStart b) str
        Expect.floatClose Accuracy.high (Interval.getEnd a) (Interval.getEnd b) str

    testList
        "Signal.OutlierTests"
        [ testList
              "Z-Score"
              [ testCase "Z-Score in a population"
                <| fun () ->
                    let s = Seq.stDevPopulation (ls) //4.745144887
                    Expect.floatClose
                        Accuracy.high
                        (zScore -1.4 m s)
                        -0.5184246337
                        "Z-Score in a population was calculated incorrectly"

                testCase "Z-Score in a sample"
                <| fun () ->
                    let sSample = Seq.stDev (ls)
                    Expect.floatClose
                        Accuracy.high
                        (zScore -1.4 m sSample)
                        -0.4918207913
                        "Z-Score in a sample was calculated incorrectly"

                testCase "Z-Scores of a population"
                <| fun () ->
                    let zLs =
                        [ -0.5184246337
                          -0.5184246337
                          -0.4973504616
                          -1.88824582
                          1.757585953
                          -0.5394988058
                          0.8303223808
                          1.251805823
                          0.008429668841
                          0.1138005294 ]
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (zScoresOfPopulation ls)
                        zLs
                        "Z-Score of a population was calculated incorrectly"

                testCase "Z-Scores of a sample"
                <| fun () ->
                    let zLsSample =
                        [ -0.4918207913
                          -0.4918207913
                          -0.4718280762
                          -1.791347272
                          1.667392439
                          -0.5118135064
                          0.7877129747
                          1.187567277
                          0.007997086037
                          0.1079606615 ]
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (zScoresOfSample ls)
                        zLsSample
                        "Z-Score of a sample was calculated incorrectly"

                testCase "Population interval by Z-Score"
                <| fun () ->
                    let populationInterval = Interval.Closed(-0.3635434661, 3.432572444)
                    compareIntervals
                        (populationIntervalByZScore -0.3 0.5 ls)
                        populationInterval
                        "Z-Score interval in a population was calculated incorrectly"

                testCase "Sample interval by Z-Score"
                <| fun () ->
                    let sampleInterval = Interval.Closed(-0.4405465671, 3.560910945)
                    compareIntervals
                        (sampleIntervalByZscore -0.3 0.5 ls)
                        sampleInterval
                        "Z-Score interval in a sample was calculated incorrectly" ]

          testList
              "Mahalanobi's Distance"
              [ testCase "Mahalanobi's Distance for an observation in a matrix"
                <| fun () ->
                    let obs =
                        Vector.ofList
                            [ 20.
                              11. ]
                    Expect.floatClose
                        Accuracy.high
                        (mahalanobisDistanceOfEntry dataRow Matrix.Sample Matrix.RowWise obs)
                        1.843936618
                        "Mahalanobi's Distance for an observation(Sample, RowWise) calculated incorrectly"
                    Expect.floatClose
                        Accuracy.high
                        (mahalanobisDistanceOfEntry dataColumn Matrix.Sample Matrix.ColWise obs)
                        1.843936618
                        "Mahalanobi's Distance for an observation calculated(Sample, ColWise) incorrectly"
                    Expect.floatClose
                        Accuracy.high
                        (mahalanobisDistanceOfEntry dataRow Matrix.Population Matrix.RowWise obs)
                        1.943679857
                        "Mahalanobi's Distance for an observation calculated(Population, RowWise) incorrectly"
                    Expect.floatClose
                        Accuracy.high
                        (mahalanobisDistanceOfEntry dataColumn Matrix.Population Matrix.ColWise obs)
                        1.943679857
                        "Mahalanobi's Distance for an observation calculated(Population, ColWise) incorrectly"

                testCase "Mahalanobi's Distance for every observation in a matrix"
                <| fun () ->
                    let mahalDistancesSample =
                        [ 1.843936618
                          1.315823162
                          1.395764847
                          1.698572419
                          0.1305760401
                          1.862248734
                          1.280527036
                          1.28097611
                          0.934074348
                          0.6301069471 ]
                    let mahalDistancesPopulation =
                        [ 1.943679857
                          1.386999396
                          1.471265332
                          1.790452538
                          0.1376392315
                          1.962982523
                          1.349794013
                          1.350267379
                          0.9846008145
                          0.6641910408 ]
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (mahalanobisDistances Matrix.Sample Matrix.RowWise dataRow)
                        mahalDistancesSample
                        "Mahalanobi's Distance for every observation in a matrix(Sample, RowWise) was calculated incorrectly"
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (mahalanobisDistances Matrix.Population Matrix.RowWise dataRow)
                        mahalDistancesPopulation
                        "Mahalanobi's Distance for every observation in a matrix(Population, RowWise) was calculated incorrectly"
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (mahalanobisDistances Matrix.Sample Matrix.ColWise dataColumn)
                        mahalDistancesSample
                        "Mahalanobi's Distance for every observation in a matrix(Sample, ColWise) was calculated incorrectly"
                    TestExtensions.sequenceEqual
                        Accuracy.high
                        (mahalanobisDistances Matrix.Population Matrix.ColWise dataColumn)
                        mahalDistancesPopulation
                        "Mahalanobi's Distance for every observation in a matrix(Population, ColWise) was calculated incorrectly"

                ] ]

[<Tests>]
let normalizationTests =

    let table =
        [ [ 4.5
            4.2
            3.6 ]
          [ 4.3
            4.2
            0.5 ]
          [ 2.5
            4.1
            0.6 ] ]
        |> matrix

    let tableB =
        [| [| 100.
              130.
              30. |]
           [| 80.
              200.
              30. |]
           [| 0.
              50.
              0. |]
           [| 40.
              50.
              20. |]
           [| 50.
              45.
              25. |]
           [| 40.
              50.
              15. |] |]
        |> matrix

    let tableWithNan =
        [ [ 4.5
            nan
            3.6 ]
          [ 4.3
            4.2
            nan ]
          [ 2.5
            4.1
            0.6 ] ]
        |> matrix

    testList
        "Signal.NormalizationTests"
        [ testCase "MedianOfRatios"
          <| fun () ->

              let expectedNormalizedTable =
                  [ [ 3.29784
                      2.08239
                      10.99283 ]
                    [ 3.15127
                      2.08239
                      1.52678 ]
                    [ 1.83213
                      2.03281
                      1.83213 ]

                    ]
                  |> matrix

              let result = Normalization.medianOfRatios table

              TestExtensions.sequenceEqual
                  4
                  result.NormedData
                  expectedNormalizedTable
                  "Matrix was not normalized correctly"

          testCase "MedianOfRatiosIgnoreNans"
          <| fun () ->

              let result =
                  Normalization.medianOfRatiosBy (fun x -> if System.Double.IsNaN x then 0.1 else x) tableWithNan

              Expect.hasCountOf
                  result.NormedData
                  2u
                  System.Double.IsNaN
                  "Only initial nan values should be nans afterwards"

          testCase "MedianOfRatioWides"
          <| fun () ->

              let result = Normalization.medianOfRatiosWide table
              let expected =
                  table
                  |> Matrix.transpose
                  |> Normalization.medianOfRatios
                  |> fun x -> x.NormedData
                  |> Matrix.transpose
              TestExtensions.sequenceEqual
                  4
                  result.NormedData
                  expected
                  "Wide method should return the same result as the non wide method on a transposed matrix"

          testCase "quantile"
          <| fun () ->

              let expectedNormalizedTable =
                  [ [ 110.
                      80.
                      80. ]
                    [ 80.
                      110.
                      110. ]
                    [ 15.
                      35.
                      15. ]
                    [ 35.
                      36.6666666667
                      36.6666666667 ]
                    [ 41.6666666667
                      15
                      41.6666666667 ]
                    [ 36.6666666667
                      41.6666666667
                      35. ] ]
                  |> matrix

              let result = Normalization.quantile tableB

              TestExtensions.sequenceEqual 4 result expectedNormalizedTable "Matrix was not normalized correctly" ]


[<Tests>]
let binningTests =

    let testData =
        [ "AT5G40650", 0.6142592186244475
          "AT5G36950", 0.02961887351477155
          "AT4G35320", 0.5711371856687455
          "AT1G52030", 0.13714132092557502
          "AT1G25480", 0.1777802253955505
          "AT1G13608", 0.1835805021082776
          "AT5G36950", 0.02961887351477155 //duplicate
          "AT5G06120", 0.5109225016759817
          "AT5G49150", 0.597941654040864
          "AT4G36770", 0.6812994122019935
          "AT5G10780", 0.003410975374229297 ]

    let testData1 =
        [ 0.05
          0.1
          0.2
          0.2
          0.3
          0.3
          0.3
          0.3
          0.4
          3.0
          3.0
          4.0
          6.0 ]

    testList
        "Signal.BinningTests"
        [ testCase "binBy"
          <| fun () ->

              let expected =
                  [| 0.05,
                     [ "AT5G36950", 0.02961887351477155
                       "AT5G36950", 0.02961887351477155
                       "AT5G10780", 0.003410975374229297 ]
                     0.15,
                     [ "AT1G52030", 0.13714132092557502
                       "AT1G25480", 0.1777802253955505
                       "AT1G13608", 0.1835805021082776 ]
                     0.55,
                     [ "AT4G35320", 0.5711371856687455
                       "AT5G06120", 0.5109225016759817
                       "AT5G49150", 0.597941654040864 ]
                     0.65,
                     [ "AT5G40650", 0.6142592186244475
                       "AT4G36770", 0.6812994122019935 ] |]

              let expectedBins = expected |> Array.map fst
              let expectedIds = expected |> Array.map (snd >> List.map fst)
              let expectedVals = expected |> Array.map (snd >> List.map snd)

              let actual =
                  Signal.Binning.binBy snd 0.1 testData
                  |> Map.map (fun a b -> List.ofSeq b)
                  |> Map.toArray

              let actualBins = actual |> Array.map fst
              let actualIds = actual |> Array.map (snd >> List.map fst)
              let actualVals = actual |> Array.map (snd >> List.map snd)

              TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

              expectedVals
              |> Array.iteri (fun i e ->
                  TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
              )

              Expect.equal actualIds expectedIds "Binning was not performed correctly"

          testCase "zeroBindwith"
          <| fun () ->

              let zeroBandwidth () =
                  Signal.Binning.binBy snd 0.0 testData |> ignore

              Expect.throwsT<(System.DivideByZeroException)> zeroBandwidth "Binning was not performed correctly"

          testCase "bin0.1"
          <| fun () ->

              let actual =
                  Signal.Binning.bin 0.1 testData1
                  |> Map.map (fun a b -> List.ofSeq b)
                  |> Map.toArray

              let actualBins = actual |> Array.map fst
              let actualIds = actual |> Array.map snd
              let actualVals = actual |> Array.map snd

              let expected =
                  [| 0.05, [ 0.05 ]
                     0.15, [ 0.1 ]
                     0.25,
                     [ 0.2
                       0.2 ]
                     0.35,
                     [ 0.3
                       0.3
                       0.3
                       0.3 ]
                     0.45, [ 0.4 ]
                     3.05,
                     [ 3.
                       3. ]
                     4.05, [ 4. ]
                     6.05, [ 6. ] |]

              let expectedBins = expected |> Array.map fst
              let expectedIds = expected |> Array.map snd
              let expectedVals = expected |> Array.map snd

              TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

              expectedVals
              |> Array.iteri (fun i e ->
                  TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
              )

              Expect.equal actualIds expectedIds "Binning was not performed correctly"

          testCase "bin1.0"
          <| fun () ->

              let actual =
                  Signal.Binning.bin 1. testData1
                  |> Map.map (fun a b -> List.ofSeq b)
                  |> Map.toArray

              let actualBins = actual |> Array.map fst
              let actualIds = actual |> Array.map snd
              let actualVals = actual |> Array.map snd

              let expected =
                  [| 0.5,
                     [ 0.05
                       0.1
                       0.2
                       0.2
                       0.3
                       0.3
                       0.3
                       0.3
                       0.4 ]
                     3.5,
                     [ 3.
                       3. ]
                     4.5, [ 4. ]
                     6.5, [ 6. ] |]

              let expectedBins = expected |> Array.map fst
              let expectedIds = expected |> Array.map snd
              let expectedVals = expected |> Array.map snd

              TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

              expectedVals
              |> Array.iteri (fun i e ->
                  TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
              )

              Expect.equal actualIds expectedIds "Binning was not performed correctly" ]


[<Tests>]
let paddingTests =
    let rnd = System.Random()
    let dataLength = 20
    let padding = 10

    let data = Array.init dataLength (fun i -> (3.0 + float i, 7.0 - float i))

    let randomTwoDimensionalArray dimension1Length dimension2Length =
        Array2D.init dimension1Length dimension2Length (fun _ _ -> rnd.NextDouble())

    let randomArray length =
        Array.init length (fun _ -> rnd.NextDouble())

    testList
        "Signal.PaddingTests"
        [

          testCase "pad"
          <| fun () ->

              let expectLeadIn = Array.init padding (fun i -> (3.0 - float (padding - i), 0.0))
              let expectLeadOut =
                  Array.init padding (fun i -> (3.0 + float (dataLength + i), 0.0))
              let expectedPadded =
                  Array.concat
                      [ expectLeadIn
                        data
                        expectLeadOut ]

              let padded =
                  Padding.pad
                      data
                      1.0
                      Double.PositiveInfinity
                      (-)
                      (+)
                      padding
                      Padding.BorderPaddingMethod.Zero
                      Padding.InternalPaddingMethod.NaN
                      Padding.HugeGapPaddingMethod.NaN

              Expect.equal (Array.sub padded 0 padding) expectLeadIn "padding is incorrect"
              Expect.equal (Array.sub padded (padded.Length - padding) padding) expectLeadOut "padding is incorrect"
              Expect.equal
                  (Array.sub padded padding data.Length)
                  data
                  "All the original data should be contained in the padded data"
              Expect.equal
                  padded.Length
                  (data.Length + 2 * padding)
                  "Length should be the original data length plus padding at each end"
              Expect.equal
                  (padded |> Array.sortBy fst)
                  expectedPadded
                  "Result should be the lead-in, whole data, then lead-out (maybe not in order?)"
              Expect.equal padded expectedPadded "Result should be the lead-in, whole data, then lead-out"

          testCase "three dimensional pad with zeroes"
          <| fun () ->
              let originalDimension1 = 30
              let originalDimension2 = 40
              let originalData = randomTwoDimensionalArray originalDimension1 originalDimension2

              let newHeight = (originalDimension1 + 2 * padding)
              let newWidth = (originalDimension2 + 2 * padding)
              let isPointInOriginalData i j =
                  (i >= padding && i < originalDimension1 + padding)
                  && (j >= padding && j < originalDimension2 + padding)

              let expected =
                  Array2D.init
                      newHeight
                      newWidth
                      (fun i j ->
                          if isPointInOriginalData i j then
                              originalData[i - padding, j - padding]
                          else
                              0.
                      )

              let paddedData2D = ThreeDimensional.pad originalData padding ThreeDimensional.Zero

              Expect.equal paddedData2D expected "padded data is incorrect"


          testCase "three dimensional pad with random padding"
          <| fun () ->
              let originalHeight = 30
              let originalWidth = 40

              let originalData = randomTwoDimensionalArray originalHeight originalWidth

              let newHeight = (originalHeight + 2 * padding)
              let newWidth = (originalWidth + 2 * padding)
              let flattenToArray (arr: 'T[,]) = arr |> Seq.cast<'T> |> Seq.toArray

              let paddedData2D = ThreeDimensional.pad originalData padding ThreeDimensional.Random

              Expect.equal paddedData2D.Length (newHeight * newWidth) "padded data length incorrect"
              // All the padded values should belong to the original data set
              Expect.containsAll
                  (originalData |> flattenToArray)
                  (paddedData2D |> flattenToArray)
                  "padded data contains item not in original data"


          testCase "padZero to discrete data"
          <| fun () ->
              let originalData = randomArray dataLength
              let newLength = (dataLength + 2 * padding)
              let isPointInOriginalData i =
                  (i >= padding && i < dataLength + padding)

              let expected =
                  Array.init
                      newLength
                      (fun i ->
                          if isPointInOriginalData i then
                              originalData[i - padding]
                          else
                              0.
                      )

              let paddedData = padZero originalData padding

              Expect.equal paddedData expected "padded data incorrect"

          testCase "padRnd to discrete data"
          <| fun () ->
              let originalData = randomArray dataLength
              let newLength = (dataLength + 2 * padding)

              let paddedData = padRnd originalData padding

              Expect.equal paddedData.Length newLength "padded data length incorrect"
              // All the padded values should belong to the original data set
              Expect.containsAll originalData paddedData "padded data contains item not in original data" ]

[<Tests>]
let filteringTests =

    testList
        "Signal.FilteringTests"
        [

          testList
              "savitzkyGolay - basic smoothing"
              [

                testCase "smooth noisy sine wave (windowSize=11, order=2)"
                <| fun () ->
                    // Generate noisy sine wave with significant noise
                    let rnd = System.Random(42)
                    let noisyData =
                        [| 0..50 |]
                        |> Array.map (fun x ->
                            let t = float x * 0.3
                            sin (t) + (rnd.NextDouble() - 0.5) * 0.5
                        )

                    let smoothed = Signal.Filtering.savitzkyGolay 11 2 0 1 noisyData

                    // Smoothed data should have same length as input
                    Expect.equal smoothed.Length noisyData.Length "Output length should match input"

                    // For all values should be finite
                    for i in 0 .. smoothed.Length - 1 do
                        Expect.isTrue
                            (not (System.Double.IsNaN smoothed.[i])
                             && not (System.Double.IsInfinity smoothed.[i]))
                            "All values should be finite"

                testCase "smooth linear data (should remain unchanged)"
                <| fun () ->
                    // Linear data should be perfectly reconstructed by polynomial filter
                    let linearData = [| 0.0..1.0..10.0 |]
                    let smoothed = Signal.Filtering.savitzkyGolay 5 1 0 1 linearData

                    // For linear data with order >= 1, result should be very close to original
                    TestExtensions.sequenceEqual 2 smoothed linearData "Linear data should be preserved"

                testCase "smooth quadratic data with order=2"
                <| fun () ->
                    // With proper order and window, quadratic trend should be well-preserved
                    let quadraticData = [| 0.0 .. 20.0 |] |> Array.map (fun x -> x * x)
                    let smoothed = Signal.Filtering.savitzkyGolay 11 2 0 1 quadraticData

                    // Check that smoothed data still follows quadratic trend (not perfectly preserved at edges)
                    // Check middle values
                    for i in 5..15 do
                        Expect.floatClose
                            Accuracy.low
                            smoothed.[i]
                            quadraticData.[i]
                            $"Quadratic trend should be preserved at index {i}"

                testCase "output has correct length"
                <| fun () ->
                    let data = Array.init 30 (fun i -> float i + (float i % 3.0))
                    let smoothed = Signal.Filtering.savitzkyGolay 7 2 0 1 data

                    Expect.equal smoothed.Length data.Length "Output should have same length as input" ]

          testList
              "savitzkyGolay - derivatives"
              [

                testCase "first derivative of linear function"
                <| fun () ->
                    // Derivative of linear function y = 2x should be constant 2
                    let linearData = [| 0.0 .. 10.0 |] |> Array.map (fun x -> 2.0 * x)
                    let derivative = Signal.Filtering.savitzkyGolay 5 2 1 1 linearData

                    // Check middle values (edges may have boundary effects)
                    for i in 2..7 do
                        Expect.floatClose Accuracy.low derivative.[i] 2.0 "First derivative of 2x should be ~2"

                testCase "first derivative of quadratic function"
                <| fun () ->
                    // Derivative of y = x^2 is 2x
                    let quadraticData = [| 0.0 .. 10.0 |] |> Array.map (fun x -> x * x)
                    let derivative = Signal.Filtering.savitzkyGolay 7 3 1 1 quadraticData

                    // Check middle values where x=5, derivative should be ~10
                    Expect.floatClose Accuracy.low derivative.[5] 10.0 "First derivative of x^2 at x=5 should be ~10"

                testCase "second derivative of quadratic function"
                <| fun () ->
                    // Second derivative of y = x^2 is 2
                    let quadraticData = [| 0.0 .. 20.0 |] |> Array.map (fun x -> x * x)
                    let secondDeriv = Signal.Filtering.savitzkyGolay 7 3 2 1 quadraticData

                    // Check middle values
                    for i in 5..15 do
                        Expect.floatClose Accuracy.low secondDeriv.[i] 2.0 "Second derivative of x^2 should be ~2" ]

          testList
              "savitzkyGolay - error handling"
              [

                testCase "windowSize must be odd"
                <| fun () ->
                    let data =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    Expect.throwsC
                        (fun () -> Signal.Filtering.savitzkyGolay 4 2 0 1 data |> ignore)
                        (fun ex -> Expect.stringContains (ex.Message) "odd" "Should require odd window size")

                testCase "windowSize must be positive"
                <| fun () ->
                    let data =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    Expect.throwsC
                        (fun () -> Signal.Filtering.savitzkyGolay 0 2 0 1 data |> ignore)
                        (fun ex -> Expect.stringContains (ex.Message) "positive" "Should require positive window size")

                testCase "order must be >= derivative order"
                <| fun () ->
                    let data =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0 |]
                    Expect.throwsC
                        (fun () -> Signal.Filtering.savitzkyGolay 5 1 2 1 data |> ignore)
                        (fun ex -> Expect.stringContains (ex.Message) "order must be greater" "Order must be >= deriv")

                testCase "windowSize must be large enough for polynomial order"
                <| fun () ->
                    let data =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0 |]
                    Expect.throwsC
                        (fun () -> Signal.Filtering.savitzkyGolay 5 5 0 1 data |> ignore)
                        (fun ex -> Expect.stringContains (ex.Message) "too small" "Window size must be > order + 1") ]

          testList
              "savitzkyGolay - edge cases"
              [

                testCase "minimum valid configuration (windowSize=3, order=1)"
                <| fun () ->
                    let data =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let smoothed = Signal.Filtering.savitzkyGolay 3 1 0 1 data

                    Expect.equal smoothed.Length data.Length "Output should have same length as input"
                    Expect.isNotNaN smoothed.[0] "Should not produce NaN"

                testCase "single peak removal"
                <| fun () ->
                    // Single outlier spike should be smoothed out
                    let data =
                        [| 1.0
                           1.0
                           1.0
                           5.0
                           1.0
                           1.0
                           1.0 |]
                    let smoothed = Signal.Filtering.savitzkyGolay 5 2 0 1 data

                    // The spike at index 3 should be reduced
                    Expect.isLessThan smoothed.[3] 3.0 "Spike should be smoothed"

                testCase "handles constant signal"
                <| fun () ->
                    let data = Array.create 10 5.0
                    let smoothed = Signal.Filtering.savitzkyGolay 5 2 0 1 data

                    // Constant signal should remain constant
                    for i in 0..9 do
                        Expect.floatClose Accuracy.high smoothed.[i] 5.0 "Constant signal should remain constant" ]

          testList
              "optimizeWindowWidth"
              [

                testCase "finds optimal window for noisy signal"
                <| fun () ->
                    // Create blank signal (noise only)
                    let rnd = System.Random(42)
                    let blankSignal = Array.init 100 (fun _ -> rnd.NextDouble() * 0.1 - 0.05)

                    // Create signal of interest (sine wave + noise)
                    let signalOfInterest =
                        Array.init
                            100
                            (fun i ->
                                let t = float i * 0.2
                                sin (t) + rnd.NextDouble() * 0.1 - 0.05
                            )

                    // For order=2, windowSize must be > order+1, so minimum is 5
                    let windowsToTest =
                        [| 5
                           7
                           9
                           11 |]
                    let optimalWindow =
                        Signal.Filtering.optimizeWindowWidth 2 windowsToTest blankSignal signalOfInterest

                    // Should return one of the test windows
                    Expect.contains windowsToTest optimalWindow "Should return one of the tested windows"
                    // Should be an odd number
                    Expect.equal (optimalWindow % 2) 1 "Optimal window should be odd"

                testCase "filters out even window sizes"
                <| fun () ->
                    let blankSignal = Array.init 50 (fun i -> float i * 0.01)
                    let signalOfInterest = Array.init 50 (fun i -> float i * 0.02)

                    // Include even numbers in test array (will be filtered to odd)
                    // For order=2, min window is 5
                    let windowsToTest =
                        [| 4
                           5
                           6
                           7
                           8
                           9 |]
                    let optimalWindow =
                        Signal.Filtering.optimizeWindowWidth 2 windowsToTest blankSignal signalOfInterest

                    // Should only consider odd windows >= 5
                    Expect.isTrue (optimalWindow % 2 = 1) "Should filter out even windows"
                    Expect.contains
                        [| 5
                           7
                           9 |]
                        optimalWindow
                        "Should only pick from valid odd windows"

                testCase "works with small dataset"
                <| fun () ->
                    let blankSignal =
                        [| 0.1
                           0.05
                           0.15
                           0.08
                           0.12
                           0.09
                           0.11
                           0.07
                           0.13
                           0.10 |]
                    let signalOfInterest =
                        [| 1.0
                           1.5
                           1.2
                           1.8
                           1.6
                           2.0
                           1.9
                           2.2
                           2.1
                           2.3 |]

                    // For order=2, windowSize must be > 3 (order+1), so use 5
                    let windowsToTest =
                        [| 5
                           7 |]
                    let optimalWindow =
                        Signal.Filtering.optimizeWindowWidth 2 windowsToTest blankSignal signalOfInterest

                    Expect.contains windowsToTest optimalWindow "Should return valid window"

                testCase "returns valid result for various polynomial orders"
                <| fun () ->
                    let rnd = System.Random(123)
                    let blankSignal = Array.init 100 (fun _ -> rnd.NextDouble() * 0.2)
                    let signalOfInterest =
                        Array.init 100 (fun i -> float i * 0.1 + rnd.NextDouble() * 0.2)

                    // Use windows appropriate for each polynomial order
                    // For order n, window must be > n+1, and must be odd
                    for polOrder in
                        [ 2
                          3
                          4 ] do
                        let minWindow = polOrder + 2
                        // Start from next odd number >= minWindow
                        let startWindow = if minWindow % 2 = 1 then minWindow else minWindow + 1
                        let windowsToTest = [| for i in 0..3 -> startWindow + i * 2 |]
                        let optimalWindow =
                            Signal.Filtering.optimizeWindowWidth polOrder windowsToTest blankSignal signalOfInterest
                        Expect.contains windowsToTest optimalWindow $"Should work with polynomial order {polOrder}" ] ]

[<Tests>]
let peakDetectionTests =

    testList
        "Signal.PeakDetectionTests"
        [ testList
              "localMaxima"
              [ testCase "finds single peak in simple data"
                <| fun () ->
                    // Algorithm needs at least 6 points and checks i-2, i-1, i, i+1, i+2
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           4.0
                           3.0
                           2.0
                           1.0 |]

                    let peaks = PeakDetection.localMaxima 2.0 xData yData

                    Expect.equal peaks.Length 1 "Should find one peak"
                    Expect.equal (fst peaks.[0]) 4.0 "Peak should be at x=4.0"
                    Expect.equal (snd peaks.[0]) 5.0 "Peak should have y=5.0"

                testCase "finds multiple peaks"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0
                           9.0
                           10.0
                           11.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           3.0
                           5.0
                           3.0
                           2.0
                           1.0
                           2.0
                           3.0
                           6.0
                           3.0
                           1.0 |]

                    let peaks = PeakDetection.localMaxima 2.0 xData yData

                    Expect.equal peaks.Length 2 "Should find two peaks"

                testCase "respects yThreshold"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           3.0
                           2.0
                           1.0
                           2.0
                           4.0
                           2.0
                           1.0 |]

                    // With threshold 3.5, only second peak should be found
                    let peaks = PeakDetection.localMaxima 3.5 xData yData

                    Expect.equal peaks.Length 1 "Should find one peak above threshold"
                    Expect.floatClose Accuracy.high (snd peaks.[0]) 4.0 "Found peak should be the higher one"

                testCase "returns empty for too small data"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           1.0 |]

                    let peaks = PeakDetection.localMaxima 0.0 xData yData

                    Expect.equal peaks.Length 0 "Should return empty for data with 5 or fewer points" ]

          testList
              "localMaximaIdx"
              [ testCase "finds indices of peaks"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           4.0
                           3.0
                           2.0
                           1.0 |]

                    let peakIndices = PeakDetection.localMaximaIdx 2.0 xData yData

                    Expect.equal peakIndices.Length 1 "Should find one peak"
                    Expect.equal peakIndices.[0] 4 "Peak should be at index 4"

                testCase "returns empty for too small data"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           1.0 |]

                    let peaks = PeakDetection.localMaximaIdx 0.0 xData yData

                    Expect.equal peaks.Length 0 "Should return empty for small data" ]

          testList
              "localMinima"
              [ testCase "finds single valley in simple data"
                <| fun () ->
                    // Algorithm needs at least 6 points and checks i-2, i-1, i, i+1, i+2
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 5.0
                           5.0
                           4.0
                           3.0
                           1.0
                           3.0
                           4.0
                           5.0
                           6.0 |]

                    let minima = PeakDetection.localMinima xData yData

                    Expect.equal minima.Length 1 "Should find one minimum"
                    Expect.equal (fst minima.[0]) 4.0 "Minimum should be at x=4.0"
                    Expect.equal (snd minima.[0]) 1.0 "Minimum should have y=1.0"

                testCase "finds multiple minima"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0
                           9.0
                           10.0
                           11.0 |]
                    let yData =
                        [| 5.0
                           4.0
                           3.0
                           1.0
                           3.0
                           4.0
                           5.0
                           4.0
                           3.0
                           0.5
                           3.0
                           5.0 |]

                    let minima = PeakDetection.localMinima xData yData

                    Expect.equal minima.Length 2 "Should find two minima"

                testCase "returns empty for too small data"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0 |]
                    let yData =
                        [| 3.0
                           1.0
                           3.0 |]

                    let minima = PeakDetection.localMinima xData yData

                    Expect.equal minima.Length 0 "Should return empty for small data" ]

          testList
              "localMinimaIdx"
              [ testCase "finds indices of minima"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 5.0
                           5.0
                           4.0
                           3.0
                           1.0
                           3.0
                           4.0
                           5.0
                           6.0 |]

                    let minimaIndices = PeakDetection.localMinimaIdx xData yData

                    Expect.equal minimaIndices.Length 1 "Should find one minimum"
                    Expect.equal minimaIndices.[0] 4 "Minimum should be at index 4" ]

          testList
              "idxOfHighestPeakBy"
              [ testCase "finds highest peak flanking target x-value"
                <| fun () ->
                    let xData =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let yData =
                        [| 2.0
                           5.0
                           3.0
                           6.0
                           2.0 |]

                    let idx = PeakDetection.idxOfHighestPeakBy xData yData 3.5

                    // Should find the highest of the two flanking peaks
                    Expect.equal idx 3 "Should return index of highest flanking peak"

                testCase "handles target before first value"
                <| fun () ->
                    let xData =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let yData =
                        [| 2.0
                           5.0
                           3.0
                           6.0
                           2.0 |]

                    let idx = PeakDetection.idxOfHighestPeakBy xData yData 0.5

                    // Should return first index
                    Expect.equal idx 0 "Should return first index for target before start"

                testCase "handles target after last value"
                <| fun () ->
                    let xData =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let yData =
                        [| 2.0
                           5.0
                           3.0
                           6.0
                           2.0 |]

                    let idx = PeakDetection.idxOfHighestPeakBy xData yData 6.0

                    // Should return last index
                    Expect.equal idx 4 "Should return last index for target after end" ]

          testList
              "idxOfClosestPeakBy"
              [ testCase "finds closest peak to target x-value"
                <| fun () ->
                    let xData =
                        [| 1.0
                           3.0
                           5.0
                           7.0
                           9.0 |]
                    let yData =
                        [| 2.0
                           5.0
                           3.0
                           6.0
                           2.0 |]

                    let idx = PeakDetection.idxOfClosestPeakBy xData yData 6.5

                    // 6.5 is closest to 7.0 at index 3
                    Expect.equal idx 3 "Should return index 3"

                testCase "handles exact match"
                <| fun () ->
                    let xData =
                        [| 1.0
                           3.0
                           5.0
                           7.0
                           9.0 |]
                    let yData =
                        [| 2.0
                           5.0
                           3.0
                           6.0
                           2.0 |]

                    let idx = PeakDetection.idxOfClosestPeakBy xData yData 5.0

                    Expect.equal idx 2 "Should return exact match index"

                testCase "handles empty array"
                <| fun () ->
                    let xData = [||]
                    let yData = [||]

                    let idx = PeakDetection.idxOfClosestPeakBy xData yData 5.0

                    Expect.equal idx 0 "Should return 0 for empty array" ]

          testList
              "labelPeaks"
              [ testCase "labels both positive and negative peaks"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let yData =
                        [| 1.0
                           2.0
                           5.0
                           2.0
                           0.5
                           2.0
                           6.0
                           2.0
                           1.0 |]

                    let labeled = PeakDetection.labelPeaks 1.0 3.0 xData yData

                    Expect.equal labeled.Length xData.Length "Should return labeled data for all points"

                    // Check that we have positive peaks
                    let positivePeaks =
                        labeled |> Array.filter (fun x -> x.Meta = PeakDetection.Extrema.Positive)
                    Expect.isGreaterThan positivePeaks.Length 0 "Should find positive peaks"

                testCase "returns None for edge points"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0 |]
                    let yData =
                        [| 10.0
                           2.0
                           5.0
                           2.0
                           10.0
                           2.0
                           5.0
                           2.0 |]

                    let labeled = PeakDetection.labelPeaks 1.0 3.0 xData yData

                    // First 3 and last 3 points should be None
                    Expect.equal labeled.[0].Meta PeakDetection.Extrema.None "First point should be None"
                    Expect.equal labeled.[1].Meta PeakDetection.Extrema.None "Second point should be None"
                    Expect.equal labeled.[2].Meta PeakDetection.Extrema.None "Third point should be None"

                testCase "returns empty for too small data"
                <| fun () ->
                    let xData =
                        [| 0.0
                           1.0
                           2.0 |]
                    let yData =
                        [| 1.0
                           5.0
                           1.0 |]

                    let labeled = PeakDetection.labelPeaks 1.0 3.0 xData yData

                    Expect.equal labeled.Length 0 "Should return empty for small data" ]

          testList
              "iterUntil"
              [ testCase "finds first matching element forward"
                <| fun () ->
                    let data =
                        [| 1
                           2
                           3
                           4
                           5
                           6
                           7 |]

                    let result = PeakDetection.iterUntil (fun x -> x > 5) 1 2 data

                    Expect.equal result (Some 5) "Should find index 5 (value 6)"

                testCase "finds first matching element backward"
                <| fun () ->
                    let data =
                        [| 1
                           2
                           3
                           4
                           5
                           6
                           7 |]

                    let result = PeakDetection.iterUntil (fun x -> x < 3) -1 5 data

                    Expect.equal result (Some 1) "Should find index 1 (value 2)"

                testCase "returns None when reaching end"
                <| fun () ->
                    let data =
                        [| 1
                           2
                           3
                           4
                           5 |]

                    let result = PeakDetection.iterUntil (fun x -> x > 10) 1 2 data

                    Expect.equal result None "Should return None when no match found"

                testCase "returns None when reaching start"
                <| fun () ->
                    let data =
                        [| 1
                           2
                           3
                           4
                           5 |]

                    let result = PeakDetection.iterUntil (fun x -> x < 0) -1 3 data

                    Expect.equal result None "Should return None when reaching start" ]

          testList
              "iterUntili"
              [ testCase "passes index to predicate"
                <| fun () ->
                    let data =
                        [| 10
                           20
                           30
                           40
                           50 |]

                    // Find first index >= 2 where value > 25
                    let result = PeakDetection.iterUntili (fun i x -> x > 25) 1 2 data

                    Expect.equal result (Some 2) "Should find index 2 where value=30 > 25" ]

          testList
              "createPeakFeature"
              [ testCase "creates peak feature with all fields"
                <| fun () ->
                    let peak = PeakDetection.createPeakFeature 5 10.0 20.0

                    Expect.equal peak.Index 5 "Index should be 5"
                    Expect.equal peak.XVal 10.0 "XVal should be 10.0"
                    Expect.equal peak.YVal 20.0 "YVal should be 20.0" ] ]
[<Tests>]
let fftTests =
    testList
        "Signal.FFTTests"
        [ testList
              "forwardInPlace and inverseInPlace - round trip"
              [ testCase "power-of-2 size (8 elements)"
                <| fun () ->
                    let original = [| for i in 0..7 -> Complex(float i, 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    // After forward+inverse, result is scaled by N, so divide by N
                    for i in 0..7 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            $"Real part at index {i} should match after round trip"
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Imaginary / float n)
                            0.0
                            $"Imaginary part at index {i} should be near zero"

                testCase "power-of-2 size (16 elements)"
                <| fun () ->
                    let original = [| for i in 0..15 -> Complex(float i * 0.5, 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..15 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Real part should match after round trip"

                testCase "non-power-of-2 size (10 elements - uses Bluestein)"
                <| fun () ->
                    let original = [| for i in 0..9 -> Complex(float i, 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..9 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Real part should match after round trip (Bluestein)"

                testCase "non-power-of-2 size (7 elements)"
                <| fun () ->
                    let original = [| for i in 0..6 -> Complex(float (i + 1), 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..6 do
                        Expect.floatClose
                            Accuracy.low
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Real part should match after round trip" ]

          testList
              "FFT properties"
              [ testCase "DC component (zero frequency) equals sum of input"
                <| fun () ->
                    // DC component (first element) should equal sum of all inputs
                    let input =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let data = input |> Array.map (fun x -> Complex(x, 0.0))

                    FFT.forwardInPlace data |> ignore

                    let expectedDC = Array.sum input
                    Expect.floatClose Accuracy.high data.[0].Real expectedDC "DC component should equal sum of inputs"

                testCase "detects single frequency sine wave"
                <| fun () ->
                    // Create a sine wave at a known frequency
                    let n = 64
                    let freq = 5.0 // 5 complete cycles in n points
                    let data =
                        [| for i in 0 .. n - 1 ->
                               let t = 2.0 * System.Math.PI * freq * float i / float n
                               Complex(sin (t), 0.0) |]

                    FFT.forwardInPlace data |> ignore

                    // The magnitude should peak at frequency 5 and n-5 (symmetry)
                    let magnitudes = data |> Array.map (fun c -> c.Magnitude)
                    let maxMagIdx = magnitudes |> Array.indexed |> Array.maxBy snd |> fst

                    // Peak should be at index 5 or 59 (n-5 due to symmetry)
                    Expect.isTrue
                        (maxMagIdx = 5 || maxMagIdx = n - 5)
                        $"Peak should be at frequency index 5 or {n - 5}, but was at {maxMagIdx}"

                testCase "constant signal has only DC component"
                <| fun () ->
                    let constantValue = 5.0
                    let data = Array.create 16 (Complex(constantValue, 0.0))

                    FFT.forwardInPlace data |> ignore

                    // DC component should be n * constant
                    Expect.floatClose
                        Accuracy.high
                        data.[0].Real
                        (16.0 * constantValue)
                        "DC component should be n * constant"

                    // All other components should be near zero
                    for i in 1..15 do
                        Expect.isLessThan data.[i].Magnitude 1e-10 $"Non-DC component at index {i} should be near zero" ]

          testList
              "Parseval's theorem (energy conservation)"
              [ testCase "energy is conserved (power-of-2)"
                <| fun () ->
                    // Parseval's theorem: sum of squares in time domain
                    // equals sum of squares of magnitudes in frequency domain (divided by N)
                    let input =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           3.0
                           2.0
                           1.0
                           0.0 |]
                    let data = input |> Array.map (fun x -> Complex(x, 0.0))

                    let timeDomainEnergy = input |> Array.sumBy (fun x -> x * x)

                    FFT.forwardInPlace data |> ignore

                    let freqDomainEnergy =
                        data
                        |> Array.sumBy (fun c -> c.Magnitude * c.Magnitude)
                        |> fun x -> x / float data.Length

                    Expect.floatClose
                        Accuracy.low
                        timeDomainEnergy
                        freqDomainEnergy
                        "Energy should be conserved (Parseval's theorem)"

                testCase "energy is conserved (non-power-of-2)"
                <| fun () ->
                    let input =
                        [| 1.0
                           3.0
                           2.0
                           4.0
                           2.0 |]
                    let data = input |> Array.map (fun x -> Complex(x, 0.0))

                    let timeDomainEnergy = input |> Array.sumBy (fun x -> x * x)

                    FFT.forwardInPlace data |> ignore

                    let freqDomainEnergy =
                        data
                        |> Array.sumBy (fun c -> c.Magnitude * c.Magnitude)
                        |> fun x -> x / float data.Length

                    Expect.floatClose
                        Accuracy.low
                        timeDomainEnergy
                        freqDomainEnergy
                        "Energy should be conserved for non-power-of-2" ]

          testList
              "scaling functions"
              [ testCase "asymmetricScalingInPlace does nothing"
                <| fun () ->
                    let input =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let original = Array.copy input

                    let result = FFT.asymmetricScalingInPlace input

                    Expect.equal result.Length original.Length "Length should be unchanged"
                    for i in 0 .. input.Length - 1 do
                        Expect.equal input.[i] original.[i] "Asymmetric scaling should not modify data"

                testCase "symmetricScalingInPlace scales by 1/sqrt(N)"
                <| fun () ->
                    let input =
                        [| 4.0
                           8.0
                           12.0
                           16.0 |]
                    let n = input.Length
                    let scaleFactor = 1.0 / sqrt (float n)

                    let result = FFT.symmetricScalingInPlace input

                    for i in 0 .. n - 1 do
                        let expected =
                            [| 4.0
                               8.0
                               12.0
                               16.0 |].[i]
                            * scaleFactor
                        Expect.floatClose
                            Accuracy.high
                            input.[i]
                            expected
                            $"Symmetric scaling should scale by 1/sqrt(N) at index {i}"

                testCase "inverseAsymmetricScalingInPlace scales by 1/sqrt(N)"
                <| fun () ->
                    let input =
                        [| 10.0
                           20.0
                           30.0
                           40.0
                           50.0 |]
                    let n = input.Length
                    let scaleFactor = 1.0 / sqrt (float n)

                    FFT.inverseAsymmetricScalingInPlace input |> ignore

                    for i in 0 .. n - 1 do
                        let expected =
                            [| 10.0
                               20.0
                               30.0
                               40.0
                               50.0 |].[i]
                            * scaleFactor
                        Expect.floatClose
                            Accuracy.high
                            input.[i]
                            expected
                            "Inverse asymmetric scaling should scale by 1/sqrt(N)"

                testCase "inverseSymmetricScalingInPlace scales by 1/N"
                <| fun () ->
                    let input =
                        [| 8.0
                           16.0
                           24.0
                           32.0 |]
                    let n = input.Length
                    let scaleFactor = 1.0 / float n

                    FFT.inverseSymmetricScalingInPlace input |> ignore

                    for i in 0 .. n - 1 do
                        let expected =
                            [| 8.0
                               16.0
                               24.0
                               32.0 |].[i]
                            * scaleFactor
                        Expect.floatClose
                            Accuracy.high
                            input.[i]
                            expected
                            "Inverse symmetric scaling should scale by 1/N"

                testCase "scaling functions return the array"
                <| fun () ->
                    let input =
                        [| 1.0
                           2.0
                           3.0 |]
                    let result1 = FFT.asymmetricScalingInPlace input
                    Expect.equal (result1 = input) true "Should return same array reference"

                    let input2 =
                        [| 1.0
                           2.0
                           3.0 |]
                    let result2 = FFT.symmetricScalingInPlace input2
                    Expect.equal (result2 = input2) true "Should return same array reference" ]

          testList
              "edge cases"
              [ testCase "single element array"
                <| fun () ->
                    let data = [| Complex(5.0, 0.0) |]
                    let original = Array.copy data
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    Expect.floatClose
                        Accuracy.high
                        (data.[0].Real / float n)
                        original.[0].Real
                        "Single element should be unchanged after round trip"

                testCase "two element array (power-of-2)"
                <| fun () ->
                    let data =
                        [| Complex(3.0, 0.0)
                           Complex(7.0, 0.0) |]
                    let original = Array.copy data
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..1 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Two element FFT should work"

                testCase "complex input with imaginary parts"
                <| fun () ->
                    let data =
                        [| Complex(1.0, 2.0)
                           Complex(3.0, 4.0)
                           Complex(5.0, 6.0)
                           Complex(7.0, 8.0) |]
                    let original = Array.copy data
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..3 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Real part should be preserved with complex input"
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Imaginary / float n)
                            original.[i].Imaginary
                            "Imaginary part should be preserved with complex input"

                testCase "zero array"
                <| fun () ->
                    let data = Array.create 8 Complex.Zero

                    FFT.forwardInPlace data |> ignore

                    for i in 0..7 do
                        Expect.floatClose Accuracy.high data.[i].Magnitude 0.0 "FFT of zeros should be zeros"

                testCase "large power-of-2 (32 elements)"
                <| fun () ->
                    let original = [| for i in 0..31 -> Complex(sin (float i * 0.5), 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..31 do
                        Expect.floatClose
                            Accuracy.high
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Large array should work correctly"

                testCase "prime number size (13 elements - uses Bluestein)"
                <| fun () ->
                    let original = [| for i in 0..12 -> Complex(float i * 0.3, 0.0) |]
                    let data = Array.copy original
                    let n = data.Length

                    FFT.forwardInPlace data |> ignore
                    FFT.inverseInPlace data |> ignore

                    for i in 0..12 do
                        Expect.floatClose
                            Accuracy.low
                            (data.[i].Real / float n)
                            original.[i].Real
                            "Prime size should work with Bluestein algorithm" ] ]
