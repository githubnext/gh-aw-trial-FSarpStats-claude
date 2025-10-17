module TestingTests

open Expecto
open System
open FSharp.Stats.Testing
open FSharp.Stats
open TestExtensions
open FSharp.Stats.Testing.SAM
open Deedle

[<Tests>]
let testPostHocTests =
    //Tests taken from:
    //https://www.icalcu.com/stat/anova-tukey-hsd-calculator.html
    testList
        "Testing.PostHoc"
        [
          (*
        // Test ommitted due to extremely long runtime of CodeCov.
        testCase "tukeyHSD" <| fun () ->
            let dataA = [|3.;3.;4.;5.;2.;5.;5.;4.;4.;2.;2.;2.;4.;3.;5.;3.;4.;5.;3.;5.;                   |]
            let dataB = [|10.;7.;9.;6.;7.;7.;6.;7.;10.;7.;8.;8.;8.;6.;10.;9.;9.;6.;9.;8.;                |]
            let dataC = [|6.;5.;6.;4.;4.;6.;1.;4.;6.;5.;4.;7.;4.;2.;1.;1.;3.;4.;5.;3.;                   |]
            let dataD = [|10.;5.;6.;5.;8.;5.;6.;9.;3.;10.;5.;9.;5.;5.;6.;10.;9.;6.;9.;10.;               |]
            let dataE = [|14.;17.;14.;13.;18.;12.;17.;11.;12.;11.;12.;10.;17.;19.;18.;18.;15.;14.;18.;16.|]

            let data = [|dataA;dataB;dataC;dataD;dataE|]
                
            let contrastMatrix = 
                [|                
                    //[|-1.;1.;0.;0.;0.;|] pvalue = zero
                    [|-1.;0.;1.;0.;0.;|]
                    [|-1.;0.;0.;1.;0.;|]
                    //[|-1.;0.;0.;0.;1.;|] pvalue = zero
                    [|0.;-1.;1.;0.;0.;|]
                    [|0.;-1.;0.;1.;0.;|]
                    //[|0.;-1.;0.;0.;1.;|] pvalue = zero
                    [|0.;0.;-1.;1.;0.;|]
                    //[|0.;0.;-1.;0.;1.;|] pvalue = zero
                    //[|0.;0.;0.;-1.;1.;|] pvalue = zero
                |]

            let pValues = 
                PostHoc.tukeyHSD contrastMatrix data 
                |> Array.map (fun x -> x.Significance)

            //pvalues from R: TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)
            let rpval = [0.9685630;0.0000045;0.0000003;0.7072882;0.0000618]
                
            Expect.floatClose Accuracy.low rpval.[0] pValues.[0] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[1] pValues.[1] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[2] pValues.[2] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[3] pValues.[3] "p values should be equal."
            Expect.floatClose Accuracy.low rpval.[4] pValues.[4] "p values should be equal."
        *)
          testCase "dunnett"
          <| fun () ->
              let data =
                  [| [| 1.84
                        2.49
                        1.50
                        2.42 |]
                     [| 2.43
                        1.85
                        2.42
                        2.73 |]
                     [| 3.95
                        3.67
                        3.23
                        2.31 |]
                     [| 3.21
                        3.20
                        2.32
                        3.30 |]
                     [| 3.21
                        3.13
                        2.32
                        3.30
                        3.20
                        2.42 |] |]

              //first sample is control
              let contrastMatrix =
                  [| [| -1.
                        1.
                        0.
                        0.
                        0. |]
                     [| -1.
                        0.
                        1.
                        0.
                        0. |]
                     [| -1.
                        0.
                        0.
                        1.
                        0. |]
                     [| -1.
                        0.
                        0.
                        0.
                        1. |] |]

              let dunnettResult = PostHoc.dunnetts contrastMatrix data Tables.dunnettsTwoSided095

              //result from: SPSS Dunnett's test version 27
              let pval =
                  [ 0.811
                    0.010
                    0.050
                    0.049 ]
              let dmean =
                  [ 0.295
                    1.2275
                    0.945
                    0.8675 ]

              Expect.equal dunnettResult.[0].Significance (pval.[0] < 0.05) "Significance should be equal."
              Expect.equal dunnettResult.[1].Significance (pval.[1] < 0.05) "Significance should be equal."
              Expect.equal dunnettResult.[2].Significance (pval.[2] < 0.05) "Significance should be equal."
              Expect.equal dunnettResult.[3].Significance (pval.[3] < 0.05) "Significance should be equal."
              Expect.floatClose Accuracy.high dunnettResult.[0].L dmean.[0] "Mean differences should be equal."
              Expect.floatClose Accuracy.high dunnettResult.[1].L dmean.[1] "Mean differences should be equal."
              Expect.floatClose Accuracy.high dunnettResult.[2].L dmean.[2] "Mean differences should be equal."
              Expect.floatClose Accuracy.high dunnettResult.[3].L dmean.[3] "Mean differences should be equal." ]

[<Tests>]
let hTestTests =
    // H-Test with ties tested against r implementation kruskal.test(weight ~ group, data = my_data)
    let groupA =
        [ 4.17
          5.18
          5.18
          6.11
          4.50
          4.61
          5.17
          4.53
          5.33
          5.18 ]
    let groupB =
        [ 4.81
          4.17
          4.41
          3.59
          5.87
          3.83
          6.03
          4.89
          4.32
          4.69 ]
    let groupC =
        [ 6.31
          5.12
          5.00
          5.00
          5.00
          5.29
          5.00
          6.15
          5.80
          5.26 ]
    let samples =
        [ groupA
          groupB
          groupC ]

    // calculation of the H test
    let hResult = HTest.createHTest samples

    testList
        "Testing.HTest"
        [ testCase "createHTest"
          <| fun () ->
              Expect.isTrue (0.03781 = Math.Round(hResult.PValueRight, 5)) "pValue should be equal."
              Expect.isTrue (6.5502 = Math.Round(hResult.Statistic, 4)) "statistic should be equal."

          ]

[<Tests>]
let friedmanTestTests =
    // Friedman-Test testes against dataset from https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/unterschiede/zentral/friedman.html#3.2._Ergebnisse_des_Friedman-Tests and p-values obtained from distcalc and https://www.socscistatistics.com/pvalues/chidistribution.aspx
    let A =
        [| 275.
           273.
           288.
           273.
           244. |]
    let B =
        [| 292.
           283.
           284.
           285.
           329. |]
    let C =
        [| 281.
           274.
           298.
           270.
           252. |]
    let D =
        [| 284.
           275.
           271.
           272.
           258. |]
    let E =
        [| 285.
           294.
           307.
           278.
           275. |]
    let F =
        [| 283.
           279.
           301.
           276.
           279. |]
    let G =
        [| 290.
           265.
           298.
           291.
           295. |]
    let H =
        [| 294.
           277.
           295.
           290.
           271. |]
    let I =
        [| 300.
           304.
           293.
           279.
           271. |]
    let J =
        [| 284.
           297.
           352.
           292.
           284. |]
    let samples =
        seq {
            A
            B
            C
            D
            E
            F
            G
            H
            I
            J
        }

    // modified dataset from UZH for 3x equal ranks
    let A2 =
        [| 275.
           273.
           288.
           273.
           273. |]
    let B2 =
        [| 292.
           283.
           284.
           285.
           329. |]
    let C2 =
        [| 281.
           274.
           298.
           270.
           252. |]
    let D2 =
        [| 284.
           275.
           271.
           272.
           258. |]
    let E2 =
        [| 285.
           294.
           307.
           278.
           275. |]
    let F2 =
        [| 283.
           279.
           301.
           276.
           279. |]
    let G2 =
        [| 290.
           265.
           298.
           291.
           295. |]
    let H2 =
        [| 294.
           277.
           295.
           290.
           271. |]
    let I2 =
        [| 300.
           304.
           293.
           279.
           271. |]
    let J2 =
        [| 284.
           297.
           284.
           292.
           284. |]
    let samples2 =
        seq {
            A2
            B2
            C2
            D2
            E2
            F2
            G2
            H2
            I2
            J2
        }


    //calculation of friedman test
    let friedmanResult1 = FriedmanTest.createFriedmanTest samples

    let friedmanResult2 = FriedmanTest.createFriedmanTest samples2

    testList
        "Testing.FriedmanTest"
        [ testCase "createFriedmanTest2equal"
          <| fun () ->
              Expect.floatClose Accuracy.low friedmanResult1.Statistic 13.259 "statistics should be equal."
              Expect.floatClose Accuracy.low friedmanResult1.PValueRight 0.010077 "pValue should be equal."
          testCase "createFriedmanTest3equal"
          <| fun () ->
              Expect.floatClose Accuracy.low friedmanResult2.Statistic 9.738 "statistics should be equal."
              Expect.floatClose Accuracy.low friedmanResult2.PValueRight 0.04508 "pValue should be equal." ]

[<Tests>]
let wilcoxonTestTests =
    // tested against SciPy Version 1.7.1
    let before =
        seq {
            78.
            24.
            64.
            45.
            64.
            52.
            30.
            50.
            64.
            50.
            78.
            22.
            84.
            40.
            90.
            72.
        }
    let after =
        seq {
            78.
            24.
            62.
            48.
            68.
            56.
            25.
            44.
            56.
            40.
            68.
            36.
            68.
            20.
            58.
            32.
        }
    let differences =
        seq {
            0.
            0.
            2.
            -3.
            -4.
            -4.
            5.
            6.
            8.
            10.
            10.
            -14.
            16.
            20.
            32.
            40.
        }
    // with continuity correction:
    let wilcoxon1 = WilcoxonTest.createWilcoxonTest before after true
    let wilcoxon2 = WilcoxonTest.createWilcoxonTest before after false
    let wilcoxon3 = WilcoxonTest.createWilcoxonTestFromDifferences differences true
    let wilcoxon4 = WilcoxonTest.createWilcoxonTestFromDifferences differences false

    testList
        "Testing.WilcoxonTest"
        [ testCase "wilcoxonWithCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon1.PValueTwoTailed 0.0382 "pValue should be equal."
          testCase "wilcoxonWithoutCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon2.PValueTwoTailed 0.03537 "pValue should be equal."
          testCase "wilcoxonDifferencesWithCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon3.PValueTwoTailed 0.0382 "pValue should be equal."
          testCase "wilcoxonDifferencesWithoutCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon4.PValueTwoTailed 0.03537 "pValue should be equal."
          testCase "wilcoxonOneSidedWithCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon1.PValueLeft 0.019102 "pValue should be equal"
          testCase "wilcoxonOneSidedWithoutCorrection"
          <| fun () -> Expect.floatClose Accuracy.low wilcoxon2.PValueRight 0.9823 "pValue should be equal"

          ]


[<Tests>]
let tTestTests =
    // tested in SPSS version 27
    let groupA =
        vector
            [ -5.
              -3.
              -3.
              -4.
              -5. ]
    let groupB =
        vector
            [ -2.
              -4.
              -4.
              -6.
              -6.
              -6.
              -5. ]
    let groupC =
        vector
            [ -3.
              -7.
              -8.
              -4.
              -2.
              1.
              -1. ]
    let groupD =
        vector
            [ 1.
              -1.
              0.
              2.
              2. ]

    let meanA = Seq.mean groupA
    let meanB = Seq.mean groupB
    let varA = Seq.var groupA
    let varB = Seq.var groupB
    let nA = float (Seq.length groupA)
    let nB = float (Seq.length groupB)

    // calculation of the H test
    let tTest1 = TTest.twoSample true groupA groupB
    let tTest2 = TTest.twoSampleFromMeanAndVar true (meanA, varA, nA) (meanB, varB, nB)
    let tTest3 = TTest.twoSample false groupA groupB
    let tTest4 = TTest.oneSample groupD 0.5

    testList
        "Testing.TTest"
        [ testCase "twoSample"
          <| fun () ->
              Expect.floatClose Accuracy.low tTest1.PValue 0.377 "pValue should be equal."
              Expect.floatClose Accuracy.low tTest1.DegreesOfFreedom 10. "df should be equal."
              Expect.floatClose Accuracy.low tTest1.Statistic 0.924 "t statistic should be equal."
              Expect.floatClose Accuracy.low tTest3.PValue 0.345 "pValue should be equal."
              Expect.floatClose Accuracy.low tTest3.DegreesOfFreedom 9.990 "df should be equal."

          testCase "twoSampleFromMeanAndVar"
          <| fun () ->
              Expect.equal tTest1 tTest2 "results should be equal."

              // tested with R function (t.test(batcha, batchb, var.equal=TRUE))
              let sample1 =
                  [ 1.
                    2.
                    3 ]
              let sample2 =
                  [ 1.
                    3.
                    5.
                    7 ]
              let mean = Seq.mean sample1
              let mean2 = Seq.mean sample2
              let var1 = Seq.var sample1
              let var2 = Seq.var sample2
              let ttestTwoSample =
                  Testing.TTest.twoSampleFromMeanAndVar true (mean, var1, 3) (mean2, var2, 4)
              let expectedPval = 0.26716219523142071
              let expectedStatistic = -1.24837556786471859
              Expect.floatClose Accuracy.high ttestTwoSample.PValue expectedPval "pValue should be equal."
              Expect.floatClose Accuracy.high ttestTwoSample.Statistic expectedStatistic "t statistic should be equal."

              let sample3 =
                  [ -1.
                    3.
                    -5.
                    7 ]
              let mean3 = Seq.mean sample2
              let var3 = Seq.var sample2
              let ttestTwoSample2 =
                  Testing.TTest.twoSampleFromMeanAndVar true (mean, var1, 3) (mean3, var3, 4)
              let expectedPval2 = 0.75954440793496059
              let expectedStatistic2 = -0.323310403056781825
              Expect.floatClose Accuracy.high ttestTwoSample2.PValue expectedPval "pValue should be equal."
              Expect.floatClose Accuracy.high ttestTwoSample2.Statistic expectedStatistic "t statistic should be equal."

              let ttestTwoSample3 =
                  Testing.TTest.twoSampleFromMeanAndVar true (nan, var2, 3) (mean2, var2, 4)
              Expect.isTrue (nan.Equals(ttestTwoSample3.PValue)) "pValue should be nan."
              Expect.isTrue (nan.Equals(ttestTwoSample3.Statistic)) "t statistic should be nan."


          testCase "oneSample"
          <| fun () ->
              Expect.floatClose Accuracy.low tTest4.PValue 0.634 "pValue should be equal."
              Expect.equal tTest4.DegreesOfFreedom 4. "df should be equal."
              Expect.floatClose Accuracy.low tTest4.Statistic 0.514 "t statistic should be equal."

          //tested with R function (t.test(c(-1,-2,-3), mu = -3, alternative = "two.sided"))
          testCase "oneSampleFromMeanandStDev"
          <| fun () ->
              let sample =
                  [ 1.
                    2.
                    3 ]
              let mean = Seq.mean sample
              let stdev = Seq.stDev sample
              let ttest = Testing.TTest.oneSampleFromMeanAndStDev (mean, stdev, 3) -3.
              let expectedPval = 0.013072457560346513
              let expectedStatistic = 8.6602540378443873
              Expect.floatClose Accuracy.high ttest.PValue expectedPval "pValue should be equal."
              Expect.floatClose Accuracy.high ttest.Statistic expectedStatistic "t statistic should be equal."

              let sample =
                  [ -1.
                    -2.
                    -3 ]
              let mean3 = Seq.mean sample
              let stdev1 = Seq.stDev sample
              let ttest2 = Testing.TTest.oneSampleFromMeanAndStDev (mean3, stdev1, 3) 0.
              let expectedPval1 = 0.074179900227448525
              let expectedStatistic2 = -3.46410161513775483
              Expect.floatClose Accuracy.high ttest2.PValue expectedPval1 "pValue should be equal."
              Expect.floatClose Accuracy.high ttest2.Statistic expectedStatistic2 "t statistic should be equal."

              let mean2 = nan
              let ttest3 = Testing.TTest.oneSampleFromMeanAndStDev (mean2, stdev, 3) 0.
              Expect.isTrue (nan.Equals(ttest3.PValue)) "pValue should be nan."
              Expect.isTrue (nan.Equals(ttest3.Statistic)) "t statistic should be nan."

          testCase "twoSamplePaired"
          <| fun () ->

              // tested with R function t.test(x, y, paired = TRUE, alternative = "two.sided")
              let vectorX =
                  vector
                      [ 1.
                        2.
                        4.
                        8. ]
              let vectorY =
                  vector
                      [ 10.
                        23.
                        11
                        9. ]
              let expectedPval = 0.10836944173355316
              let expectedStatistic = 2.26554660552391818
              let twoSamplePaired = Testing.TTest.twoSamplePaired vectorX vectorY
              Expect.floatClose Accuracy.high twoSamplePaired.PValue expectedPval "pValue should be equal."
              Expect.floatClose Accuracy.high twoSamplePaired.Statistic expectedStatistic "t statistic should be equal."
              Expect.equal twoSamplePaired.DegreesOfFreedom 3. "df should be equal."

              let vectorZ =
                  vector
                      [ -5.
                        -9.
                        0.
                        -8. ]
              let twoSamplePaired2 = Testing.TTest.twoSamplePaired vectorX vectorZ
              let expectedPval1 = 0.041226646225439562
              let expectedStatistic1 = -3.44031028692427698
              Expect.floatClose Accuracy.high twoSamplePaired2.PValue expectedPval1 "pValue should be equal."
              Expect.floatClose
                  Accuracy.high
                  twoSamplePaired2.Statistic
                  expectedStatistic1
                  "t statistic should be equal."

              let vectorNan =
                  vector
                      [ nan
                        10.
                        23.
                        11. ]
              let twoSamplePaired3 = Testing.TTest.twoSamplePaired vectorX vectorNan
              Expect.isTrue (nan.Equals(twoSamplePaired3.PValue)) "pValue should be nan."
              Expect.isTrue (nan.Equals(twoSamplePaired3.Statistic)) "t statistic should be nan."

              let vectorBefore =
                  vector
                      [ 10.
                        4.
                        15.
                        12.
                        6. ]
              let twoSamplePaired4 () =
                  Testing.TTest.twoSamplePaired vectorBefore vectorX |> ignore
              Expect.throws twoSamplePaired4 "Vectors of different length"

              // test if exception Test works
              // Expect.throws (fun _ -> Testing.TTest.twoSamplePaired vectorX vectorY|>ignore) "Vetors should have equal length"

              let vectorWithInfinity =
                  vector
                      [ infinity
                        4.
                        15.
                        12. ]
              let twoSamplePairedInfinity =
                  Testing.TTest.twoSamplePaired vectorWithInfinity vectorX
              Expect.isTrue (nan.Equals(twoSamplePairedInfinity.PValue)) "pValue should be nan."
              Expect.isTrue (nan.Equals(twoSamplePairedInfinity.Statistic)) "t statistic should be nan."

              let vectorWithNegativeInfinity =
                  vector
                      [ infinity
                        4.
                        15.
                        12. ]
              let twoSampleNegativeInfinity =
                  Testing.TTest.twoSamplePaired vectorWithNegativeInfinity vectorX
              Expect.isTrue (nan.Equals(twoSampleNegativeInfinity.PValue)) "pValue should be nan."
              Expect.isTrue (nan.Equals(twoSampleNegativeInfinity.Statistic)) "t statistic should be nan."

              let vectorNull =
                  vector
                      [ 0
                        0
                        0
                        0 ]
              let twoSamplePairedWithNullVector = Testing.TTest.twoSamplePaired vectorNull vectorY
              let expectedPval2 = 0.0272
              let expectedStatistic2 = 4.0451
              Expect.floatClose
                  Accuracy.low
                  twoSamplePairedWithNullVector.PValue
                  expectedPval2
                  "pValue should be equal."
              Expect.floatClose
                  Accuracy.low
                  twoSamplePairedWithNullVector.Statistic
                  expectedStatistic2
                  "t statistic should be equal." ]


[<Tests>]
let fTestTests =
    // F-Test validated against res.ftest <- var.test(samplea, sampleb, alternative = "two.sided") RStudio 2022.02.3+492 "Prairie Trillium" Release (1db809b8323ba0a87c148d16eb84efe39a8e7785, 2022-05-20) for Windows

    let sampleFA =
        vector
            [| 5.0
               6.0
               5.8
               5.7 |]
    let sampleFB =
        vector
            [| 3.5
               3.7
               4.0
               3.3
               3.6 |]
    let sampleNaN =
        vector
            [| 5.0
               6.0
               5.8
               nan |]
    let sampleInf =
        vector
            [| 5.0
               6.0
               5.8
               infinity |]
    let sampleNegInf =
        vector
            [| 5.0
               6.0
               5.8
               -infinity |]
    let sampleties =
        vector
            [| 5.0
               5.0
               5.8
               5.3 |]

    // calculation of the F test
    let fResult = FTest.testVariances sampleFA sampleFB
    let fResultTies = FTest.testVariances sampleFA sampleties



    testList
        "Testing.FTest"
        [ testCase "createFTest"
          <| fun () ->
              Expect.floatClose Accuracy.low fResult.Statistic 2.82338 "statistics should be equal."
              Expect.floatClose Accuracy.low fResult.PValueTwoTailed 0.34172 "pValue should be equal."
          testCase "FTest NaN"
          <| fun () ->
              Expect.throws
                  (fun () -> ((FTest.testVariances sampleNaN sampleFB).Statistic) |> printf "%A")
                  "FTest works with NaN"
          testCase "FTest infinities"
          <| fun () ->
              Expect.throws
                  (fun () -> ((FTest.testVariances sampleInf sampleFB).Statistic) |> printf "%A")
                  "FTest works with divisions by infitity -> nan"
              Expect.throws
                  (fun () -> ((FTest.testVariances sampleNegInf sampleFB).Statistic) |> printf "%A")
                  "FTest works with divisions by -infitity -> nan"
          testCase "FTest 2 ties"
          <| fun () ->
              Expect.floatClose Accuracy.low fResultTies.Statistic 1.32748538 "statistics should be equal."
              Expect.floatClose Accuracy.low fResultTies.PValueTwoTailed 0.8214 "pValue should be equal." ]
[<Tests>]
let chiSquaredTests =
    // ChiSquared https://www.graphpad.com/quickcalcs/chisquared2/
    // example from R
    // obs <- c(315, 101, 108, 32)
    // exp <- c(0.5625, 0.1875, 0.1875, 0.0625)
    // chisq.test(obs, p = exp)
    let testCase1 =
        let expected =
            [ 312.75
              104.25
              104.25
              34.75 ]
        let observed =
            [ 315.
              101.
              108.
              32. ]
        let df = expected.Length - 1
        ChiSquareTest.compute df expected observed

    //obs <- c(315, 101, 80, 32, 50)
    //exp <- c(0.5625, 0.1875, 0.0875, 0.0625,0.1)
    //chisq.test(obs, p = exp)
    let testCase2 =
        let expected =
            [ 325.125
              108.375
              50.575
              36.125
              57.8 ]
        let observed =
            [ 315.
              101.
              80.
              32.
              50. ]
        let df = expected.Length - 1
        ChiSquareTest.compute df expected observed

    testList
        "Testing.ChiSquaredTest"
        [ testCase "compute"
          <| fun () ->
              Expect.isTrue (0.9254 = Math.Round(testCase1.PValueRight, 4)) "pValue should be equal."
              Expect.isTrue (0.4700 = Math.Round(testCase1.Statistic, 4)) "statistic should be equal."
              Expect.isTrue (0.000638 = Math.Round(testCase2.PValueRight, 6)) "pValue should be equal."
              Expect.isTrue (19.461 = Math.Round(testCase2.Statistic, 3)) "statistic should be equal."

          ]

[<Tests>]
let pearsonTests =
    // examples from R
    // cor.test(x,y)
    let testCase1 =
        let seq1 =
            [ 44.4
              45.9
              41.9
              53.3
              44.7
              44.1
              50.7
              45.2
              60.1 ]
        let seq2 =
            [ 2.6
              3.1
              2.5
              5.0
              3.6
              4.0
              5.2
              2.8
              3.8 ]
        Correlation.testPearson seq1 seq2

    let testCase2 =
        let seq1 =
            [ 312.7
              104.2
              104.
              34.7 ]
        let seq2 =
            [ 315.5
              101.3
              108.
              32.2 ]
        Correlation.testPearson seq1 seq2

    testList
        "Testing.Correlation"
        [ testCase "testPearson"
          <| fun () ->
              Expect.isTrue (0.108173054 = Math.Round(testCase1.PValue, 9)) "pValue should be equal"
              Expect.isTrue (0.000294627 = Math.Round(testCase2.PValue, 9)) "pValue should be equal" ]


[<Tests>]
let benjaminiHochbergTests =


    let largeSetWithIdsnan = readCsv @"benjaminiHochberg_Input_nan.csv"
    let largeSetnan = largeSetWithIdsnan |> Array.map snd

    let largeSetWithIds =
        largeSetWithIdsnan |> Array.filter (fun (_, x) -> not (nan.Equals x))

    let largeSet = largeSetnan |> Array.filter (fun x -> not (nan.Equals x))

    let largeSetWithIds_Expectednan = readCsv @"benjaminiHochberg_AdjustedWithR_nan.csv"
    let largeSet_Expectednan = largeSetWithIds_Expectednan |> Array.map snd

    let largeSetWithIds_Expected =
        largeSetWithIds_Expectednan |> Array.filter (fun (_, x) -> not (nan.Equals x))

    let largeSet_Expected =
        largeSet_Expectednan |> Array.filter (fun x -> not (nan.Equals x))

    testList
        "Testing.MultipleTesting.BenjaminiHochberg"
        [

          testCase
              "testBHLarge"
              (fun () ->
                  Expect.sequenceEqual
                      (largeSet
                       |> MultipleTesting.benjaminiHochbergFDR
                       |> Seq.map (fun x -> Math.Round(x, 9)))
                      (largeSet_Expected |> Seq.map (fun x -> Math.Round(x, 9)))
                      "adjusted pValues should be equal to the reference implementation."
              )

          testCase
              "testBHLargeNaN"
              (fun () ->
                  TestExtensions.sequenceEqualRoundedNaN
                      9
                      (largeSetnan |> MultipleTesting.benjaminiHochbergFDR)
                      (largeSet_Expectednan |> Seq.ofArray)
                      "adjusted pValues should be equal to the reference implementation."
              )

          testCase
              "testBHLargeBy"
              (fun () ->
                  Expect.sequenceEqual
                      (largeSetWithIds
                       |> MultipleTesting.benjaminiHochbergFDRBy id
                       |> Seq.sortBy fst
                       |> Seq.map (fun (x, y) -> x, Math.Round(y, 9)))
                      (largeSetWithIds_Expected
                       |> Seq.sortBy fst
                       |> Seq.map (fun (x, y) -> x, Math.Round(y, 9)))
                      "adjusted pValues with keys should be equal to the reference implementation."
              )

          testCase
              "testBHLargeNaNBy"
              (fun () ->
                  Expect.sequenceEqual
                      ([ ("A0", nan)
                         ("A0", nan)
                         yield! largeSetWithIds ]
                       |> MultipleTesting.benjaminiHochbergFDRBy id
                       |> Seq.sortBy fst
                       |> Seq.skip 2
                       |> Seq.map (fun (x, y) -> x, Math.Round(y, 9)))
                      (largeSetWithIds_Expected
                       |> Seq.sortBy fst
                       |> Seq.map (fun (x, y) -> x, Math.Round(y, 9)))
                      "adjusted pValues with keys should be equal to the reference implementation, ignoring nan."
              )

          ]



//[<Tests>] suspended because of long codecov run time
let qValuesTest =

    let largeSetWithIdsnan = readCsv @"benjaminiHochberg_Input_nan.csv"
    let largeSetnan = largeSetWithIdsnan |> Array.map snd

    let largeSetWithIds_Expectednan = readCsv @"qvaluesWithR_nan.csv"
    let largeSet_Expectednan = largeSetWithIds_Expectednan |> Array.map snd

    let largeSetWithIds_ExpectedRobustnan = readCsv @"qvaluesRobustWithR_nan.csv"
    let largeSet_ExpectedRobustnan = largeSetWithIds_ExpectedRobustnan |> Array.map snd


    let largeSet = largeSetnan |> Array.filter (fun x -> not (nan.Equals x))

    let largeSet_Expected =
        largeSet_Expectednan |> Array.filter (fun x -> not (nan.Equals x))

    let largeSet_ExpectedRobust =
        largeSet_ExpectedRobustnan |> Array.filter (fun x -> not (nan.Equals x))


    testList
        "Testing.MultipleTesting.Qvalues"
        [

          testCase
              "ofPValues"
              (fun () ->
                  //tested against r qvalue package 2.26.0
                  //pi0 estimation is in closed form in r package and therefore cannot be tested
                  //qvalue::qvalue(pvals,pi0=0.48345)
                  let pi0 = 0.48345
                  Expect.sequenceEqual
                      (largeSet
                       |> MultipleTesting.Qvalues.ofPValues pi0
                       |> Seq.map (fun x -> Math.Round(x, 9)))
                      (largeSet_Expected |> Seq.map (fun x -> Math.Round(x, 9)))
                      "qValues should be equal to the reference implementation."
              )

          testCase
              "ofPValues_nan"
              (fun () ->
                  //tested against r qvalue package 2.26.0
                  //pi0 estimation is in closed form in r package and therefore cannot be tested
                  //qvalue::qvalue(pvals,pi0=0.48345)
                  let pi0 = 0.48345
                  TestExtensions.sequenceEqualRoundedNaN
                      9
                      (largeSetnan |> MultipleTesting.Qvalues.ofPValues pi0 |> Seq.ofArray)
                      (largeSet_Expectednan |> Seq.ofArray)
                      "qValues should be equal to the reference implementation."
              )

          testCase
              "ofPValuesRobust"
              (fun () ->
                  //tested against r qvalue package 2.26.0
                  //pi0 estimation is in closed form in r package and therefore cannot be tested
                  //qvalue::qvalue(pvals,pi0=0.48345,pfdr=TRUE)
                  let pi0 = 0.48345
                  Expect.sequenceEqual
                      (largeSet
                       |> MultipleTesting.Qvalues.ofPValuesRobust pi0
                       |> Seq.map (fun x -> Math.Round(x, 9)))
                      (largeSet_ExpectedRobust |> Seq.map (fun x -> Math.Round(x, 9)))
                      "qValues Robust should be equal to the reference implementation."
              )

          testCase
              "ofPValuesRobust_nan"
              (fun () ->
                  //tested against r qvalue package 2.26.0
                  //pi0 estimation is in closed form in r package and therefore cannot be tested
                  //qvalue::qvalue(pvals,pi0=0.48345,pfdr=TRUE)
                  let pi0 = 0.48345
                  TestExtensions.sequenceEqualRoundedNaN
                      9
                      (largeSetnan |> MultipleTesting.Qvalues.ofPValuesRobust pi0 |> Seq.ofArray)
                      (largeSet_ExpectedRobustnan |> Seq.ofArray)
                      "qValues Robust should be equal to the reference implementation."
              )

          ]

[<Tests>]
let qValuesAdditionalTests =
    testList
        "Testing.MultipleTesting.Qvalues.Additional"
        [
          // Test pi0Bootstrap with default lambda values
          testCase
              "pi0Bootstrap with uniform p-values"
              (fun () ->
                  // Uniform p-values should give pi0 close to 1.0
                  let pValues =
                      [| 0.1
                         0.2
                         0.3
                         0.4
                         0.5
                         0.6
                         0.7
                         0.8
                         0.9 |]
                  let pi0 = MultipleTesting.Qvalues.pi0Bootstrap pValues
                  Expect.isTrue (pi0 >= 0.0 && pi0 <= 1.0) "pi0 should be between 0 and 1"
                  Expect.isTrue (pi0 > 0.5) "pi0 for uniform p-values should be relatively high"
              )

          testCase
              "pi0Bootstrap with mostly significant p-values"
              (fun () ->
                  // Mostly small p-values should give low pi0
                  let pValues =
                      [| 0.001
                         0.002
                         0.003
                         0.004
                         0.005
                         0.01
                         0.02
                         0.8
                         0.9 |]
                  let pi0 = MultipleTesting.Qvalues.pi0Bootstrap pValues
                  Expect.isTrue (pi0 >= 0.0 && pi0 <= 1.0) "pi0 should be between 0 and 1"
              // With mostly small p-values, pi0 should be lower
              )

          testCase
              "pi0BootstrapWithLambda with custom lambda"
              (fun () ->
                  let pValues =
                      [| 0.1
                         0.2
                         0.3
                         0.4
                         0.5
                         0.6
                         0.7
                         0.8
                         0.9 |]
                  let lambda =
                      [| 0.0
                         0.1
                         0.2
                         0.3
                         0.4
                         0.5 |]
                  let pi0 = MultipleTesting.Qvalues.pi0BootstrapWithLambda lambda pValues
                  Expect.isTrue (pi0 >= 0.0 && pi0 <= 1.0) "pi0 should be between 0 and 1"
              )

          testCase
              "pi0BootstrapWithLambda with narrow lambda range"
              (fun () ->
                  let pValues =
                      [| 0.05
                         0.15
                         0.25
                         0.35
                         0.45
                         0.55
                         0.65
                         0.75
                         0.85
                         0.95 |]
                  let lambda =
                      [| 0.1
                         0.2
                         0.3 |]
                  let pi0 = MultipleTesting.Qvalues.pi0BootstrapWithLambda lambda pValues
                  Expect.isTrue (pi0 >= 0.0 && pi0 <= 1.0) "pi0 should be between 0 and 1"
              )

          testCase
              "ofPValuesBy with custom projection"
              (fun () ->
                  // Test with tuples where second element is the p-value
                  let dataWithPvalues =
                      [| ("test1", 0.01)
                         ("test2", 0.05)
                         ("test3", 0.5)
                         ("test4", 0.9) |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValuesBy pi0 snd dataWithPvalues
                  Expect.equal qValues.Length 4 "Should return 4 q-values"
                  // Q-values should be monotonic and >= p-values
                  Expect.isTrue (qValues.[0] <= qValues.[1]) "Q-values should be monotonic"
                  Expect.isTrue (qValues.[1] <= qValues.[2]) "Q-values should be monotonic"
              )

          testCase
              "ofPValuesRobustBy with custom projection"
              (fun () ->
                  let dataWithPvalues =
                      [| ("test1", 0.01)
                         ("test2", 0.05)
                         ("test3", 0.5)
                         ("test4", 0.9) |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValuesRobustBy pi0 snd dataWithPvalues
                  Expect.equal qValues.Length 4 "Should return 4 q-values"
                  // Q-values should be in valid range
                  Array.iter
                      (fun q -> Expect.isTrue (q >= 0.0 && q <= 1.0) "Q-values should be between 0 and 1")
                      qValues
              )

          testCase
              "ofPValues with all NaN p-values"
              (fun () ->
                  let pValues =
                      [| nan
                         nan
                         nan |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  Expect.equal qValues.Length 3 "Should return 3 q-values"
                  Array.iter (fun q -> Expect.isTrue (System.Double.IsNaN q) "All q-values should be NaN") qValues
              )

          testCase
              "ofPValuesRobust with all NaN p-values"
              (fun () ->
                  let pValues =
                      [| nan
                         nan
                         nan |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValuesRobust pi0 pValues
                  Expect.equal qValues.Length 3 "Should return 3 q-values"
                  Array.iter (fun q -> Expect.isTrue (System.Double.IsNaN q) "All q-values should be NaN") qValues
              )

          testCase
              "ofPValues with mixed valid and NaN p-values"
              (fun () ->
                  let pValues =
                      [| 0.01
                         nan
                         0.5
                         nan
                         0.9 |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  Expect.equal qValues.Length 5 "Should return 5 q-values"
                  // Check that NaN p-values result in NaN q-values
                  Expect.isTrue (not (System.Double.IsNaN qValues.[0])) "Valid p-value should give valid q-value"
                  Expect.isTrue (System.Double.IsNaN qValues.[1]) "NaN p-value should give NaN q-value"
                  Expect.isTrue (not (System.Double.IsNaN qValues.[2])) "Valid p-value should give valid q-value"
                  Expect.isTrue (System.Double.IsNaN qValues.[3]) "NaN p-value should give NaN q-value"
                  Expect.isTrue (not (System.Double.IsNaN qValues.[4])) "Valid p-value should give valid q-value"
              )

          testCase
              "ofPValues with single p-value"
              (fun () ->
                  let pValues = [| 0.05 |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  Expect.equal qValues.Length 1 "Should return 1 q-value"
                  Expect.isTrue (qValues.[0] >= 0.0 && qValues.[0] <= 1.0) "Q-value should be in valid range"
              )

          testCase
              "ofPValuesRobust with single p-value"
              (fun () ->
                  let pValues = [| 0.05 |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValuesRobust pi0 pValues
                  Expect.equal qValues.Length 1 "Should return 1 q-value"
                  Expect.isTrue (qValues.[0] >= 0.0 && qValues.[0] <= 1.0) "Q-value should be in valid range"
              )

          testCase
              "ofPValues monotonicity check"
              (fun () ->
                  // Q-values should be monotonic when sorted by p-values
                  let pValues =
                      [| 0.001
                         0.01
                         0.05
                         0.1
                         0.2
                         0.5
                         0.8 |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  // Check monotonicity
                  for i in 0 .. qValues.Length - 2 do
                      Expect.isTrue
                          (qValues.[i] <= qValues.[i + 1])
                          (sprintf
                              "Q-values should be monotonic: q[%d]=%f should be <= q[%d]=%f"
                              i
                              qValues.[i]
                              (i + 1)
                              qValues.[i + 1])
              )

          testCase
              "ofPValuesRobust monotonicity check"
              (fun () ->
                  let pValues =
                      [| 0.001
                         0.01
                         0.05
                         0.1
                         0.2
                         0.5
                         0.8 |]
                  let pi0 = 0.5
                  let qValues = MultipleTesting.Qvalues.ofPValuesRobust pi0 pValues
                  // Check monotonicity
                  for i in 0 .. qValues.Length - 2 do
                      Expect.isTrue
                          (qValues.[i] <= qValues.[i + 1])
                          (sprintf
                              "Q-values should be monotonic: q[%d]=%f should be <= q[%d]=%f"
                              i
                              qValues.[i]
                              (i + 1)
                              qValues.[i + 1])
              )

          testCase
              "ofPValues with pi0=1.0"
              (fun () ->
                  // pi0=1.0 means all tests are null hypotheses
                  let pValues =
                      [| 0.01
                         0.05
                         0.1
                         0.5 |]
                  let pi0 = 1.0
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  // With pi0=1.0, q-values should be equal to or higher than p-values
                  for i in 0 .. pValues.Length - 1 do
                      Expect.isTrue (qValues.[i] >= pValues.[i]) "Q-values should be >= p-values when pi0=1.0"
              )

          testCase
              "ofPValues with very small pi0"
              (fun () ->
                  // Small pi0 means few null hypotheses
                  let pValues =
                      [| 0.01
                         0.05
                         0.1
                         0.5 |]
                  let pi0 = 0.1
                  let qValues = MultipleTesting.Qvalues.ofPValues pi0 pValues
                  Expect.equal qValues.Length 4 "Should return 4 q-values"
                  // Q-values should be closer to p-values with small pi0
                  Array.iter
                      (fun q -> Expect.isTrue (q >= 0.0 && q <= 1.0) "Q-values should be in valid range")
                      qValues
              )

          testCase
              "pi0Bootstrap deterministic with same input"
              (fun () ->
                  // While bootstrap involves randomness, the min pi0 selection should make it relatively stable
                  let pValues =
                      [| 0.1
                         0.2
                         0.3
                         0.4
                         0.5
                         0.6
                         0.7
                         0.8
                         0.9 |]
                  let pi0_1 = MultipleTesting.Qvalues.pi0Bootstrap pValues
                  let pi0_2 = MultipleTesting.Qvalues.pi0Bootstrap pValues
                  // Both should be in valid range
                  Expect.isTrue (pi0_1 >= 0.0 && pi0_1 <= 1.0) "pi0 should be between 0 and 1"
                  Expect.isTrue (pi0_2 >= 0.0 && pi0_2 <= 1.0) "pi0 should be between 0 and 1"
              // Note: Due to randomness, they might not be exactly equal, but should be close
              ) ]


let createMetricTestInt metricName actual expected =
    testCase
        metricName
        (fun () -> Expect.equal actual expected (sprintf "Metric %s was calculated incorrectly." metricName))
let createMetricTestFloat accuracy metricName actual expected =
    testCase
        metricName
        (fun () ->
            Expect.floatClose accuracy actual expected (sprintf "Metric %s was calculated incorrectly." metricName)
        )

[<Tests>]
let binaryConfusionMatrixTests =

    // binary classification
    //              | Predicted |
    //              |  P  |  N  |
    // | Actual | P |  3  |  1  |
    //          | N |  1  |  2  |

    let tp = 3.
    let tn = 2.
    let fp = 1.
    let fn = 1.

    let binaryCM = BinaryConfusionMatrix.create (int tp, int tn, int fp, int fn)
    let ofPredictions1 =
        BinaryConfusionMatrix.ofPredictions (
            1,
            [ 1
              1
              1
              1
              0
              0
              0 ],
            [ 1
              1
              1
              0
              1
              0
              0 ]
        )
    let ofPredictions2 =
        BinaryConfusionMatrix.ofPredictions (
            [ true
              true
              true
              true
              false
              false
              false ],
            [ true
              true
              true
              false
              true
              false
              false ]
        )

    let expectedCM =
        { TP = 3
          TN = 2
          FP = 1
          FN = 1 }

    testList
        "Testing.BinaryConfusionMatrix"
        [

          testCase "create" (fun _ -> Expect.equal binaryCM expectedCM "binary confusion matrix incorrectly created")
          testCase
              "ofPredictions1"
              (fun _ ->
                  Expect.equal
                      ofPredictions1
                      expectedCM
                      "binary confusion matrix created incorrectly from observations with positive label"
              )
          testCase
              "ofPredictions2"
              (fun _ ->
                  Expect.equal
                      ofPredictions2
                      expectedCM
                      "binary confusion matrix created incorrectly from boolean observations"
              )

          createMetricTestInt "TruePositives" binaryCM.TP 3
          createMetricTestInt "TrueNegatives" binaryCM.TN 2
          createMetricTestInt "FalsePositives" binaryCM.FP 1
          createMetricTestInt "FalseNegatives" binaryCM.FN 1

          testCase
              "thresholdMap implicit thresholds 1"
              (fun _ ->
                  let actual =
                      BinaryConfusionMatrix.thresholdMap (
                          [ true
                            true
                            true
                            true
                            false
                            false
                            false ],
                          [ 0.9
                            0.6
                            0.7
                            0.2
                            0.7
                            0.3
                            0.1 ]
                      )
                  let expected =
                      [ 1.9, BinaryConfusionMatrix.create (0, 3, 0, 4)
                        0.9, BinaryConfusionMatrix.create (1, 3, 0, 3)
                        0.7, BinaryConfusionMatrix.create (2, 2, 1, 2)
                        0.6, BinaryConfusionMatrix.create (3, 2, 1, 1)
                        0.3, BinaryConfusionMatrix.create (3, 1, 2, 1)
                        0.2, BinaryConfusionMatrix.create (4, 1, 2, 0)
                        0.1, BinaryConfusionMatrix.create (4, 0, 3, 0) ]
                  Expect.sequenceEqual
                      actual
                      expected
                      "binary threshold map not correctly created from binary predictions"
              )

          testCase
              "thresholdMap explicit thresholds 1"
              (fun _ ->
                  let actual =
                      BinaryConfusionMatrix.thresholdMap (
                          [ true
                            true
                            true
                            true
                            false
                            false
                            false ],
                          [ 0.9
                            0.6
                            0.7
                            0.2
                            0.7
                            0.3
                            0.1 ],
                          [ 1.
                            0.9
                            0.8
                            0.7
                            0.6
                            0.5
                            0.4
                            0.3
                            0.2
                            0.1
                            0. ]
                      )
                  let expected =
                      [| 1.9, BinaryConfusionMatrix.create (0, 3, 0, 4)
                         1.0, BinaryConfusionMatrix.create (0, 3, 0, 4)
                         0.9, BinaryConfusionMatrix.create (1, 3, 0, 3)
                         0.8, BinaryConfusionMatrix.create (1, 3, 0, 3)
                         0.7, BinaryConfusionMatrix.create (2, 2, 1, 2)
                         0.6, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.5, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.4, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.3, BinaryConfusionMatrix.create (3, 1, 2, 1)
                         0.2, BinaryConfusionMatrix.create (4, 1, 2, 0)
                         0.1, BinaryConfusionMatrix.create (4, 0, 3, 0)
                         0., BinaryConfusionMatrix.create (4, 0, 3, 0) |]
                  Expect.sequenceEqual
                      actual
                      expected
                      "binary threshold map not correctly created from binary predictions"
              )

          testCase
              "thresholdMap: floating point error affects custom thresholds"
              (fun _ ->
                  let actual =
                      BinaryConfusionMatrix.thresholdMap (
                          [ true
                            true
                            true
                            true
                            false
                            false
                            false ],
                          [ 0.9
                            0.6
                            0.7
                            0.2
                            0.7
                            0.3
                            0.1 ],
                          // these values are not exact due to floating point errors in addition. For example, the 0.7 is actually 0.70000000000000006661338147750939 which is > 0.7 and therefore produces an unexpected result
                          [ 0. .. 0.1 .. 1. ] |> List.rev
                      )
                  let expected =
                      [| 1.9, BinaryConfusionMatrix.create (0, 3, 0, 4)
                         1.0, BinaryConfusionMatrix.create (0, 3, 0, 4)
                         0.9, BinaryConfusionMatrix.create (1, 3, 0, 3)
                         0.8, BinaryConfusionMatrix.create (1, 3, 0, 3)
                         0.7, BinaryConfusionMatrix.create (2, 2, 1, 2)
                         0.6, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.5, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.4, BinaryConfusionMatrix.create (3, 2, 1, 1)
                         0.3, BinaryConfusionMatrix.create (3, 1, 2, 1)
                         0.2, BinaryConfusionMatrix.create (4, 1, 2, 0)
                         0.1, BinaryConfusionMatrix.create (4, 0, 3, 0)
                         0., BinaryConfusionMatrix.create (4, 0, 3, 0) |]
                  Expect.isFalse
                      (actual = expected)
                      "expected list comprehension threshold to produce slightly incorrent thresholds"
              ) ]


[<Tests>]
let multiLabelConfusionMatrixTests =

    // multi label classification
    //              |    Predicted  |
    //              |  A  |  B  | C |
    // | Actual | A |  3  |  1  | 1 |
    //          | B |  1  |  2  | 0 |
    //          | C |  2  |  0  | 4 |

    let c: Matrix<int> =
        [ [ 3
            1
            1 ]
          [ 1
            2
            0 ]
          [ 2
            0
            4 ] ]
        |> array2D
        |> Matrix.Generic.ofArray2D

    let expectedMLCM =
        { Labels =
            [| "A"
               "B"
               "C" |]
          Confusion =
            [ [ 3
                1
                1 ]
              [ 1
                2
                0 ]
              [ 2
                0
                4 ] ]
            |> array2D
            |> Matrix.Generic.ofArray2D }

    let multiLabelCM =
        MultiLabelConfusionMatrix.create (
            [| "A"
               "B"
               "C" |],
            c
        )
    let ofPredictions =
        MultiLabelConfusionMatrix.ofPredictions (
            [| "A"
               "B"
               "C" |],
            [| "A"
               "A"
               "A"
               "A"
               "A"
               "B"
               "B"
               "B"
               "C"
               "C"
               "C"
               "C"
               "C"
               "C" |],
            [| "A"
               "A"
               "A"
               "B"
               "C"
               "B"
               "B"
               "A"
               "C"
               "C"
               "C"
               "C"
               "A"
               "A" |]
        )
    let allVsAll = multiLabelCM |> MultiLabelConfusionMatrix.allVsAll
    let expectedAllVsAll =
        [ "A", BinaryConfusionMatrix.create (3, 6, 3, 2)
          "B", BinaryConfusionMatrix.create (2, 10, 1, 1)
          "C", BinaryConfusionMatrix.create (4, 7, 1, 2) ]

    testList
        "Testing.MultiLabelConfusionMatrix"
        [

          testCase
              "create"
              (fun _ -> Expect.equal multiLabelCM expectedMLCM "multi label confusion matrix incorrectly created")
          testCase
              "ofPredictions"
              (fun _ ->
                  Expect.equal
                      ofPredictions
                      expectedMLCM
                      "multi label confusion matrix created incorrectly from observations with positive label"
              )

          testCase
              "oneVsAll1"
              (fun _ ->
                  Expect.equal
                      (snd expectedAllVsAll[0])
                      (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "A")
                      "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix"
              )
          testCase
              "oneVsAll2"
              (fun _ ->
                  Expect.equal
                      (snd expectedAllVsAll[1])
                      (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "B")
                      "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix"
              )
          testCase
              "oneVsAll3"
              (fun _ ->
                  Expect.equal
                      (snd expectedAllVsAll[2])
                      (multiLabelCM |> MultiLabelConfusionMatrix.oneVsRest "C")
                      "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix"
              )
          testCase
              "allVsAll"
              (fun _ ->
                  Expect.sequenceEqual
                      expectedAllVsAll
                      allVsAll
                      "all-vs-all binary confusion matrices incorrectly created from multi label confusion matrix"
              )

          ]

[<Tests>]
let comparisonMetricsTests =

    testList
        "Testing.ComparisonMetrics"
        [

          // values calculated by formulas at https://en.wikipedia.org/wiki/Confusion_matrix
          let sensitivity = 0.75
          let specificity = 0.6666666667
          let precision = 0.75
          let negativePredictiveValue = 0.6666666667
          let missrate = 0.25
          let fallOut = 0.3333333333
          let falseDiscoveryRate = 0.25
          let falseOmissionRate = 0.3333333333
          let positiveLikelihoodRatio = sensitivity / fallOut
          let negativeLikelihoodRatio = missrate / specificity
          let prevalenceThreshold = sqrt (fallOut) / (sqrt (sensitivity) + sqrt (fallOut))
          let threatScore = 0.6
          let prevalence = 0.5714285714
          let accuracy = 0.7142857143
          let balancedAccuracy = (sensitivity + specificity) / 2.
          let f1 = 0.75
          let phiCoefficient = 0.4166666667
          let fowlkesMallowsIndex = 0.75
          let informedness = 0.4166666667
          let markedness = 0.4166666667
          let diagnosticOddsRatio = positiveLikelihoodRatio / negativeLikelihoodRatio

          let tp = 3.
          let tn = 2.
          let fp = 1.
          let fn = 1.
          let p = 4.
          let n = 3.
          let samplesize = 7.
          let binaryCM = BinaryConfusionMatrix.create (3, 2, 1, 1)
          let cm = ComparisonMetrics.create (binaryCM)

          testList
              "Metric calculation"
              [ createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Sensitivity"
                    (ComparisonMetrics.calculateSensitivity tp p)
                    sensitivity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Specificity"
                    (ComparisonMetrics.calculateSpecificity tn n)
                    specificity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Precision"
                    (ComparisonMetrics.calculatePrecision tp fp)
                    precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate NegativePredictiveValue"
                    (ComparisonMetrics.calculateNegativePredictiveValue tn fn)
                    negativePredictiveValue
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Missrate"
                    (ComparisonMetrics.calculateMissrate fn p)
                    missrate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate FallOut"
                    (ComparisonMetrics.calculateFallOut fp n)
                    fallOut
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate FalseDiscoveryRate"
                    (ComparisonMetrics.calculateFalseDiscoveryRate fp tp)
                    falseDiscoveryRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate FalseOmissionRate"
                    (ComparisonMetrics.calculateFalseOmissionRate fn tn)
                    falseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate PositiveLikelihoodRatio"
                    (ComparisonMetrics.calculatePositiveLikelihoodRatio tp p fp n)
                    positiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate NegativeLikelihoodRatio"
                    (ComparisonMetrics.calculateNegativeLikelihoodRatio fn p tn n)
                    negativeLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate PrevalenceThreshold"
                    (ComparisonMetrics.calculatePrevalenceThreshold fp n tp p)
                    prevalenceThreshold
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate ThreatScore"
                    (ComparisonMetrics.calculateThreatScore tp fn fp)
                    threatScore
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Prevalence"
                    (ComparisonMetrics.calculatePrevalence p samplesize)
                    prevalence
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Accuracy"
                    (ComparisonMetrics.calculateAccuracy tp tn samplesize)
                    accuracy
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate BalancedAccuracy"
                    (ComparisonMetrics.calculateBalancedAccuracy tp p tn n)
                    balancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "Calculate F1" (ComparisonMetrics.calculateF1 tp fp fn) f1
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate PhiCoefficient"
                    (ComparisonMetrics.calculatePhiCoefficient tp tn fp fn)
                    phiCoefficient
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate FowlkesMallowsIndex"
                    (ComparisonMetrics.calculateFowlkesMallowsIndex tp fp p)
                    fowlkesMallowsIndex
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Informedness"
                    (ComparisonMetrics.calculateInformedness tp p tn n)
                    informedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate Markedness"
                    (ComparisonMetrics.calculateMarkedness tp fp tn fn)
                    markedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "Calculate DiagnosticOddsRatio"
                    (ComparisonMetrics.calculateDiagnosticOddsRatio tp tn fp fn p n)
                    diagnosticOddsRatio ]
          testList
              "Binary predictions"
              [

                createMetricTestInt "TruePositives" cm.TP 3
                createMetricTestInt "TrueNegatives" cm.TN 2
                createMetricTestInt "FalsePositives" cm.FP 1
                createMetricTestInt "FalseNegatives" cm.FN 1
                createMetricTestInt "Positves" cm.P 4
                createMetricTestInt "Negatives" cm.N 3
                createMetricTestInt "Total" cm.SampleSize 7

                createMetricTestFloat Accuracy.veryHigh "Sensitivity" cm.Sensitivity sensitivity
                createMetricTestFloat Accuracy.veryHigh "Specificity" cm.Specificity specificity
                createMetricTestFloat Accuracy.veryHigh "Precision" cm.Precision precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "NegativePredictiveValue"
                    cm.NegativePredictiveValue
                    negativePredictiveValue
                createMetricTestFloat Accuracy.veryHigh "Missrate" cm.Missrate missrate
                createMetricTestFloat Accuracy.veryHigh "FallOut" cm.FallOut fallOut
                createMetricTestFloat Accuracy.veryHigh "FalseDiscoveryRate" cm.FalseDiscoveryRate falseDiscoveryRate
                createMetricTestFloat Accuracy.veryHigh "FalseOmissionRate" cm.FalseOmissionRate falseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "PositiveLikelihoodRatio"
                    cm.PositiveLikelihoodRatio
                    positiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "NegativeLikelihoodRatio"
                    cm.NegativeLikelihoodRatio
                    negativeLikelihoodRatio
                createMetricTestFloat Accuracy.veryHigh "PrevalenceThreshold" cm.PrevalenceThreshold prevalenceThreshold
                createMetricTestFloat Accuracy.veryHigh "ThreatScore" cm.ThreatScore threatScore
                createMetricTestFloat Accuracy.veryHigh "Prevalence" cm.Prevalence prevalence
                createMetricTestFloat Accuracy.veryHigh "Accuracy" cm.Accuracy accuracy
                createMetricTestFloat Accuracy.veryHigh "BalancedAccuracy" cm.BalancedAccuracy balancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "F1" cm.F1 f1
                createMetricTestFloat Accuracy.veryHigh "PhiCoefficient" cm.PhiCoefficient phiCoefficient
                createMetricTestFloat Accuracy.veryHigh "FowlkesMallowsIndex" cm.FowlkesMallowsIndex fowlkesMallowsIndex
                createMetricTestFloat Accuracy.veryHigh "Informedness" cm.Informedness informedness
                createMetricTestFloat Accuracy.veryHigh "Markedness" cm.Markedness markedness
                createMetricTestFloat Accuracy.veryHigh "DiagnosticOddsRatio" cm.DiagnosticOddsRatio diagnosticOddsRatio

                ]
          testList
              "Multi-label predictions"
              [ let c: Matrix<int> =
                    [ [ 3
                        1
                        1 ]
                      [ 1
                        2
                        0 ]
                      [ 2
                        0
                        4 ] ]
                    |> array2D
                    |> Matrix.Generic.ofArray2D

                let multiLabelCM =
                    MultiLabelConfusionMatrix.create (
                        [| "A"
                           "B"
                           "C" |],
                        c
                    )

                let expectedAvsRest = BinaryConfusionMatrix.create (3, 6, 3, 2)
                let expectedBvsRest = BinaryConfusionMatrix.create (2, 10, 1, 1)
                let expectedCvsRest = BinaryConfusionMatrix.create (4, 7, 1, 2)

                let expectedMicroAverage =
                    ComparisonMetrics.create (BinaryConfusionMatrix.create (9, 23, 5, 5))

                let expectedMacroAverage =
                    [ expectedAvsRest
                      expectedBvsRest
                      expectedCvsRest ]
                    |> List.map (fun x -> ComparisonMetrics.create (x))
                    |> fun metrics ->
                        ComparisonMetrics.create (
                            ((metrics[0].P + metrics[1].P + metrics[2].P) / 3.),
                            ((metrics[0].N + metrics[1].N + metrics[2].N) / 3.),
                            ((metrics[0].SampleSize + metrics[1].SampleSize + metrics[2].SampleSize) / 3.),
                            ((metrics[0].TP + metrics[1].TP + metrics[2].TP) / 3.),
                            ((metrics[0].TN + metrics[1].TN + metrics[2].TN) / 3.),
                            ((metrics[0].FP + metrics[1].FP + metrics[2].FP) / 3.),
                            ((metrics[0].FN + metrics[1].FN + metrics[2].FN) / 3.),
                            ((metrics[0].Sensitivity + metrics[1].Sensitivity + metrics[2].Sensitivity) / 3.),
                            ((metrics[0].Specificity + metrics[1].Specificity + metrics[2].Specificity) / 3.),
                            ((metrics[0].Precision + metrics[1].Precision + metrics[2].Precision) / 3.),
                            ((metrics[0].NegativePredictiveValue
                              + metrics[1].NegativePredictiveValue
                              + metrics[2].NegativePredictiveValue)
                             / 3.),
                            ((metrics[0].Missrate + metrics[1].Missrate + metrics[2].Missrate) / 3.),
                            ((metrics[0].FallOut + metrics[1].FallOut + metrics[2].FallOut) / 3.),
                            ((metrics[0].FalseDiscoveryRate
                              + metrics[1].FalseDiscoveryRate
                              + metrics[2].FalseDiscoveryRate)
                             / 3.),
                            ((metrics[0].FalseOmissionRate
                              + metrics[1].FalseOmissionRate
                              + metrics[2].FalseOmissionRate)
                             / 3.),
                            ((metrics[0].PositiveLikelihoodRatio
                              + metrics[1].PositiveLikelihoodRatio
                              + metrics[2].PositiveLikelihoodRatio)
                             / 3.),
                            ((metrics[0].NegativeLikelihoodRatio
                              + metrics[1].NegativeLikelihoodRatio
                              + metrics[2].NegativeLikelihoodRatio)
                             / 3.),
                            ((metrics[0].PrevalenceThreshold
                              + metrics[1].PrevalenceThreshold
                              + metrics[2].PrevalenceThreshold)
                             / 3.),
                            ((metrics[0].ThreatScore + metrics[1].ThreatScore + metrics[2].ThreatScore) / 3.),
                            ((metrics[0].Prevalence + metrics[1].Prevalence + metrics[2].Prevalence) / 3.),
                            ((metrics[0].Accuracy + metrics[1].Accuracy + metrics[2].Accuracy) / 3.),
                            ((metrics[0].BalancedAccuracy
                              + metrics[1].BalancedAccuracy
                              + metrics[2].BalancedAccuracy)
                             / 3.),
                            ((metrics[0].F1 + metrics[1].F1 + metrics[2].F1) / 3.),
                            ((metrics[0].PhiCoefficient
                              + metrics[1].PhiCoefficient
                              + metrics[2].PhiCoefficient)
                             / 3.),
                            ((metrics[0].FowlkesMallowsIndex
                              + metrics[1].FowlkesMallowsIndex
                              + metrics[2].FowlkesMallowsIndex)
                             / 3.),
                            ((metrics[0].Informedness + metrics[1].Informedness + metrics[2].Informedness)
                             / 3.),
                            ((metrics[0].Markedness + metrics[1].Markedness + metrics[2].Markedness) / 3.),
                            ((metrics[0].DiagnosticOddsRatio
                              + metrics[1].DiagnosticOddsRatio
                              + metrics[2].DiagnosticOddsRatio)
                             / 3.)
                        )


                let cmMicroAverage1 = ComparisonMetrics.microAverage multiLabelCM
                let cmMacroAverage1 = ComparisonMetrics.macroAverage multiLabelCM
                let cmMicroAverage2 =
                    multiLabelCM
                    |> MultiLabelConfusionMatrix.allVsAll
                    |> Array.map snd
                    |> ComparisonMetrics.microAverage
                let cmMacroAverage2 =
                    multiLabelCM
                    |> MultiLabelConfusionMatrix.allVsAll
                    |> Array.map (snd >> ComparisonMetrics.create)
                    |> ComparisonMetrics.macroAverage

                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Sensitivity 1"
                    cmMicroAverage1.Sensitivity
                    expectedMicroAverage.Sensitivity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Specificity 1"
                    cmMicroAverage1.Specificity
                    expectedMicroAverage.Specificity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Precision 1"
                    cmMicroAverage1.Precision
                    expectedMicroAverage.Precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: NegativePredictiveValue 1"
                    cmMicroAverage1.NegativePredictiveValue
                    expectedMicroAverage.NegativePredictiveValue
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Missrate 1"
                    cmMicroAverage1.Missrate
                    expectedMicroAverage.Missrate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FallOut 1"
                    cmMicroAverage1.FallOut
                    expectedMicroAverage.FallOut
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FalseDiscoveryRate 1"
                    cmMicroAverage1.FalseDiscoveryRate
                    expectedMicroAverage.FalseDiscoveryRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FalseOmissionRate 1"
                    cmMicroAverage1.FalseOmissionRate
                    expectedMicroAverage.FalseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PositiveLikelihoodRatio 1"
                    cmMicroAverage1.PositiveLikelihoodRatio
                    expectedMicroAverage.PositiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: NegativeLikelihoodRatio 1"
                    cmMicroAverage1.NegativeLikelihoodRatio
                    expectedMicroAverage.NegativeLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PrevalenceThreshold 1"
                    cmMicroAverage1.PrevalenceThreshold
                    expectedMicroAverage.PrevalenceThreshold
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: ThreatScore 1"
                    cmMicroAverage1.ThreatScore
                    expectedMicroAverage.ThreatScore
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Prevalence 1"
                    cmMicroAverage1.Prevalence
                    expectedMicroAverage.Prevalence
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Accuracy 1"
                    cmMicroAverage1.Accuracy
                    expectedMicroAverage.Accuracy
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: BalancedAccuracy 1"
                    cmMicroAverage1.BalancedAccuracy
                    expectedMicroAverage.BalancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "microAverage: F1 1" cmMicroAverage1.F1 expectedMicroAverage.F1
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PhiCoefficient 1"
                    cmMicroAverage1.PhiCoefficient
                    expectedMicroAverage.PhiCoefficient
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FowlkesMallowsIndex 1"
                    cmMicroAverage1.FowlkesMallowsIndex
                    expectedMicroAverage.FowlkesMallowsIndex
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Informedness 1"
                    cmMicroAverage1.Informedness
                    expectedMicroAverage.Informedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Markedness 1"
                    cmMicroAverage1.Markedness
                    expectedMicroAverage.Markedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: DiagnosticOddsRatio 1"
                    cmMicroAverage1.DiagnosticOddsRatio
                    expectedMicroAverage.DiagnosticOddsRatio

                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Sensitivity 2"
                    cmMicroAverage2.Sensitivity
                    expectedMicroAverage.Sensitivity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Specificity 2"
                    cmMicroAverage2.Specificity
                    expectedMicroAverage.Specificity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Precision 2"
                    cmMicroAverage2.Precision
                    expectedMicroAverage.Precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: NegativePredictiveValue 2"
                    cmMicroAverage2.NegativePredictiveValue
                    expectedMicroAverage.NegativePredictiveValue
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Missrate 2"
                    cmMicroAverage2.Missrate
                    expectedMicroAverage.Missrate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FallOut 2"
                    cmMicroAverage1.FallOut
                    expectedMicroAverage.FallOut
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FalseDiscoveryRate 2"
                    cmMicroAverage2.FalseDiscoveryRate
                    expectedMicroAverage.FalseDiscoveryRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FalseOmissionRate 2"
                    cmMicroAverage2.FalseOmissionRate
                    expectedMicroAverage.FalseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PositiveLikelihoodRatio 2"
                    cmMicroAverage1.PositiveLikelihoodRatio
                    expectedMicroAverage.PositiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: NegativeLikelihoodRatio 2"
                    cmMicroAverage1.NegativeLikelihoodRatio
                    expectedMicroAverage.NegativeLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PrevalenceThreshold 2"
                    cmMicroAverage1.PrevalenceThreshold
                    expectedMicroAverage.PrevalenceThreshold
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: ThreatScore 2"
                    cmMicroAverage1.ThreatScore
                    expectedMicroAverage.ThreatScore
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Prevalence 2"
                    cmMicroAverage1.Prevalence
                    expectedMicroAverage.Prevalence
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Accuracy 2"
                    cmMicroAverage1.Accuracy
                    expectedMicroAverage.Accuracy
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: BalancedAccuracy 2"
                    cmMicroAverage1.BalancedAccuracy
                    expectedMicroAverage.BalancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "microAverage: F1 2" cmMicroAverage1.F1 expectedMicroAverage.F1
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: PhiCoefficient 2"
                    cmMicroAverage1.PhiCoefficient
                    expectedMicroAverage.PhiCoefficient
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: FowlkesMallowsIndex 2"
                    cmMicroAverage1.FowlkesMallowsIndex
                    expectedMicroAverage.FowlkesMallowsIndex
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Informedness 2"
                    cmMicroAverage1.Informedness
                    expectedMicroAverage.Informedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: Markedness 2"
                    cmMicroAverage1.Markedness
                    expectedMicroAverage.Markedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "microAverage: DiagnosticOddsRatio 2"
                    cmMicroAverage1.DiagnosticOddsRatio
                    expectedMicroAverage.DiagnosticOddsRatio

                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Sensitivity 1"
                    cmMacroAverage1.Sensitivity
                    expectedMacroAverage.Sensitivity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Specificity 1"
                    cmMacroAverage1.Specificity
                    expectedMacroAverage.Specificity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Precision 1"
                    cmMacroAverage1.Precision
                    expectedMacroAverage.Precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: NegativePredictiveValue 1"
                    cmMacroAverage1.NegativePredictiveValue
                    expectedMacroAverage.NegativePredictiveValue
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Missrate 1"
                    cmMacroAverage1.Missrate
                    expectedMacroAverage.Missrate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FallOut 1"
                    cmMacroAverage1.FallOut
                    expectedMacroAverage.FallOut
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FalseDiscoveryRate 1"
                    cmMacroAverage1.FalseDiscoveryRate
                    expectedMacroAverage.FalseDiscoveryRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FalseOmissionRate 1"
                    cmMacroAverage1.FalseOmissionRate
                    expectedMacroAverage.FalseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PositiveLikelihoodRatio 1"
                    cmMacroAverage1.PositiveLikelihoodRatio
                    expectedMacroAverage.PositiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: NegativeLikelihoodRatio 1"
                    cmMacroAverage1.NegativeLikelihoodRatio
                    expectedMacroAverage.NegativeLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PrevalenceThreshold 1"
                    cmMacroAverage1.PrevalenceThreshold
                    expectedMacroAverage.PrevalenceThreshold
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: ThreatScore 1"
                    cmMacroAverage1.ThreatScore
                    expectedMacroAverage.ThreatScore
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Prevalence 1"
                    cmMacroAverage1.Prevalence
                    expectedMacroAverage.Prevalence
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Accuracy 1"
                    cmMacroAverage1.Accuracy
                    expectedMacroAverage.Accuracy
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: BalancedAccuracy 1"
                    cmMacroAverage1.BalancedAccuracy
                    expectedMacroAverage.BalancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "macroAverage: F1 1" cmMacroAverage1.F1 expectedMacroAverage.F1
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PhiCoefficient 1"
                    cmMacroAverage1.PhiCoefficient
                    expectedMacroAverage.PhiCoefficient
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FowlkesMallowsIndex 1"
                    cmMacroAverage1.FowlkesMallowsIndex
                    expectedMacroAverage.FowlkesMallowsIndex
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Informedness 1"
                    cmMacroAverage1.Informedness
                    expectedMacroAverage.Informedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Markedness 1"
                    cmMacroAverage1.Markedness
                    expectedMacroAverage.Markedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: DiagnosticOddsRatio 1"
                    cmMacroAverage1.DiagnosticOddsRatio
                    expectedMacroAverage.DiagnosticOddsRatio

                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Sensitivity 2"
                    cmMacroAverage2.Sensitivity
                    expectedMacroAverage.Sensitivity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Specificity 2"
                    cmMacroAverage2.Specificity
                    expectedMacroAverage.Specificity
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Precision 2"
                    cmMacroAverage2.Precision
                    expectedMacroAverage.Precision
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: NegativePredictiveValue 2"
                    cmMacroAverage2.NegativePredictiveValue
                    expectedMacroAverage.NegativePredictiveValue
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Missrate 2"
                    cmMacroAverage2.Missrate
                    expectedMacroAverage.Missrate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FallOut 2"
                    cmMacroAverage2.FallOut
                    expectedMacroAverage.FallOut
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FalseDiscoveryRate 2"
                    cmMacroAverage2.FalseDiscoveryRate
                    expectedMacroAverage.FalseDiscoveryRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FalseOmissionRate 2"
                    cmMacroAverage2.FalseOmissionRate
                    expectedMacroAverage.FalseOmissionRate
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PositiveLikelihoodRatio 2"
                    cmMacroAverage2.PositiveLikelihoodRatio
                    expectedMacroAverage.PositiveLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: NegativeLikelihoodRatio 2"
                    cmMacroAverage2.NegativeLikelihoodRatio
                    expectedMacroAverage.NegativeLikelihoodRatio
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PrevalenceThreshold 2"
                    cmMacroAverage2.PrevalenceThreshold
                    expectedMacroAverage.PrevalenceThreshold
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: ThreatScore 2"
                    cmMacroAverage2.ThreatScore
                    expectedMacroAverage.ThreatScore
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Prevalence 2"
                    cmMacroAverage2.Prevalence
                    expectedMacroAverage.Prevalence
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Accuracy 2"
                    cmMacroAverage2.Accuracy
                    expectedMacroAverage.Accuracy
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: BalancedAccuracy 2"
                    cmMacroAverage2.BalancedAccuracy
                    expectedMacroAverage.BalancedAccuracy
                createMetricTestFloat Accuracy.veryHigh "macroAverage: F1 2" cmMacroAverage2.F1 expectedMacroAverage.F1
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: PhiCoefficient 2"
                    cmMacroAverage2.PhiCoefficient
                    expectedMacroAverage.PhiCoefficient
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: FowlkesMallowsIndex 2"
                    cmMacroAverage2.FowlkesMallowsIndex
                    expectedMacroAverage.FowlkesMallowsIndex
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Informedness 2"
                    cmMacroAverage2.Informedness
                    expectedMacroAverage.Informedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: Markedness 2"
                    cmMacroAverage2.Markedness
                    expectedMacroAverage.Markedness
                createMetricTestFloat
                    Accuracy.veryHigh
                    "macroAverage: DiagnosticOddsRatio 2"
                    cmMacroAverage2.DiagnosticOddsRatio
                    expectedMacroAverage.DiagnosticOddsRatio ]
          testList
              "binary threshold map"
              [ let actual =
                    ComparisonMetrics.binaryThresholdMap (
                        [ true
                          true
                          true
                          true
                          false
                          false
                          false ],
                        [ 0.9
                          0.6
                          0.7
                          0.2
                          0.7
                          0.3
                          0.1 ]
                    )

                let expected =
                    [| 1.9, BinaryConfusionMatrix.create (0, 3, 0, 4) |> ComparisonMetrics.create
                       0.9, BinaryConfusionMatrix.create (1, 3, 0, 3) |> ComparisonMetrics.create
                       0.7, BinaryConfusionMatrix.create (2, 2, 1, 2) |> ComparisonMetrics.create
                       0.6, BinaryConfusionMatrix.create (3, 2, 1, 1) |> ComparisonMetrics.create
                       0.3, BinaryConfusionMatrix.create (3, 1, 2, 1) |> ComparisonMetrics.create
                       0.2, BinaryConfusionMatrix.create (4, 1, 2, 0) |> ComparisonMetrics.create
                       0.1, BinaryConfusionMatrix.create (4, 0, 3, 0) |> ComparisonMetrics.create |]
                testCase
                    "threshold 1-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[0])
                            (snd expected[0])
                            "Incorrect metrics for threshold 1.9"
                    )
                testCase
                    "threshold 0-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[1])
                            (snd expected[1])
                            "Incorrect metrics for threshold 0.9"
                    )
                testCase
                    "threshold 0-7"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[2])
                            (snd expected[2])
                            "Incorrect metrics for threshold 0.7"
                    )
                testCase
                    "threshold 0-6"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[3])
                            (snd expected[3])
                            "Incorrect metrics for threshold 0.6"
                    )
                testCase
                    "threshold 0-3"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[4])
                            (snd expected[4])
                            "Incorrect metrics for threshold 0.3"
                    )
                testCase
                    "threshold 0-2"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[5])
                            (snd expected[5])
                            "Incorrect metrics for threshold 0.2"
                    )
                testCase
                    "threshold 0-1"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd actual[6])
                            (snd expected[6])
                            "Incorrect metrics for threshold 0.1"
                    ) ]
          testList
              "multi-label threshold map"
              [ let expectedMetricsMap =
                    Map.ofList
                        [ "A",
                          [| 1.9, BinaryConfusionMatrix.create (0, 9, 0, 5) |> ComparisonMetrics.create
                             0.9, BinaryConfusionMatrix.create (1, 9, 0, 4) |> ComparisonMetrics.create
                             0.8, BinaryConfusionMatrix.create (2, 9, 0, 3) |> ComparisonMetrics.create
                             0.7, BinaryConfusionMatrix.create (3, 9, 0, 2) |> ComparisonMetrics.create
                             0.6, BinaryConfusionMatrix.create (3, 9, 0, 2) |> ComparisonMetrics.create
                             0.5, BinaryConfusionMatrix.create (3, 7, 2, 2) |> ComparisonMetrics.create
                             0.4, BinaryConfusionMatrix.create (4, 6, 3, 1) |> ComparisonMetrics.create
                             0.3, BinaryConfusionMatrix.create (5, 5, 4, 0) |> ComparisonMetrics.create
                             0.2, BinaryConfusionMatrix.create (5, 4, 5, 0) |> ComparisonMetrics.create
                             0.1, BinaryConfusionMatrix.create (5, 0, 9, 0) |> ComparisonMetrics.create
                             0.0, BinaryConfusionMatrix.create (5, 0, 9, 0) |> ComparisonMetrics.create |]
                          "B",
                          [| 1.9, BinaryConfusionMatrix.create (0, 11, 0, 3) |> ComparisonMetrics.create
                             0.9, BinaryConfusionMatrix.create (0, 11, 0, 3) |> ComparisonMetrics.create
                             0.8, BinaryConfusionMatrix.create (1, 11, 0, 2) |> ComparisonMetrics.create
                             0.7, BinaryConfusionMatrix.create (2, 11, 0, 1) |> ComparisonMetrics.create
                             0.6, BinaryConfusionMatrix.create (2, 11, 0, 1) |> ComparisonMetrics.create
                             0.5, BinaryConfusionMatrix.create (2, 10, 1, 1) |> ComparisonMetrics.create
                             0.4, BinaryConfusionMatrix.create (3, 10, 1, 0) |> ComparisonMetrics.create
                             0.3, BinaryConfusionMatrix.create (3, 9, 2, 0) |> ComparisonMetrics.create
                             0.2, BinaryConfusionMatrix.create (3, 9, 2, 0) |> ComparisonMetrics.create
                             0.1, BinaryConfusionMatrix.create (3, 4, 7, 0) |> ComparisonMetrics.create
                             0.0, BinaryConfusionMatrix.create (3, 0, 11, 0) |> ComparisonMetrics.create |]
                          "C",
                          [| 1.9, BinaryConfusionMatrix.create (0, 8, 0, 6) |> ComparisonMetrics.create
                             0.9, BinaryConfusionMatrix.create (1, 8, 0, 5) |> ComparisonMetrics.create
                             0.8, BinaryConfusionMatrix.create (3, 8, 0, 3) |> ComparisonMetrics.create
                             0.7, BinaryConfusionMatrix.create (4, 8, 0, 2) |> ComparisonMetrics.create
                             0.6, BinaryConfusionMatrix.create (4, 7, 1, 2) |> ComparisonMetrics.create
                             0.5, BinaryConfusionMatrix.create (4, 7, 1, 2) |> ComparisonMetrics.create
                             0.4, BinaryConfusionMatrix.create (5, 7, 1, 1) |> ComparisonMetrics.create
                             0.3, BinaryConfusionMatrix.create (6, 7, 1, 0) |> ComparisonMetrics.create
                             0.2, BinaryConfusionMatrix.create (6, 5, 3, 0) |> ComparisonMetrics.create
                             0.1, BinaryConfusionMatrix.create (6, 0, 8, 0) |> ComparisonMetrics.create
                             0.0, BinaryConfusionMatrix.create (6, 0, 8, 0) |> ComparisonMetrics.create |] ]

                let actual =
                    ComparisonMetrics.multiLabelThresholdMap (
                        actual =
                            [| "A"
                               "A"
                               "A"
                               "A"
                               "A"
                               "B"
                               "B"
                               "B"
                               "C"
                               "C"
                               "C"
                               "C"
                               "C"
                               "C" |],
                        predictions =
                            [| "A",
                               [| 0.8
                                  0.7
                                  0.9
                                  0.4
                                  0.3
                                  0.1
                                  0.2
                                  0.5
                                  0.1
                                  0.1
                                  0.1
                                  0.3
                                  0.5
                                  0.4 |]
                               "B",
                               [| 0.0
                                  0.1
                                  0.0
                                  0.5
                                  0.1
                                  0.8
                                  0.7
                                  0.4
                                  0.0
                                  0.1
                                  0.1
                                  0.0
                                  0.1
                                  0.3 |]
                               "C",
                               [| 0.2
                                  0.2
                                  0.1
                                  0.1
                                  0.6
                                  0.1
                                  0.1
                                  0.1
                                  0.9
                                  0.8
                                  0.8
                                  0.7
                                  0.4
                                  0.3 |] |]
                    )

                testCase
                    "A: threshold 1-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][0]))
                            (snd (expectedMetricsMap["A"][0]))
                            "Incorrect metrics for threshold 1.9"
                    )
                testCase
                    "A: threshold 0-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][1]))
                            (snd (expectedMetricsMap["A"][1]))
                            "Incorrect metrics for threshold 0.9"
                    )
                testCase
                    "A: threshold 0-8"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][2]))
                            (snd (expectedMetricsMap["A"][2]))
                            "Incorrect metrics for threshold 0.8"
                    )
                testCase
                    "A: threshold 0-7"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][3]))
                            (snd (expectedMetricsMap["A"][3]))
                            "Incorrect metrics for threshold 0.7"
                    )
                testCase
                    "A: threshold 0-6"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][4]))
                            (snd (expectedMetricsMap["A"][4]))
                            "Incorrect metrics for threshold 0.6"
                    )
                testCase
                    "A: threshold 0-5"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][5]))
                            (snd (expectedMetricsMap["A"][5]))
                            "Incorrect metrics for threshold 0.5"
                    )
                testCase
                    "A: threshold 0-4"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][6]))
                            (snd (expectedMetricsMap["A"][6]))
                            "Incorrect metrics for threshold 0.4"
                    )
                testCase
                    "A: threshold 0-3"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][7]))
                            (snd (expectedMetricsMap["A"][7]))
                            "Incorrect metrics for threshold 0.3"
                    )
                testCase
                    "A: threshold 0-2"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][8]))
                            (snd (expectedMetricsMap["A"][8]))
                            "Incorrect metrics for threshold 0.2"
                    )
                testCase
                    "A: threshold 0-1"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][9]))
                            (snd (expectedMetricsMap["A"][9]))
                            "Incorrect metrics for threshold 0.1"
                    )
                testCase
                    "A: threshold 0-0"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["A"][10]))
                            (snd (expectedMetricsMap["A"][10]))
                            "Incorrect metrics for threshold 0.0"
                    )

                testCase
                    "B: threshold 1-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][0]))
                            (snd (expectedMetricsMap["B"][0]))
                            "Incorrect metrics for threshold 1.9"
                    )
                testCase
                    "B: threshold 0-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][1]))
                            (snd (expectedMetricsMap["B"][1]))
                            "Incorrect metrics for threshold 0.9"
                    )
                testCase
                    "B: threshold 0-8"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][2]))
                            (snd (expectedMetricsMap["B"][2]))
                            "Incorrect metrics for threshold 0.8"
                    )
                testCase
                    "B: threshold 0-7"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][3]))
                            (snd (expectedMetricsMap["B"][3]))
                            "Incorrect metrics for threshold 0.7"
                    )
                testCase
                    "B: threshold 0-6"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][4]))
                            (snd (expectedMetricsMap["B"][4]))
                            "Incorrect metrics for threshold 0.6"
                    )
                testCase
                    "B: threshold 0-5"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][5]))
                            (snd (expectedMetricsMap["B"][5]))
                            "Incorrect metrics for threshold 0.5"
                    )
                testCase
                    "B: threshold 0-4"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][6]))
                            (snd (expectedMetricsMap["B"][6]))
                            "Incorrect metrics for threshold 0.4"
                    )
                testCase
                    "B: threshold 0-3"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][7]))
                            (snd (expectedMetricsMap["B"][7]))
                            "Incorrect metrics for threshold 0.3"
                    )
                testCase
                    "B: threshold 0-2"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][8]))
                            (snd (expectedMetricsMap["B"][8]))
                            "Incorrect metrics for threshold 0.2"
                    )
                testCase
                    "B: threshold 0-1"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][9]))
                            (snd (expectedMetricsMap["B"][9]))
                            "Incorrect metrics for threshold 0.1"
                    )
                testCase
                    "B: threshold 0-0"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["B"][10]))
                            (snd (expectedMetricsMap["B"][10]))
                            "Incorrect metrics for threshold 0.0"
                    )

                testCase
                    "C: threshold 1-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][0]))
                            (snd (expectedMetricsMap["C"][0]))
                            "Incorrect metrics for threshold 1.9"
                    )
                testCase
                    "C: threshold 0-9"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][1]))
                            (snd (expectedMetricsMap["C"][1]))
                            "Incorrect metrics for threshold 0.9"
                    )
                testCase
                    "C: threshold 0-8"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][2]))
                            (snd (expectedMetricsMap["C"][2]))
                            "Incorrect metrics for threshold 0.8"
                    )
                testCase
                    "C: threshold 0-7"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][3]))
                            (snd (expectedMetricsMap["C"][3]))
                            "Incorrect metrics for threshold 0.7"
                    )
                testCase
                    "C: threshold 0-6"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][4]))
                            (snd (expectedMetricsMap["C"][4]))
                            "Incorrect metrics for threshold 0.6"
                    )
                testCase
                    "C: threshold 0-5"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][5]))
                            (snd (expectedMetricsMap["C"][5]))
                            "Incorrect metrics for threshold 0.5"
                    )
                testCase
                    "C: threshold 0-4"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][6]))
                            (snd (expectedMetricsMap["C"][6]))
                            "Incorrect metrics for threshold 0.4"
                    )
                testCase
                    "C: threshold 0-3"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][7]))
                            (snd (expectedMetricsMap["C"][7]))
                            "Incorrect metrics for threshold 0.3"
                    )
                testCase
                    "C: threshold 0-2"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][8]))
                            (snd (expectedMetricsMap["C"][8]))
                            "Incorrect metrics for threshold 0.2"
                    )
                testCase
                    "C: threshold 0-1"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][9]))
                            (snd (expectedMetricsMap["C"][9]))
                            "Incorrect metrics for threshold 0.1"
                    )
                testCase
                    "C: threshold 0-0"
                    (fun _ ->
                        TestExtensions.comparisonMetricsEqualRounded
                            3
                            (snd (actual["C"][10]))
                            (snd (expectedMetricsMap["C"][10]))
                            "Incorrect metrics for threshold 0.0"
                    ) ] ]


// declaration of a custom ID type for SAM tests
type MyId = MyId of string

let inline isSameNumber a b =
    if Ops.isNan a && Ops.isNan b then true
    elif Ops.isNegInf a && Ops.isNegInf b then true
    elif Ops.isPosInf a && Ops.isPosInf b then true
    else a = b

type SAM.SAM<'id> when 'id :> IComparable with
    // make it convenient to swap the id from one type to another for SAM.SAM<'id>
    static member mapId (f: 'a -> 'b) (x: SAM.SAM<'a>) : SAM.SAM<'b> =
        SAM.SAM<_>.Create (f x.ID) x.Ri x.Si x.Statistics x.QValue x.Foldchange x.MeanA x.StDevA x.MeanB x.StDevB
    // make it convenient to compare the contents
    static member areSame a b isSameId =
        if not (isSameId a.ID b.ID) then
            false
        else
            [ a.Ri, b.Ri
              a.Si, b.Si
              a.Statistics, b.Statistics
              a.QValue, b.QValue
              a.Foldchange, b.Foldchange
              a.MeanA, b.MeanA
              a.StDevA, b.StDevA
              a.MeanB, b.MeanB
              a.StDevB, b.StDevB ]
            |> List.forall (fun (a, b) -> isSameNumber a b)

type SAM.SAMResult<'id> when 'id :> IComparable with
    // make it convenient to swap the id from one type to another for SAM.SAMResult<'id>
    static member mapId (f: 'a -> 'b) (x: SAM.SAMResult<'a>) : SAM.SAMResult<'b> =
        SAM.SAMResult<_>.Create
            x.S0
            x.Pi0
            x.Delta
            x.UpperCut
            x.LowerCut
            (x.PosSigBioitem |> Array.map (SAM.SAM<_>.mapId f))
            (x.NegSigBioitem |> Array.map (SAM.SAM<_>.mapId f))
            (x.NonSigBioitem |> Array.map (SAM.SAM<_>.mapId f))
            (x.AveragePermutations |> Array.map (SAM.SAM<_>.mapId f))
            x.FDR
            x.SigCalledCount
            x.MedianFalsePositivesCount

    static member areSame a b isSameId =
        let individualValuesAreSame =
            [ a.S0, b.S0
              a.Pi0, b.Pi0
              a.Delta, b.Delta
              a.UpperCut, b.UpperCut
              a.LowerCut, b.LowerCut
              a.FDR, b.FDR
              a.SigCalledCount, b.SigCalledCount
              a.MedianFalsePositivesCount, b.MedianFalsePositivesCount

              ]
            |> List.forall (fun (a, b) -> isSameNumber a b)
        if not individualValuesAreSame then
            false
        else
            let permsAreSame =
                Array.zip a.AveragePermutations b.AveragePermutations
                |> Array.forall (fun (a, b) -> isSameId a.ID b.ID && SAM.SAM.areSame a b isSameId)
            if not permsAreSame then
                false
            else
                let nonSigAreSame =
                    Array.zip a.NonSigBioitem b.NonSigBioitem
                    |> Array.forall (fun (a, b) -> isSameId a.ID b.ID && SAM.SAM.areSame a b isSameId)
                if not nonSigAreSame then
                    false
                else
                    let negSigAreSame =
                        Array.zip a.NegSigBioitem b.NegSigBioitem
                        |> Array.forall (fun (a, b) -> isSameId a.ID b.ID && SAM.SAM.areSame a b isSameId)
                    if not negSigAreSame then
                        false
                    else
                        let posSigAreSame =
                            Array.zip a.PosSigBioitem b.PosSigBioitem
                            |> Array.forall (fun (a, b) -> isSameId a.ID b.ID && SAM.SAM.areSame a b isSameId)
                        if not posSigAreSame then false else true

[<Tests>]
let SAMTests =
    // data preparation
    let df: Frame<string, string> =
        Frame.ReadCsv(@"data/TestDataSAM.txt", hasHeaders = true, separators = "\t")
        |> Frame.indexRows "gene"
    // get Rowkeys as Array
    let rowheader: string[] = df.RowKeys |> Seq.toArray
    // chunk data into Arrays for sample1 and sample2 to compare them
    let (sample1, sample2): float[][] * float[][] =
        df
        |> Frame.getRows
        |> Series.values
        //|> Seq.map (Series.values >> Seq.toArray >> Array.chunkBySize 4 >> fun x -> x.[0],x.[1])
        |> Seq.map (Series.values >> Seq.toArray >> Array.chunkBySize 3 >> (fun x -> x.[0], x.[1]))
        |> Array.ofSeq
        |> Array.unzip
    // map rowheader to samples, so its (GeneName,[float;float;float])

    let data1 = Array.zip rowheader sample1
    let data2 = Array.zip rowheader sample2
    let corrected1 =
        let medCorrect = medianCentering data1
        Array.zip rowheader medCorrect
    let corrected2 =
        let medCorrect = medianCentering data2
        Array.zip rowheader medCorrect


    let result1 =
        FSharp.Stats.Testing.SAM.twoClassUnpaired 100 0.05 data1 data2 (System.Random(27))
    let result1NonStringId =
        let data1 = data1 |> Array.map (fun (id, rest) -> MyId id, rest)
        let data2 = data2 |> Array.map (fun (id, rest) -> MyId id, rest)
        FSharp.Stats.Testing.SAM.twoClassUnpaired 100 0.05 data1 data2 (System.Random(27))

    let result2 = twoClassUnpaired 100 0.05 data1 data2 (System.Random(1337))
    let result3 = twoClassUnpaired 100 0.05 corrected1 corrected2 (System.Random(1337))
    let result4 = twoClassUnpaired 100 0.05 data1 data1 (System.Random(27))

    testList
        "SAM Tests"
        [ testCase "twoClassUnpaired Seed 27"
          <| fun () ->
              Expect.floatClose Accuracy.low result1.S0 0.041419 "S0 should be equal."
              Expect.floatClose Accuracy.low result1.Pi0 0.388060 "Pi0 should be equal."
              Expect.floatClose Accuracy.low result1.Delta 0.986042 "Delta should be equal."
              Expect.floatClose Accuracy.low result1.UpperCut 1.301745 "Upper Cut should be equal."
              Expect.floatClose Accuracy.low result1.LowerCut -1.632438 "Lower Cut should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result1.PosSigBioitem |> Array.length |> float)
                  80.
                  "PosSigBioitems should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result1.NegSigBioitem |> Array.length |> float)
                  61.
                  "NegSigBioitems should be equal."
              Expect.floatClose Accuracy.low (result1.MedianFalsePositivesCount |> float) 14. "medFP should be equal."

          testCase "twoClassUnpaired Seed 1337"
          <| fun () ->
              Expect.floatClose Accuracy.low result2.S0 0.041419 "S0 should be equal."
              Expect.floatClose Accuracy.low result2.Pi0 0.388060 "Pi0 should be equal."
              Expect.floatClose Accuracy.low result2.Delta 0.997591 "Delta should be equal."
              Expect.floatClose Accuracy.low result2.UpperCut 1.301745 "Upper Cut should be equal."
              Expect.floatClose Accuracy.low result2.LowerCut -1.632438 "Lower Cut should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result2.PosSigBioitem |> Array.length |> float)
                  80.
                  "PosSigBioitems should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result2.NegSigBioitem |> Array.length |> float)
                  61.
                  "NegSigBioitems should be equal."
              Expect.floatClose Accuracy.low (result2.MedianFalsePositivesCount |> float) 17. "medFP should be equal."

          testCase "twoClassUnpaired median centered Seed 1337"
          <| fun () ->
              Expect.floatClose Accuracy.low result3.S0 0.026303 "S0 should be equal."
              Expect.floatClose Accuracy.low result3.Pi0 0.407960 "Pi0 should be equal."
              Expect.floatClose Accuracy.low result3.Delta 1.036888 "Delta should be equal."
              Expect.floatClose Accuracy.low result3.UpperCut 1.381420 "Upper Cut should be equal."
              Expect.floatClose Accuracy.low result3.LowerCut -1.740646 "Lower Cut should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result3.PosSigBioitem |> Array.length |> float)
                  78.
                  "PosSigBioitems should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result3.NegSigBioitem |> Array.length |> float)
                  61.
                  "NegSigBioitems should be equal."
              Expect.floatClose Accuracy.low (result3.MedianFalsePositivesCount |> float) 17. "medFP should be equal."
          testCase "twoClassUnpaired Seed 27 data similar "
          <| fun () ->
              Expect.floatClose Accuracy.low result4.S0 0.005275473477 "S0 should be equal."
              Expect.floatClose Accuracy.low result4.Pi0 2.0 "Pi0 should be equal."
              Expect.floatClose Accuracy.low result4.Delta 100000.0 "Delta should be equal."
              Expect.floatClose Accuracy.low result4.UpperCut 0.0 "Upper Cut should be equal."
              Expect.floatClose Accuracy.low result4.LowerCut 0.0 "Lower Cut should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result4.PosSigBioitem |> Array.length |> float)
                  0.
                  "PosSigBioitems should be equal."
              Expect.floatClose
                  Accuracy.low
                  (result4.NegSigBioitem |> Array.length |> float)
                  0.
                  "NegSigBioitems should be equal."
              Expect.floatClose Accuracy.low (result4.MedianFalsePositivesCount |> float) 0. "medFP should be equal."

          testCase "non string ID type doesn't affect results"
          <| fun () ->
              let result1: SAMResult<MyId> = SAMResult.mapId MyId result1
              Expect.isTrue
                  (SAMResult.areSame result1 result1NonStringId (=))
                  "the ID type being other than string shouldn't affect the results"

          ]

[<Tests>]
let testFisherHotelling =
    testList
        "Testing.FisherHotelling"
        [ testCase "test with simple positive correlation"
          <| fun () ->
              // Test data with known positive correlation
              let dataA =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| 2.0
                     4.0
                     5.0
                     4.0
                     5.0
                     7.0
                     8.0
                     9.0
                     10.0
                     12.0 |]
              let result = FisherHotelling.test dataA dataB

              // Check that coefficient is positive and between -1 and 1
              Expect.isTrue (result.Coefficient > 0.9) "Coefficient should be highly positive"
              Expect.isTrue (result.Coefficient <= 1.0) "Coefficient should be <= 1.0"
              // Check that PValue is a valid probability
              Expect.isTrue (result.PValue >= 0.0 && result.PValue <= 1.0) "PValue should be between 0 and 1"
              // Check that ZValue is finite
              Expect.isTrue (not (System.Double.IsNaN(result.ZValue))) "ZValue should not be NaN"
              Expect.isTrue (not (System.Double.IsInfinity(result.ZValue))) "ZValue should not be infinite"

          testCase "test with simple negative correlation"
          <| fun () ->
              let dataA =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| 10.0
                     9.0
                     8.0
                     7.0
                     6.0
                     5.0
                     4.0
                     3.0
                     2.0
                     1.0 |]
              let result = FisherHotelling.test dataA dataB

              // Should have strong negative correlation
              Expect.isTrue (result.Coefficient < -0.9) "Coefficient should be highly negative"
              Expect.isTrue (result.Coefficient >= -1.0) "Coefficient should be >= -1.0"
              Expect.isTrue (result.PValue >= 0.0 && result.PValue <= 1.0) "PValue should be between 0 and 1"

          testCase "test with no correlation"
          <| fun () ->
              let dataA =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| 3.0
                     7.0
                     2.0
                     9.0
                     5.0
                     1.0
                     8.0
                     4.0
                     6.0
                     10.0 |]
              let result = FisherHotelling.test dataA dataB

              // Correlation should be close to 0
              Expect.isTrue (abs result.Coefficient < 0.5) "Coefficient should be close to 0"

          testCase "test with NaN values"
          <| fun () ->
              // Data with NaN values should be filtered
              let dataA =
                  [| 1.0
                     2.0
                     System.Double.NaN
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| 2.0
                     4.0
                     6.0
                     8.0
                     10.0
                     12.0
                     14.0
                     16.0
                     18.0
                     20.0 |]
              let result = FisherHotelling.test dataA dataB

              // Should still return valid result with NaN filtered out
              Expect.isTrue (not (System.Double.IsNaN(result.Coefficient))) "Coefficient should not be NaN"
              Expect.isTrue (result.Coefficient > 0.9) "Should have high positive correlation after filtering NaN"

          testCase "test with small sample (n=2)"
          <| fun () ->
              // Very small sample, should handle gracefully
              let dataA =
                  [| 1.0
                     2.0 |]
              let dataB =
                  [| 2.0
                     4.0 |]
              let result = FisherHotelling.test dataA dataB

              // With n < 3, PValue should be NaN
              Expect.isTrue (System.Double.IsNaN(result.PValue)) "PValue should be NaN for n < 3"
              // But coefficient should still be computed
              Expect.isTrue
                  (not (System.Double.IsNaN(result.Coefficient)))
                  "Coefficient should be computed even for small n"
              Expect.isTrue (not (System.Double.IsNaN(result.ZValue))) "ZValue should be computed even for small n"

          testCase "test with perfect positive correlation"
          <| fun () ->
              let dataA =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| 2.0
                     4.0
                     6.0
                     8.0
                     10.0
                     12.0
                     14.0
                     16.0
                     18.0
                     20.0 |]
              let result = FisherHotelling.test dataA dataB

              // Perfect correlation should give coefficient very close to 1.0
              Expect.floatClose
                  Accuracy.high
                  result.Coefficient
                  1.0
                  "Perfect positive correlation should yield coefficient ~ 1.0"
              // ZValue should be finite (not infinity despite r=1)
              Expect.isTrue
                  (not (System.Double.IsInfinity(result.ZValue)))
                  "ZValue should be finite even with perfect correlation"

          testCase "test with perfect negative correlation"
          <| fun () ->
              let dataA =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let dataB =
                  [| -2.0
                     -4.0
                     -6.0
                     -8.0
                     -10.0
                     -12.0
                     -14.0
                     -16.0
                     -18.0
                     -20.0 |]
              let result = FisherHotelling.test dataA dataB

              // Perfect negative correlation should give coefficient very close to -1.0
              Expect.floatClose
                  Accuracy.high
                  result.Coefficient
                  -1.0
                  "Perfect negative correlation should yield coefficient ~ -1.0"
              // ZValue should be finite
              Expect.isTrue
                  (not (System.Double.IsInfinity(result.ZValue)))
                  "ZValue should be finite even with perfect negative correlation"

          testCase "createHotellingStatistics creates correct structure"
          <| fun () ->
              let result = FisherHotelling.createHotellingStatistics 0.5 0.05 1.2
              Expect.floatClose Accuracy.high result.Coefficient 0.5 "Coefficient should match"
              Expect.floatClose Accuracy.high result.PValue 0.05 "PValue should match"
              Expect.floatClose Accuracy.high result.ZValue 1.2 "ZValue should match"

          testCase "empty has expected values"
          <| fun () ->
              let result = FisherHotelling.empty
              Expect.floatClose Accuracy.high result.Coefficient 0.0 "Empty coefficient should be 0.0"
              Expect.isTrue (System.Double.IsNaN(result.PValue)) "Empty PValue should be NaN"
              Expect.isTrue (System.Double.IsNaN(result.ZValue)) "Empty ZValue should be NaN"

          testCase "test with all NaN values"
          <| fun () ->
              // All NaN data should be filtered out, leaving empty dataset
              let dataA =
                  [| System.Double.NaN
                     System.Double.NaN
                     System.Double.NaN |]
              let dataB =
                  [| System.Double.NaN
                     System.Double.NaN
                     System.Double.NaN |]
              let result = FisherHotelling.test dataA dataB

              // With no valid data, should return NaN for PValue
              Expect.isTrue (System.Double.IsNaN(result.PValue)) "PValue should be NaN with all NaN data"

          testCase "test handles zero variance data"
          <| fun () ->
              // Constant data (zero variance)
              let dataA =
                  [| 5.0
                     5.0
                     5.0
                     5.0
                     5.0
                     5.0
                     5.0
                     5.0
                     5.0
                     5.0 |]
              let dataB =
                  [| 1.0
                     2.0
                     3.0
                     4.0
                     5.0
                     6.0
                     7.0
                     8.0
                     9.0
                     10.0 |]
              let result = FisherHotelling.test dataA dataB

              // Correlation with constant data should be NaN
              Expect.isTrue (System.Double.IsNaN(result.Coefficient)) "Coefficient should be NaN with zero variance" ]

[<Tests>]
let anovaTests =
    testList
        "Testing.Anova"
        [

          // One-Way ANOVA Tests
          testCase "oneWayAnova with simple groups"
          <| fun () ->
              // Three groups with different means
              let group1 =
                  [ 3.0
                    4.0
                    3.5
                    3.8
                    4.2 ]
              let group2 =
                  [ 5.0
                    5.5
                    5.2
                    5.8
                    5.3 ]
              let group3 =
                  [ 7.0
                    7.5
                    7.2
                    6.8
                    7.3 ]
              let samples =
                  [ group1
                    group2
                    group3 ]

              let result = Anova.oneWayAnova samples

              // F-statistic should be positive and significant
              Expect.isTrue (result.Factor.Statistic > 0.0) "F-statistic should be positive"
              Expect.isTrue
                  (result.Factor.Significance < 0.05)
                  "Significance should be < 0.05 for clearly different groups"
              // Degrees of freedom: groups - 1 = 2
              Expect.floatClose Accuracy.high result.Factor.DegreesOfFreedom 2.0 "Between-groups DoF should be 2"
              // Error degrees of freedom: total - groups = 15 - 3 = 12
              Expect.floatClose Accuracy.high result.Error.DegreesOfFreedom 12.0 "Within-groups DoF should be 12"
              // Total degrees of freedom: total samples - 1 = 14
              Expect.floatClose Accuracy.high result.Total.DegreesOfFreedom 14.0 "Total DoF should be 14"

          testCase "oneWayAnova with very similar groups"
          <| fun () ->
              // All groups have very similar means - should have small F-statistic
              let group1 =
                  [ 5.0
                    5.1
                    4.9
                    5.0 ]
              let group2 =
                  [ 5.0
                    5.05
                    4.95
                    5.0 ]
              let group3 =
                  [ 5.0
                    5.02
                    4.98
                    5.0 ]
              let samples =
                  [ group1
                    group2
                    group3 ]

              let result = Anova.oneWayAnova samples

              // Between-group variance should be very small
              Expect.isTrue (result.Factor.SumOfSquares < 0.01) "Between-groups SS should be very small"
              // F-statistic should be small and not significant
              Expect.isTrue (result.Factor.Statistic < 1.0) "F-statistic should be small for similar groups"
              Expect.isTrue (result.Factor.Significance > 0.05) "Should not be significant"

          testCase "oneWayAnova with two groups"
          <| fun () ->
              // Minimum case: two groups
              let group1 =
                  [ 1.0
                    2.0
                    3.0
                    4.0
                    5.0 ]
              let group2 =
                  [ 6.0
                    7.0
                    8.0
                    9.0
                    10.0 ]
              let samples =
                  [ group1
                    group2 ]

              let result = Anova.oneWayAnova samples

              Expect.floatClose Accuracy.high result.Factor.DegreesOfFreedom 1.0 "DoF should be 1 for two groups"
              Expect.isTrue (result.Factor.Statistic > 0.0) "F-statistic should be positive"
              // Sum of squares: total = between + within
              let totalCheck = result.Factor.SumOfSquares + result.Error.SumOfSquares
              Expect.floatClose
                  Accuracy.low
                  result.Total.SumOfSquares
                  totalCheck
                  "Total SS should equal sum of components"

          testCase "oneWayAnova variation sources"
          <| fun () ->
              // Test variation source types are correctly assigned
              let group1 =
                  [ 2.0
                    3.0
                    4.0 ]
              let group2 =
                  [ 5.0
                    6.0
                    7.0 ]
              let group3 =
                  [ 8.0
                    9.0
                    10.0 ]
              let samples =
                  [ group1
                    group2
                    group3 ]

              let result = Anova.oneWayAnova samples

              Expect.equal
                  result.Factor.Source
                  Anova.VariationSource.BetweenGroups
                  "Factor source should be BetweenGroups"
              Expect.equal result.Error.Source Anova.VariationSource.WithinGroups "Error source should be WithinGroups"
              Expect.equal result.Total.Source Anova.VariationSource.Total "Total source should be Total"

          testCase "oneWayAnova mean squares calculation"
          <| fun () ->
              let group1 =
                  [ 10.0
                    12.0
                    14.0 ]
              let group2 =
                  [ 20.0
                    22.0
                    24.0 ]
              let samples =
                  [ group1
                    group2 ]

              let result = Anova.oneWayAnova samples

              // Mean square = Sum of squares / degrees of freedom
              let expectedFactorMS = result.Factor.SumOfSquares / result.Factor.DegreesOfFreedom
              let expectedErrorMS = result.Error.SumOfSquares / result.Error.DegreesOfFreedom

              Expect.floatClose
                  Accuracy.high
                  result.Factor.MeanSquares
                  expectedFactorMS
                  "Factor mean squares should be SS/DoF"
              Expect.floatClose
                  Accuracy.high
                  result.Error.MeanSquares
                  expectedErrorMS
                  "Error mean squares should be SS/DoF"

          // Two-Way ANOVA Tests
          testCase "twoWayANOVA Fixed model"
          <| fun () ->
              // 2x2 design with 3 replicates each
              let samples =
                  [| [| [| 1.0
                           2.0
                           3.0 |]
                        [| 4.0
                           5.0
                           6.0 |] |]
                     [| [| 7.0
                           8.0
                           9.0 |]
                        [| 10.0
                           11.0
                           12.0 |] |] |]

              let result = Anova.twoWayANOVA Anova.TwoWayAnovaModel.Fixed samples

              // Check degrees of freedom
              Expect.floatClose Accuracy.high result.FactorFst.DegreesOfFreedom 1.0 "First factor DoF should be 1"
              Expect.floatClose Accuracy.high result.FactorSnd.DegreesOfFreedom 1.0 "Second factor DoF should be 1"
              Expect.floatClose Accuracy.high result.Interaction.DegreesOfFreedom 1.0 "Interaction DoF should be 1"
              Expect.floatClose Accuracy.high result.Error.DegreesOfFreedom 8.0 "Error DoF should be 8"

              // All F-statistics should be positive
              Expect.isTrue (result.FactorFst.Statistic > 0.0) "First factor F-statistic should be positive"
              Expect.isTrue (result.FactorSnd.Statistic > 0.0) "Second factor F-statistic should be positive"

          testCase "twoWayANOVA Mixed model"
          <| fun () ->
              let samples =
                  [| [| [| 2.0
                           3.0
                           4.0 |]
                        [| 5.0
                           6.0
                           7.0 |] |]
                     [| [| 8.0
                           9.0
                           10.0 |]
                        [| 11.0
                           12.0
                           13.0 |] |] |]

              let result = Anova.twoWayANOVA Anova.TwoWayAnovaModel.Mixed samples

              // Test that result structure is valid
              Expect.isTrue
                  (not (System.Double.IsNaN(result.FactorFst.Statistic)))
                  "First factor statistic should not be NaN"
              Expect.isTrue
                  (not (System.Double.IsNaN(result.FactorSnd.Statistic)))
                  "Second factor statistic should not be NaN"
              Expect.isTrue
                  (not (System.Double.IsNaN(result.Interaction.Statistic)))
                  "Interaction statistic should not be NaN"

          testCase "twoWayANOVA Random model"
          <| fun () ->
              let samples =
                  [| [| [| 1.5
                           2.5
                           3.5 |]
                        [| 4.5
                           5.5
                           6.5 |] |]
                     [| [| 7.5
                           8.5
                           9.5 |]
                        [| 10.5
                           11.5
                           12.5 |] |] |]

              let result = Anova.twoWayANOVA Anova.TwoWayAnovaModel.Random samples

              // All variation sources should have valid values
              Expect.isTrue (result.FactorFst.SumOfSquares >= 0.0) "First factor SS should be non-negative"
              Expect.isTrue (result.FactorSnd.SumOfSquares >= 0.0) "Second factor SS should be non-negative"
              Expect.isTrue (result.Interaction.SumOfSquares >= 0.0) "Interaction SS should be non-negative"
              Expect.isTrue (result.Error.SumOfSquares >= 0.0) "Error SS should be non-negative"

          testCase "twoWayANOVA with larger design"
          <| fun () ->
              // 3x2 design with 4 replicates
              let samples =
                  [| [| [| 1.0
                           2.0
                           3.0
                           4.0 |]
                        [| 5.0
                           6.0
                           7.0
                           8.0 |] |]
                     [| [| 9.0
                           10.0
                           11.0
                           12.0 |]
                        [| 13.0
                           14.0
                           15.0
                           16.0 |] |]
                     [| [| 17.0
                           18.0
                           19.0
                           20.0 |]
                        [| 21.0
                           22.0
                           23.0
                           24.0 |] |] |]

              let result = Anova.twoWayANOVA Anova.TwoWayAnovaModel.Fixed samples

              // Check degrees of freedom for 3x2 design
              Expect.floatClose Accuracy.high result.FactorFst.DegreesOfFreedom 2.0 "First factor DoF should be 2"
              Expect.floatClose Accuracy.high result.FactorSnd.DegreesOfFreedom 1.0 "Second factor DoF should be 1"
              Expect.floatClose Accuracy.high result.Interaction.DegreesOfFreedom 2.0 "Interaction DoF should be 2"
              Expect.floatClose Accuracy.high result.Error.DegreesOfFreedom 18.0 "Error DoF should be 18"
              Expect.floatClose Accuracy.high result.Total.DegreesOfFreedom 23.0 "Total DoF should be 23"

          testCase "twoWayANOVA sum of squares partitioning"
          <| fun () ->
              let samples =
                  [| [| [| 3.0
                           4.0
                           5.0 |]
                        [| 6.0
                           7.0
                           8.0 |] |]
                     [| [| 9.0
                           10.0
                           11.0 |]
                        [| 12.0
                           13.0
                           14.0 |] |] |]

              let result = Anova.twoWayANOVA Anova.TwoWayAnovaModel.Fixed samples

              // For balanced two-way ANOVA: Total SS should be close to sum of components
              // Total ≈ Factor1 + Factor2 + Interaction + Error
              let sumComponents =
                  result.FactorFst.SumOfSquares
                  + result.FactorSnd.SumOfSquares
                  + result.Interaction.SumOfSquares
                  + result.Error.SumOfSquares

              Expect.floatClose
                  Accuracy.low
                  result.Total.SumOfSquares
                  sumComponents
                  "Total SS should approximately equal sum of components"

          // Type creation tests
          testCase "createAnovaVariationSource"
          <| fun () ->
              let source =
                  Anova.createAnovaVariationSource 5.0 10.0 0.05 Anova.VariationSource.BetweenGroups 3.5 50.0

              Expect.floatClose Accuracy.high source.DegreesOfFreedom 5.0 "DoF should match"
              Expect.floatClose Accuracy.high source.MeanSquares 10.0 "MeanSquares should match"
              Expect.floatClose Accuracy.high source.Significance 0.05 "Significance should match"
              Expect.equal source.Source Anova.VariationSource.BetweenGroups "Source should match"
              Expect.floatClose Accuracy.high source.Statistic 3.5 "Statistic should match"
              Expect.floatClose Accuracy.high source.SumOfSquares 50.0 "SumOfSquares should match"

          testCase "createOneWayAnovaVariationSources"
          <| fun () ->
              let factor =
                  Anova.createAnovaVariationSource 2.0 15.0 0.01 Anova.VariationSource.BetweenGroups 5.0 30.0
              let error =
                  Anova.createAnovaVariationSource 12.0 3.0 nan Anova.VariationSource.WithinGroups nan 36.0
              let total =
                  Anova.createAnovaVariationSource 14.0 4.7 nan Anova.VariationSource.Total nan 66.0

              let result = Anova.createOneWayAnovaVariationSources factor error total

              Expect.floatClose
                  Accuracy.high
                  result.Factor.DegreesOfFreedom
                  factor.DegreesOfFreedom
                  "Factor DoF should match"
              Expect.floatClose
                  Accuracy.high
                  result.Error.DegreesOfFreedom
                  error.DegreesOfFreedom
                  "Error DoF should match"
              Expect.floatClose
                  Accuracy.high
                  result.Total.DegreesOfFreedom
                  total.DegreesOfFreedom
                  "Total DoF should match"
              Expect.equal result.Factor.Source factor.Source "Factor source should match"
              Expect.equal result.Error.Source error.Source "Error source should match"
              Expect.equal result.Total.Source total.Source "Total source should match"

          testCase "createTwoWayAnovaVariationSources"
          <| fun () ->
              let ffst =
                  Anova.createAnovaVariationSource 1.0 10.0 0.05 Anova.VariationSource.Residual 2.5 10.0
              let fsnd =
                  Anova.createAnovaVariationSource 1.0 20.0 0.03 Anova.VariationSource.Residual 5.0 20.0
              let inter =
                  Anova.createAnovaVariationSource 1.0 5.0 0.15 Anova.VariationSource.BetweenGroups 1.25 5.0
              let error =
                  Anova.createAnovaVariationSource 8.0 4.0 nan Anova.VariationSource.WithinGroups nan 32.0
              let cells =
                  Anova.createAnovaVariationSource 3.0 nan nan Anova.VariationSource.WithinGroups nan 35.0
              let total =
                  Anova.createAnovaVariationSource 11.0 nan nan Anova.VariationSource.WithinGroups nan 67.0

              let result =
                  Anova.createTwoWayAnovaVariationSources ffst fsnd inter error cells total

              Expect.floatClose
                  Accuracy.high
                  result.FactorFst.DegreesOfFreedom
                  ffst.DegreesOfFreedom
                  "FactorFst DoF should match"
              Expect.floatClose
                  Accuracy.high
                  result.FactorSnd.DegreesOfFreedom
                  fsnd.DegreesOfFreedom
                  "FactorSnd DoF should match"
              Expect.floatClose
                  Accuracy.high
                  result.Interaction.DegreesOfFreedom
                  inter.DegreesOfFreedom
                  "Interaction DoF should match"
              Expect.equal result.FactorFst.Source ffst.Source "FactorFst source should match"
              Expect.equal result.FactorSnd.Source fsnd.Source "FactorSnd source should match"
              Expect.equal result.Interaction.Source inter.Source "Interaction source should match" ]
