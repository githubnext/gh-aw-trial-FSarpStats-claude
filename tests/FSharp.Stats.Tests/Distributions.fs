module DistributionsTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open Distance.OneDimensional

// Defining an accuracy appropriate for testing random sampling and inference
let fittingAccuracy: Accuracy =
    { absolute = 0.1
      relative = 0.1 }

[<Tests>]
let distanceFunctionsTests =
    // Tests taken directly from the source implementation in scipy
    //
    // WassersteinDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L6986
    // EnergyDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L7068
    testList
        "Distributions.Distance"
        [ testCase "test_WassersteinDistance"
          <| fun () ->
              let xs =
                  [| 3.4
                     3.9
                     7.5
                     7.8 |]
              let ys =
                  [| 4.5
                     1.4 |]
              let xWeights =
                  [| 1.4
                     0.9
                     3.1
                     7.2 |]
              let yWeights =
                  [| 3.2
                     3.5 |]
              let distance = wassersteinDistanceWeighted xs ys xWeights yWeights
              Expect.floatClose Accuracy.high distance 4.0781331438047861 "Should be equal (double precision)"
          testCase "test_EnergyDistance"
          <| fun () ->
              let xs =
                  [| 0.7
                     7.4
                     2.4
                     6.8 |]
              let ys =
                  [| 1.4
                     8. |]
              let xWeights =
                  [| 2.1
                     4.2
                     7.4
                     8. |]
              let yWeights =
                  [| 7.6
                     8.8 |]
              let distance = energyDistanceWeighted xs ys xWeights yWeights
              Expect.floatClose Accuracy.high distance 0.88003340976158217 "Should be equal (double precision)" ]

[<Tests>]
let frequencyTests =
    testList
        "Distributions.Frequency"
        [ testCase "createGeneric basic histogram"
          <| fun () ->
              let data =
                  [ 'a'
                    'b'
                    'a'
                    'c'
                    'b'
                    'b' ]
              let hist = Frequency.createGeneric data
              Expect.equal (hist.['a']) 2 "Should count 'a' twice"
              Expect.equal (hist.['b']) 3 "Should count 'b' three times"
              Expect.equal (hist.['c']) 1 "Should count 'c' once"

          testCase "createGeneric empty list"
          <| fun () ->
              let hist = Frequency.createGeneric []
              Expect.equal (Map.count hist) 0 "Empty list should produce empty map"

          testCase "createGeneric single element"
          <| fun () ->
              let hist = Frequency.createGeneric [ 42 ]
              Expect.equal (hist.[42]) 1 "Single element should have count 1"

          testCase "create with positive bandwidth and positive values"
          <| fun () ->
              let data =
                  [ 0.5
                    1.5
                    2.5
                    3.5
                    4.5 ]
              let hist = Frequency.create 1.0 data
              Expect.equal (Map.count hist) 5 "Should create 5 bins"
              // Each value falls in its own bin
              Expect.isTrue (hist.ContainsKey 0.5) "Should contain bin at 0.5"
              Expect.isTrue (hist.ContainsKey 1.5) "Should contain bin at 1.5"

          testCase "create with bandwidth grouping values"
          <| fun () ->
              let data =
                  [ 0.1
                    0.2
                    0.3
                    1.1
                    1.2 ]
              let hist = Frequency.create 1.0 data
              // Values 0.1, 0.2, 0.3 should be in one bin
              // Values 1.1, 1.2 should be in another bin
              Expect.equal (Map.count hist) 2 "Should create 2 bins"

          testCase "create with negative values"
          <| fun () ->
              let data =
                  [ -2.5
                    -1.5
                    -0.5
                    0.5
                    1.5 ]
              let hist = Frequency.create 1.0 data
              Expect.equal (Map.count hist) 5 "Should handle negative values"

          testCase "getZip returns sorted key-value pairs"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ (3, 1)
                        (1, 2)
                        (2, 3) ]
              let zipped = Frequency.getZip hist |> Seq.toList
              Expect.equal (List.length zipped) 3 "Should have 3 elements"
              Expect.equal (fst zipped.[0]) 1 "First key should be 1"
              Expect.equal (fst zipped.[1]) 2 "Second key should be 2"
              Expect.equal (fst zipped.[2]) 3 "Third key should be 3"

          testCase "sum returns total frequency count"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 5)
                        ('b', 3)
                        ('c', 7) ]
              let total = Frequency.sum hist
              Expect.equal total 15 "Sum should be 5+3+7=15"

          testCase "sum empty histogram"
          <| fun () ->
              let hist = Map.empty
              let total = Frequency.sum hist
              Expect.equal total 0 "Sum of empty histogram should be 0"

          testCase "average returns mean frequency"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 2)
                        ('b', 4)
                        ('c', 6) ]
              let avg = Frequency.average hist
              Expect.floatClose Accuracy.high avg 4.0 "Average should be (2+4+6)/3 = 4.0"

          testCase "maxLike returns highest frequency"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 5)
                        ('b', 12)
                        ('c', 3) ]
              let maxFreq = Frequency.maxLike hist
              Expect.equal maxFreq 12 "Maximum frequency should be 12"

          testCase "frequencyAt existing key"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 7)
                        ('b', 3) ]
              let freq = Frequency.frequencyAt hist 'a'
              Expect.equal freq 7 "Frequency at 'a' should be 7"

          testCase "frequencyAt missing key"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 7)
                        ('b', 3) ]
              let freq = Frequency.frequencyAt hist 'z'
              Expect.equal freq 0 "Frequency at missing key should be 0"

          testCase "frequencies returns all frequency values"
          <| fun () ->
              let hist =
                  Map.ofList
                      [ ('a', 5)
                        ('b', 3)
                        ('c', 8) ]
              let freqs = Frequency.frequencies hist |> Seq.toList
              Expect.equal (List.length freqs) 3 "Should have 3 frequencies"
              Expect.isTrue (List.contains 5 freqs) "Should contain 5"
              Expect.isTrue (List.contains 3 freqs) "Should contain 3"
              Expect.isTrue (List.contains 8 freqs) "Should contain 8"

          testCase "isSubset true when A is subset of B"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1.0, 2)
                        (2.0, 3) ]
              let histB =
                  Map.ofList
                      [ (1.0, 5)
                        (2.0, 4)
                        (3.0, 1) ]
              let result = Frequency.isSubset histA histB
              Expect.isTrue result "A should be subset of B"

          testCase "isSubset false when A has higher frequency"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1.0, 6)
                        (2.0, 3) ]
              let histB =
                  Map.ofList
                      [ (1.0, 5)
                        (2.0, 4) ]
              let result = Frequency.isSubset histA histB
              Expect.isFalse result "A should not be subset of B (frequency too high)"

          testCase "isSubset false when A has key not in B"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1.0, 2)
                        (99.0, 1) ]
              let histB =
                  Map.ofList
                      [ (1.0, 5)
                        (2.0, 4) ]
              let result = Frequency.isSubset histA histB
              Expect.isFalse result "A should not be subset of B (key 99.0 missing in B)"

          testCase "isSubset empty histogram is subset"
          <| fun () ->
              let histA = Map.empty
              let histB =
                  Map.ofList
                      [ (1.0, 5)
                        (2.0, 4) ]
              let result = Frequency.isSubset histA histB
              Expect.isTrue result "Empty histogram should be subset of any histogram"

          testCase "merge supersedes values from A with B"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1, 10)
                        (2, 20) ]
              let histB =
                  Map.ofList
                      [ (2, 99)
                        (3, 30) ]
              let merged = Frequency.merge true histA histB
              Expect.equal merged.[1] 10 "Key 1 only in A should remain"
              Expect.equal merged.[2] 99 "Key 2 in both should use B's value (99)"
              Expect.equal merged.[3] 30 "Key 3 only in B should be added"

          testCase "merge rejects unequal bandwidth for continuous data"
          <| fun () ->
              let histA = Map.ofList [ (1, 10) ]
              let histB = Map.ofList [ (2, 20) ]
              Expect.throws (fun () -> Frequency.merge false histA histB |> ignore) "Should throw for unequal bandwidth"

          testCase "add combines frequencies"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1, 10)
                        (2, 20) ]
              let histB =
                  Map.ofList
                      [ (2, 5)
                        (3, 15) ]
              let summed = Frequency.add true histA histB
              Expect.equal summed.[1] 10 "Key 1 should be 10"
              Expect.equal summed.[2] 25 "Key 2 should be 20+5=25"
              Expect.equal summed.[3] 15 "Key 3 should be 15"

          testCase "subtract subtracts frequencies"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1, 10)
                        (2, 20) ]
              let histB =
                  Map.ofList
                      [ (2, 5)
                        (3, 3) ]
              let result = Frequency.subtract true histA histB
              Expect.equal result.[1] 10 "Key 1 should be 10"
              Expect.equal result.[2] 15 "Key 2 should be 20-5=15"
              // Key 3 only exists in histB, so mergeBy uses (fun a b -> a - b) where a comes from histA
              // Since key 3 is not in histA, Map.mergeBy treats it as only in histB and just includes b
              Expect.equal result.[3] 3 "Key 3 only in B should be 3"

          testCase "mergeBy with custom function"
          <| fun () ->
              let histA =
                  Map.ofList
                      [ (1, 10)
                        (2, 20) ]
              let histB =
                  Map.ofList
                      [ (2, 5)
                        (3, 15) ]
              // Custom function: take maximum
              let merged = Frequency.mergeBy true (fun a b -> max a b) histA histB
              Expect.equal merged.[1] 10 "Key 1 should be 10"
              Expect.equal merged.[2] 20 "Key 2 should be max(20,5)=20"
              Expect.equal merged.[3] 15 "Key 3 should be 15" ]

[<Tests>]
let bandWithTests =
    testList
        "Distribution.Bandwidth.BinNumber"
        [
          //Reference:https://www.statisticshowto.com/choose-bin-sizes-statistics/#rice
          // tested with r Function ceiling(1+log2(x))

          testCase "Distribution.Bandwidth.BinNumber.sturges"
          <| fun () ->
              let sturges1 = Distributions.Bandwidth.BinNumber.sturges 1.
              Expect.floatClose Accuracy.veryHigh 1 sturges1 "desirable number of classes Should be equal"

              let sturgesForNull = Distributions.Bandwidth.BinNumber.sturges 0.
              Expect.isTrue (-infinity = sturgesForNull) "desirable number of classes should be equal"

              let sturgesForNegative = Distributions.Bandwidth.BinNumber.sturges -1.
              Expect.isTrue (nan.Equals(sturgesForNegative)) "desirable number of classes should be nan."

              let sturgesForNan = Distributions.Bandwidth.BinNumber.sturges nan
              Expect.isTrue (nan.Equals(sturgesForNan)) "desirable number of classes should be nan."

              let sturgesForPositivInifinity = Distributions.Bandwidth.BinNumber.sturges infinity
              Expect.isTrue (infinity = sturgesForPositivInifinity) "desirable number of classes should be equal"

              let sturgesForNegativeInfinity = Distributions.Bandwidth.BinNumber.sturges -infinity
              Expect.isTrue (nan.Equals(sturgesForNegativeInfinity)) "desirable number of classes should be nan."

              let sturgesWithRealWorldProblem = Distributions.Bandwidth.BinNumber.sturges 1000.
              Expect.floatClose
                  Accuracy.veryHigh
                  sturgesWithRealWorldProblem
                  11
                  "desirable number of bins should be equal"

          // reference:https://www.rdocumentation.org/packages/npsp/versions/0.7-5/topics/rule
          // tested with R function ceiling(2*(n ** (1./3.)))

          testCase "Distribution.Bandwidth.BinNumber.riceRule"
          <| fun () ->
              let riceRule1 = Distributions.Bandwidth.BinNumber.riceRule 1.
              Expect.floatClose Accuracy.veryHigh riceRule1 2. "desirable number of classes should be equal"

              let riceRuleForNull = Distributions.Bandwidth.BinNumber.riceRule 0.
              Expect.floatClose
                  Accuracy.veryHigh
                  riceRuleForNull
                  0.
                  "desirbale number of classes should be equal to expected Value"

              let riceRuleForNan = Distributions.Bandwidth.BinNumber.riceRule nan
              Expect.isTrue (nan.Equals(riceRuleForNan)) "desirable number of classes should be nan."

              let riceRuleForNegative = Distributions.Bandwidth.BinNumber.riceRule (-1.)
              Expect.isTrue (nan.Equals(riceRuleForNegative)) "desirable number of classes should be nan."

              let riceRuleForPositiveInfinity =
                  Distributions.Bandwidth.BinNumber.riceRule infinity
              Expect.isTrue (infinity = riceRuleForPositiveInfinity) "desirable number of classes should be equal"

              let riceRuleForNegativeInfinity =
                  Distributions.Bandwidth.BinNumber.riceRule -infinity
              Expect.isTrue (infinity = riceRuleForNegativeInfinity) "desirable number of classes should be -infinity."

              let riceRuleWithRealWorldExample = Distributions.Bandwidth.BinNumber.riceRule 1000.
              Expect.floatClose
                  Accuracy.veryHigh
                  riceRuleWithRealWorldExample
                  20.
                  "desirable number of bins should be equal"


          ]



//[<Tests>]
//let GammaDistributionTests =

//    let alpha = 0.4
//    let beta  = 4.2

//    let d     = ContinuousDistribution.gamma alpha beta

//    let mean  = d.Mean
//    let var   = d.Variance
//    let cdfs  = [| 0.; 0.251017; 0.328997; 0.38435; 0.428371; 0.465289;
//                   0.497226; 0.525426; 0.55069; 0.573571 |]

//    let pdfs = [| 0.987114; 0.635929; 0.486871; 0.400046; 0.341683;
//                  0.299071; 0.266236; 0.239956; 0.218323; 0.200126; |]



//    testList "Distributions.Continuous.Gamma" [

//        //testCase "Mean" <| fun () ->
//        //    Expect.floatClose Accuracy.high mean 0.21105527638190955 "Mean should be equal"

//        //testCase "Variance" <| fun () ->
//        //    Expect.floatClose Accuracy.high var 0.055689279830523512 "Variance should be equal"

//        testCase "Cdfs" <| fun () ->
//            cdfs
//            |> Array.iteri (fun i v ->
//                let cdf = d.CDF (float i / 10.0)
//                Expect.floatClose Accuracy.low cdf cdfs[i] "Cdf should be equal"
//                )

//        testCase "Pdfs" <| fun () ->
//            cdfs
//            |> Array.iteri (fun i v ->
//                let pdf = d.PDF ((float i + 1.) / 10.0)
//                Expect.floatClose Accuracy.low pdf pdfs[i] "Cdf should be equal"
//                )

//        //testCase "Pdf" <| fun () ->
//        //    Expect.floatClose Accuracy.high pdf 0.987114 "Pdf should be equal"

//        testCase "FitTest" <| fun () ->
//            let observations = Array.init 999999 (fun _ -> float (Continuous.Gamma.Sample alpha beta))
//            let alpha',beta' = Continuous.Gamma.Fit observations

//            Expect.floatClose fittingAccuracy alpha alpha'
//                "alpha"
//            Expect.floatClose fittingAccuracy beta beta'
//                "beta"

//        testCase "FitTest_from_observations" <| fun () ->
//            let observations = [| 1275.56; 1239.44; 1237.92; 1237.22; 1237.1; 1238.41; 1238.62; 1237.05;
//                1237.19; 1236.51; 1264.6; 1238.19; 1237.39; 1235.79; 1236.53; 1236.8; 1238.06;
//                1236.5; 1235.32; 1236.44; 1236.58; 1236.3; 1237.91; 1238.6; 1238.49; 1239.21;
//                1238.57; 1244.63; 1236.06; 1236.4; 1237.88; 1237.56; 1236.66; 1236.59; 1236.53;
//                1236.32; 1238.29; 1237.79; 1237.86; 1236.42; 1236.23; 1236.37; 1237.18; 1237.63;
//                1245.8; 1238.04; 1238.55; 1238.39; 1236.75; 1237.07; 1250.78; 1238.6; 1238.36;
//                1236.58; 1236.82; 1238.4; 1257.68; 1237.78; 1236.52; 1234.9; 1237.9; 1238.58;
//                1238.12; 1237.89; 1236.54; 1236.55; 1238.37; 1237.29; 1237.64; 1236.8; 1237.73;
//                1236.71; 1238.23; 1237.84; 1236.26; 1237.58; 1238.31; 1238.4; 1237.08; 1236.61;
//                1235.92; 1236.41; 1237.89; 1237.98; 1246.75; 1237.92; 1237.1; 1237.97; 1238.69;
//                1237.05; 1236.96; 1239.44; 1238.49; 1237.88; 1236.01; 1236.57; 1236.44; 1235.76;
//                1237.62; 1238; 1263.14; 1237.66; 1237; 1236; 1261.96; 1238.58; 1237.77; 1237.06;
//                1236.31; 1238.63; 1237.23; 1236.85; 1236.23; 1236.46; 1236.9; 1237.85; 1238;
//                1237.02; 1236.19; 1236.05; 1235.73; 1258.3; 1235.98; 1237.76; 1246.93; 1239.1;
//                1237.72; 1237.67; 1236.79; 1237.61; 1238.41; 1238.29; 1238.11; 1237; 1236.52;
//                1236.6; 1236.31; 1237.77; 1238.58; 1237.88; 1247.35; 1236.14; 1236.83; 1236.15;
//                1237.93; 1238.16; 1237.34; 1236.78; 1238.66; 1237.76; 1237.19; 1236.7; 1236.04;
//                1236.66; 1237.86; 1238.54; 1238.05; 1238.41; 1236.94; 1240.95; 1261.01; 1237.72;
//                1237.91; 1238.2; 1235.68; 1236.89; 1235.12; 1271.31; 1236.97; 1270.76; 1238.52;
//                1238.19; 1238.6; 1237.16; 1236.72; 1236.71; 1237.14; 1238.48; 1237.95; 1237.42;
//                1235.86; 1236.39; 1236.13; 1236.58; 1237.95; 1237.76; 1237.39; 1238.16; 1236.31;
//                1236.41; 1236.12; 1238.7; 1236.48; 1237.84; 1236.38; 1237.95; 1238.48; 1236.51;
//                1236.56 |]
//            let alpha, beta = Continuous.Gamma.Fit observations
//            //let mean = 1238.8734170854279
//            let alpha' = 41566.439533445438
//            let beta'  = 0.029804655654680219

//            Expect.floatClose fittingAccuracy alpha alpha'
//                "Gamma Distribution Fit"
//            Expect.floatClose fittingAccuracy beta beta'
//                "Gamma Distribution Fit"
//    //0.10000000000000000555; relative=0.10000000000000000555},
//    //but was 1238.8734068085332183. actual=1.0276894821207402346e-05 expected=1238.8734170854279455

//    ]
