﻿module DistanceMetricsTests

open Expecto
open FSharp.Stats
open FSharp.Stats.DistanceMetrics.Vector
open FSharp.Stats.DistanceMetrics
[<Tests>]
let hammingfunctiontests =
    testList
        "DistanceMetrics.hamming"
        [ testCase "hamming"
          <| fun () ->
              let seq1 =
                  seq {
                      0
                      0
                      0
                      0
                  }
              let seq2 =
                  seq {
                      1
                      1
                      1
                      1
                  }
              let distance = hamming seq1 seq2
              Expect.equal distance 4 "Should be equal"
          testCase "hamming0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = hamming seq1 seq2
              Expect.equal distance 0 "Should be equal"
          testCase "hamminginfinity"
          <| fun () ->
              let seq1 =
                  seq {
                      infinity
                      -infinity
                  }
              let seq2 =
                  seq {
                      infinity
                      -infinity
                  }
              let distance = hamming seq1 seq2
              Expect.equal distance 0 "Should be equal"
          testCase "hammingcharacters"
          <| fun () ->
              let seq1 =
                  seq {
                      "a"
                      "b"
                      "c"
                  }
              let seq2 =
                  seq {
                      "a"
                      "b"
                      "c"
                  }
              let distance = hamming seq1 seq2
              Expect.equal distance 0 "Should be equal"
          testCase "hamminglists"
          <| fun () ->
              let l1 =
                  [ 1.0
                    2.0 ]
              let l2 =
                  [ 1.0
                    3.0 ]
              let distance = hamming l1 l2
              Expect.equal distance 1 "Should be equal"
          testCase "hammingstrings"
          <| fun () ->
              let s1 = "karolin"
              let s2 = "kathrin"
              let distance = hamming s1 s2
              Expect.equal distance 3 "Should be equal"
          testCase "hammingexception"
          <| fun () ->
              let seq1 = seq { 0 }
              let seq2 =
                  seq {
                      1
                      1
                  }
              Expect.throws (fun () -> hamming seq1 seq2 |> ignore) "Should throw" ]

[<Tests>]
let hammingvecfunctiontests =
    testList
        "DistanceMetrics.hammingvector"
        [ testCase "hamming"
          <| fun () ->
              let v1 =
                  vector
                      [ 0
                        0
                        0
                        0 ]
              let v2 =
                  vector
                      [ 1
                        1
                        1
                        1 ]
              let distance = Vector.hamming v1 v2
              Expect.equal distance 4 "Should be equal"
          testCase "hamming0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.hamming v1 v2
              Expect.equal distance 0 "Should be equal"
          testCase "hamminginfinity"
          <| fun () ->
              let v1 =
                  vector
                      [ infinity
                        -infinity ]
              let v2 =
                  vector
                      [ infinity
                        -infinity ]
              let distance = Vector.hamming v1 v2
              Expect.equal distance 0 "Should be equal"
          testCase "hammingexception"
          <| fun () ->
              let v1 = vector [ 0 ]
              let v2 =
                  vector
                      [ 1
                        1 ]
              Expect.throws (fun () -> Vector.hamming v1 v2 |> ignore) "Should throw" ]

[<Tests>]
let hammingarrayfunctiontests =
    testList
        "DistanceMetrics.hammingarray"
        [ testCase "hamming"
          <| fun () ->
              let a1 =
                  [| 0
                     0
                     0
                     0 |]
              let a2 =
                  [| 1
                     1
                     1
                     1 |]
              let distance = Array.hamming a1 a2
              Expect.equal distance 4 "Should be equal"
          testCase "hamming0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.hamming a1 a2
              Expect.equal distance 0 "Should be equal"
          testCase "hamminginfinity"
          <| fun () ->
              let a1 =
                  [| infinity
                     -infinity |]
              let a2 =
                  [| infinity
                     -infinity |]
              let distance = Array.hamming a1 a2
              Expect.equal distance 0 "Should be equal"
          testCase "hammingcharacters"
          <| fun () ->
              let a1 =
                  [| "a"
                     "b"
                     "c" |]
              let a2 =
                  [| "a"
                     "b"
                     "c" |]
              let distance = Array.hamming a1 a2
              Expect.equal distance 0 "Should be equal"
          testCase "hammingexception"
          <| fun () ->
              let a1 = [| 0 |]
              let a2 =
                  [| 1
                     1 |]
              Expect.throws (fun () -> Array.hamming a1 a2 |> ignore) "Should throw" ]

[<Tests>]
let euclidianseqfunctiontests =
    testList
        "DistanceMetrics.euclidiansequence"
        [ testCase "euclidian"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclidean seq1 seq2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianinf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclidean seq1 seq2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidian0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = euclidean seq1 seq2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidiannan"
          <| fun () ->
              let seq1 =
                  seq {
                      00.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclidean seq1 seq2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "euclidianNaN"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianNaNinf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaN seq1 seq2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidianNaN0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = euclideanNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidianNaNnan"
          <| fun () ->
              let seq1 =
                  seq {
                      00.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

          testCase "euclidianNaNsqrt"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaNSquared seq1 seq2
              Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
          testCase "euclidianNaNsqrtinf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaNSquared seq1 seq2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidianNaNsqrt0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = euclideanNaNSquared seq1 seq2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidianNaNsqrtnan"
          <| fun () ->
              let seq1 =
                  seq {
                      00.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = euclideanNaNSquared seq1 seq2
              Expect.floatClose Accuracy.high distance 67.996001 "Should be equal" ]



[<Tests>]
let euclidianvecfunctiontests =
    testList
        "DistanceMetrics.euclidianvector"
        [ testCase "euclidian"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclidean v1 v2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianinf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclidean v1 v2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidian0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.euclidean v1 v2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidiannan"
          <| fun () ->
              let v1 =
                  vector
                      [ 00.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclidean v1 v2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "euclidiansqrt"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = euclideanSquared v1 v2
              Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
          testCase "euclidiansqrtinf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = euclideanSquared v1 v2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidiansqrt0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = euclideanSquared v1 v2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidiansqrtnan"
          <| fun () ->
              let v1 =
                  vector
                      [ 00.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = euclideanSquared v1 v2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "euclidianNaN"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclideanNaN v1 v2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianNaNinf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclideanNaN v1 v2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidianNaN0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.euclideanNaN v1 v2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidianNaNnan"
          <| fun () ->
              let v1 =
                  vector
                      [ 00.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.euclideanNaN v1 v2
              Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

          ]

[<Tests>]
let euclidianarrayqualifiedfunctiontests =
    testList
        "DistanceMetrics.Array.euclidean"
        [ testCase "euclidean"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclidean a1 a2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclideaninf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclidean a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidean0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.euclidean a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclideannan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclidean a1 a2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"
          testCase "euclideanDifferentLengths"
          <| fun () ->
              let a1 =
                  [| 1.0
                     2.0 |]
              let a2 =
                  [| 3.0
                     4.0
                     5.0
                     6.0 |]
              let distance = Array.euclidean a1 a2
              // Should use min length (2)
              Expect.floatClose Accuracy.high distance 2.828427125 "Should be equal"

          testCase "euclideanNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclideanNaNinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaN a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclideanNaN0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclideanNaNnan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

          testCase "euclideanNaNSquared"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
          testCase "euclideanNaNSquaredinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaNSquared a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclideanNaNSquared0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclideanNaNSquarednan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 67.996001 "Should be equal" ]

[<Tests>]
let euclidianarrayfunctiontests =
    testList
        "DistanceMetrics.euclidianarray"
        [ testCase "euclidian"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclidean a1 a2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclidean a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidian0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = euclidean a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidiannan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclidean a1 a2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "euclidianNaNsqrt"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 99980069 "Should be equal"
          testCase "euclidianNaNsqrtinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaNSquared a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidianNaNsqrt0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidianNaNsqrtnan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaNSquared a1 a2
              Expect.floatClose Accuracy.high distance 67.996001 "Should be equal"

          testCase "euclidianNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 9999.0034 "Should be equal"
          testCase "euclidianNaNinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaN a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "euclidianNaN0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "euclidianNaNnan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = euclideanNaN a1 a2
              Expect.floatClose Accuracy.high distance 8.245968773 "Should be equal"

          ]

[<Tests>]
let cityblockseqfunctiontests =
    testList
        "DistanceMetrics.cityblockseq"
        [ testCase "cityblock"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblock seq1 seq2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockinf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblock seq1 seq2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblock0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = cityblock seq1 seq2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblocknan"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblock seq1 seq2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "cityblockNaN"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblockNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockNaNinf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblockNaN seq1 seq2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblockNaN0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = cityblockNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblockNaNnan"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = cityblockNaN seq1 seq2
              Expect.floatClose Accuracy.high distance 9.999 "Should be equal" ]

[<Tests>]
let cityblockvectorfunctiontests =
    testList
        "DistanceMetrics.cityblockvector"
        [ testCase "cityblock"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblock v1 v2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockinf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblock v1 v2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblock0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.cityblock v1 v2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblocknan"
          <| fun () ->
              let v1 =
                  vector
                      [ 00.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblock v1 v2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "cityblockNaN"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblockNaN v1 v2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockNaNinf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblockNaN v1 v2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblockNaN0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.cityblockNaN v1 v2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblockNaNnan"
          <| fun () ->
              let v1 =
                  vector
                      [ 00.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.cityblockNaN v1 v2
              Expect.floatClose Accuracy.high distance 9.999 "Should be equal" ]

[<Tests>]
let cityblockarrayqualifiedfunctiontests =
    testList
        "DistanceMetrics.Array.cityblock"
        [ testCase "cityblock"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblock a1 a2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblock a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblock0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.cityblock a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblocknan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblock a1 a2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"
          testCase "cityblockDifferentLengths"
          <| fun () ->
              let a1 =
                  [| 1.0
                     2.0 |]
              let a2 =
                  [| 3.0
                     4.0
                     5.0 |]
              let distance = Array.cityblock a1 a2
              // Should use min length (2): |1-3| + |2-4| = 2 + 2 = 4
              Expect.floatClose Accuracy.high distance 4.0 "Should be equal"

          testCase "cityblockNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockNaNinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblockNaN a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblockNaN0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblockNaNnan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 9.999 "Should be equal" ]

[<Tests>]
let cityblockarrayfunctiontests =
    testList
        "DistanceMetrics.cityblockarray"
        [ testCase "cityblock"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblock a1 a2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblock a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblock0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = cityblock a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblocknan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblock a1 a2
              Expect.isTrue (nan.Equals(distance)) "Distance should be NaN"

          testCase "cityblockNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 10008.999 "Should be equal"
          testCase "cityblockNaNinf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblockNaN a1 a2
              Expect.equal distance infinity "Should be equal"
          testCase "cityblockNaN0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 0.0 "Should be equal"
          testCase "cityblockNaNnan"
          <| fun () ->
              let a1 =
                  [| 00.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = cityblockNaN a1 a2
              Expect.floatClose Accuracy.high distance 9.999 "Should be equal" ]

[<Tests>]
let Levenshteindistancetest =
    testList
        "DistanceMetrics.levenshteindistance"
        [
          // normal test case from Aung, K. M. M. (2019). Comparison of levenshtein distance algorithm and needleman-wunsch distance algorithm for string matching (Doctoral dissertation, MERAL Portal).
          testCase "Levenstein"
          <| fun () ->
              let string1 = "hello"
              let string2 = "helo"
              let distnace = wagnerFischerLazy string1 string2
              Expect.equal distnace 1 "should be equal"
          //TestCases from R stringdist('hello', '', method = 'lv') "R version 4.0.3 (2020-10-10)"
          testCase "Levensteinoneempty"
          <| fun () ->
              let string1 = "hello"
              let string2 = ""
              let distance = wagnerFischerLazy string1 string2
              Expect.equal distance 5 "should be equal"
          //TestCases from R stringdist('', '', method = 'lv') "R version 4.0.3 (2020-10-10)"
          testCase "Levensteinbothempty"
          <| fun () ->
              let string1 = ""
              let string2 = ""
              let distance = wagnerFischerLazy string1 string2
              Expect.equal distance 0 "should be equal"

          ]

[<Tests>]
let minkowskiseqfunctiontests =
    testList
        "DistanceMetrics.minkowskisequence"
        [ testCase "minkowskiNoValue"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10_000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = minkowski seq1 seq2 0.0
              Expect.isTrue distance.IsNone "No Value"

          testCase "minkowskiVsEuclidian"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10_000.0
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = minkowski seq1 seq2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiOrder3"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      1.5
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.5
                      1.0
                  }
              let distance = minkowski seq1 seq2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.04267819780 "Should be equal"

          testCase "minkowskiOrder5"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      1.5
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.5
                      1.0
                  }
              let distance = minkowski seq1 seq2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiOrder0.5"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      1.5
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.5
                      1.0
                  }
              let distance = minkowski seq1 seq2 0.5
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 5.6565006518965619264 "Should be equal"

          testCase "minkowskiLengths"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      1.5
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.5
                      1.0
                      1_000.0
                      6.0
                      7.
                  } // last elements are ignored
              let distance = minkowski seq1 seq2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiWithNaN"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                      0.0
                  }
              let distance = minkowski seq1 seq2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.isTrue (nan.Equals(distance.Value)) "Distance should be NaN"

          testCase "minkowskiNaN"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      0.0
                      10_000.0
                      nan
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                      0.0
                  }
              let distance = minkowskiNaN seq1 seq2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiInf"
          <| fun () ->
              let seq1 =
                  seq {
                      0.001
                      -2.0
                      -infinity
                      infinity
                  }
              let seq2 =
                  seq {
                      2.0
                      -10.0
                      0.0
                      1.0
                  }
              let distance = minkowski seq1 seq2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.equal distance.Value infinity "Should be equal"

          testCase "minkowski0"
          <| fun () ->
              let seq1 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let seq2 =
                  seq {
                      0.0
                      0.0
                      0.0
                      0.0
                  }
              let distance = minkowski seq1 seq2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 0.0 "Should be equal" ]

[<Tests>]
let minkowskivectorfunctiontests =
    testList
        "DistanceMetrics.minkowskivector"
        [ testCase "minkowskiNoValue"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10_000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.minkowski v1 v2 0.0
              Expect.isTrue distance.IsNone "No Value"

          testCase "minkowskiVsEuclidian"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10_000.0 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.minkowski v1 v2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiOrder3"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        1.5 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.5
                        1.0 ]
              let distance = Vector.minkowski v1 v2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.04267819780 "Should be equal"

          testCase "minkowskiOrder5"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        1.5 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.5
                        1.0 ]
              let distance = Vector.minkowski v1 v2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiOrder0.5"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        1.5 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.5
                        1.0 ]
              let distance = Vector.minkowski v1 v2 0.5
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 5.6565006518965619264 "Should be equal"

          testCase "minkowskiLengths"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        1.5 ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.5
                        1.0
                        1_000.0
                        6.0
                        7.0 ] // last elements are ignored
              let distance = Vector.minkowski v1 v2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiWithNaN"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0
                        0.0 ]
              let distance = Vector.minkowski v1 v2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.isTrue (nan.Equals(distance.Value)) "Distance should be NaN"

          testCase "minkowskiNaN"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        0.0
                        10_000.0
                        nan ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0
                        0.0 ]
              let distance = Vector.minkowskiNaN v1 v2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiInf"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.001
                        -2.0
                        -infinity
                        infinity ]
              let v2 =
                  vector
                      [ 2.0
                        -10.0
                        0.0
                        1.0 ]
              let distance = Vector.minkowski v1 v2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.equal distance.Value infinity "Should be equal"

          testCase "minkowski0"
          <| fun () ->
              let v1 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let v2 =
                  vector
                      [ 0.0
                        0.0
                        0.0
                        0.0 ]
              let distance = Vector.minkowski v1 v2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 0.0 "Should be equal" ]

[<Tests>]
let minkowskiarrayqualifiedfunctiontests =
    testList
        "DistanceMetrics.Array.minkowski"
        [ testCase "minkowskiNoValue"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 0.0
              Expect.isTrue distance.IsNone "No Value"

          testCase "minkowskiVsEuclidean"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiOrder3"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.04267819780 "Should be equal"

          testCase "minkowskiOrder5"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiOrder0.5"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 0.5
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 5.6565006518965619264 "Should be equal"

          testCase "minkowskiLengths"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0
                     1_000.0
                     6.0
                     7.0 |] // last elements are ignored
              let distance = Array.minkowski a1 a2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiWithNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0
                     0.0 |]
              let distance = Array.minkowski a1 a2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.isTrue (nan.Equals(distance.Value)) "Distance should be NaN"

          testCase "minkowskiNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0
                     0.0 |]
              let distance = Array.minkowskiNaN a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiInf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.equal distance.Value infinity "Should be equal"

          testCase "minkowski0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 0.0 "Should be equal" ]

[<Tests>]
let minkowskiarrayfunctiontests =
    testList
        "DistanceMetrics.minkowskiarray"
        [ testCase "minkowskiNoValue"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 0.0
              Expect.isTrue distance.IsNone "No Value"

          testCase "minkowskiVsEuclidian"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiOrder3"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.04267819780 "Should be equal"

          testCase "minkowskiOrder5"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiOrder0.5"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0 |]
              let distance = Array.minkowski a1 a2 0.5
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 5.6565006518965619264 "Should be equal"

          testCase "minkowskiLengths"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     1.5 |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.5
                     1.0
                     1_000.0
                     6.0
                     7.0 |] // last elements are ignored
              let distance = Array.minkowski a1 a2 5.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 8.00156104008 "Should be equal"

          testCase "minkowskiWithNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0
                     0.0 |]
              let distance = Array.minkowski a1 a2 3.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.isTrue (nan.Equals(distance.Value)) "Distance should be NaN"

          testCase "minkowskiNaN"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     0.0
                     10_000.0
                     nan |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0
                     0.0 |]
              let distance = Array.minkowskiNaN a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 9999.0034 "Should be equal"

          testCase "minkowskiInf"
          <| fun () ->
              let a1 =
                  [| 0.001
                     -2.0
                     -infinity
                     infinity |]
              let a2 =
                  [| 2.0
                     -10.0
                     0.0
                     1.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.equal distance.Value infinity "Should be equal"

          testCase "minkowski0"
          <| fun () ->
              let a1 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let a2 =
                  [| 0.0
                     0.0
                     0.0
                     0.0 |]
              let distance = Array.minkowski a1 a2 2.0
              Expect.isTrue distance.IsSome "Has Value"
              Expect.floatClose Accuracy.high distance.Value 0.0 "Should be equal" ]
