module InterpolationTests

open Expecto
open FSharp.Stats
open FSharp.Stats.Interpolation

open TestExtensions

[<Tests>]
let cubicInterpolationTests =
    let t =
        vector
            [ 0.0
              1.0
              2.0
              3.0 ]
    let y =
        vector
            [ 187.6
              185.7
              193.7
              197.0 ]
    let tt =
        vector
            [ 0.0
              0.25
              0.5
              0.75
              1.
              1.25
              1.5
              1.75
              2.
              2.25
              2.5
              2.75
              3.0 ]

    let u =
        vector
            [ 1.0
              4.0
              9.0
              16.0 ]
    let t2 =
        vector
            [ 1.0
              2.0
              3.0
              4.0 ]

    testList
        "Interpolation.CubicSpline"
        [ testCase "Natural Cubic Spline"
          <| fun () ->
              //Verifies that the fitted point match the expectred fittied points
              //https://columbiaeconomics.com/2010/01/20/how-economists-convert-quarterly-data-into-monthly-cubic-spline-interpolation/comment-page-1/
              let coefficientsSpline = CubicSpline.interpolate CubicSpline.Natural t y
              let fitOutPut = tt |> Vector.map (CubicSpline.predict coefficientsSpline)
              let expectedValues =
                  vector
                      [ 187.6
                        186.4328125
                        185.5425
                        185.2059375
                        185.7
                        187.179375
                        189.31
                        191.635625
                        193.7
                        195.1528125
                        196.0675
                        196.6234375
                        197.0 ]
              TestExtensions.sequenceEqual
                  Accuracy.low
                  expectedValues
                  fitOutPut
                  "Fitted Values and Expected Output should be equal (double precision)"

          testCase "Quadratic Cubic Spline"
          <| fun () ->
              let coefficientsQuadraticSpline = CubicSpline.interpolate CubicSpline.Quadratic t2 u
              let fittingFunc x =
                  CubicSpline.predictWithinRange coefficientsQuadraticSpline x
              Expect.floatClose Accuracy.high (fittingFunc 1.5) 2.25 "Fitted Value should be equal (double precision)"
              Expect.floatClose Accuracy.high (fittingFunc 2.5) 6.25 "Fitted Value should be equal (double precision)"
              Expect.floatClose Accuracy.high (fittingFunc 3.5) 12.25 "Fitted Value should be equal (double precision)"

          let seriesx =
              [| 20.15
                 24.41
                 28.78 |]
              |> Array.sort
              |> vector
          let seriesy =
              [| 0.367
                 0.591
                 0.796 |]
              |> Array.sort
              |> vector
          testCase "Parabolic Cubic Interpolation"
          <| fun () ->
              //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_cubic_spline_interpolation.html%23
              let coeffParabolic = CubicSpline.interpolate CubicSpline.Parabolic seriesx seriesy
              let fittingFuncParabolic x = CubicSpline.predict coeffParabolic x

              let genrateX = vector [ 20.0 .. 25.0 ]
              let interpParabolic = genrateX |> Vector.map fittingFuncParabolic
              let parabolicSndDeriv x =
                  CubicSpline.getSecondDerivative coeffParabolic x

              Expect.floatClose
                  Accuracy.high
                  (parabolicSndDeriv interpParabolic.[0])
                  (parabolicSndDeriv interpParabolic.[1])
                  "the second derivative at the first and second points should be equal (double precision)" ]

[<Tests>]
let akimaInterpolationTests =
    let t =
        vector
            [ 0.0
              1.0
              2.0
              3.0 ]
    let y =
        vector
            [ 187.6
              185.7
              193.7
              197.0 ]
    let tt =
        vector
            [ 0.0
              0.25
              0.5
              0.75
              1.
              1.25
              1.5
              1.75
              2.
              2.25
              2.5
              2.75
              3.0 ]

    let u =
        vector
            [ 1.0
              4.0
              9.0
              16.0 ]
    let t2 =
        vector
            [ 1.0
              2.0
              3.0
              4.0 ]

    testList
        "Interpolation.CubicSpline"
        [

          let values =
              [| 0.0
                 2.0
                 1.0
                 3.0
                 2.0
                 6.0
                 5.5
                 5.5
                 2.7
                 5.1
                 3.0 |]
          let time = [| 0.0 .. 10.0 |]
          testCase "Akima Interpolation"
          <| fun () ->
              let splineCoefsAkima = Akima.interpolate time values
              let fittingFuncAkima x = Akima.predict splineCoefsAkima x
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 0.5)
                  1.375
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 1.0)
                  2.0
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 1.5)
                  1.5
                  "Fitted Value  should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 2.5)
                  1.953125
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 3.5)
                  2.484375
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.medium
                  (fittingFuncAkima 4.5)
                  4.136363
                  "Fitted Value should be equal (double precision)"

          ]

[<Tests>]
let polynomialInterpolationTests =
    testList
        "Interpolation.Polynomial"
        [ let datax =
              vector
                  [ 301.0
                    306.0
                    318.0
                    332.0
                    333.0 ]
          let datay =
              vector
                  [ 0.02
                    0.2
                    -0.04
                    0.06
                    0.17 ]

          testCase "Polynomial Interpolation"
          <| fun () ->
              //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_polynomial_interpolation.html%23wwID0E3LVS
              let coeffs = Polynomial.interpolate datax datay
              let expectedCoeffs =
                  [ 18489.1150794045
                    -249.9950165
                    1.2620688143
                    -0.0028205075
                    0.0000023552 ]
              let polyInterpFit x = Polynomial.predict coeffs x
              Expect.floatClose
                  Accuracy.high
                  (polyInterpFit 328.0)
                  -0.1894337636
                  "Fitted Value should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (datax |> Seq.map polyInterpFit)
                  datay
                  "Fitted Value should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  coeffs.C0_CX
                  expectedCoeffs
                  "Coefficients should be equal (double precision)" ]

[<Tests>]
let BezierInterpolationTests =
    testList
        "Interpolation.Bezier"
        [

          testCase "Bezier Interpolation of degree 1"
          <| fun () ->
              // Without control point, this is just linear interpolation
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     p1 |]
              let interpolate = Bezier.interpolate data
              let expectedMiddle = p0 + 0.5 * (p1 - p0)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          testCase "Bezier Interpolation of degree 2"
          <| fun () ->
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let c0 =
                  vector
                      [| 1.1
                         2.1
                         2. |] //control point 0
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     c0
                     p1 |]
              let interpolate = Bezier.interpolate data
              let a = p0 + 0.5 * (c0 - p0)
              let b = c0 + 0.5 * (p1 - c0)
              let expectedMiddle = a + 0.5 * (b - a)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          testCase "Bezier Interpolation of degree 3"
          <| fun () ->
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let c0 =
                  vector
                      [| 1.1
                         2.1
                         2. |] //control point 0
              let c1 =
                  vector
                      [| 3.8
                         1.6
                         1.4 |] //control point 1
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     c0
                     c1
                     p1 |]
              let interpolate = Bezier.interpolate data
              let a = p0 + 0.5 * (c0 - p0)
              let b = c0 + 0.5 * (c1 - c0)
              let c = c1 + 0.5 * (p1 - c1)
              let d = a + 0.5 * (b - a)
              let e = b + 0.5 * (c - b)
              let expectedMiddle = d + 0.5 * (e - d)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          ]

[<Tests>]
let linearSplineInterpolationTests =
    testList
        "Interpolation.LinearSpline"
        [
          // Basic interpolation tests
          testCase "LinearSpline interpolateSorted - simple linear data"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3.
                     4. |]
              let yData =
                  [| 0.
                     2.
                     4.
                     6.
                     8. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              // Test exact knot predictions
              Expect.floatClose Accuracy.high (coefs.Predict 0.) 0. "Should predict exact value at x=0"
              Expect.floatClose Accuracy.high (coefs.Predict 1.) 2. "Should predict exact value at x=1"
              Expect.floatClose Accuracy.high (coefs.Predict 2.) 4. "Should predict exact value at x=2"

              // Test interpolated value
              Expect.floatClose Accuracy.high (coefs.Predict 1.5) 3. "Should interpolate correctly"
              Expect.floatClose Accuracy.high (coefs.Predict 2.7) 5.4 "Should interpolate correctly"

          testCase "LinearSpline interpolateSorted - non-uniform spacing"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     5.
                     7.
                     10. |]
              let yData =
                  [| 1.
                     5.
                     9.
                     13.
                     16. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              // Between x=1 and x=5: slope = (9-5)/(5-1) = 1.0
              Expect.floatClose
                  Accuracy.high
                  (coefs.Predict 3.)
                  7.
                  "Should interpolate correctly with non-uniform spacing"

          testCase "LinearSpline interpolate - unsorted data"
          <| fun () ->
              let xData =
                  [| 0.
                     3.
                     1.
                     4.
                     2. |]
              let yData =
                  [| 0.
                     6.
                     2.
                     8.
                     4. |]
              let coefs = LinearSpline.interpolate xData yData

              // Should sort and then interpolate correctly
              Expect.floatClose Accuracy.high (coefs.Predict 1.5) 3. "Should handle unsorted data"
              Expect.floatClose Accuracy.high (coefs.Predict 2.5) 5. "Should handle unsorted data"

          testCase "LinearSpline predict - extrapolation beyond upper bound"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              // Should use last segment slope for extrapolation
              Expect.floatClose Accuracy.high (coefs.Predict 3.) 6. "Should extrapolate using last segment"

          testCase "LinearSpline predict - extrapolation below lower bound"
          <| fun () ->
              let xData =
                  [| 1.
                     2.
                     3. |]
              let yData =
                  [| 2.
                     4.
                     6. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              // Should use first segment for extrapolation
              Expect.floatClose Accuracy.high (coefs.Predict 0.) 0. "Should extrapolate using first segment"

          testCase "LinearSpline differentiate - constant slope"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 1.
                     3.
                     5.
                     7. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              // Slope should be 2.0 everywhere
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 0.5) 2. "Slope should be constant"
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 1.5) 2. "Slope should be constant"
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 2.5) 2. "Slope should be constant"

          testCase "LinearSpline differentiate - varying slopes"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     4.
                     9. |] // y=x² at the knots
              let coefs = LinearSpline.interpolateSorted xData yData

              // Between x=0 and x=1: slope = 1
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 0.5) 1. "Slope in first segment"
              // Between x=1 and x=2: slope = 3
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 1.5) 3. "Slope in second segment"
              // Between x=2 and x=3: slope = 5
              Expect.floatClose Accuracy.high (LinearSpline.differentiate coefs 2.5) 5. "Slope in third segment"

          testCase "LinearSpline member Differentiate"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coefs = LinearSpline.interpolateSorted xData yData
              let derivCoefs = coefs.Differentiate()

              // Derivative should have slopes as C0 and zeros as C1
              // The Differentiate method appends the last slope, so length is same as original
              Expect.equal derivCoefs.C0.Length 3 "Should have correct number of coefficients"
              Expect.floatClose Accuracy.high derivCoefs.C0.[0] 2. "First derivative coefficient"
              Expect.floatClose Accuracy.high derivCoefs.C0.[1] 2. "Second derivative coefficient"
              Expect.floatClose Accuracy.high (derivCoefs.Predict 0.5) 2. "Derivative should predict slope"

          testCase "LinearSpline member getDerivative"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     4.
                     9. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              Expect.floatClose Accuracy.high (coefs.getDerivative 0.5) 1. "Member getDerivative should work"
              Expect.floatClose Accuracy.high (coefs.getDerivative 1.5) 3. "Member getDerivative should work"

          testCase "LinearSpline interpolateInplace - modifies arrays"
          <| fun () ->
              let xData =
                  [| 3.
                     1.
                     2. |]
              let yData =
                  [| 6.
                     2.
                     4. |]
              let coefs = LinearSpline.interpolateInplace xData yData

              // Data should be sorted after interpolation
              Expect.equal xData.[0] 1. "X data should be sorted"
              Expect.equal xData.[1] 2. "X data should be sorted"
              Expect.equal xData.[2] 3. "X data should be sorted"
              Expect.floatClose Accuracy.high (coefs.Predict 1.5) 3. "Should interpolate correctly"

          // Error handling tests
          testCase "LinearSpline interpolateSorted - array length mismatch"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1. |]
              Expect.throws
                  (fun () -> LinearSpline.interpolateSorted xData yData |> ignore)
                  "Should throw on length mismatch"

          testCase "LinearSpline interpolateSorted - insufficient data points"
          <| fun () ->
              let xData = [| 0. |]
              let yData = [| 0. |]
              Expect.throws
                  (fun () -> LinearSpline.interpolateSorted xData yData |> ignore)
                  "Should throw with single data point"

          testCase "LinearSpline interpolate - array length mismatch"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1. |]
              Expect.throws (fun () -> LinearSpline.interpolate xData yData |> ignore) "Should throw on length mismatch"

          testCase "LinearSpline interpolateInplace - array length mismatch"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1. |]
              Expect.throws
                  (fun () -> LinearSpline.interpolateInplace xData yData |> ignore)
                  "Should throw on length mismatch"

          testCase "LinearSpline - two point interpolation"
          <| fun () ->
              let xData =
                  [| 0.
                     1. |]
              let yData =
                  [| 0.
                     3. |]
              let coefs = LinearSpline.interpolateSorted xData yData

              Expect.floatClose Accuracy.high (coefs.Predict 0.5) 1.5 "Two-point interpolation" ]

[<Tests>]
let stepInterpolationTests =
    testList
        "Interpolation.Step"
        [ testCase "Step interpolateSorted - basic step function"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 1.
                     5.
                     9.
                     13. |]
              let coefs = Step.interpolateSorted xData yData

              // At exact knots, should return the value
              Expect.floatClose Accuracy.high (coefs.Predict 0.) 1. "Should predict exact value at x=0"
              Expect.floatClose Accuracy.high (coefs.Predict 1.) 5. "Should predict exact value at x=1"
              Expect.floatClose Accuracy.high (coefs.Predict 2.) 9. "Should predict exact value at x=2"

              // Between knots, should return the left value
              Expect.floatClose Accuracy.high (coefs.Predict 0.5) 1. "Should return left value between knots"
              Expect.floatClose Accuracy.high (coefs.Predict 1.5) 5. "Should return left value between knots"
              Expect.floatClose Accuracy.high (coefs.Predict 2.9) 9. "Should return left value between knots"

          testCase "Step interpolate - unsorted data"
          <| fun () ->
              let xData =
                  [| 2.
                     0.
                     3.
                     1. |]
              let yData =
                  [| 9.
                     1.
                     13.
                     5. |]
              let coefs = Step.interpolate xData yData

              // Should sort and then create step function correctly
              Expect.floatClose Accuracy.high (coefs.Predict 0.5) 1. "Should handle unsorted data"
              Expect.floatClose Accuracy.high (coefs.Predict 1.5) 5. "Should handle unsorted data"

          testCase "Step predict - extrapolation beyond upper bound"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     5.
                     9. |]
              let coefs = Step.interpolateSorted xData yData

              // Should return last value for extrapolation
              Expect.floatClose Accuracy.high (coefs.Predict 3.) 9. "Should extrapolate with last value"
              Expect.floatClose Accuracy.high (coefs.Predict 10.) 9. "Should extrapolate with last value"

          testCase "Step predict - extrapolation below lower bound"
          <| fun () ->
              let xData =
                  [| 1.
                     2.
                     3. |]
              let yData =
                  [| 5.
                     9.
                     13. |]
              let coefs = Step.interpolateSorted xData yData

              // Should return first value for extrapolation
              Expect.floatClose Accuracy.high (coefs.Predict 0.) 5. "Should extrapolate with first value"
              Expect.floatClose Accuracy.high (coefs.Predict -1.) 5. "Should extrapolate with first value"

          testCase "Step differentiate - always zero"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 1.
                     5.
                     9.
                     13. |]
              let coefs = Step.interpolateSorted xData yData

              // Derivative of step function is zero everywhere (except at discontinuities)
              Expect.floatClose Accuracy.high (Step.differentiate coefs 0.5) 0. "Derivative should be zero"
              Expect.floatClose Accuracy.high (Step.differentiate coefs 1.5) 0. "Derivative should be zero"
              Expect.floatClose Accuracy.high (Step.differentiate coefs 2.5) 0. "Derivative should be zero"

          testCase "Step member Differentiate"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     5.
                     9. |]
              let coefs = Step.interpolateSorted xData yData
              let derivCoefs = coefs.Differentiate()

              // All derivative coefficients should be zero
              Expect.equal derivCoefs.C0.Length 3 "Should have correct number of coefficients"
              Expect.floatClose Accuracy.high derivCoefs.C0.[0] 0. "Derivative coefficient should be zero"
              Expect.floatClose Accuracy.high derivCoefs.C0.[1] 0. "Derivative coefficient should be zero"
              Expect.floatClose Accuracy.high derivCoefs.C0.[2] 0. "Derivative coefficient should be zero"

          testCase "Step member getDerivative"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     5.
                     9. |]
              let coefs = Step.interpolateSorted xData yData

              // Member function should also return zero
              Expect.floatClose Accuracy.high (coefs.getDerivative 0.5) 0. "Member getDerivative should return zero"
              Expect.floatClose Accuracy.high (coefs.getDerivative 1.5) 0. "Member getDerivative should return zero"

          // Error handling tests
          testCase "Step interpolateSorted - array length mismatch"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     5. |]
              Expect.throws (fun () -> Step.interpolateSorted xData yData |> ignore) "Should throw on length mismatch"

          testCase "Step interpolate - array length mismatch"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     5. |]
              Expect.throws (fun () -> Step.interpolate xData yData |> ignore) "Should throw on length mismatch"

          testCase "Step - single point interpolation"
          <| fun () ->
              let xData = [| 0. |]
              let yData = [| 5. |]
              let coefs = Step.interpolateSorted xData yData

              // With a single point, should always return that value
              Expect.floatClose Accuracy.high (coefs.Predict 0.) 5. "Single point"
              Expect.floatClose Accuracy.high (coefs.Predict -1.) 5. "Single point extrapolation"
              Expect.floatClose Accuracy.high (coefs.Predict 1.) 5. "Single point extrapolation"

          testCase "Step - two point interpolation"
          <| fun () ->
              let xData =
                  [| 0.
                     1. |]
              let yData =
                  [| 2.
                     7. |]
              let coefs = Step.interpolateSorted xData yData

              Expect.floatClose Accuracy.high (coefs.Predict 0.) 2. "First point"
              Expect.floatClose Accuracy.high (coefs.Predict 0.5) 2. "Between points (left value)"
              Expect.floatClose Accuracy.high (coefs.Predict 1.) 7. "Second point" ]
