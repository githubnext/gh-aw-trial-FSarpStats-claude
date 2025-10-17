module Optimization


open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Optimization




[<Tests>]
let NelderMeadTests =
    testList
        "Optimization.NelderMead"
        [

          let poly (xs: vector) = System.Math.Pow(xs[0], 2)

          // Rosenbrock's valley or Rosenbrock's banana function
          let rosenbrock (xs: vector) =
              let x, y = xs.[0], xs.[1]
              pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2

          // Fletcher and Powell's helic valley
          let fphv (x: vector) =
              100. * (x[2] - 10. * (atan2 x[1] x[0]) / (2. * Ops.pi)) ** 2.
              + (sqrt (x[0] ** 2. + x[1] ** 2.) - 1.) ** 2.
              + x[2] ** 2.

          // Powell's Singular Function (PSF)
          let psf (x: vector) =
              (x[0] + 10. * x[1]) ** 2.
              + 5. * (x[2] - x[3]) ** 2.
              + (x[1] - 2. * x[2]) ** 4.
              + 10. * (x[0] - x[3]) ** 4.

          testList
              "Test rosenbrock"
              [

                let x0 =
                    vector
                        [| 1.3
                           0.7
                           0.8
                           1.9
                           1.2 |]
                let nmc = NelderMead.NmConfig.defaultInit ()

                let optim = NelderMead.minimize nmc x0 rosenbrock

                test "rosenbrock: solution value" {
                    let expected = 0.0
                    let actual = optim.Solution
                    Expect.floatClose Accuracy.low actual expected "rosenbrock: solution did not match"
                }

                test "rosenbrock: x1" {
                    let expected = 1.0
                    let actual = optim.SolutionVector[0]
                    Expect.floatClose Accuracy.low actual expected "rosenbrock: x1 did not match"
                }

                test "rosenbrock: x2" {
                    let expected = 1.0
                    let actual = optim.SolutionVector[1]
                    Expect.floatClose Accuracy.low actual expected "rosenbrock: x2 did not match"
                } ]

          testList
              "Test Fletcher"
              [

                let x0 =
                    vector
                        [| -1.0
                           0.0
                           0.0 |]
                let nmc = NelderMead.NmConfig.defaultInit ()
                let optim =
                    NelderMead.minimizeWithStopCriteria
                        nmc
                        x0
                        fphv
                        { OptimizationStop.defaultStopCriteria with MinFunctionEpsilon = 1e-24 }

                //test "Fletcher: solution value" {
                //    let expected = 0.
                //    let actual   = optim.Solution
                //    Expect.floatClose Accuracy.low actual expected "fletcher: solution did not match"
                //}
                //seq [0.9999272578; -2.558463341e-05; -4.073735628e-05]; #  1 0 0
                testCase "Fletcher: solution vector"
                <| fun () ->
                    Expect.floatClose Accuracy.low optim.SolutionVector[0] 1. "fletcher: x1 did not match"
                    Expect.floatClose Accuracy.low optim.SolutionVector[1] 0. "fletcher: x2 did not match"
                    Expect.floatClose Accuracy.low optim.SolutionVector[2] 0. "fletcher: x3 did not match"

                ]

          testList
              "Test Powell's Singular Function"
              [

                let x0 =
                    vector
                        [| 3.0
                           -1.0
                           0.0
                           1.0 |]
                let nmc = NelderMead.NmConfig.defaultInit ()

                let optim = NelderMead.minimize nmc x0 psf

                test "Psf: solution value" {
                    let expected = 5.675294665e-09
                    let actual = optim.Solution
                    Expect.floatClose Accuracy.low actual expected "psf: solution did not match"
                }
                //
                testCase "v: solution vector"
                <| fun () ->
                    let expected =
                        [| -0.0005532762725
                           5.500401575e-05
                           -0.002250883404
                           -0.002282958824 |]

                    Expect.floatClose Accuracy.low optim.SolutionVector[0] expected[0] "psf: x1 did not match"
                    Expect.floatClose Accuracy.low optim.SolutionVector[1] expected[1] "psf: x2 did not match"
                    Expect.floatClose Accuracy.low optim.SolutionVector[2] expected[2] "psf: x3 did not match"
                    Expect.floatClose Accuracy.low optim.SolutionVector[3] expected[3] "psf: x4 did not match" ]

          ]

[<Tests>]
let BrentOptimizationTests =
    testList
        "Optimization.Brent"
        [

          // Test 1: Simple quadratic function - minimize x^2, minimum at x=0
          testCase "minimize simple quadratic"
          <| fun () ->
              let f x = x * x
              let result = Brent.minimize f -2.0 2.0
              match result with
              | Some x ->
                  Expect.floatClose Accuracy.medium x 0.0 "Minimum should be at x=0"
                  Expect.floatClose Accuracy.medium (f x) 0.0 "Function value at minimum should be 0"
              | None -> failtest "Should have found minimum"

          // Test 2: Shifted quadratic - minimize (x-3)^2, minimum at x=3
          testCase "minimize shifted quadratic"
          <| fun () ->
              let f x = (x - 3.0) * (x - 3.0)
              let result = Brent.minimize f 0.0 10.0
              match result with
              | Some x ->
                  Expect.floatClose Accuracy.high x 3.0 "Minimum should be at x=3"
                  Expect.floatClose Accuracy.high (f x) 0.0 "Function value at minimum should be 0"
              | None -> failtest "Should have found minimum"

          // Test 3: Cubic function with minimum
          testCase "minimize cubic function"
          <| fun () ->
              let f x = x * x * x - 6.0 * x * x + 9.0 * x + 1.0
              // Derivative: 3x^2 - 12x + 9 = 0, roots at x=1 and x=3
              // Second derivative: 6x - 12, at x=3 positive (minimum)
              let result = Brent.minimize f 2.0 5.0
              match result with
              | Some x -> Expect.floatClose Accuracy.medium x 3.0 "Minimum should be near x=3"
              | None -> failtest "Should have found minimum"

          // Test 4: Transcendental function - minimize sin(x)
          testCase "minimize transcendental function"
          <| fun () ->
              let f x = sin (x)
              // Minimum of sin(x) occurs at x = 3π/2 + 2πk
              // In [4, 5], minimum is at 3π/2 ≈ 4.71239
              let result = Brent.minimize f 4.0 5.0
              match result with
              | Some x ->
                  let expected = 1.5 * System.Math.PI
                  Expect.floatClose Accuracy.medium x expected "Minimum should be at 3π/2"
                  Expect.floatClose Accuracy.medium (f x) -1.0 "Function value should be -1"
              | None -> failtest "Should have found minimum"

          // Test 5: Exponential function
          testCase "minimize exponential with quadratic"
          <| fun () ->
              let f x = exp (x * x)
              let result = Brent.minimize f -1.0 1.0
              match result with
              | Some x ->
                  Expect.floatClose Accuracy.medium x 0.0 "Minimum should be at x=0"
                  Expect.floatClose Accuracy.medium (f x) 1.0 "Function value should be e^0=1"
              | None -> failtest "Should have found minimum"

          // Test 6: Minimum at boundary (lower)
          testCase "minimize at lower bound"
          <| fun () ->
              let f x = x * x // Minimum at 0, but we search [2, 5]
              let result = Brent.minimize f 2.0 5.0
              match result with
              | Some x -> Expect.floatClose Accuracy.high x 2.0 "Should find minimum at lower bound"
              | None -> failtest "Should have found minimum"

          // Test 7: Minimum at boundary (upper)
          testCase "minimize at upper bound"
          <| fun () ->
              let f x = -x * x // Maximum at 0, so minimum at edges
              let result = Brent.minimize f -5.0 -2.0
              match result with
              | Some x -> Expect.floatClose Accuracy.high x -5.0 "Should find minimum at lower bound (larger |x|)"
              | None -> failtest "Should have found minimum"

          // Test 8: Test with custom tolerance and iterations
          testCase "minimizeWith custom parameters"
          <| fun () ->
              let f x = (x - 1.5) * (x - 1.5)
              let result = Brent.minimizeWith f 0.0 5.0 1e-10 200
              match result with
              | Some x -> Expect.floatClose Accuracy.veryHigh x 1.5 "Should find minimum with high precision"
              | None -> failtest "Should have found minimum"

          // Test 9: Test with few iterations (should reach max)
          testCase "minimizeWith insufficient iterations"
          <| fun () ->
              let f x = (x - 1.0) * (x - 1.0)
              let result = Brent.minimizeWith f -100.0 100.0 1e-10 1
              match result with
              | Some _ -> failtest "Should return None when max iterations reached"
              | None -> () // Expected

          // Test 10: Test maximize (find maximum of negative quadratic)
          testCase "maximize negative quadratic"
          <| fun () ->
              let f x = -(x - 2.0) * (x - 2.0) + 5.0
              let result = Brent.maximize f -5.0 10.0
              match result with
              | Some x ->
                  Expect.floatClose Accuracy.high x 2.0 "Maximum should be at x=2"
                  Expect.floatClose Accuracy.high (f x) 5.0 "Function value at maximum should be 5"
              | None -> failtest "Should have found maximum"

          // Test 11: Test maximizeWith
          testCase "maximizeWith custom parameters"
          <| fun () ->
              let f x = -x * x + 4.0 * x - 1.0
              // Derivative: -2x + 4 = 0, maximum at x=2
              let result = Brent.maximizeWith f 0.0 5.0 1e-8 100
              match result with
              | Some x -> Expect.floatClose Accuracy.high x 2.0 "Maximum should be at x=2"
              | None -> failtest "Should have found maximum"

          // Test 12: Error handling - NaN lower bound
          testCase "error on NaN lower bound"
          <| fun () ->
              let f x = x * x
              Expect.throws (fun () -> Brent.minimize f nan 5.0 |> ignore) "Should throw on NaN lower bound"

          // Test 13: Error handling - Infinity upper bound
          testCase "error on infinity upper bound"
          <| fun () ->
              let f x = x * x
              Expect.throws (fun () -> Brent.minimize f 0.0 infinity |> ignore) "Should throw on infinity upper bound"

          // Test 14: Error handling - NaN tolerance
          testCase "error on NaN tolerance"
          <| fun () ->
              let f x = x * x
              Expect.throws (fun () -> Brent.minimizeWith f 0.0 5.0 nan 100 |> ignore) "Should throw on NaN tolerance"

          // Test 15: Error handling - zero tolerance
          testCase "error on zero tolerance"
          <| fun () ->
              let f x = x * x
              Expect.throws (fun () -> Brent.minimizeWith f 0.0 5.0 0.0 100 |> ignore) "Should throw on zero tolerance"

          // Test 16: Error handling - negative tolerance
          testCase "error on negative tolerance"
          <| fun () ->
              let f x = x * x
              Expect.throws
                  (fun () -> Brent.minimizeWith f 0.0 5.0 -0.1 100 |> ignore)
                  "Should throw on negative tolerance"

          // Test 17: Reversed bounds (should auto-swap)
          testCase "minimize with reversed bounds"
          <| fun () ->
              let f x = (x - 1.0) * (x - 1.0)
              let result = Brent.minimize f 5.0 -5.0 // Reversed
              match result with
              | Some x -> Expect.floatClose Accuracy.high x 1.0 "Should handle reversed bounds"
              | None -> failtest "Should have found minimum"

          // Test 18: Very narrow interval
          testCase "minimize in narrow interval"
          <| fun () ->
              let f x = x * x
              let result = Brent.minimize f -0.1 0.1
              match result with
              | Some x -> Expect.floatClose Accuracy.high x 0.0 "Should find minimum in narrow interval"
              | None -> failtest "Should have found minimum"

          // Test 19: Error handling - function returns NaN
          testCase "error when function returns NaN"
          <| fun () ->
              let f x = if x > 0.5 then nan else x * x
              Expect.throws (fun () -> Brent.minimize f 0.0 2.0 |> ignore) "Should throw when function returns NaN"

          // Test 20: Error handling - function returns Infinity
          testCase "error when function returns infinity"
          <| fun () ->
              let f x = if x > 0.5 then infinity else x * x
              Expect.throws (fun () -> Brent.minimize f 0.0 2.0 |> ignore) "Should throw when function returns infinity"

          // Test 21: Cosine function (periodic)
          testCase "minimize cosine function"
          <| fun () ->
              let f x = cos (x)
              let result = Brent.minimize f 2.0 4.0
              match result with
              | Some x ->
                  // Minimum of cos(x) in [2, 4] is at π ≈ 3.14159
                  Expect.floatClose Accuracy.medium x System.Math.PI "Should find minimum at π"
                  Expect.floatClose Accuracy.medium (f x) -1.0 "Function value should be -1"
              | None -> failtest "Should have found minimum"

          // Test 22: Maximize cosine function
          testCase "maximize cosine function"
          <| fun () ->
              let f x = cos (x)
              let result = Brent.maximize f -1.0 1.0
              match result with
              | Some x ->
                  // Maximum of cos(x) in [-1, 1] is at x=0
                  Expect.floatClose Accuracy.high x 0.0 "Should find maximum at 0"
                  Expect.floatClose Accuracy.high (f x) 1.0 "Function value should be 1"
              | None -> failtest "Should have found maximum"

          // Test 23: Quartic function
          testCase "minimize quartic function"
          <| fun () ->
              let f x = (x - 1.0) ** 4.0 + (x - 2.0) ** 2.0
              let result = Brent.minimize f 0.0 3.0
              match result with
              | Some x ->
                  // This has a complex minimum, but should find something reasonable
                  Expect.isTrue (x > 1.0 && x < 2.0) "Should find minimum between 1 and 2"
              | None -> failtest "Should have found minimum"

          // Test 24: Absolute value function (non-smooth)
          testCase "minimize absolute value"
          <| fun () ->
              let f x = abs (x - 2.5)
              let result = Brent.minimize f 0.0 5.0
              match result with
              | Some x -> Expect.floatClose Accuracy.medium x 2.5 "Should find minimum at x=2.5"
              | None -> failtest "Should have found minimum" ]
