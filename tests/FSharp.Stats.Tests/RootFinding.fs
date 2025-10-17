module RootFindingTests

open Expecto
open FSharp.Stats.Rootfinding

[<Tests>]
let brentTests =
    testList
        "RootFinding.Brent"
        [ testCase "simple linear function"
          <| fun () ->
              // f(x) = x - 2, root at x = 2
              let func x = x - 2.0
              let result = Brent.tryFindRoot func 0.0 5.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 2.0) < 1e-6) "Root should be approximately 2.0"
              | None -> failtest "Should find a root"

          testCase "simple quadratic function"
          <| fun () ->
              // f(x) = x^2 - 4, root at x = 2 (between 0 and 5)
              let func x = x * x - 4.0
              let result = Brent.tryFindRoot func 0.0 5.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 2.0) < 1e-6) "Root should be approximately 2.0"
              | None -> failtest "Should find a root"

          testCase "cubic function with single root"
          <| fun () ->
              // f(x) = x^3 - x - 2, root approximately at x = 1.5214
              let func x = x * x * x - x - 2.0
              let result = Brent.tryFindRoot func 1.0 2.0
              match result with
              | Some root ->
                  let fRoot = func root
                  Expect.isTrue (abs fRoot < 1e-6) "Function value at root should be approximately 0"
              | None -> failtest "Should find a root"

          testCase "transcendental function - cos(x) - x"
          <| fun () ->
              // f(x) = cos(x) - x, root approximately at x = 0.7391
              let func x = cos x - x
              let result = Brent.tryFindRoot func 0.0 1.0
              match result with
              | Some root ->
                  let fRoot = func root
                  Expect.isTrue (abs fRoot < 1e-6) "Function value at root should be approximately 0"
                  Expect.isTrue (abs (root - 0.7390851332) < 1e-6) "Root should be approximately 0.7391"
              | None -> failtest "Should find a root"

          testCase "exponential function - exp(x) - 2"
          <| fun () ->
              // f(x) = exp(x) - 2, root at x = ln(2) ≈ 0.6931
              let func x = exp x - 2.0
              let result = Brent.tryFindRoot func 0.0 1.0
              match result with
              | Some root -> Expect.isTrue (abs (root - log 2.0) < 1e-6) "Root should be approximately ln(2)"
              | None -> failtest "Should find a root"

          testCase "no sign change - returns None"
          <| fun () ->
              // f(x) = x^2 + 1, no real roots
              let func x = x * x + 1.0
              let result = Brent.tryFindRoot func 0.0 5.0
              Expect.isNone result "Should return None when no sign change"

          testCase "both bounds same sign - returns None"
          <| fun () ->
              // f(x) = x - 2, both bounds on same side of root
              let func x = x - 2.0
              let result = Brent.tryFindRoot func 3.0 5.0
              Expect.isNone result "Should return None when both bounds have same sign"

          testCase "root at lower bound"
          <| fun () ->
              // f(x) = x - 1, root exactly at lower bound
              let func x = x - 1.0
              let result = Brent.tryFindRoot func 1.0 2.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 1.0) < 1e-6) "Root should be approximately 1.0"
              | None -> failtest "Should find a root"

          testCase "root at upper bound"
          <| fun () ->
              // f(x) = x - 2, root exactly at upper bound
              let func x = x - 2.0
              let result = Brent.tryFindRoot func 1.0 2.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 2.0) < 1e-6) "Root should be approximately 2.0"
              | None -> failtest "Should find a root"

          testCase "negative root"
          <| fun () ->
              // f(x) = x + 3, root at x = -3
              let func x = x + 3.0
              let result = Brent.tryFindRoot func -5.0 0.0
              match result with
              | Some root -> Expect.isTrue (abs (root + 3.0) < 1e-6) "Root should be approximately -3.0"
              | None -> failtest "Should find a root"

          testCase "very steep function"
          <| fun () ->
              // f(x) = 10^4 * (x - 0.5), steep line with root at 0.5
              let func x = 10000.0 * (x - 0.5)
              let result = Brent.tryFindRoot func 0.0 1.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 0.5) < 1e-6) "Root should be approximately 0.5"
              | None -> failtest "Should find a root"

          testCase "very flat near root"
          <| fun () ->
              // f(x) = (x - 0.5)^5, very flat near root
              let func x =
                  let d = x - 0.5
                  d * d * d * d * d
              let result = Brent.tryFindRoot func 0.0 1.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 0.5) < 1e-5) "Root should be approximately 0.5"
              | None -> failtest "Should find a root"

          testCase "custom accuracy and iterations"
          <| fun () ->
              // f(x) = x^2 - 9, root at x = 3
              let func x = x * x - 9.0
              let result = Brent.tryFindRootWith 1e-10 200 func 0.0 5.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 3.0) < 1e-9) "Root should be very close to 3.0"
              | None -> failtest "Should find a root"

          testCase "trigonometric function - sin(x)"
          <| fun () ->
              // f(x) = sin(x), root at x = π in range [3, 4]
              let func x = sin x
              let result = Brent.tryFindRoot func 3.0 4.0
              match result with
              | Some root -> Expect.isTrue (abs (root - System.Math.PI) < 1e-6) "Root should be approximately π"
              | None -> failtest "Should find a root"

          testCase "polynomial with multiple roots - finds one in range"
          <| fun () ->
              // f(x) = (x - 1)(x - 3), has roots at 1 and 3
              let func x = (x - 1.0) * (x - 3.0)
              let result = Brent.tryFindRoot func 0.0 2.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 1.0) < 1e-6) "Root should be approximately 1.0"
              | None -> failtest "Should find a root"

          testCase "narrow search interval"
          <| fun () ->
              // f(x) = x - 0.5, very narrow search interval
              let func x = x - 0.5
              let result = Brent.tryFindRoot func 0.4 0.6
              match result with
              | Some root -> Expect.isTrue (abs (root - 0.5) < 1e-6) "Root should be approximately 0.5"
              | None -> failtest "Should find a root"

          testCase "wide search interval"
          <| fun () ->
              // f(x) = x - 50, wide search interval
              let func x = x - 50.0
              let result = Brent.tryFindRoot func -100.0 100.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 50.0) < 1e-6) "Root should be approximately 50.0"
              | None -> failtest "Should find a root"

          testCase "function with asymptote - 1/(x-0.5) - 2"
          <| fun () ->
              // f(x) = 1/(x-0.5) - 2, root at x = 1.0
              let func x = 1.0 / (x - 0.5) - 2.0
              let result = Brent.tryFindRoot func 0.6 2.0
              match result with
              | Some root -> Expect.isTrue (abs (root - 1.0) < 1e-6) "Root should be approximately 1.0"
              | None -> failtest "Should find a root"

          testCase "oscillating function in range"
          <| fun () ->
              // f(x) = sin(x) - 0.5, root approximately at x = 0.5236 (π/6)
              let func x = sin x - 0.5
              let result = Brent.tryFindRoot func 0.0 1.0
              match result with
              | Some root ->
                  let fRoot = func root
                  Expect.isTrue (abs fRoot < 1e-6) "Function value at root should be approximately 0"
                  Expect.isTrue (abs (root - asin 0.5) < 1e-6) "Root should be approximately arcsin(0.5)"
              | None -> failtest "Should find a root"

          testCase "validates convergence"
          <| fun () ->
              // f(x) = x^3 - 2x - 5, root approximately at x = 2.0946
              let func x = x * x * x - 2.0 * x - 5.0
              let result = Brent.tryFindRoot func 2.0 3.0
              match result with
              | Some root ->
                  let fRoot = func root
                  Expect.isTrue (abs fRoot < 1e-6) "Function value at root should converge to 0"
              | None -> failtest "Should find a root" ]
