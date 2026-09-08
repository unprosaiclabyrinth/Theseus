/** Preserve the existing regression checks while adding MUnit algorithm suites. */
class RegressionSuite extends munit.FunSuite:
  test("existing simulator, RLA, LLM, arithmetic, and shell-era regressions") {
    RegressionTests.main(Array.empty)
  }
