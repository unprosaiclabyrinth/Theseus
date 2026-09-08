class PlannerSuite extends munit.FunSuite:
  test("all rational sign normalization examples") {
    val cases = List((1,-2,-1,2), (-1,-2,1,2), (2,-4,-1,2), (0,-7,0,1), (-3,6,-1,2))
    cases.foreach { (n,d,rn,rd) =>
      val actual = new SimpleReflexAgent.Rational(n,d)
      assertEquals(actual, new SimpleReflexAgent.Rational(rn,rd))
      assert(actual.denom > 0)
    }
  }
