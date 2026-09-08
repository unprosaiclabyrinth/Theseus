import java.nio.file.Files

/** Reproducibility and outcome invariants, not a brittle golden policy score. */
class SeededPlannerSuite extends munit.FunSuite:
  test("ten fixed worlds reproduce planner outcomes across independent runs") {
    val folder = Files.createTempDirectory("theseus-seeded-planner-")
    def run(name: String): Vector[Vector[String]] =
      val output = folder.resolve(name)
      assertEquals(
        WorldApplication.run(
          Array(
            "--agent",
            "uba",
            "-t",
            "10",
            "-r",
            "42",
            "-s",
            "20",
            "--simulations",
            "30",
            "--horizon",
            "5",
            "--quiet",
            "-f",
            output.toString
          )
        ),
        0
      )
      Files
        .readString(folder.resolve(name + ".scores.csv"))
        .linesIterator
        .drop(1)
        .map(_.split(",").dropRight(1).toVector)
        .toVector
    val first = run("first")
    assertEquals(first, run("second"))
    first.foreach { row =>
      val score = row(4).toInt
      val actions = row(5).toInt
      val arrows = row(6).toInt
      val kills = row(7).toInt
      assert(actions <= 20 && arrows <= 1 && kills <= arrows)
      assert(!(row(8) == "true" && row(9) == "true"))
      if row(9) == "true" then assert(score > 900)
      if row(8) == "true" then assert(score <= -1000)
    }
  }
