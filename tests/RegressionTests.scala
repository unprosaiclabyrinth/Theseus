import java.io.{BufferedWriter, StringWriter}
import java.nio.file.Files
import java.lang.reflect.InvocationTargetException

object RegressionTests:
  private var passed = 0
  private def check(name: String)(test: => Unit): Unit =
    test
    passed += 1
    println(s"PASS: $name")
  private def rejects(test: => Any): Unit =
    var rejected = false
    try test catch { case _: Exception => rejected = true }
    assert(rejected, "Expected an exception")
  private def make(name: String, args: AnyRef*): AnyRef =
    Class.forName(name).getConstructors.find(_.getParameterCount == args.size).get.newInstance(args*)
  private def member(name: String, field: String): AnyRef = Class.forName(name).getField(field).get(null)
  private def call(obj: AnyRef, name: String, args: AnyRef*): AnyRef =
    val method = obj.getClass.getMethods.find(m => m.getName == name && m.getParameterCount == args.size).get
    method.invoke(obj, args*)
  private def emptyPercept = new TransferPercept(null):
    override def getBump() = false
    override def getBreeze() = false
    override def getStench() = false
    override def getGlitter() = false
    override def getScream() = false
  private def emptyWorld(): Array[Array[Array[Char]]] =
    val world = Array.fill(4, 4, 4)(' ')
    world(0)(0)(3) = '>'
    world

  def main(args: Array[String]): Unit =
    check("negative denominators preserve rational value and ordering") {
      val half = new SimpleReflexAgent.Rational(1, -2)
      assert(half.toBigDecimal == BigDecimal("-0.5"))
      assert(half == new SimpleReflexAgent.Rational(-1, 2))
      assert(half < new SimpleReflexAgent.Rational(0, 1))
      assert((new SimpleReflexAgent.Rational(1, 2) / half).toBigDecimal == -1)
    }
    check("weighted sampling supports huge denominators without allocation") {
      val denominator = BigInt(10).pow(40)
      val result = SimpleReflexAgent.probabilisticChoice(Map(
        "rare" -> SimpleReflexAgent.Probability(1, denominator),
        "common" -> SimpleReflexAgent.Probability(denominator - 1, denominator)))
      assert(Set("rare", "common").contains(result))
      rejects(SimpleReflexAgent.Probability(-1, 2))
    }
    check("rate limiter spaces quick calls and accepts slow calls") {
      var now = 0L
      var slept = List.empty[Long]
      val limiter = new RequestRateLimiter(4000, () => now, ms => { slept = slept :+ ms; now += ms * 1000000 })
      limiter.acquire()
      now += 1000000000L
      limiter.acquire()
      assert(slept == List(3000L))
      now += 5000000000L
      limiter.acquire()
      assert(slept == List(3000L))
    }
    check("LLM actions require exact valid JSON enum values") {
      assert(LlmResponse.decode("""{"best_action":"left","belief_state_after_action":"ok"}""")._1 == Action.TURN_LEFT)
      rejects(LlmResponse.decode("""{"best_action":"do not shoot; go forward","belief_state_after_action":"ok"}"""))
      rejects(LlmResponse.decode("""{"best_action":"FORWARD","belief_state_after_action":"ok"}"""))
      rejects(LlmResponse.decode("""{"best_action":"grab"}"""))
      rejects(LlmResponse.decode("not json"))
    }
    check("LLM trial reset clears history and shutdown makes no extra request") {
      var sizes = List.empty[Int]
      var closes = 0
      val fake = new CompletionClient:
        override def query(messages: ujson.Arr): String =
          sizes = sizes :+ messages.value.size
          """{"best_action":"forward","belief_state_after_action":"ok"}"""
        override def close(): Unit = closes += 1
      LLMBasedAgent.configureClient(fake, 1)
      assert(LLMBasedAgent.process(emptyPercept) == Action.GO_FORWARD)
      LLMBasedAgent.process(emptyPercept)
      LLMBasedAgent.reset()
      LLMBasedAgent.process(emptyPercept)
      LLMBasedAgent.stop()
      LLMBasedAgent.stop()
      assert(sizes == List(2, 4, 2))
      assert(closes == 1)
    }
    check("RLA grabs gold even when a movement is queued") {
      ReactiveLearningAgent.reset()
      ReactiveLearningAgent.process(emptyPercept)
      val glitter = new TransferPercept(null):
        override def getBump() = false
        override def getBreeze() = false
        override def getStench() = false
        override def getGlitter() = true
        override def getScream() = false
      assert(ReactiveLearningAgent.process(glitter) == Action.GRAB)
      ReactiveLearningAgent.reset()
    }
    check("RLA reset discards queued moves") {
      ReactiveLearningAgent.reset()
      val initial = ReactiveLearningAgent.process(emptyPercept)
      ReactiveLearningAgent.reset()
      assert(ReactiveLearningAgent.process(emptyPercept) == initial)
      assert(initial == Action.TURN_RIGHT)
      ReactiveLearningAgent.reset()
    }
    check("learning maximizes discrete likelihood instead of rounding") {
      assert(LearningEstimator.estimate(List(true, true, false, true, true, false)) == BigDecimal(1) / 3)
      assert(LearningEstimator.estimate(List.fill(15)(true)) == 1)
      assert(LearningEstimator.estimate(List.fill(14)(true) :+ false) == BigDecimal("0.8"))
    }
    check("UBA successful grab is terminal and can only pay once") {
      val u = make("UbaModel$UnobservableWithWumpus", (1,1), (4,4), (3,3), (3,4))
      val state = make("UbaModel$StateWithWumpus", (1,1), member("UbaModel$Orientation$", "East"), java.lang.Boolean.TRUE, u)
      val grab = member("UbaModel$Move$", "Grab")
      assert(call(state, "reward", grab) == Integer.valueOf(1000))
      val next = call(state, "transition", grab)
      assert(call(next, "isTerminal") == java.lang.Boolean.TRUE)
      assert(call(next, "reward", grab) == Integer.valueOf(0))
    }
    check("RLA shooting merges probability mass for identical successor states") {
      val one = make("ReactiveLearningAgent$UnobservableWithWumpus", (1,1), (2,2), (3,1), (3,3), (4,4), java.lang.Boolean.FALSE)
      val two = make("ReactiveLearningAgent$UnobservableWithWumpus", (1,1), (2,2), (4,1), (3,3), (4,4), java.lang.Boolean.FALSE)
      val miss = make("ReactiveLearningAgent$UnobservableWithWumpus", (1,1), (2,2), (1,4), (3,3), (4,4), java.lang.Boolean.FALSE)
      val state = make("ReactiveLearningAgent$BeliefState", member("ReactiveLearningAgent$Orientation$", "East"),
        java.lang.Boolean.TRUE, Map(one -> BigDecimal(1), two -> BigDecimal(1), miss -> BigDecimal(1)), member("ReactiveLearningAgent$Move$", "NoOp"))
      val next = call(state, "transition", member("ReactiveLearningAgent$Move$", "Shoot"))
      val weights = call(next, "belief").asInstanceOf[Map[Any, BigDecimal]].values.toList.sorted
      assert(weights.size == 2)
      assert((weights.head - BigDecimal(1)/3).abs < BigDecimal("1e-30"))
      assert((weights.last - BigDecimal(2)/3).abs < BigDecimal("1e-30"))
    }
    check("RLA consumes a scream before executing another queued action") {
      ReactiveLearningAgent.reset()
      val alive = make("ReactiveLearningAgent$UnobservableWithWumpus", (1,1), (2,2), (1,4), (3,3), (4,4), java.lang.Boolean.FALSE)
      val dead = make("ReactiveLearningAgent$UnobservableSansWumpus", (1,1), (2,2), (3,3), (4,4), java.lang.Boolean.FALSE)
      val belief = make("ReactiveLearningAgent$BeliefState", member("ReactiveLearningAgent$Orientation$", "East"),
        java.lang.Boolean.FALSE, Map(alive -> BigDecimal("0.5"), dead -> BigDecimal("0.5")), member("ReactiveLearningAgent$Move$", "Shoot"))
      val cls = ReactiveLearningAgent.getClass
      def field(suffix: String) =
        val f = cls.getDeclaredFields.find(_.getName.endsWith(suffix)).get
        f.setAccessible(true)
        f
      field("globB").set(null, belief)
      field("forwardProbability").set(null, BigDecimal(1) / 3)
      field("actionQueue").get(null).asInstanceOf[scala.collection.mutable.Queue[Int]].enqueue(Action.TURN_RIGHT)
      val learning = Class.forName("ReactiveLearningAgent$Learning$")
      learning.getDeclaredFields.find(_.getName.endsWith("state")).get.set(null,
        member("ReactiveLearningAgent$Learning$LearningState$", "Stop"))
      val scream = new TransferPercept(null):
        override def getBump() = false
        override def getBreeze() = false
        override def getStench() = false
        override def getGlitter() = false
        override def getScream() = true
      assert(ReactiveLearningAgent.process(scream) == Action.TURN_RIGHT)
      val posterior = call(field("globB").get(null), "belief").asInstanceOf[Map[Any, BigDecimal]]
      assert(posterior.size == 1 && posterior.keys.head.getClass.getName.endsWith("UnobservableSansWumpus"))
      ReactiveLearningAgent.reset()
    }
    check("environment clears the previous agent marker") {
      val world = new Environment(4, emptyWorld(), new BufferedWriter(new StringWriter()))
      val agent = new Agent(world, new TransferPercept(world), 1)
      world.placeAgent(agent)
      agent.goForward()
      world.placeAgent(agent)
      val field = classOf[Environment].getDeclaredField("wumpusWorld")
      field.setAccessible(true)
      val grid = field.get(world).asInstanceOf[Array[Array[Array[Char]]]]
      assert(grid.iterator.flatMap(_.iterator).count(_(3) != ' ') == 1)
    }
    check("CLI rejects invalid, missing, and unsupported values") {
      List(Array("-s"), Array("-n", "NaN"), Array("-n", "Infinity"), Array("-t", "0"),
        Array("-s", "-1"), Array("--oops"), Array("-d", "5"), Array("-a", "true"),
        Array("--agent", "bad"), Array("--agent", "uba", "-n", "0.8"),
        Array("-f", "same", "--scores", "same")).foreach(a => rejects(WorldApplication.parse(a)))
    }
    check("simulator propagates agent failures and rejects invalid actions") {
      val agent = new AgentFunction("broken", new AgentFunctionImpl:
        override def reset(): Unit = ()
        override def process(tp: TransferPercept): Int = throw new IllegalStateException("test failure")
      )
      val writer = new BufferedWriter(new StringWriter())
      rejects(new Simulation(new Environment(4, emptyWorld(), writer), 1, writer, 1, agent, new java.util.Random(1)))
      val invalid = new AgentFunction("invalid", new AgentFunctionImpl:
        override def reset(): Unit = ()
        override def process(tp: TransferPercept): Int = 99
      )
      rejects(new Simulation(new Environment(4, emptyWorld(), writer), 1, writer, 1, invalid, new java.util.Random(1)))
    }
    check("seeded stochastic evaluations are reproducible and preserve every trial") {
      val folder = Files.createTempDirectory("theseus-tests-")
      val first = folder.resolve("first.txt")
      val second = folder.resolve("second.txt")
      for path <- List(first, second) do
        assert(WorldApplication.run(Array("--agent", "sra", "-t", "20", "-s", "10", "-r", "42", "-n", "0.8", "--quiet", "-f", path.toString)) == 0)
      val a = Files.readString(folder.resolve("first.txt.scores.csv"))
      val b = Files.readString(folder.resolve("second.txt.scores.csv"))
      assert(a == b)
      assert(a.linesIterator.size == 21)
    }
    check("mixed evaluation retains every group's scores") {
      val output = Files.createTempDirectory("theseus-mixed-").resolve("out.txt")
      assert(WorldApplication.run(Array("--agent", "rla", "--mixed", "-t", "6", "-s", "1", "-r", "42", "--quiet", "-f", output.toString)) == 0)
      val rows = Files.readString(output.resolveSibling("out.txt.scores.csv")).linesIterator.drop(1).toList
      assert(rows.size == 6)
      assert(rows.map(_.split(",")(3)).groupBy(identity).values.forall(_.size == 2))
    }
    check("output failures return nonzero status") {
      val missing = Files.createTempDirectory("theseus-missing-").resolve("absent/out.txt")
      assert(WorldApplication.run(Array("-f", missing.toString)) == 1)
    }
    println(s"$passed regression tests passed.")
