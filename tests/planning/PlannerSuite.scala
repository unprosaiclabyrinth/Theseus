class PlannerSuite extends munit.FunSuite:
  test("all rational sign normalization examples") {
    val cases = List((1,-2,-1,2), (-1,-2,1,2), (2,-4,-1,2), (0,-7,0,1), (-3,6,-1,2))
    cases.foreach { (n,d,rn,rd) =>
      val actual = new SimpleReflexAgent.Rational(n,d)
      assertEquals(actual, new SimpleReflexAgent.Rational(rn,rd))
      assert(actual.denom > 0)
    }
  }

  import UbaModel.*
  private val world = UnobservableWithWumpus((4,4), (4,2), (3,3), (3,4))
  private def belief(us: Set[Unobservable] = Set(world), at: Position = (2,2), o: Orientation = Orientation.East) =
    BeliefState(at, o, true, us, History.empty)

  test("orientation turns and movements agree in all four directions") {
    Orientation.values.foreach { o =>
      assertEquals(o.turnLeft.turnRight, o)
      assertEquals(o.turnBack.turnBack, o)
      assertEquals(o.turnLeft.turnLeft, o.turnBack)
      assertEquals(o.rightFrom((2,2)), o.turnRight.forwardFrom((2,2)))
      assertEquals(o.leftFrom((2,2)), o.turnLeft.forwardFrom((2,2)))
      assertEquals(o.backFrom((2,2)), o.turnBack.forwardFrom((2,2)))
      Vector(Move.GoForward -> o, Move.GoLeft -> o.turnLeft, Move.GoRight -> o.turnRight, Move.GoBack -> o.turnBack).foreach { (m, facing) =>
        val b = belief(o = o).transition(m)
        assertEquals(b.agentOrientation, facing)
        assertEquals(b.agentPosition, facing.forwardFrom((2,2)))
      }
    }
  }

  test("shots hit only their forward ray, from every orientation") {
    for o <- Orientation.values; m <- Vector(Move.Shoot, Move.ShootLeft, Move.ShootRight) do
      val facing = m match
        case Move.ShootLeft => o.turnLeft
        case Move.ShootRight => o.turnRight
        case _ => o
      for target <- Vector(facing.forwardFrom((2,2)), facing.backFrom((2,2)), (4,4)) do
        val u = world.copy(wumpus = target)
        val s = StateWithWumpus((2,2), o, true, u)
        val next = s.transition(m)
        assertEquals(next.isInstanceOf[StateSansWumpus], target == facing.forwardFrom((2,2)))
        assertEquals(GenerativeModel.observation(s, next).scream, next.isInstanceOf[StateSansWumpus])
  }

  test("kills retain posterior mass of distinct original worlds") {
    val worlds: Set[Unobservable] = Set(world.copy(wumpus=(2,1)), world.copy(wumpus=(3,1)), world.copy(wumpus=(4,1),gold=(4,3)))
    val posterior = belief(worlds, (1,1)).transition(Move.Shoot).observe(Percept4(false,false,false,true))
    assertEquals(posterior.belief.size, 3)
    assertEquals(posterior.belief.count { case u: UnobservableSansWumpus => u.gold == (4,4); case _ => false }, 2)
    scala.util.Random.setSeed(12)
    val count = (1 to 6000).count(_ => posterior.sampleState.asInstanceOf[StateSansWumpus].u.gold == (4,4))
    assert(math.abs(count / 6000.0 - 2.0/3) < .035)
  }

  test("observations exactly filter stench breeze and glitter") {
    val us: Set[Unobservable] = Set(world, world.copy(wumpus=(2,3)), world.copy(pit1=(1,2)), world.copy(gold=(2,2)))
    val b = belief(us)
    for u <- us do
      val state = b.u2State(u)
      val obs = GenerativeModel.observation(state,state)
      assertEquals(b.observe(obs).belief, us.filter(v => GenerativeModel.observation(b.u2State(v),b.u2State(v)) == obs))
    intercept[IllegalArgumentException](belief(Set.empty).sampleState)
  }

  test("terminal rewards happen once and terminal rollouts return zero") {
    val s = StateWithWumpus((2,2),Orientation.East,true,world.copy(gold=(2,2)))
    val config = PlannerConfig(simulations=3, shaping=ShapingKind.None)
    assertEquals(GenerativeModel.generate(s,Move.Grab,config)._3, BigDecimal(1000))
    assertEquals(Won.transition(Move.Grab), Won)
    assertEquals(Won.reward(Move.Grab), 0)
    assertEquals(new POMCP(config).rollout(Won,belief(),0), BigDecimal(0))
    for (m, cost) <- Vector(Move.GoForward -> -1000, Move.GoLeft -> -1001, Move.GoBack -> -1002) do
      val destination = belief().transition(m).agentPosition
      val doomed = s.copy(u=world.copy(pit1=destination))
      assertEquals(doomed.reward(m), cost)
      assertEquals(doomed.transition(m).reward(m), 0)
  }

  test("walls and empty arrows agree with primitive costs") {
    val s = StateWithWumpus((1,1),Orientation.West,false,world)
    assertEquals(s.transition(Move.GoForward),s)
    assertEquals(s.reward(Move.Shoot),-1)
    assertEquals(s.reward(Move.ShootLeft),-2)
    assert(!s.transition(Move.ShootLeft).isInstanceOf[StateSansWumpus])
  }

  test("potential shaping telescopes with the configured discount") {
    val config = PlannerConfig(discount=.8)
    val s = StateWithWumpus((1,1),Orientation.East,true,world)
    val n = s.transition(Move.GoForward)
    val end = n.transition(Move.GoLeft)
    val actual = RewardModel.shaping(s,n,config) + BigDecimal(.8)*RewardModel.shaping(n,end,config)
    assertEquals(actual, BigDecimal(.64)*RewardModel.potential(end)-RewardModel.potential(s))
    assertEquals(RewardModel.shaping(s,n,config.copy(shaping=ShapingKind.None)),BigDecimal(0))
  }

  test("canonical UCT explores unseen actions and final choice uses mean then visits") {
    val candidates = Vector(ActionEstimate(Move.GoForward,10,100),ActionEstimate(Move.Shoot,9,0))
    assertEquals(TreePolicies.select(candidates,100,belief(),PlannerConfig(treePolicy=TreePolicyKind.CanonicalUCT)),Move.Shoot)
    assertEquals(TreePolicies.best(candidates),Move.GoForward)
    assertEquals(TreePolicies.best(Vector(ActionEstimate(Move.GoLeft,10,3),ActionEstimate(Move.GoRight,10,4))),Move.GoRight)
    assertEquals(TreePolicies.explorationBonus(1,1,2),0.0)
    assertEquals(TreePolicies.best(Vector(ActionEstimate(Move.NoOp,0,0))),Move.NoOp)
  }

  test("tree visit counts, incremental means and pruning remain consistent") {
    val planner = new POMCP(PlannerConfig(simulations=12,horizon=2,treePolicy=TreePolicyKind.CanonicalUCT,rollout=RolloutKind.Uniform,shaping=ShapingKind.None))
    planner.pruneTree(Percept4(false,false,false,false))
    scala.util.Random.setSeed(42)
    val move = planner.plan
    val tree = planner.Tree
    val snapshot = tree.snapshot
    assertEquals(snapshot(tree.root).visitedCount,12)
    assertEquals(snapshot(tree.root).children.values.map(snapshot(_).visitedCount).sum,11)
    val id = snapshot(tree.root).children(move)
    val old = tree.snapshot(id)
    tree.visit(id); tree.updateMeanValue(id,100)
    assertEquals(tree.snapshot(id).value,old.value+(BigDecimal(100)-old.value)/(old.visitedCount+1))
    planner.pruneTree(move)
    assertEquals(tree.snapshot(tree.root).parent,None)
    assert(!tree.snapshot.contains(snapshot.keys.min))
    val reachable = scala.collection.mutable.Set.empty[Int]
    def walk(n: Int): Unit = if reachable.add(n) then tree.snapshot(n).children.values.foreach(walk)
    walk(tree.root)
    assertEquals(reachable.toSet,tree.snapshot.keySet)
    planner.reset()
    assertEquals(tree.snapshot.size,1)
  }

  test("planner CLI rejects invalid or misplaced settings") {
    intercept[IllegalArgumentException](WorldApplication.parse(Array("--discount","NaN")))
    intercept[IllegalArgumentException](WorldApplication.parse(Array("--simulations","0")))
    intercept[IllegalArgumentException](WorldApplication.parse(Array("--agent","sra","--horizon","3")))
    val parsed=WorldApplication.parse(Array("--tree-policy","canonical","--rollout","uniform","--shaping","none","--simulations","250"))
    assertEquals(parsed.planner().simulations,250)
    assertEquals(parsed.planner().treePolicy,TreePolicyKind.CanonicalUCT)
  }
