import UbaModel.*

  /**
   * Implement partially observable Monte Carlo planning (POMCP)
   */
class POMCP(val config: PlannerConfig):
  val Tree = new SearchTree(config)
  private val TIME_HORIZON = config.horizon
  private val NUM_SIMULATIONS = config.simulations
  private val DISCOUNT = BigDecimal(config.discount)

  /**
   * THE SEARCH METHOD
   * Searches for the best move in the current belief state using the POMCP algorithm
   * @return the best move (according to it)
   */
  def plan: Move =
    (1 to NUM_SIMULATIONS).foreach(_ =>
      // sample a state
      val s: State = Tree.beliefStateAt(Tree.root).sampleState
      simulate(s, Tree.root, 0)
    )
    val bestMove = Tree.bestMove(Tree.root)
    bestMove

  /**
   * Traverse the MCST until a leaf following best moves using the selection policy,
   * simulate a rollout at the leaf, and back-propagate the results tonthe root.
   * @param s a (deterministic) state sampled from the current belief state.
   * @param n the node index corresponding to the current belief state.
   * @param depth the depth of the current node.
   * @return the aggregate utility calculated as sum of the immediate reward and the discounted utility
   */
  private def simulate(s: State, n: Int, depth: Int): BigDecimal =
    if depth >= TIME_HORIZON || (config.discount == 0 && depth > 0) then 0.0
    else if s.isTerminal then 0.0
    else if Tree.isLeaf(n) then
      Tree.beliefStateAt(n).possibleMoves.foreach(m => Tree.expandFrom(n, m))
      val playoutVal: BigDecimal = rollout(s, Tree.beliefStateAt(n), depth)
      Tree.visit(n)
      Tree.setValue(n, playoutVal)
      playoutVal
    else
      val (nextMove, nextNode) = Tree.selectionPolicy(n)
      val (successor, o, r) = generate(s, nextMove)
      val utility = if successor.isTerminal then r
      else r + (DISCOUNT * simulate(successor, Tree.getObsNode(nextNode, o), depth + 1))
      Tree.visit(n)
      Tree.updateMeanValue(n, utility)
      Tree.visit(nextNode)
      Tree.updateMeanValue(nextNode, utility)
      utility

  /**
   * Define a rollout policy that dictates that choice of the next move during a rollout given the belief state
   * @param b a belief state
   * @return a move
   */
  private def rolloutPolicy(b: BeliefState): Move = RolloutPolicies.choose(b, config.rollout)

  /**
   * Generate a 3-tuple of a (state, percept, reward) given a state and move executed in that state.
   * Simply generates the successor state, the computed percept in the successor state, and the immediate
   * reward + heuristic of executing the given move in the given state.
   * @param s a state
   * @param m a move
   * @return a 3-tuple of (state, percept, reward)
   */
  def generate(s: State, m: Move): (State, Percept4, BigDecimal) = GenerativeModel.generate(s, m, config)

  /**
   * Recursively simulate a rollout until a specified time horizon.
   * @param s a state
   * @param b the belief state from which that state has been sampled
   * @param depth the current depth of the simulation (between 0 and the time horizon)
   * @return the value of the rollout computed as the sum of the immediate reward and the discounted utility
   */
  def rollout(s: State, b: BeliefState, depth: Int): BigDecimal =
    if depth >= TIME_HORIZON || (config.discount == 0 && depth > 0) then 0.0
    else if s.isTerminal then 0.0
    else if b.isTerminal then throw new IllegalStateException("Empty belief for a live rollout.")
    else
      val m = rolloutPolicy(b)
      val (successor, o, r) = generate(s, m)
      if successor.isTerminal then r
      else r + (DISCOUNT * rollout(successor, b.transition(m).observe(o), depth + 1))

  /**
   * Prune the MCST given the update at the root.
   * @param update the update at the root.
   */
  def pruneTree(update: Move | Percept4): Unit = Tree.prune(update)

  /**
   * Reset the MCST.
   */
  def reset(): Unit = Tree.reset()

  /**
   * Dump the tree to stdout for debugging.
   * (WARNING: don't use unless on the verge of dying of frustration while debugging).
   */
  def dumpTree(): Unit = Tree.dump()

