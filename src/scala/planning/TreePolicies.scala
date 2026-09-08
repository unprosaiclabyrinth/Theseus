import UbaModel.*

case class ActionEstimate(move: Move, mean: BigDecimal, visits: Int)

object TreePolicies:
  private def random[A](xs: Vector[A]): A = xs(scala.util.Random.nextInt(xs.size))

  def explorationBonus(parentVisits: Int, visits: Int, coefficient: Double): Double =
    require(parentVisits >= 0 && visits >= 0)
    if visits == 0 then Double.PositiveInfinity
    else coefficient * math.sqrt(math.log(math.max(1, parentVisits)) / visits)

  def select(
      actions: Vector[ActionEstimate],
      parentVisits: Int,
      belief: BeliefState,
      config: PlannerConfig
  ): Move =
    require(actions.nonEmpty, "Cannot select from an empty action set.")
    val ordered = actions.sortBy(_.move.ordinal)
    config.treePolicy match
      case TreePolicyKind.CanonicalUCT =>
        val unseen = ordered.filter(_.visits == 0)
        if unseen.nonEmpty then random(unseen).move
        else
          ordered
            .maxBy(a => a.mean.toDouble + explorationBonus(parentVisits, a.visits, config.exploration))
            .move
      case TreePolicyKind.HeuristicUCT =>
        val unseen = ordered.filter(a => a.visits == 0 && !RolloutPolicies.shots.contains(a.move))
        if unseen.nonEmpty then
          val preferred = RolloutPolicies.choose(belief, config.rollout)
          unseen.find(_.move == preferred).getOrElse(random(unseen)).move
        else
          ordered.maxBy { a =>
            a.mean.toDouble + config.exploration * domainExplorationBias(a.move, belief.history) *
              math.sqrt(math.log(math.max(1, parentVisits)) / (a.visits + 1))
          }.move

  /** Historical domain bias, retained as an explicit experimental strategy. */
  def domainExplorationBias(move: Move, history: History): Double = move match
    case Move.GoForward                                => 111
    case Move.GoLeft | Move.GoRight                    => 110
    case Move.GoBack                                   => 1
    case Move.Shoot | Move.ShootLeft | Move.ShootRight => 50 * history.countObs(_.stench)
    case Move.NoOp                                     => 50
    case Move.Grab                                     => 0

  def best(actions: Vector[ActionEstimate]): Move =
    require(actions.nonEmpty, "Cannot choose an action in a terminal tree.")
    val visited = actions.filter(_.visits > 0)
    if visited.isEmpty then actions.find(_.move == Move.NoOp).getOrElse(actions.minBy(_.move.ordinal)).move
    else visited.maxBy(a => (a.mean, a.visits, -a.move.ordinal)).move
