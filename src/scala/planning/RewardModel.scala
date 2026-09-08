import UbaModel.*

object RewardModel:
  /** Simulator score only; shaping never changes the reported environment score. */
  def environment(state: State, move: Move, successor: State): Int =
    if state.isTerminal then 0
    else if successor == Won then 1000
    else if successor.isTerminal then -1000 - (move match
      case Move.GoLeft | Move.GoRight => 1
      case Move.GoBack => 2
      case _ => 0)
    else if RolloutPolicies.shots.contains(move) && (state match
      case s: StateWithWumpus => !s.hasArrow
      case _ => true) then if move == Move.Shoot then -1 else -2
    else move match
      case Move.GoForward | Move.Grab => -1
      case Move.GoLeft | Move.GoRight => -2
      case Move.GoBack => -3
      case Move.Shoot => -10
      case Move.ShootLeft | Move.ShootRight => -11
      case Move.NoOp => 0

  def potential(state: State): BigDecimal = state match
    case s if s.isTerminal => 0
    case s: StateWithWumpus => -4 * manhattanDistance(s.u.gold, s.agentPosition)
    case s: StateSansWumpus => -4 * manhattanDistance(s.u.gold, s.agentPosition) + 9
    case Won => 0

  def shaping(state: State, successor: State, config: PlannerConfig): BigDecimal =
    if state.isTerminal then 0
    else config.shaping match
      case ShapingKind.None => 0
      case ShapingKind.Legacy => potential(successor) - potential(state)
      case ShapingKind.Potential => BigDecimal(config.discount) * potential(successor) - potential(state)
