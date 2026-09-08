import UbaModel.*

object GenerativeModel:
  def observation(previous: State, successor: State): Percept4 = successor match
    case Won                => Percept4(false, false, false, false)
    case s: StateWithWumpus =>
      Percept4(
        neighborsOf(s.agentPosition).contains(s.u.wumpus),
        neighborsOf(s.agentPosition).intersect(s.u.pits).nonEmpty,
        s.agentPosition == s.u.gold,
        false
      )
    case s: StateSansWumpus =>
      Percept4(
        false,
        neighborsOf(s.agentPosition).intersect(s.u.pits).nonEmpty,
        s.agentPosition == s.u.gold,
        previous.isInstanceOf[StateWithWumpus]
      )

  def generate(state: State, move: Move, config: PlannerConfig): (State, Percept4, BigDecimal) =
    val successor = state.transition(move)
    (
      successor,
      observation(state, successor),
      BigDecimal(RewardModel.environment(state, move, successor)) + RewardModel.shaping(
        state,
        successor,
        config
      )
    )
