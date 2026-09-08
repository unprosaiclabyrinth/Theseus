import UbaModel.*

object RolloutPolicies:
  val shots: Set[Move] = Set(Move.Shoot, Move.ShootLeft, Move.ShootRight)
  def choose(belief: BeliefState, kind: RolloutKind): Move =
    val legal = belief.possibleMoves
    require(legal.nonEmpty, "Cannot roll out an empty belief.")
    val choices = kind match
      case RolloutKind.Uniform => legal
      case RolloutKind.Informed => belief.history.obsHist.lastOption match
        case Some(o) if o.glitter => Set(Move.Grab)
        case Some(o) if o.stench && belief.hasArrow => legal.intersect(shots)
        case Some(_) => legal.intersect(Set(Move.GoForward, Move.GoLeft, Move.GoRight, Move.NoOp))
        case None => legal -- shots
    val indexed = (if choices.nonEmpty then choices else legal).toVector.sortBy(_.ordinal)
    indexed(scala.util.Random.nextInt(indexed.size))
