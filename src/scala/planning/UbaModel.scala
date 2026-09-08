/** Exact Wumpus model and deterministic macro-actions used by the UBA. */
object UbaModel:
  type Position = (Int, Int)
  private def randomElem[A](values: List[A]): A =
    require(values.nonEmpty, "Cannot choose from an empty collection.")
    values(scala.util.Random.nextInt(values.size))

  /** Modeling the percept in the wumpus world.
    * @param stench
    *   was there a stench?
    * @param breeze
    *   was there a breeze?
    * @param glitter
    *   was there glitter?
    * @param scream
    *   was therea scream?
    */
  case class Percept4(stench: Boolean, breeze: Boolean, glitter: Boolean, scream: Boolean):
    /** Returns whether the percept is "None" defined as no percept
      * @return
      *   boolean value indicating whether there is no percept
      */
    def isNone: Boolean = !stench && !breeze && !glitter && !scream

  /** A deterministic state in the wumpus world, characterized by:- (1) the agent's position (2) the agent's
    * orientation (3) whether the agent has the arrow (4) the wumpus' position (if wumpus is not dead) (5) the
    * pits' positions (6) the gold's position
    */
  sealed trait State:
    def isTerminal: Boolean

    /** Transition from this state using the given move.
      * @param m
      *   a move
      * @return
      *   the new (successor/next) state
      */
    def transition(m: Move): State =
      if isTerminal then return this
      if m == Move.Grab && (this match {
          case s: StateWithWumpus => s.agentPosition == s.u.gold
          case s: StateSansWumpus => s.agentPosition == s.u.gold
          case Won                => false
        })
      then return Won
      val dummy = (this match {
        case s: StateWithWumpus =>
          BeliefState(s.agentPosition, s.agentOrientation, s.hasArrow, Set(s.u), History.empty)
        case s: StateSansWumpus =>
          BeliefState(s.agentPosition, s.agentOrientation, false, Set(s.u), History.empty)
        case Won => return Won
      }).transition(m)
      if dummy.belief.isEmpty then
        this match {
          case s: StateWithWumpus =>
            StateWithWumpus(dummy.agentPosition, dummy.agentOrientation, dummy.hasArrow, s.u)
          case s: StateSansWumpus => StateSansWumpus(dummy.agentPosition, dummy.agentOrientation, s.u)
          case Won                => Won
        }
      else dummy.u2State(dummy.belief.head)

    /** Calculate the immediate reward on executing the given move in this state.
      * @param m
      *   a move
      * @return
      *   the immediate reward on executing the move.
      */
    def reward(m: Move): Int =
      RewardModel.environment(this, m, transition(m))

    /** Define a heuristic that rewards "good" actions like getting close to the goal and killing the wumpus.
      * @param m
      *   a move
      * @return
      *   the "added" reward on executing the move.
      */
    def heuristic(m: Move, config: PlannerConfig = PlannerConfig()): BigDecimal =
      RewardModel.shaping(this, transition(m), config)

  case object Won extends State:
    override def isTerminal: Boolean = true

  // States with wumpus alive and those with wumpus dead are modeled and handled separately
  // They inherit from State.
  case class StateWithWumpus(
      agentPosition: Position,
      agentOrientation: Orientation,
      hasArrow: Boolean,
      u: UnobservableWithWumpus
  ) extends State:
    override def isTerminal: Boolean =
      agentPosition == u.wumpus || agentPosition == u.pit1 || agentPosition == u.pit2

  case class StateSansWumpus(
      agentPosition: Position,
      agentOrientation: Orientation,
      u: UnobservableSansWumpus
  ) extends State:
    override def isTerminal: Boolean = agentPosition == u.pit1 || agentPosition == u.pit2

  /** Encapsulate the *unobservable* variables of the state in a single model.
    */
  sealed trait Unobservable

  // Unobservables with the wumpus position and without it are modeled and handled separately.
  case class UnobservableWithWumpus(gold: Position, wumpus: Position, pit1: Position, pit2: Position)
      extends Unobservable:
    /** Remove the wumpus position var and return corresponding unobservable.
      * @return
      *   corresponding unobservable with wumpus pos removed.
      */
    def toSans: UnobservableSansWumpus = UnobservableSansWumpus(gold, pit1, pit2, wumpus)

    /** @return
      *   the pit combination as a set.
      */
    def pits: Set[Position] = Set(pit1, pit2)

  // Retain latent identity after a kill: distinct equally likely worlds must not
  // collapse in a Set, which would silently discard probability mass.
  case class UnobservableSansWumpus(
      gold: Position,
      pit1: Position,
      pit2: Position,
      originalWumpus: Position = (0, 0)
  ) extends Unobservable:
    def pits: Set[Position] = Set(pit1, pit2)

  /** Model the **belief state** of the agent as a combination of the observable and the unobservable
    * variables. Equal posterior weights follow from a uniform prior, deterministic dynamics, deterministic
    * observations, and retained original-world identity after kills. This support-only representation must
    * NOT be used with noisy sensors or stochastic movement: those require explicit posterior weights (as in
    * RLA).
    * @param agentPosition
    *   the agent's position.
    * @param agentOrientation
    *   the agent's orientation.
    * @param hasArrow
    *   whether the agent has the arrow.
    * @param belief
    *   the set of possible unobservables (uniform prior).
    * @param history
    *   the history that has resulted in this belief state.
    */
  case class BeliefState(
      agentPosition: Position,
      agentOrientation: Orientation,
      hasArrow: Boolean,
      belief: Set[Unobservable],
      history: History
  ):
    /** Given values for the unobservable variables, combine them with the observables encapsulated as a
      * deterministic state.
      * @param u
      *   values for unobservable variables
      * @return
      *   a deterministic state encapsulating the observable and unobservable variables.
      */
    def u2State(u: Unobservable): State =
      require(belief contains u, "u2State: I don't believe this!")
      u match {
        case uWithWumpus: UnobservableWithWumpus =>
          StateWithWumpus(agentPosition, agentOrientation, hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus =>
          StateSansWumpus(agentPosition, agentOrientation, uSansWumpus)
      }

    private enum Direction:
      case Forward, Right, Left, Back

    /** Helps in filtering for possible moves.
      * @return
      *   a map from relative direction from the current position and orientation to the corresponding
      *   neighboring positions.
      */
    private def directions2Neighbors: Map[Direction, Position] =
      val (x, y) = agentPosition
      {
        agentOrientation match {
          case Orientation.North =>
            Map(
              Direction.Forward -> (x, y + 1),
              Direction.Right -> (x + 1, y),
              Direction.Left -> (x - 1, y),
              Direction.Back -> (x, y - 1)
            )
          case Orientation.South =>
            Map(
              Direction.Forward -> (x, y - 1),
              Direction.Right -> (x - 1, y),
              Direction.Left -> (x + 1, y),
              Direction.Back -> (x, y + 1)
            )
          case Orientation.East =>
            Map(
              Direction.Forward -> (x + 1, y),
              Direction.Right -> (x, y - 1),
              Direction.Left -> (x, y + 1),
              Direction.Back -> (x - 1, y)
            )
          case Orientation.West =>
            Map(
              Direction.Forward -> (x - 1, y),
              Direction.Right -> (x, y + 1),
              Direction.Left -> (x, y - 1),
              Direction.Back -> (x + 1, y)
            )
        }
      } filter { case (_, (x, y)) =>
        x >= 1 && x <= 4 && y >= 1 && y <= 4
      }

    /** Compute possible moves from the current belief state.
      * @return
      *   a set of possible moves
      */
    def possibleMoves: Set[Move] =
      if isTerminal then return Set.empty
      val d2n = directions2Neighbors
      Move.values.toSet filter {
        case Move.GoForward  => d2n.contains(Direction.Forward)
        case Move.GoLeft     => d2n.contains(Direction.Left)
        case Move.GoRight    => d2n.contains(Direction.Right)
        case Move.GoBack     => d2n.contains(Direction.Back)
        case Move.Shoot      => d2n.contains(Direction.Forward) && hasArrow
        case Move.ShootLeft  => d2n.contains(Direction.Left) && hasArrow
        case Move.ShootRight => d2n.contains(Direction.Right) && hasArrow
        case _               => true
      }

    /** Check whether the belief distribution is empty to conclude that the agent is dead.
      * @return
      *   is the agent dead?
      */
    def isTerminal: Boolean = belief.isEmpty

    /** Private helper hypothesis filter that filters world hypotheses based on a condition for the
      * unobservables given a percept (stench, breeze, giltter, scream).
      * @param prior
      *   the prior belief distribution
      * @param obs
      *   a percept (stench, breeze, glitter, scream)
      * @param condition
      *   a condition for the unobservables based on which the particles are filtered.
      * @return
      *   the filtered posterior distribution
      */
    private def hypothesisFilter(
        prior: Set[Unobservable],
        obs: Boolean,
        condition: Unobservable => Boolean
    ): Set[Unobservable] =
      prior.filter(condition(_) == obs) // if obs then condition should be true else should be false

    /** Filter hypotheses given a percept.
      * @param percept
      *   a percept with 4 variables.
      * @return
      *   the new belief state after exact belief filtering for all four percepts.
      */
    def observe(percept: Percept4): BeliefState =
      val neighbors = neighborsOf(agentPosition)

      val posterior =
        hypothesisFilter( // glitter update
          hypothesisFilter( // breeze update
            hypothesisFilter( // stench update
              hypothesisFilter( // scream update
                belief,
                percept.scream,
                u =>
                  history.lastOption match {
                    case Some(update) =>
                      update match {
                        case m: Move =>
                          if Set(Move.Shoot, Move.ShootLeft, Move.ShootRight) contains m then
                            u.isInstanceOf[UnobservableSansWumpus]
                          else
                            percept.scream // no filtering, essentially identity function, keep belief as is
                        case o: Percept4 => assert(false, "Observation after an observation in history.")
                      }
                    case None =>
                      percept.scream // no filtering, essentially identity function, keep belief as is
                  }
              ),
              percept.stench,
              {
                case uWithWumpus: UnobservableWithWumpus => neighbors contains uWithWumpus.wumpus
                case _                                   => false
              }
            ),
            percept.breeze,
            {
              case u: UnobservableWithWumpus => u.pits.exists(neighbors.contains)
              case u: UnobservableSansWumpus => u.pits.exists(neighbors.contains)
            }
          ),
          percept.glitter,
          {
            case u: UnobservableWithWumpus => agentPosition == u.gold
            case u: UnobservableSansWumpus => agentPosition == u.gold
          }
        )

      copy(belief = posterior, history = history.appendObs(percept))

    /** Transition from this belief state using the given move.
      * @param move
      *   a move
      * @return
      *   the new (successor/next) belief state
      */
    def transition(move: Move): BeliefState =
      // A macro still turns when its forward step hits a wall. Shooting without
      // an arrow likewise turns, but cannot kill anything.
      if !hasArrow && RolloutPolicies.shots.contains(move) then
        val orientation = move match
          case Move.ShootLeft  => agentOrientation.turnLeft
          case Move.ShootRight => agentOrientation.turnRight
          case _               => agentOrientation
        return copy(agentOrientation = orientation, history = history.appendMove(move))
      val (candidatePosition, ao, ha, posterior) = move match {
        case Move.GoForward =>
          (agentOrientation.forwardFrom(agentPosition), agentOrientation, hasArrow, belief)
        case Move.GoRight =>
          (agentOrientation.rightFrom(agentPosition), agentOrientation.turnRight, hasArrow, belief)
        case Move.GoLeft =>
          (agentOrientation.leftFrom(agentPosition), agentOrientation.turnLeft, hasArrow, belief)
        case Move.GoBack =>
          (agentOrientation.backFrom(agentPosition), agentOrientation.turnBack, hasArrow, belief)
        // move = Shoot only when hasArrow is true as dictated by possibleMoves, similarly for shootRight and shootLeft
        case Move.Shoot =>
          (
            agentPosition,
            agentOrientation,
            false,
            belief.map {
              case u: UnobservableWithWumpus =>
                val (xA, yA) = agentPosition
                val (xW, yW) = u.wumpus
                agentOrientation match {
                  case Orientation.North if xW == xA && yW > yA => u.toSans
                  case Orientation.South if xW == xA && yW < yA => u.toSans
                  case Orientation.East if xW > xA && yW == yA  => u.toSans
                  case Orientation.West if xW < xA && yW == yA  => u.toSans
                  case _                                        => u
                }
              case u: UnobservableSansWumpus => u
            }
          )
        case Move.ShootRight =>
          (
            agentPosition,
            agentOrientation.turnRight,
            false,
            belief.map {
              case u: UnobservableWithWumpus =>
                val (xA, yA) = agentPosition
                val (xW, yW) = u.wumpus
                agentOrientation match {
                  case Orientation.North if xW > xA && yW == yA => u.toSans
                  case Orientation.South if xW < xA && yW == yA => u.toSans
                  case Orientation.East if xW == xA && yW < yA  => u.toSans
                  case Orientation.West if xW == xA && yW > yA  => u.toSans
                  case _                                        => u
                }
              case u: UnobservableSansWumpus => u
            }
          )
        case Move.ShootLeft =>
          (
            agentPosition,
            agentOrientation.turnLeft,
            false,
            belief.map {
              case u: UnobservableWithWumpus =>
                val (xA, yA) = agentPosition
                val (xW, yW) = u.wumpus
                agentOrientation match {
                  case Orientation.North if xW < xA && yW == yA => u.toSans
                  case Orientation.South if xW > xA && yW == yA => u.toSans
                  case Orientation.East if xW == xA && yW > yA  => u.toSans
                  case Orientation.West if xW == xA && yW < yA  => u.toSans
                  case _                                        => u
                }
              case u: UnobservableSansWumpus => u
            }
          )
        case _ => (agentPosition, agentOrientation, hasArrow, belief)
      }

      val ap =
        if candidatePosition._1 < 1 || candidatePosition._1 > 4 ||
          candidatePosition._2 < 1 || candidatePosition._2 > 4
        then agentPosition
        else candidatePosition
      // Filter out states in which agent is dead
      val alive = posterior.filter {
        case u: UnobservableWithWumpus => !StateWithWumpus(ap, ao, ha, u).isTerminal
        case u: UnobservableSansWumpus => !StateSansWumpus(ap, ao, u).isTerminal
      }

      BeliefState(ap, ao, ha, alive, history.appendMove(move))

    /** Sample a (deterministic) state from the belief prior.
      * @return
      *   a state
      */
    private lazy val indexedSupport = belief.toVector
    def sampleState: State =
      require(indexedSupport.nonEmpty, "Cannot sample an empty belief.")
      u2State(indexedSupport(scala.util.Random.nextInt(indexedSupport.size)))

  /** Model the orientation as an enum with possible values:- North, South, East, West. For each value, the
    * following are defined:- (1) `forwardFrom`: the position that is reached on going forward from the given
    * position in this orientation. (2) `rightFrom`: the position that is reached on going right from the
    * given position in this orientation. (3) `leftFrom`: the position that is reached on going left from the
    * given position in this orientation. (4) `backFrom`: the position that is reached on going back from the
    * given position in this orientation. (5) `turnRight`: the orientation that is reached on turning right
    * from this orientation. (6) `turnLeft`: the orientation that is reached on turning left from this
    * orientation. (7) `turnBack`: the orientation that is reached on turning back from this orientation.
    */
  enum Orientation:
    case North, South, East, West

    def forwardFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y + 1)
        case South => (x, y - 1)
        case East  => (x + 1, y)
        case West  => (x - 1, y)
      }

    def rightFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x + 1, y)
        case South => (x - 1, y)
        case East  => (x, y - 1)
        case West  => (x, y + 1)
      }

    def leftFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x - 1, y)
        case South => (x + 1, y)
        case East  => (x, y + 1)
        case West  => (x, y - 1)
      }

    def backFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y - 1)
        case South => (x, y + 1)
        case East  => (x - 1, y)
        case West  => (x + 1, y)
      }

    def turnRight: Orientation = this match {
      case North => East
      case South => West
      case East  => South
      case West  => North
    }

    def turnLeft: Orientation = this match {
      case North => West
      case South => East
      case East  => North
      case West  => South
    }

    def turnBack: Orientation = this match {
      case North => South
      case South => North
      case East  => West
      case West  => East
    }

  /** Remodel the acions to be coarser than the original model. Essentially define "compound actions" and plan
    * in terms of those.
    * @param toActionSeq
    *   a translator from this move or "compound action" into a sequence of the atomic moves that results in
    *   the same change (e.g.:- GoRight.toActionSeq() = TURN_RIGHT + GO_FORWARD)
    */
  enum Move(val toActionSeq: () => List[Int]):
    case GoForward extends Move(() => List(Action.GO_FORWARD))
    case GoLeft extends Move(() => List(Action.TURN_LEFT, Action.GO_FORWARD))
    case GoRight extends Move(() => List(Action.TURN_RIGHT, Action.GO_FORWARD))
    case GoBack
        extends Move(() => {
          val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
          List(randomTurn, randomTurn, Action.GO_FORWARD)
        })
    case Shoot extends Move(() => List(Action.SHOOT))
    case ShootLeft extends Move(() => List(Action.TURN_LEFT, Action.SHOOT))
    case ShootRight extends Move(() => List(Action.TURN_RIGHT, Action.SHOOT))
    case NoOp extends Move(() => List(Action.NO_OP))
    case Grab extends Move(() => List(Action.GRAB))

  /** Model the formal notion of a "history" defined in POMDP theory as an alternating sequence of actions and
    * observations: moves and percepts, in this case.
    * @param moveHist
    *   the history of moves until now
    * @param obsHist
    *   the history of percepts until now
    */
  case class History(moveHist: List[Move], obsHist: List[Percept4]):
    require((moveHist.length - obsHist.length).abs <= 1, "Action-observation mismatch.")

    /** Check whether no moves or observations have been recorded
      * @return
      *   is the history empty?
      */
    private def isEmpty: Boolean = moveHist.isEmpty && obsHist.isEmpty

    /** Append a move to the history.
      * @param m
      *   a move
      * @return
      *   a new history with the given move appended.
      */
    def appendMove(m: Move): History = History(moveHist :+ m, obsHist)

    /** Append an observation to the history.
      * @param o
      *   a percept
      * @return
      *   a new history with the given percept appended.
      */
    def appendObs(o: Percept4): History = History(moveHist, obsHist :+ o)

    /** Retrieve the last update from the history.
      * @return
      *   the last update as an option; None if history is empty.
      */
    lazy val lastOption: Option[Move | Percept4] =
      if obsHist.length - moveHist.length == 1 then Some(obsHist.last)
      else moveHist.lastOption

    /** Compute the number of moves executed so far.
      * @return
      *   length of the move history.
      */
    def numMoves: Int = moveHist.length

    /** Count the number of times a certain condition for an observation is true in the history.
      * @param condition
      *   a condition for percepts.
      * @return
      *   the number of observations in the history that satisfy the given condition.
      */
    def countObs(condition: Percept4 => Boolean): Int = obsHist.count(condition)

  // Companion object for the History case class.
  case object History:
    /** Initialize an empty history.
      * @return
      *   an empty history (with empty move and observation histories).
      */
    def empty: History = History(List.empty, List.empty)

  /** Compute the initial belief prior by considering all position combinations of the pits, wumpus, and gold.
    * @return
    *   a set of world hypotheses forming a uniform prior
    */
  lazy val initialBeliefPrior: Set[Unobservable] =
    val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet
    // 1. Pit possibilities: 105 of these
    val possiblePitCombinations: Set[(Position, Position)] =
      (allSquares - ((1, 1))).toList.combinations(2).toSet.map {
        case Seq(p1, p2) => (p1, p2)
        case _           => assert(false, "Expected a pair of pit locations.")
      }
    // 2. Wumpus possibilities: 15 of these
    val possibleWumpusPositions: Set[Position] = allSquares - ((1, 1))
    // 3. Gold possibilities: 16 of these
    val possibleGoldLocations: Set[Position] = allSquares
    // A belief state is characterized by the positions of the gold, wumpus and both the pits
    possiblePitCombinations.flatMap { case (pit1, pit2) =>
      possibleWumpusPositions.flatMap(wumpus =>
        possibleGoldLocations.map(gold => UnobservableWithWumpus(gold, wumpus, pit1, pit2))
      )
    }

  /** Compute the neighbors of a square in the wumpus world. A neighbor of a square is any other square that
    * is adjacent but not diagonally adjacent. Hence, a square has at least 2 and at most 4 neighbors.
    * @param sq
    *   a position (x, y).
    * @return
    *   a set of positions that are all neighbors of (x, y).
    */
  def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter { case (x, y) =>
      x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  /** Compute the Manhattan distance between two squares in the wumpus world.
    * @param pos1
    *   a position (x1, y1)
    * @param pos2
    *   another position (x2, y2)
    * @return
    *   the Manhattan distance := |x2 - x1| + |y2 - y1|
    */
  def manhattanDistance(pos1: Position, pos2: Position): Int =
    val (x1, y1) = pos1
    val (x2, y2) = pos2
    (x2 - x1).abs + (y2 - y1).abs
