/*
 * Agent function for a utility-based agent in the wumpus environment
 * f_UBA: Ω* -> A
 * 
 * Project 3 - Utility-Based Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 * 
 */
import scala.collection.mutable
import scala.math.{log, sqrt}

object UtilityBasedAgent extends AgentFunctionImpl:
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  /**
   * Modeling the percept in the wumpus world.
   * @param stench was there a stench?
   * @param breeze was there a breeze?
   * @param glitter was there glitter?
   * @param scream was therea scream?
   */
  private case class Percept4(stench: Boolean, breeze: Boolean, glitter: Boolean, scream: Boolean):
    /**
     * Returns whether the percept is "None" defined as no percept
     * @return boolean value indicating whether there is no percept
     */
    def isNone: Boolean = !stench && !breeze && !glitter && !scream

  /**
   * A deterministic state in the wumpus world, characterized by:-
   * (1) the agent's position
   * (2) the agent's orientation
   * (3) whether the agent has the arrow
   * (4) the wumpus' position (if wumpus is not dead)
   * (5) the pits' positions
   * (6) the gold's position
   */
  private sealed trait State:
    def isTerminal: Boolean

    /**
     * Transition from this state using the given move.
     * @param m a move
     * @return the new (successor/next) state
     */
    def transition(m: Move): State =
      val dummy = (this match {
        case s: StateWithWumpus => BeliefState(s.agentPosition, s.agentOrientation, s.hasArrow, Set(s.u), History.empty)
        case s: StateSansWumpus => BeliefState(s.agentPosition, s.agentOrientation, false, Set(s.u), History.empty)
      }).transition(m)
      if dummy.belief.isEmpty then this match {
        case s: StateWithWumpus => StateWithWumpus(dummy.agentPosition, dummy.agentOrientation, dummy.hasArrow, s.u)
        case s: StateSansWumpus => StateSansWumpus(dummy.agentPosition, dummy.agentOrientation, s.u)
      }
      else dummy.u2State(dummy.belief.head)

    /**
     * Calculate the immediate reward on executing the given move in this state.
     * @param m a move
     * @return the immediate reward on executing the move.
     */
    def reward(m: Move): Int =
      val successor = transition(m)
      {
        if successor.isTerminal then -1000
        else m match {
          case Move.GoForward => -1
          case Move.GoLeft | Move.GoRight => -2
          case Move.GoBack => -3
          case Move.Shoot => -10
          case Move.ShootLeft | Move.ShootRight => -11
          case Move.NoOp => 0
          case Move.Grab => this match {
            case s: StateWithWumpus if s.agentPosition == s.u.gold => 1000
            case s: StateSansWumpus if s.agentPosition == s.u.gold => 1000
            case _ => -1
          }
        }
      }

    /**
     * Define a heuristic that rewards "good" actions like getting close to the goal and killing the wumpus.
     * @param m a move
     * @return the "added" reward on executing the move.
     */
    def heuristic(m: Move): Int =
     val successor = transition(m)
     // The distance to the gold is a big one
     def goldDist(state: State): Int =
       state match {
         case s: StateWithWumpus => manhattanDistance(s.u.gold, s.agentPosition)
         case s: StateSansWumpus => manhattanDistance(s.u.gold, s.agentPosition)
       }
     val h1 = -4 * (goldDist(successor) - goldDist(this))
     // Assign a "wumpus score"
     def wumpusScore(state: State): Int =
       state match {
         case s: StateWithWumpus => 0
         case s: StateSansWumpus => 9
       }
     val h2 = wumpusScore(successor) - wumpusScore(this)
     h1 + h2

  // States with wumpus alive and those with wumpus dead are modeled and handled separately
  // They inherit from State.
  private case class StateWithWumpus(agentPosition: Position, agentOrientation: Orientation, hasArrow: Boolean,
                                     u: UnobservableWithWumpus) extends State:
    override def isTerminal: Boolean = Set(u.wumpus, u.pit1, u.pit2) contains agentPosition

  private case class StateSansWumpus(agentPosition: Position, agentOrientation: Orientation,
                                     u: UnobservableSansWumpus) extends State:
    override def isTerminal: Boolean = Set(u.pit1, u.pit2) contains agentPosition

  /**
   * Encapsulate the *unobservable* variables of the state in a single model.
   */
  private sealed trait Unobservable

  // Unobservables with the wumpus position and without it are modeled and handled separately.
  private case class UnobservableWithWumpus(gold: Position, wumpus: Position,
                                            pit1: Position, pit2: Position) extends Unobservable:
    /**
     * Remove the wumpus position var and return corresponding unobservable.
     * @return corresponding unobservable with wumpus pos removed.
     */
    def toSans: UnobservableSansWumpus = UnobservableSansWumpus(gold, pit1, pit2)

    /**
     * @return the pit combination as a set.
     */
    def pits: Set[Position] = Set(pit1, pit2)

  private case class UnobservableSansWumpus(gold: Position, pit1: Position, pit2: Position) extends Unobservable:
    def pits: Set[Position] = Set(pit1, pit2)

  /**
   * Model the **belief state** of the agent as a combination of the observable and the unobservable variables.
   * A uniform prior is defined on the unobservable variables at every time step since all unobservables are
   * equally likely.
   * @param agentPosition the agent's position.
   * @param agentOrientation the agent's orientation.
   * @param hasArrow whether the agent has the arrow.
   * @param belief the set of possible unobservables (uniform prior).
   * @param history the history that has resulted in this belief state.
   */
  private case class BeliefState(agentPosition: Position,
                                 agentOrientation: Orientation,
                                 hasArrow: Boolean,
                                 belief: Set[Unobservable],
                                 history: History):
    /**
     * Given values for the unobservable variables, combine them with the observables
     * encapsulated as a deterministic state.
     * @param u values for unobservable variables
     * @return a deterministic state encapsulating the observable and unobservable variables.
     */
    def u2State(u: Unobservable): State =
      u match {
        case uWithWumpus: UnobservableWithWumpus => StateWithWumpus(agentPosition, agentOrientation, hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus => StateSansWumpus(agentPosition, agentOrientation, uSansWumpus)
      }

    private enum Direction:
      case Forward, Right, Left, Back

    /**
     * Helps in filtering for possible moves.
     * @return a map from relative direction from the current position and orientation
     *         to the corresponding neighboring positions.
     */
    private def directions2Neighbors: Map[Direction, Position] =
      val (x, y) = agentPosition
      {
        agentOrientation match {
          case Orientation.North => Map(Direction.Forward -> (x, y + 1), Direction.Right -> (x + 1, y),
            Direction.Left -> (x - 1, y), Direction.Back -> (x, y - 1))
          case Orientation.South => Map(Direction.Forward -> (x, y - 1), Direction.Right -> (x - 1, y),
            Direction.Left -> (x + 1, y), Direction.Back -> (x, y + 1))
          case Orientation.East => Map(Direction.Forward -> (x + 1, y), Direction.Right -> (x, y - 1),
            Direction.Left -> (x, y + 1), Direction.Back -> (x - 1, y))
          case Orientation.West => Map(Direction.Forward -> (x - 1, y), Direction.Right -> (x, y + 1),
            Direction.Left -> (x, y - 1), Direction.Back -> (x + 1, y))
        }
      } filter {
        case (_, (x, y)) => x >= 1 && x <= 4 && y >= 1 && y <= 4
      }

    /**
     * Compute possible moves from the current belief state.
     * @return a set of possible moves
     */
    def possibleMoves: Set[Move] =
      if isTerminal then return Set.empty
      val d2n = directions2Neighbors
      Move.values.toSet filter {
        case Move.GoForward => d2n.contains(Direction.Forward)
        case Move.GoLeft => d2n.contains(Direction.Left)
        case Move.GoRight => d2n.contains(Direction.Right)
        case Move.GoBack => d2n.contains(Direction.Back)
        case Move.Shoot => d2n.contains(Direction.Forward) && hasArrow
        case Move.ShootLeft => d2n.contains(Direction.Left) && hasArrow
        case Move.ShootRight => d2n.contains(Direction.Right) && hasArrow
        case _ => true
      }

    /**
     * Check whether the belief distribution is empty to conclude that the agent is dead.
     * @return is the agent dead?
     */
    def isTerminal: Boolean = belief.isEmpty

    /**
     * Private helper particle filter that filters belief particles based on a condition
     * for the unobservables given a percept (stench, breeze, giltter, scream).
     * @param prior the prior belief distribution
     * @param obs a percept (stench, breeze, glitter, scream)
     * @param condition a condition for the unobservables based on which the particles are filtered.
     * @return the filtered posterior distribution
     */
    private def particleFilter(prior: Set[Unobservable], obs: Boolean,
                               condition: Unobservable => Boolean): Set[Unobservable] =
      prior.filter(condition(_) == obs) // if obs then condition should be true else should be false

    /**
     * Filter particles given a percept.
     * @param percept a percept with 4 variables.
     * @return the new belief state after particle filtering for all four percepts.
     */
    def observe(percept: Percept4): BeliefState =
      val neighbors = neighborsOf(agentPosition)

      val posterior =
        particleFilter( // glitter update
          particleFilter( // breeze update
            particleFilter( // stench update
              particleFilter( // scream update
                belief, percept.scream, u => history.lastOption match {
                  case Some(update) => update match {
                    case m: Move =>
                      if Set(Move.Shoot, Move.ShootLeft, Move.ShootRight) contains m then
                        u.isInstanceOf[UnobservableSansWumpus]
                      else percept.scream // no filtering, essentially identity function, keep belief as is
                    case o: Percept4 => assert(false, "Observation after an observation in history.")
                  }
                  case None => percept.scream // no filtering, essentially identity function, keep belief as is
                }
              ), percept.stench, {
                case uWithWumpus: UnobservableWithWumpus => neighbors contains uWithWumpus.wumpus
                case _ => false
              }
            ), percept.breeze, {
              case u: UnobservableWithWumpus => u.pits.exists(neighbors.contains)
              case u: UnobservableSansWumpus => u.pits.exists(neighbors.contains)
            }
          ), percept.glitter, {
            case u: UnobservableWithWumpus => agentPosition == u.gold
            case u: UnobservableSansWumpus => agentPosition == u.gold
          }
        )

      copy(belief = posterior, history = history.appendObs(percept))

    /**
     * Transition from this belief state using the given move.
     * @param move a move
     * @return the new (successor/next) belief state
     */
    def transition(move: Move): BeliefState =
      val (ap, ao, ha, posterior) = move match {
        case Move.GoForward => (agentOrientation.forwardFrom(agentPosition), agentOrientation, hasArrow, belief)
        case Move.GoRight => (agentOrientation.rightFrom(agentPosition), agentOrientation.turnRight, hasArrow, belief)
        case Move.GoLeft => (agentOrientation.leftFrom(agentPosition), agentOrientation.turnLeft, hasArrow, belief)
        case Move.GoBack => (agentOrientation.backFrom(agentPosition), agentOrientation.turnBack, hasArrow, belief)
        // move = Shoot only when hasArrow is true as dictated by possibleMoves, similarly for shootRight and shootLeft
        case Move.Shoot => (agentPosition, agentOrientation, false, belief.map {
          case u: UnobservableWithWumpus =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus
            agentOrientation match {
              case Orientation.North if xW == xA && yW > yA => u.toSans
              case Orientation.South if xW == xA && yW < yA => u.toSans
              case Orientation.East if xW > xA && yW == yA => u.toSans
              case Orientation.West if xW < xA && yW == yA => u.toSans
              case _ => u
            }
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus found before shooting?!")
        })
        case Move.ShootRight => (agentPosition, agentOrientation.turnRight, false, belief.map {
          case u: UnobservableWithWumpus =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus
            agentOrientation match {
              case Orientation.North if xW > xA && yW == yA => u.toSans
              case Orientation.South if xW < xA && yW == yA => u.toSans
              case Orientation.East if xW == xA && yW < yA => u.toSans
              case Orientation.West if xW == xA && yW > yA => u.toSans
              case _ => u
            }
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus found before shooting?!")
        })
        case Move.ShootLeft => (agentPosition, agentOrientation.turnLeft, false, belief.map {
          case u: UnobservableWithWumpus =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus
            agentOrientation match {
              case Orientation.North if xW < xA && yW == yA => u.toSans
              case Orientation.South if xW > xA && yW == yA => u.toSans
              case Orientation.East if xW == xA && yW > yA => u.toSans
              case Orientation.West if xW == xA && yW < yA => u.toSans
              case _ => u
            }
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus found before shooting?!")
        })
        case _ => (agentPosition, agentOrientation, hasArrow, belief)
      }

      // Filter out states in which agent is dead
      val alive = posterior.filter {
        case u: UnobservableWithWumpus => !StateWithWumpus(ap, ao, ha, u).isTerminal
        case u: UnobservableSansWumpus => !StateSansWumpus(ap, ao, u).isTerminal
      }

      BeliefState(ap, ao, ha, alive, history.appendMove(move))

    /**
     * Sample a (deterministic) state from the belief prior.
     * @return a state
     */
    def sampleState: State = u2State(randomElem(belief.toList))

  /**
   * Model the orientation as an enum with possible values:- North, South, East, West.
   * For each value, the following are defined:-
   * (1) `forwardFrom`: the position that is reached on going forward from the given position in this orientation.
   * (2) `rightFrom`: the position that is reached on going right from the given position in this orientation.
   * (3) `leftFrom`: the position that is reached on going left from the given position in this orientation.
   * (4) `backFrom`: the position that is reached on going back from the given position in this orientation.
   * (5) `turnRight`: the orientation that is reached on turning right from this orientation.
   * (6) `turnLeft`: the orientation that is reached on turning left from this orientation.
   * (7) `turnBack`: the orientation that is reached on turning back from this orientation.
   */
  private enum Orientation:
    case North, South, East, West

    def forwardFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y + 1)
        case South => (x, y - 1)
        case East => (x + 1, y)
        case West => (x - 1, y)
      }

    def rightFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x + 1, y)
        case South => (x - 1, y)
        case East => (x, y - 1)
        case West => (x, y + 1)
      }

    def leftFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x - 1, y)
        case South => (x + 1, y)
        case East => (x, y + 1)
        case West => (x, y - 1)
      }

    def backFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y - 1)
        case South => (x, y + 1)
        case East => (x - 1, y)
        case West => (x + 1, y)
      }

    def turnRight: Orientation = this match {
      case North => East
      case South => West
      case East => South
      case West => North
    }

    def turnLeft: Orientation = this match {
      case North => West
      case South => East
      case East => North
      case West => South
    }

    def turnBack: Orientation = this match {
      case North => South
      case South => North
      case East => West
      case West => East
    }

  /**
   * Remodel the acions to be coarser than the original model. Essentially define "compound actions"
   * and plan in terms of those.
   * @param toActionSeq a translator from this move or "compound action" into a sequence of the atomic
   *                    moves that results in the same change (e.g.:- GoRight.toActionSeq() = TURN_RIGHT + GO_FORWARD)
   */
  private enum Move(val toActionSeq: () => List[Int]):
    case GoForward extends Move(() => List(Action.GO_FORWARD))
    case GoLeft extends Move(() => List(Action.TURN_LEFT, Action.GO_FORWARD))
    case GoRight extends Move(() => List(Action.TURN_RIGHT, Action.GO_FORWARD))
    case GoBack extends Move(() => {
      val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
      List(randomTurn, randomTurn, Action.GO_FORWARD)
    })
    case Shoot extends Move(() => List(Action.SHOOT))
    case ShootLeft extends Move(() => List(Action.TURN_LEFT, Action.SHOOT))
    case ShootRight extends Move(() => List(Action.TURN_RIGHT, Action.SHOOT))
    case NoOp extends Move(() => List(Action.NO_OP))
    case Grab extends Move(() => List(Action.GRAB))

  /**
   * Model the formal notion of a "history" defined in POMDP theory as an alternating sequence of
   * actions and observations: moves and percepts, in this case.
   * @param moveHist the history of moves until now
   * @param obsHist the history of percepts until now
   */
  private case class History(moveHist: List[Move], obsHist: List[Percept4]):
    require((moveHist.length - obsHist.length).abs <= 1, "Action-observation mismatch.")

    /**
     * Check whether no moves or observations have been recorded
     * @return is the history empty?
     */
    private def isEmpty: Boolean = moveHist.isEmpty && obsHist.isEmpty

    /**
     * Append a move to the history.
     * @param m a move
     * @return a new history with the given move appended.
     */
    def appendMove(m: Move): History = History(moveHist :+ m, obsHist)

    /**
     * Append an observation to the history.
     * @param o a percept
     * @return a new history with the given percept appended.
     */
    def appendObs(o: Percept4): History = History(moveHist, obsHist :+ o)

    /**
     * Retrieve the last update from the history.
     * @return the last update as an option; None if history is empty.
     */
    def lastOption: Option[Move | Percept4] =
      if obsHist.length - moveHist.length == 1 then Some(obsHist.last)
      else moveHist.lastOption

    /**
     * Compute the number of moves executed so far.
     * @return length of the move history.
     */
    def numMoves: Int = moveHist.length

    /**
     * Count the number of times a certain condition for an observation is true in the history.
     * @param condition a condition for percepts.
     * @return the number of observations in the history that satisfy the given condition.
     */
    def countObs(condition: Percept4 => Boolean): Int = obsHist.count(condition)

  // Companion object for the History case class.
  private case object History:
    /**
     * Initialize an empty history.
     * @return an empty history (with empty move and observation histories).
     */
    def empty: History = History(List.empty, List.empty)

  /**
   * Implement partially observable Monte Carlo planning (POMCP)
   */
  private case object POMCP:
    /**
     * Model a node in the Monte Carlo search tree (MCST)
     * @param beliefState the corresponding belief state.
     * @param parent the parent node index.
     * @param children a map from updates that have occurred at this node to the resulting children node indices.
     * @param visitedCount the number of times this node has been visited.
     * @param value the expected utility value of this node.
     */
    private case class Node(beliefState: BeliefState,
                            parent: Option[Int],
                            children: mutable.Map[Move | Percept4, Int],
                            visitedCount: Int,
                            value: BigDecimal)

    // parameters that influence the performance of POMCP, pretty self-explanatory
    private final val TIME_HORIZON = 15
    private final val DISCOUNT_HORIZON = 0
    private final val NUM_SIMULATIONS = 1000
    private final val DISCOUNT = BigDecimal(0.2)
    private final val EXPLORATION_CONST = BigDecimal(sqrt(2)) // used in UCT - UCB1

    /**
     * Model the MCST
     */
    private case object Tree:
      var root = -1 // current root index
      private val count: Counter = Counter(-1) // a counter that generates node indices on demand

      // a map from indices to MCST nodes
      private val nodes: mutable.Map[Int, Node] = mutable.Map(-1 -> Node(
        BeliefState((1, 1), Orientation.East, true, initialBeliefPrior, History.empty), None, mutable.Map.empty, 0, 0.0
      ))

      /**
       * Reset the MCST
       */
      def reset(): Unit =
        root = -1
        count.reset()
        nodes.clear()
        nodes += (-1 -> Node(
          BeliefState((1, 1), Orientation.East, true, initialBeliefPrior, History.empty), None, mutable.Map.empty, 0, 0.0
        ))

      /**
       * Expand from a node in the MCST to obtain a child via the given update.
       * Add the child node to the parent's children set as well as to the MCST's nodes map.
       * @param parent the node to expand from
       * @param update the update to expand via
       */
      def expandFrom(parent: Int, update: Move | Percept4): Unit =
        update match {
          case m: Move =>
            nodes += (count.next -> Node(
              nodes(parent).beliefState.transition(m), Some(parent), mutable.Map.empty, 0, 0.0
            ))
          case o: Percept4 =>
            nodes += (count.next -> Node(
              nodes(parent).beliefState.observe(o), Some(parent), mutable.Map.empty, 0, 0.0
            ))
        }
        nodes(parent).children += (update -> count.get)

      /**
       * Checks whether the given node has not been visited since that is the leaf condition
       * @param n a node index
       * @return is the node with index n a leaf in the MCST?
       */
      def isLeaf(n: Int): Boolean =
        require(nodes contains n, "isLeaf: No such node.")
        nodes(n).children.isEmpty

      /**
       * Retrieves the index of the child of the parent that is obtained via a given observation.
       * If no such child exists, then the child is created and its index is returned.
       * @param n a node in the MCST
       * @param o a percept
       * @return index of the child obtained on observing pecept o @ node n
       */
      def getObsNode(n: Int, o: Percept4): Int =
        require(nodes contains n, "getObsNode: Invalid node index.")
        if !(nodes(n).children contains o) then expandFrom(n, o)
        nodes(n).children(o)

      /**
       * Private helper that prunes the tree recursively to keep the subtree rooted at the new root.
       * @param root the current root
       * @param newRoot the new root where the pruned tree should be rooted
       */
      private def pruneRecursively(root: Int, newRoot: Int): Unit =
        if root != newRoot then
          nodes(root).children foreach ((_, n) => pruneRecursively(n, newRoot))
          nodes.subtractOne(root)

      /**
       * Computes the child of the current root that is obtained from the given update, makes
       * it the new root and deletes everything else. Essentially traverses the tree one step
       * based on a real update (move or percept) from the real world.
       * @param update a move or percept from the real world.
       */
      def prune(update: Move | Percept4): Unit =
        val children = nodes(root).children
        val newRoot = update match {
          case m: Move =>
            require(children contains update, "prune: No such child.")
            children(m)
          case o: Percept4 => getObsNode(root, o)
        }
        children foreach ((_, n) => pruneRecursively(n, newRoot))
        nodes.get(root).foreach(curr => nodes(root) = curr.copy(children = children.filter(_._2 == newRoot)))
        root = newRoot

      /**
       * When the tree is pruned using `prune()`, the parent of the new root (i.e. the old root in all but one case)
       * is left in (i.e. not pruned) since its data is required for UCB1 computation. Hence, the root parent must
       * be pruned separately when it is no longer needed. This method prunes just parent and sets the parent of the
       * new root to None. Hence, make the "new root" the new root.
       */
      def pruneRootParent(): Unit =
        require(nodes contains root, "pruneRootParent: Root node not found.")
        nodes(root).parent match {
          case Some(p) => nodes subtractOne p
          case None => // parent is already pruned or no parent
        }
        nodes.get(root).foreach(curr => nodes(root) = curr.copy(parent = None))

      /**
       * Compute the weighted UCB1 score for a node.
       * @param n a node index in the MCST
       * @param expltMoveWeight a functional parameter that returns an exploitation weight for a given move
       * @param explrMoveWeight a functional parameter that returns an exploration weight for a given move
       * @return the UCB1 value
       */
      private def ucb1(n: Int, expltMoveWeight: Move => BigDecimal, explrMoveWeight: Move => BigDecimal): BigDecimal =
        require(nodes contains n, "UCB1: No such node.")

        val moveToHere =
          nodes(n).parent match {
            case Some(p) =>
              val moveToHere = randomElem(nodes(p).children.filter(_._2 == n).keySet.toList)
              assert(moveToHere.isInstanceOf[Move], "UCB1 is called for a non-action node??")
              moveToHere.asInstanceOf[Move]
            case None => Move.NoOp
          }

        val node = nodes(n)
        (expltMoveWeight(moveToHere) * node.value) + (EXPLORATION_CONST * explrMoveWeight(moveToHere) * sqrt(log(
          node.parent match {
            case Some(p) => nodes(p).visitedCount
            case None => 1
          }
        ) / (node.visitedCount + 1))) // avoid division by 0 for leaves

      /**
       * Dictate the selection of a move at a node using a selection policy
       * @param n a node index in the MCST
       * @return a move and the child node after executing the move
       */
      def selectionPolicy(n: Int): (Move, Int) =
        require(nodes contains n, "selection policy: No such node.")
        val actionLeaves = nodes(n).children.filter((t, c) => t.isInstanceOf[Move] &&
          !(Set(Move.Shoot, Move.ShootLeft, Move.ShootRight) contains t.asInstanceOf[Move]) && isLeaf(c)
        )
        if actionLeaves.nonEmpty then // if there exist leaf children
          val rp = rolloutPolicy(nodes(n).beliefState) // try matching against the rollout policy
          // if the node corresponding to the rollout policy move is a leaf, select that
          if actionLeaves contains rp then (rp, actionLeaves(rp))
          else // otherwise choose a random action that leads to a leaf (cannot be one of the shoots)
            val ret = randomElem(actionLeaves.keySet.toList).asInstanceOf[Move]
            (ret, actionLeaves(ret))
        else // else (there are no action leaves except Shoot leaves) choose the node with the highest UCB1 score
          val h = nodes(n).beliefState.history
          val best = nodes(n).children.maxBy((_, c) => ucb1(c, _ => 1, {
            case Move.GoForward => 111
            case Move.GoLeft => 110
            case Move.GoRight => 110
            case Move.GoBack => 1
            case Move.Shoot => 50 * h.countObs(_.stench)
            case Move.ShootLeft => 50 * h.countObs(_.stench)
            case Move.ShootRight => 50 * h.countObs(_.stench)
            case Move.NoOp => 50
            case Move.Grab => 0
          }))
          best._1 match {
            case m: Move => (m, best._2)
            case o: Percept4 => assert(false, "selectionPolicy: Finding an observation child is not possible.")
          }

      /**
       * Retrieve the belief state at a node in the MCST
       * @param n a node index in the MCST
       * @return the corresponding belief state
       */
      def beliefStateAt(n: Int): BeliefState =
        require(nodes contains n, "beliefStateAt: No such node.")
        nodes(n).beliefState

      /**
       * "Visit" a node by incrementing its visitedCount
       * @param n the node index of the node to visit in the MCST
       */
      def visit(n: Int): Unit =
        require(nodes contains n, "visit: No such node.")
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(visitedCount = curr.visitedCount + 1))

      /**
       * Update the mean value of a node given a new utility
       * @param n a node index in the MCST
       * @param u a new utility value
       */
      def updateMeanValue(n: Int, u: BigDecimal): Unit =
        require(nodes contains n, "valueOf: No such node.")
        val m = nodes(n).visitedCount
        val v = nodes(n).value
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = v + (u - v)/m))

      /**
       * Overwrite the value of a node with a new value
       * @param n a node index in the MCST
       * @param newValue a new utility value
       */
      def setValue(n: Int, newValue: BigDecimal): Unit =
        require(nodes contains n, "visit: No such node.")
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = newValue))

      /**
       * A debugging helper that recursively dumps the MCST to stdout.
       * (WARNING: don't use unless on the verge of dying of frustration while debugging)
       * @param node a node index in the MCST
       * @param depth dumps the node at this depth
       */
      def dump(node: Int = root, depth: Int = 0): Unit =
        require(nodes contains node, s"dump: No such node $node")

        val prefix = "  " * depth
        val n = nodes(node)
        val parentStr = n.parent.map(_.toString).getOrElse("None")

        println(s"$prefix- Node $node")
        println(s"$prefix  | Parent: $parentStr")
        println(s"$prefix  | Value: ${n.value}")
        println(s"$prefix  | Visits: ${n.visitedCount}")
        println(s"$prefix  | Children: ${n.children.keys.mkString(", ")}")

        n.children.values.foreach(child => dump(child, depth + 1))

    /**
     * THE SEARCH METHOD
     * Searches for the best move in the current belief state using the POMCP algorithm
     * @return the best move (according to it)
     */
    def plan: Move =
      (1 to NUM_SIMULATIONS).map(_ =>
        // sample a state
        val s: State = Tree.beliefStateAt(Tree.root).sampleState
        simulate(s, Tree.root, 0)
      )
      val bestMove = Tree.selectionPolicy(Tree.root)._1
      // prune root parent since its job is done in UCB1 computation
      Tree.pruneRootParent()
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
      if (depth >= TIME_HORIZON || DISCOUNT.pow(depth) <= DISCOUNT_HORIZON) && depth > 0 then 0.0
      else if s.isTerminal then -1000.0
      else if Tree.isLeaf(n) then
        Tree.beliefStateAt(n).possibleMoves.foreach(m => Tree.expandFrom(n, m))
        val playoutVal: BigDecimal = rollout(s, Tree.beliefStateAt(n), depth)
        Tree.visit(n)
        Tree.setValue(n, playoutVal)
        playoutVal
      else
        val (nextMove, nextNode) = Tree.selectionPolicy(n)
        val (successor, o, r) = generate(s, nextMove)
        val utility = r + (DISCOUNT * simulate(successor, Tree.getObsNode(nextNode, o), depth + 1))
        Tree.visit(n)
        Tree.visit(nextNode)
        Tree.updateMeanValue(nextNode, utility)
        utility

    /**
     * Define a rollout policy that dictates that choice of the next move during a rollout given the belief state
     * @param b a belief state
     * @return a move
     */
    private def rolloutPolicy(b: BeliefState): Move =
      val possibleMoves = b.possibleMoves
      val wumpusSquares =
        b.belief.filter(_.isInstanceOf[UnobservableWithWumpus])
          .map(_.asInstanceOf[UnobservableWithWumpus].wumpus)
      val pitCombinations = b.belief.map {
        case u: UnobservableWithWumpus => u.pits
        case u: UnobservableSansWumpus => u.pits
      }
      val goldLocations = b.belief.map {
        case u: UnobservableWithWumpus => u.gold
        case u: UnobservableSansWumpus => u.gold
      }
      b.history.lastOption match {
        case Some(update) => update match {
          case m: Move => assert(false, "Action after an action in history.")
          case o: Percept4 =>
            if o.glitter then Move.Grab
            else if o.stench && b.hasArrow then
              randomElem(possibleMoves.intersect(Set(Move.Shoot, Move.ShootRight, Move.ShootLeft)).toList)
            else
              randomElem(possibleMoves.intersect(Set(Move.GoForward, Move.GoLeft, Move.GoRight, Move.NoOp)).toList)
        }
        case None => randomElem((possibleMoves -- Set(Move.Shoot, Move.ShootRight, Move.ShootLeft)).toList)
      }

    /**
     * Generate a 3-tuple of a (state, percept, reward) given a state and move executed in that state.
     * Simply generates the successor state, the computed percept in the successor state, and the immediate
     * reward + heuristic of executing the given move in the given state.
     * @param s a state
     * @param m a move
     * @return a 3-tuple of (state, percept, reward)
     */
    private def generate(s: State, m: Move): (State, Percept4, BigDecimal) =
      val successor = s.transition(m)
      successor match {
        case sWithWumpus: StateWithWumpus => (sWithWumpus, Percept4(
          neighborsOf(sWithWumpus.agentPosition) contains sWithWumpus.u.wumpus,
          neighborsOf(sWithWumpus.agentPosition).intersect(sWithWumpus.u.pits).nonEmpty,
          sWithWumpus.agentPosition == sWithWumpus.u.gold,
          false
        ), s.reward(m) + s.heuristic(m))
        case sSansWumpus: StateSansWumpus => (sSansWumpus, Percept4(
          false,
          neighborsOf(sSansWumpus.agentPosition).intersect(sSansWumpus.u.pits).nonEmpty,
          sSansWumpus.agentPosition == sSansWumpus.u.gold,
          s.isInstanceOf[StateWithWumpus]
        ), s.reward(m) + s.heuristic(m))
      }

    /**
     * Recursively simulate a rollout until a specified time horizon.
     * @param s a state
     * @param b the belief state from which that state has been sampled
     * @param depth the current depth of the simulation (between 0 and the time horizon)
     * @return the value of the rollout computed as the sum of the immediate reward and the discounted utility
     */
    private def rollout(s: State, b: BeliefState, depth: Int): BigDecimal =
      if (depth >= TIME_HORIZON || DISCOUNT.pow(depth) <= DISCOUNT_HORIZON) && depth > 0 then 0.0
      else if s.isTerminal || b.isTerminal then -1000.0
      else
        val m = rolloutPolicy(b)
        val (successor, o, r) = generate(s, m)
        r + (DISCOUNT * rollout(successor, b.transition(m).observe(o), depth + 1))

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

  /**
   * Respawn the agent. Reset the search. Forget all insights.
   */
  override def reset(): Unit =
    POMCP.reset()
    actionQueue.clear()

  /**
   * Compute the initial belief prior by considering all position combinations of the pits, wumpus, and gold.
   * @return a set of belief particles forming a uniform prior
   */
  private def initialBeliefPrior: Set[Unobservable] =
    val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet
    // 1. Pit possibilities: 105 of these
    val possiblePitCombinations: Set[(Position, Position)] = (allSquares - ((1, 1))).toList.combinations(2).toSet.map {
      case Seq(p1, p2) => (p1, p2)
      case _ => assert(false, "Wtf?! 2-combinations should have size 2.")
    }
    // 2. Wumpus possibilities: 15 of these
    val possibleWumpusPositions: Set[Position] = allSquares - ((1, 1))
    // 3. Gold possibilities: 16 of these
    val possibleGoldLocations: Set[Position] = allSquares
    // A belief state is characterized by the positions of the gold, wumpus and both the pits
    possiblePitCombinations.flatMap {
      case (pit1, pit2) =>
        possibleWumpusPositions.flatMap(wumpus => possibleGoldLocations.map(gold =>
          UnobservableWithWumpus(gold, wumpus, pit1, pit2)
        ))
    }

  /**
   * Compute the neighbors of a square in the wumpus world. A neighbor of a square is any other square
   * that is adjacent but not diagonally adjacent. Hence, a square has at least 2 and at most 4 neighbors.
   * @param sq a position (x, y).
   * @return a set of positions that are all neighbors of (x, y).
   */
  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  /**
   * Compute the Manhattan distance between two squares in the wumpus world.
   * @param pos1 a position (x1, y1)
   * @param pos2 another position (x2, y2)
   * @return the Manhattan distance:= |x2 - x1| + |y2 - y1| 
   */
  private def manhattanDistance(pos1: Position, pos2: Position): Int =
    val (x1, y1) = pos1
    val (x2, y2) = pos2
    (x2 - x1).abs + (y2 - y1).abs

  /**
   * Compute the action to be executed by the UBA given the percepts: f_UBA: Ω* -> A.
   * @param tp the percepts.
   * @return the action to be executed by the utility-based agent.
   */
  override def process(tp: TransferPercept): Int =
    if actionQueue.isEmpty then
      POMCP.pruneTree(Percept4(tp.getStench, tp.getBreeze, tp.getGlitter, tp.getScream))
      val bestMove = POMCP.plan // search
      POMCP.pruneTree(bestMove)
      actionQueue.enqueueAll(bestMove.toActionSeq())
    actionQueue.dequeue

// 23:18