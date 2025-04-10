/*
 * Agent function for a hybrid learning agent in the unknown wumpus environment
 * f_HLA: Ω* -> A
 *
 * Project 4 - Learning Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.collection.mutable
import scala.language.postfixOps
import scala.math.{abs, pow, sqrt, log}
import scala.util.Random

object HybridLearningAgent extends AgentFunctionImpl:
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  private case class Percept(bump: Boolean, breeze: Boolean, stench: Boolean, glitter: Boolean, scream: Boolean):
    def isNone: Boolean = !bump && ! breeze && !stench && !glitter && !scream

  private sealed trait Unobservable

  // Unobservables with the wumpus position and without it are modeled and handled separately.
  private case class UnobservableWithWumpus(agent: Position, gold: Position, wumpus: Position,
                                            pit1: Position, pit2: Position) extends Unobservable:
    def toSans: UnobservableSansWumpus = UnobservableSansWumpus(agent, gold, pit1, pit2)

    def pits: Set[Position] = Set(pit1, pit2)

  private case class UnobservableSansWumpus(agent: Position, gold: Position,
                                            pit1: Position, pit2: Position) extends Unobservable:
    def pits: Set[Position] = Set(pit1, pit2)

  private sealed trait State:
    def isTerminal: Boolean

    def transition(m: Move): BeliefState =
      (this match {
        case s: StateWithWumpus => BeliefState(s.agentOrientation, s.hasArrow, Map(s.u -> 1), History.empty)
        case s: StateSansWumpus => BeliefState(s.agentOrientation, false, Map(s.u -> 1), History.empty)
      }).transition(m)


    def reward(m: Move): Int =
      val successor = transition(m)
      {
        if successor.isTerminal then -1000
        else m match {
          case Move.GoForward | Move.TurnLeft | Move.TurnRight => -1
          case Move.Shoot => -10
          case Move.NoOp => 0
          case Move.Grab => this match {
            case s: StateWithWumpus if s.u.agent == s.u.gold => 1000
            case s: StateSansWumpus if s.u.agent == s.u.gold => 1000
            case _ => -1
          }
        }
      }

    def heuristic(m: Move): Int =
      val successor = transition(m)

      // The distance to the gold is a big one
      def goldDist(state: State): Int =
        state match {
          case s: StateWithWumpus => manhattanDistance(s.u.gold, s.u.agent)
          case s: StateSansWumpus => manhattanDistance(s.u.gold, s.u.agent)
        }

      val h1 = -4 * (goldDist(successor.sampleState) - goldDist(this))

      // Assign a "wumpus score"
      def wumpusScore(state: State): Int =
        state match {
          case s: StateWithWumpus => 0
          case s: StateSansWumpus => 9
        }

      val h2 = wumpusScore(successor.sampleState) - wumpusScore(this)
      h1 + h2

  // States with wumpus alive and those with wumpus dead are modeled and handled separately
  // They inherit from State.
  private case class StateWithWumpus(agentOrientation: Orientation, hasArrow: Boolean,
                                     u: UnobservableWithWumpus) extends State:
    override def isTerminal: Boolean = Set(u.wumpus, u.pit1, u.pit2) contains u.agent

  private case class StateSansWumpus(agentOrientation: Orientation,
                                     u: UnobservableSansWumpus) extends State:
    override def isTerminal: Boolean = Set(u.pit1, u.pit2) contains u.agent

  private case class BeliefState(agentOrientation: Orientation,
                                 hasArrow: Boolean,
                                 belief: Map[Unobservable, BigDecimal],
                                 history: History):
    def u2State(u: Unobservable): State =
      require(belief contains u, "u2State: I don't believe this!")
      u match {
        case uWithWumpus: UnobservableWithWumpus => StateWithWumpus(agentOrientation, hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus => StateSansWumpus(agentOrientation, uSansWumpus)
      }

    def possibleMoves: Set[Move] =
      if isTerminal then return Set.empty
      Move.values.toSet filter {
        case Move.Shoot => hasArrow
        case _ => true
      }

    def isTerminal: Boolean = belief.isEmpty

    private def weightedParticleFilter(prior: Map[Unobservable, BigDecimal], obs: Boolean,
                               condition: Unobservable => Boolean): Map[Unobservable, BigDecimal] =
      // if obs then condition should be true else should be false
      val posterior = prior.filter((u, _) => condition(u) == obs)

      // normalize
      val t = posterior.values.sum
      posterior.map((u, p) => u -> p/t)

    def observe(percept: Percept): BeliefState =
      val posterior =
        weightedParticleFilter( // bump update
          weightedParticleFilter( // glitter update
            weightedParticleFilter( // breeze update
              weightedParticleFilter( // stench update
                weightedParticleFilter( // scream update
                  belief, percept.scream, u => history.lastOption match {
                    case Some(update) => update match {
                      case m: Move =>
                        if m == Move.Shoot then u.isInstanceOf[UnobservableSansWumpus]
                        else percept.scream // no filtering, essentially identity function, keep belief as is
                      case o: Percept => assert(false, "Observation after an observation in history.")
                    }
                    case None => percept.scream // no filtering, essentially identity function, keep belief as is
                  }
                ), percept.stench, {
                  case uWithWumpus: UnobservableWithWumpus =>
                    neighborsOf(uWithWumpus.agent) contains uWithWumpus.wumpus
                  case _ => false
                }
              ), percept.breeze, {
                case u: UnobservableWithWumpus =>
                  u.pits.exists(neighborsOf(u.agent).contains)
                case u: UnobservableSansWumpus =>
                  u.pits.exists(neighborsOf(u.agent).contains)
              }
            ), percept.glitter, {
              case u: UnobservableWithWumpus => u.agent == u.gold
              case u: UnobservableSansWumpus => u.agent == u.gold
            }
          ), percept.bump, {
            case u: UnobservableWithWumpus =>
              percept.bump && !(Set((2, 2), (2, 3), (3, 2), (3, 3)) contains u.agent)
            case u: UnobservableSansWumpus =>
              percept.bump && !(Set((2, 2), (2, 3), (3, 2), (3, 3)) contains u.agent)
          }
        )

      // normalize the posterior
      copy(belief = posterior, history = history.appendObs(percept))

    def transition(move: Move): BeliefState =
      val (ao, ha, posterior) = move match {
        case Move.GoForward => (agentOrientation, hasArrow,
          belief.toList.flatMap((u, p) => u match {
            case u: UnobservableWithWumpus =>
              val f: Position = agentOrientation.forwardFrom(u.agent)
              val uf = u.copy(agent = if hitsWall(f) then u.agent else f)
              val l: Position = agentOrientation.leftFrom(u.agent)
              val ul = u.copy(agent = if hitsWall(l) then u.agent else l)
              val r: Position = agentOrientation.rightFrom(u.agent)
              val ur = u.copy(agent = if hitsWall(r) then u.agent else r)
              List((uf, p * forwardProbability), (ul, p * slipProbability), (ur, p * slipProbability))
            case u: UnobservableSansWumpus =>
              val f: Position = agentOrientation.forwardFrom(u.agent)
              val uf = u.copy(agent = if hitsWall(f) then u.agent else f)
              val l: Position = agentOrientation.leftFrom(u.agent)
              val ul = u.copy(agent = if hitsWall(l) then u.agent else l)
              val r: Position = agentOrientation.rightFrom(u.agent)
              val ur = u.copy(agent = if hitsWall(r) then u.agent else r)
              List((uf, p * forwardProbability), (ul, p * slipProbability), (ur, p * slipProbability))
          }).groupMapReduce(_._1)(_._2)(_ + _)
        )
        case Move.TurnRight => (agentOrientation.turnRight, hasArrow, belief)
        case Move.TurnLeft => (agentOrientation.turnLeft, hasArrow, belief)
        // move = Shoot only when hasArrow is true as dictated by possibleMoves
        case Move.Shoot => (agentOrientation, false,
          belief.map((u, p) => (u match {
            case u: UnobservableWithWumpus =>
              val (xA, yA) = u.agent
              val (xW, yW) = u.wumpus
              agentOrientation match {
                case Orientation.North if xW == xA && yW > yA => u.toSans
                case Orientation.South if xW == xA && yW < yA => u.toSans
                case Orientation.East if xW > xA && yW == yA => u.toSans
                case Orientation.West if xW < xA && yW == yA => u.toSans
                case _ => u
              }
            case u: UnobservableSansWumpus => assert(false, "Sans wumpus before shooting?!")
          }, p))
        )
        case _ => (agentOrientation, hasArrow, belief)
      }

      // Filter out states in which agent is dead
      val alive = posterior.filter ((u, _) => u match {
        case u: UnobservableWithWumpus => !StateWithWumpus(ao, ha, u).isTerminal
        case u: UnobservableSansWumpus => !StateSansWumpus(ao, u).isTerminal
      })

      // normalize
      val t = alive.values.sum
      BeliefState(ao, ha, alive.map((u, p) => u -> p/t), history.appendMove(move))

    def sampleState: State =
      val sorted = belief.toList.sortBy(_._2)(Ordering[BigDecimal].reverse).toMap

      val totalProbability = sorted.values.sum
      require(totalProbability > 0, "Total probability must be positive.")

      val r = BigDecimal(Random nextDouble) * totalProbability

      u2State(
        sorted.toList.foldLeft((sorted.head._1, BigDecimal(0), false)) {
          case (acc, (u, p)) =>
            val (sample, cumulative, isSet) = acc
            if isSet then (sample, cumulative, isSet)
            else if cumulative + p >= r then (u, cumulative + p, true)
            else (u, cumulative + p, isSet)
        }._1
      )

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

  private enum Move(val action: Int):
    case GoForward extends Move(Action.GO_FORWARD)
    case TurnRight extends Move(Action.TURN_RIGHT)
    case TurnLeft extends Move(Action.TURN_LEFT)
    case Shoot extends Move(Action.SHOOT)
    case NoOp extends Move(Action.NO_OP)
    case Grab extends Move(Action.GRAB)

  private case class History(moveHist: List[Move], obsHist: List[Percept]):
    require((moveHist.length - obsHist.length).abs <= 1, "Action-observation mismatch.")

    private def isEmpty: Boolean = moveHist.isEmpty && obsHist.isEmpty

    def appendMove(m: Move): History = History(moveHist :+ m, obsHist)

    def appendObs(o: Percept): History = History(moveHist, obsHist :+ o)

    def lastOption: Option[Move | Percept] =
      if obsHist.length - moveHist.length == 1 then Some(obsHist.last)
      else moveHist.lastOption

    def numMoves: Int = moveHist.length

    def countObs(condition: Percept => Boolean): Int = obsHist.count(condition)

  // Companion object for the History case class.
  private case object History:
    def empty: History = History(List.empty, List.empty)

  /**
   * Implement partially observable Monte Carlo planning (POMCP)
   */
  private case object POMCP:
    private case class Node(beliefState: BeliefState,
                            parent: Option[Int],
                            children: mutable.Map[Move | Percept, Int],
                            visitedCount: Int,
                            value: BigDecimal)

    // parameters that influence the performance of POMCP, pretty self-explanatory
    private final val TIME_HORIZON = 15
    private final val DISCOUNT_HORIZON = 0
    private final val NUM_SIMULATIONS = 1000
    private final val DISCOUNT = BigDecimal(0.2)
    private final val EXPLORATION_CONST = BigDecimal(sqrt(2)) // used in UCT - UCB1

    private case object Tree:
      var root = -1 // current root index
      private val count: Counter = Counter(-1) // a counter that generates node indices on demand

      // a map from indices to MCST nodes
      private val nodes: mutable.Map[Int, Node] = mutable.Map.empty

      // a map from belief states to indices
      private val indices: mutable.Map[BeliefState, Int] = mutable.Map.empty

      def initialize(o: Percept): Unit =
        val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet

        val possiblePitCombinations = (allSquares diff pitFree).toList.combinations(2).toSet.map {
          case Seq(p1, p2) => (p1, p2)
          case _ => assert(false, "Wtf?! 2-combinations should have size 2.")
        }

        val possibleWumpusPositions =
          if wumpus == (0, 1) then allSquares diff pitFree
          else if wumpus == (0, 0) then Set.empty[Position]
          else Set(wumpus)

        val possibleGoldLocations = allSquares diff Set((1, 1), (2, 1))

        val unweighted: Set[Unobservable] =
          if possibleWumpusPositions nonEmpty then
            possiblePitCombinations flatMap {
              case (p1, p2) => possibleWumpusPositions flatMap (w =>
                possibleGoldLocations flatMap (g => agentPosPrior map (a =>
                  UnobservableWithWumpus(a, g, w, p1, p2)
                ))
              )
            }
          else possiblePitCombinations flatMap {
            case (p1, p2) => possibleGoldLocations flatMap (g =>
              agentPosPrior map (a => UnobservableSansWumpus(a, g, p1, p2))
            )
          }

        val s = unweighted.size
        val weighted: Map[Unobservable, BigDecimal] = unweighted.map(u => u -> BigDecimal(1)/BigDecimal(s)).toMap

        val initialBeliefState: BeliefState =
          BeliefState(Orientation.East, hasArrow, weighted, learningHistory).observe(o)

        nodes += (-1 -> Node(
          initialBeliefState, None, mutable.Map.empty, 0, 0
        ))

        indices += (initialBeliefState -> -1)

      def reset(): Unit =
        root = -1
        count.reset()
        nodes.clear()
        indices.clear()

      def expandFrom(parent: Int, update: Move | Percept): Unit =
        val newBeliefState = update match {
          case m: Move => nodes(parent).beliefState.transition(m)
          case o: Percept => nodes(parent).beliefState.observe(o)
        }
        indices.get(newBeliefState) match {
          case Some(n) => nodes(parent).children += (update -> n)
          case None =>
            nodes += (count.next -> Node(newBeliefState, Some(parent), mutable.Map.empty, 0, 0.0))
            indices += (newBeliefState -> count.get)
            nodes(parent).children += (update -> count.get)
        }

      def isLeaf(n: Int): Boolean =
        require(nodes contains n, "isLeaf: No such node.")
        nodes(n).children.isEmpty

      def getObsNode(n: Int, o: Percept): Int =
        require(nodes contains n, "getObsNode: Invalid node index.")
        if !(nodes(n).children contains o) then expandFrom(n, o)
        nodes(n).children(o)

      private def pruneRecursively(root: Int, newRoot: Int): Unit =
        if root != newRoot then
          nodes(root).children foreach ((_, n) => pruneRecursively(n, newRoot))
          nodes.subtractOne(root)
          indices.filterInPlace((_, n) => n != root)

      def prune(update: Move | Percept): Unit =
        val children = nodes(root).children
        val newRoot = update match {
          case m: Move =>
            require(children contains update, "prune: No such child.")
            children(m)
          case o: Percept => getObsNode(root, o)
        }
        children foreach ((_, n) => pruneRecursively(n, newRoot))
        nodes.get(root).foreach(curr => nodes(root) = curr.copy(children = children.filter(_._2 == newRoot)))
        root = newRoot

      def pruneRootParent(): Unit =
        require(nodes contains root, "pruneRootParent: Root node not found.")
        nodes(root).parent match {
          case Some(p) => nodes subtractOne p
          case None => // parent is already pruned or no parent
        }
        nodes.get(root).foreach(curr => nodes(root) = curr.copy(parent = None))

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

      def selectionPolicy(n: Int): (Move, Int) =
        require(nodes contains n, "selection policy: No such node.")
        val actionLeaves = nodes(n).children.filter((t, c) => t.isInstanceOf[Move] &&
          t.asInstanceOf[Move] != Move.Shoot && isLeaf(c)
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
            case Move.GoForward => 1
            case Move.TurnLeft => 1
            case Move.TurnRight => 1
            case Move.Shoot => 0 //50 * h.countObs(_.stench)
            case Move.NoOp => 1 //50
            case Move.Grab => 0
          }))
          best._1 match {
            case m: Move => (m, best._2)
            case o: Percept => assert(false, "selectionPolicy: Finding an observation child is not possible.")
          }

      def beliefStateAt(n: Int): BeliefState =
        require(nodes contains n, "beliefStateAt: No such node.")
        nodes(n).beliefState

      def visit(n: Int): Unit =
        require(nodes contains n, "visit: No such node.")
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(visitedCount = curr.visitedCount + 1))

      def updateMeanValue(n: Int, u: BigDecimal): Unit =
        require(nodes contains n, "valueOf: No such node.")
        val m = nodes(n).visitedCount
        val v = nodes(n).value
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = v + (u - v) / m))

      def setValue(n: Int, newValue: BigDecimal): Unit =
        require(nodes contains n, "visit: No such node.")
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = newValue))

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
     *
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

    private def rolloutPolicy(b: BeliefState): Move =
      val possibleMoves = b.possibleMoves
      b.history.lastOption match {
        case Some(update) => update match {
          case m: Move => assert(false, "Action after an action in history.")
          case o: Percept =>
            if o.glitter then Move.Grab
            else if o.stench && b.hasArrow then Move.Shoot
            else
              randomElem(possibleMoves.intersect(Set(Move.GoForward, Move.TurnLeft, Move.TurnRight, Move.NoOp)).toList)
        }
        case None => randomElem((possibleMoves - Move.Shoot).toList)
      }

    private def generate(s: State, m: Move): (State, Percept, BigDecimal) =
      val successor = s.transition(m).sampleState
      successor match {
        case sWithWumpus: StateWithWumpus => (sWithWumpus, Percept(
          m == Move.GoForward && sWithWumpus.u.agent == s.asInstanceOf[StateWithWumpus].u.agent,
          neighborsOf(sWithWumpus.u.agent).intersect(sWithWumpus.u.pits).nonEmpty,
          neighborsOf(sWithWumpus.u.agent) contains sWithWumpus.u.wumpus,
          sWithWumpus.u.agent == sWithWumpus.u.gold,
          false
        ), s.reward(m) + s.heuristic(m))
        case sSansWumpus: StateSansWumpus => (sSansWumpus, Percept(
          m == Move.GoForward && sSansWumpus.u.agent == s.asInstanceOf[StateSansWumpus].u.agent,
          neighborsOf(sSansWumpus.u.agent).intersect(sSansWumpus.u.pits).nonEmpty,
          false,
          sSansWumpus.u.agent == sSansWumpus.u.gold,
          s.isInstanceOf[StateWithWumpus]
        ), s.reward(m) + s.heuristic(m))
      }

    private def rollout(s: State, b: BeliefState, depth: Int): BigDecimal =
      if (depth >= TIME_HORIZON || DISCOUNT.pow(depth) <= DISCOUNT_HORIZON) && depth > 0 then 0.0
      else if s.isTerminal || b.isTerminal then -1000.0
      else
        val m = rolloutPolicy(b)
        val (successor, o, r) = generate(s, m)
        r + (DISCOUNT * rollout(successor, b.transition(m).observe(o), depth + 1))

    def pruneTree(update: Move | Percept): Unit = Tree.prune(update)

    def initialize(o: Percept): Unit = Tree.initialize(o)

    def reset(): Unit = Tree.reset()

    def dumpTree(): Unit = Tree.dump()

  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def manhattanDistance(pos1: Position, pos2: Position): Int =
    val (x1, y1) = pos1
    val (x2, y2) = pos2
    (x2 - x1).abs + (y2 - y1).abs

  private def hitsWall(sq: Position): Boolean =
    val (x, y) = sq
    x < 1 || x > 4 || y < 1 || y > 4

  private var forwardProbability: BigDecimal = 0
  private def slipProbability: BigDecimal = (1 - forwardProbability) / 2

  // keep a partial model to initialize helper agents with
  private var hasArrow: Boolean = true
  private val pitFree: mutable.Set[Position] = mutable.Set((1, 1))
  private var wumpus = (0, 1)
  private val agentPosPrior: mutable.Set[Position] = mutable.Set((1, 1))
  private var learningHistory: History = History.empty

  private object Learning:
    private enum LearningState:
      case Start, GivenUp, Running, Stop

    private val maxLearningIters = 20
    private val learningExperience: mutable.ListBuffer[Boolean] = mutable.ListBuffer.empty
    private var state: LearningState = LearningState.Start

    def learnt: BigDecimal =
      // Return maximum likelihood result
      val failureCount = learningExperience.count(!_)
      if failureCount == 0 then 1
      else if failureCount == 1 then 0.8
      else
        val n: Double = learningExperience.takeWhile(identity).length
        val m: Double = learningExperience.reverse.tail.takeWhile(identity).length
        val discriminant: Double = sqrt(pow(2*m + n + 2, 2) - 8*n)
        val pMle: Double = (n - 2 + discriminant) / (2 * (n + m + 2))
        if abs(pMle - 0.8) > abs(pMle - 1D/3D) then 1D/3D
        else 0.8

    def notDone: Boolean = state != LearningState.Stop

    def reset(): Unit = state = LearningState.Start

    object StateMachine:
      def transition(o: Percept): Unit =
        state = state match {
          case LearningState.Start =>
            if o.breeze then
              actionQueue enqueue Action.NO_OP
              LearningState.GivenUp
            else
              pitFree ++= Set((1, 2), (2, 1))
              if o.stench then
                if hasArrow then
                  hasArrow = false
                  actionQueue enqueue Action.SHOOT
                  LearningState.Start
                else
                  wumpus = (1, 2)
                  actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                  LearningState.Running
              else if o.scream then
                wumpus = (0, 0)
                actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                LearningState.Running
              else
                actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                LearningState.Running
          case LearningState.GivenUp =>
            actionQueue enqueue Action.NO_OP
            LearningState.GivenUp
          case LearningState.Running =>
            learningExperience += o.bump
            val failureCount = learningExperience.count(!_)
            if !o.bump && failureCount == 1 then
              agentPosPrior -= ((1, 1)) += ((2, 1))
            if learningExperience.length == maxLearningIters || failureCount == 2 then
              if failureCount == 2 then
                agentPosPrior -= ((2, 1)) ++= Set((1, 1), (3, 1))
              actionQueue enqueue Action.TURN_LEFT
              LearningState.Stop
            else if o.breeze then
              actionQueue enqueue Action.NO_OP
              LearningState.GivenUp
            else
              if agentPosPrior.toSet == Set((2, 1)) then
                pitFree ++= Set((2, 2), (3, 1))
              if o.stench then
                if hasArrow then
                  hasArrow = false
                  actionQueue enqueue (Action.TURN_LEFT, Action.SHOOT, Action.TURN_RIGHT, Action.GO_FORWARD)
                else
                  if agentPosPrior.toSet == Set((2, 1)) then wumpus = (2, 2)
                  actionQueue enqueue Action.GO_FORWARD
              else
                if o.scream then wumpus = (0, 0)
                actionQueue enqueue Action.GO_FORWARD
              LearningState.Running
          case LearningState.Stop => LearningState.Stop
        }

  override def reset(): Unit =
    hasArrow = true
    pitFree.clear()
    pitFree += 1 -> 1
    wumpus = (0, 1)
    agentPosPrior.clear()
    agentPosPrior += 1 -> 1
    learningHistory = History.empty
    forwardProbability = 0
    Learning.reset()
    POMCP.reset()
    ModelBasedReflexAgent.reset()

  private object UtilityBasedHelper:
    def process(o: Percept, init: Boolean = false): Move =
      if init then POMCP.initialize(o)
      else POMCP.pruneTree(o)
      val utilityBasedMove = POMCP.plan
      val move =
        if o.glitter && utilityBasedMove == Move.Grab then Move.NoOp
        else if !o.stench && utilityBasedMove == Move.Shoot then Move.NoOp
        else utilityBasedMove
      POMCP.pruneTree(move)
      move

  private object SimpleReflexHelper:
    def process(o: Percept, init: Boolean = false): Move =
      if init then actionQueue enqueue Action.TURN_LEFT
      if o.breeze || o.stench then Move.NoOp
      else Move.GoForward

  override def process(tp: TransferPercept): Int =
    val percept = Percept(tp getBump, tp getBreeze, tp getStench, tp getGlitter, tp getScream)
    if actionQueue isEmpty then
      if percept.glitter then
        actionQueue enqueue Action.GRAB
      else if Learning notDone then
        Learning.StateMachine.transition(percept)
      else if forwardProbability == 0 then
        forwardProbability = Learning.learnt
        if forwardProbability == 1 then
          ModelBasedReflexAgent.reset()
          ModelBasedReflexAgent.hlaInit(hasArrow)
          actionQueue enqueue ModelBasedReflexAgent.process(tp)
        else
//          actionQueue enqueue UtilityBasedHelper.process(percept, true).action
          actionQueue enqueue SimpleReflexHelper.process(percept, true).action
      else if forwardProbability == 1 then
        actionQueue enqueue ModelBasedReflexAgent.process(tp)
      else
//        actionQueue enqueue UtilityBasedHelper.process(percept).action
        actionQueue enqueue SimpleReflexHelper.process(percept).action
    val action = actionQueue.dequeue
    if Learning notDone then
      val move =
        if action == Action.GO_FORWARD then Move.GoForward
        else if action == Action.TURN_LEFT then Move.TurnLeft
        else if action == Action.TURN_RIGHT then Move.TurnRight
        else if action == Action.GRAB then Move.Grab
        else if action == Action.SHOOT then Move.Shoot
        else Move.NoOp
      learningHistory = learningHistory.appendObs(percept).appendMove(move)
    action
