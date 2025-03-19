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

  private case class Percept4(stench: Boolean, breeze: Boolean, glitter: Boolean, scream: Boolean):
    def isNone: Boolean = !stench && !breeze && !glitter && !scream

  private sealed trait State:
    def isTerminal: Boolean
    
    def transition(m: Move): State =
      val dummy = (this match {
        case s: StateWithWumpus => BeliefState(s.agentPosition, s.agentOrientation, s.hasArrow, Set(s.u), History.empty)
        case s: StateSansWumpus => BeliefState(s.agentPosition, s.agentOrientation, false, Set(s.u), History.empty)
      }).transition(m)
      if dummy.belief.isEmpty then this match {
        case s: StateWithWumpus => StateWithWumpus(dummy.agentPosition, dummy.agentOrientation, dummy.hasArrow, s.u)
        case s: StateSansWumpus => StateSansWumpus(dummy.agentPosition, dummy.agentOrientation, s.u)
      }
      else dummy.belief.head match {
        case uWithWumpus: UnobservableWithWumpus =>
          StateWithWumpus(dummy.agentPosition, dummy.agentOrientation, dummy.hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus =>
          StateSansWumpus(dummy.agentPosition, dummy.agentOrientation, uSansWumpus)
      }

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
         case s: StateSansWumpus => 20
       }
     val h2 = wumpusScore(successor) - wumpusScore(this)
     h1 + h2

  private case class StateWithWumpus(agentPosition: Position, agentOrientation: Orientation, hasArrow: Boolean,
                                     u: UnobservableWithWumpus) extends State:
    override def isTerminal: Boolean = Set(u.wumpus, u.pit1, u.pit2) contains agentPosition

  private case class StateSansWumpus(agentPosition: Position, agentOrientation: Orientation,
                                     u: UnobservableSansWumpus) extends State:
    override def isTerminal: Boolean = Set(u.pit1, u.pit2) contains agentPosition

  private sealed trait Unobservable

  private case class UnobservableWithWumpus(gold: Position, wumpus: Position,
                                            pit1: Position, pit2: Position) extends Unobservable:
    def toSans: UnobservableSansWumpus = UnobservableSansWumpus(gold, pit1, pit2)
    def pits: Set[Position] = Set(pit1, pit2)

  private case class UnobservableSansWumpus(gold: Position, pit1: Position, pit2: Position) extends Unobservable:
    def pits: Set[Position] = Set(pit1, pit2)


  private case class BeliefState(agentPosition: Position,
                                 agentOrientation: Orientation,
                                 hasArrow: Boolean,
                                 belief: Set[Unobservable],
                                 history: History):
    def u2State(u: Unobservable): State =
      u match {
        case uWithWumpus: UnobservableWithWumpus => StateWithWumpus(agentPosition, agentOrientation, hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus => StateSansWumpus(agentPosition, agentOrientation, uSansWumpus)
      }

    private enum Direction:
      case Forward, Right, Left, Back

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

    def isTerminal: Boolean = belief.isEmpty

    private def particleFilter(prior: Set[Unobservable], obs: Boolean,
                               condition: Unobservable => Boolean): Set[Unobservable] =
      prior.filter(condition(_) == obs)

    // the percept is observed at agentPosition (in the current state)
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
      
    def sampleState: State = randomElem(belief.toList) match {
      case uWith: UnobservableWithWumpus => StateWithWumpus(agentPosition, agentOrientation, hasArrow, uWith)
      case uSans: UnobservableSansWumpus => StateSansWumpus(agentPosition, agentOrientation, uSans)
    }

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

//  private var currentHistory = History(List.empty, List.empty)

  private case class History(moveHist: List[Move], obsHist: List[Percept4]):
    require((moveHist.length - obsHist.length).abs <= 1, "Action-observation mismatch.")

    private def isEmpty: Boolean = moveHist.isEmpty && obsHist.isEmpty

    def appendMove(m: Move): History = History(moveHist :+ m, obsHist)

    def appendObs(o: Percept4): History = History(moveHist, obsHist :+ o)

    def lastOption: Option[Move | Percept4] =
      if obsHist.length - moveHist.length == 1 then Some(obsHist.last)
      else moveHist.lastOption

  private case object History:
    def empty: History = History(List.empty, List.empty)

  private case object POMCP:
    private case class Node(beliefState: BeliefState,
                            parent: Option[Int],
                            children: mutable.Map[Move | Percept4, Int],
                            visitedCount: Int,
                            value: BigDecimal)

    private final val TIME_HORIZON = 15
    private final val DISCOUNT_HORIZON = 0
    private final val NUM_SIMULATIONS = 10000
    private final val DISCOUNT = BigDecimal(0.2)
    private final val EXPLORATION_CONST = BigDecimal(sqrt(2))

    private case object Tree:
      var root = -1
      private val count: Counter = Counter(-1)
      private val nodes: mutable.Map[Int, Node] = mutable.Map(-1 -> Node(
        BeliefState((1, 1), Orientation.East, true, initialBeliefPrior, History.empty), None, mutable.Map.empty, 0, 0.0
      ))

      def reset(): Unit =
        root = -1
        count.reset()
        nodes.clear()
        nodes += (-1 -> Node(
          BeliefState((1, 1), Orientation.East, true, initialBeliefPrior, History.empty), None, mutable.Map.empty, 0, 0.0
        ))

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

      def isLeaf(n: Int): Boolean =
        require(nodes contains n, "isLeaf: No such node.")
        nodes(n).children.isEmpty

      def getObsNode(h: Int, o: Percept4): Int =
        require(nodes contains h, "getObsNode: Invalid node index.")
        if !(nodes(h).children contains o) then expandFrom(h, o)
        nodes(h).children(o)

      private def pruneRecursively(root: Int, newRoot: Int): Unit =
        if root != newRoot then
          nodes(root).children foreach ((_, n) => pruneRecursively(n, newRoot))
          nodes.subtractOne(root)

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

      def pruneRootParent(): Unit =
        require(nodes contains root, "pruneRootParent: Root node not found.")
        nodes(root).parent match {
          case Some(p) => nodes subtractOne p
          case None => // parent is already pruned
        }
        nodes.get(root).foreach(curr => nodes(root) = curr.copy(parent = None))

      private def ucb1(n: Int): BigDecimal =
        require(nodes contains n, "UCB1: No such node.")

        val moveExplrWeight =
          nodes(n).parent match {
            case Some(p) =>
              val moveToHere = randomElem(nodes(p).children.filter(_._2 == n).keySet.toList)
              assert(moveToHere.isInstanceOf[Move], "UCB1 is called for a non-action node??")
              moveToHere.asInstanceOf[Move] match {
                case Move.Grab => BigDecimal(0)
                case Move.GoBack => BigDecimal(1)
                case Move.NoOp | Move.Shoot | Move.ShootLeft | Move.ShootRight => BigDecimal(50)
                case _ => BigDecimal(111)
              }
            case None => BigDecimal(1)
          }

        val node = nodes(n)
        node.value + (EXPLORATION_CONST * moveExplrWeight * sqrt(log(
          node.parent match {
            case Some(p) => nodes(p).visitedCount
            case None => 0
          }
        ) / node.visitedCount))

      def selectionPolicy(n: Int): (Move, Int) =
        require(nodes contains n, "selection policy: No such node.")
        val actionLeaves = nodes(n).children.filter((t, c) => t.isInstanceOf[Move] && isLeaf(c))
        if actionLeaves.nonEmpty then // if there exist leaf children
          // try matching against a rollout policy
          val rp = rolloutPolicy(nodes(n).beliefState)
          if actionLeaves contains rp then (rp, actionLeaves(rp))
          else // otherwise choose a random action
            val ret = randomElem(actionLeaves.keySet.toList).asInstanceOf[Move]
            (ret, actionLeaves(ret))
        else // else choose the node with the highest value
          val best = nodes(n).children.maxBy((_, c) => ucb1(c))
          best._1 match {
            case m: Move => (m, best._2)
            case o: Percept4 => assert(false, "selectionPolicy: Finding an observation child is not possible.")
          }

      def currentBeliefState: BeliefState =
        require(nodes contains root, "currentBeliefState: No root in tree.")
        beliefStateAt(root)

      def possibleMovesFrom(n: Int): Set[Move] =
        require(nodes contains n, "possibleMovesFrom: No such node.")
        beliefStateAt(n).possibleMoves

      def beliefStateAt(n: Int): BeliefState =
        require(nodes contains n, "history: No such node.")
        nodes(n).beliefState

      def visit(n: Int): Unit =
        require(nodes contains n, "visit: No such node.")
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(visitedCount = curr.visitedCount + 1))

      def updateMeanValue(n: Int, u: BigDecimal): Unit =
        require(nodes contains n, "valueOf: No such node.")
        val m = nodes(n).visitedCount
        val v = nodes(n).value
        nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = v + (u - v)/m))

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

    def search: Move =
      (1 to NUM_SIMULATIONS).map(_ =>
        // sample a state
        val s: State = Tree.currentBeliefState.sampleState
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
        Tree.possibleMovesFrom(n).foreach(m => Tree.expandFrom(n, m))
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

    private def rollout(s: State, b: BeliefState, depth: Int): BigDecimal =
      if (depth >= TIME_HORIZON || DISCOUNT.pow(depth) <= DISCOUNT_HORIZON) && depth > 0 then 0.0
      else if s.isTerminal || b.isTerminal then -1000.0
      else
        val m = rolloutPolicy(b)
        val (successor, o, r) = generate(s, m)
        r + (DISCOUNT * rollout(successor, b.transition(m).observe(o), depth + 1))

    def pruneTree(update: Move | Percept4): Unit = Tree.prune(update)

    def reset(): Unit = Tree.reset()

    def dumpTree(): Unit = Tree.dump()

  override def reset(): Unit =
    POMCP.reset()
    actionQueue.clear()

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

  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def manhattanDistance(pos1: Position, pos2: Position): Int =
    val (x1, y1) = pos1
    val (x2, y2) = pos2
    (x2 - x1).abs + (y2 - y1).abs

  override def process(tp: TransferPercept): Int =
    if actionQueue.isEmpty then
      POMCP.pruneTree(Percept4(tp.getStench, tp.getBreeze, tp.getGlitter, tp.getScream))
      val bestMove = POMCP.search
      POMCP.pruneTree(bestMove)
//      POMCP.dumpTree()
      actionQueue.enqueueAll(bestMove.toActionSeq())
    actionQueue.dequeue
