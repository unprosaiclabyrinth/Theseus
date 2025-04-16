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
import scala.language.postfixOps

object UtilityBasedAgent extends AgentFunctionImpl:
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

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

  private enum Move(val toActionSeq: () => List[Int], val reward: Int):
    case GoForward extends Move(() => List(Action.GO_FORWARD), -1)
    case GoLeft extends Move(() => List(Action.TURN_LEFT, Action.GO_FORWARD), -2)
    case GoRight extends Move(() => List(Action.TURN_RIGHT, Action.GO_FORWARD), -2)
    case GoBack extends Move(() => {
      val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
      List(randomTurn, randomTurn, Action.GO_FORWARD)
    }, -3)
    case Shoot extends Move(() => List(Action.SHOOT), -10)
    case ShootLeft extends Move(() => List(Action.TURN_LEFT, Action.SHOOT), -11)
    case ShootRight extends Move(() => List(Action.TURN_RIGHT, Action.SHOOT), -11)
    case NoOp extends Move(() => List(Action.NO_OP), 0)
    case Grab extends Move(() => List(Action.GRAB), -1)

  private case class Percept4(breeze: Boolean, stench: Boolean, glitter: Boolean, scream: Boolean)

  private trait Unobservable:
    def pits: Set[Position] = this match {
      case u: UnobservableWithWumpus => Set(u.pit1, u.pit2)
      case u: UnobservableSansWumpus => Set(u.pit1, u.pit2)
    }

    def danger: Set[Position] = this match {
      case u: UnobservableWithWumpus => pits ++ Set(u.wumpus)
      case u: UnobservableSansWumpus => pits
    }

    def goldPos: Position = this match {
      case u: UnobservableWithWumpus => u.gold
      case u: UnobservableSansWumpus => u.gold
    }

  private case class UnobservableWithWumpus(wumpus: Position, gold: Position, pit1: Position, pit2: Position) extends Unobservable:
    infix def toSans: UnobservableSansWumpus = UnobservableSansWumpus(gold, pit1, pit2)

  private case class UnobservableSansWumpus(gold: Position, pit1: Position, pit2: Position) extends Unobservable

  private case class BeliefState(agentPosition: Position,
                                 agentOrientation: Orientation,
                                 hasArrow: Boolean,
                                 belief: Set[Unobservable],
                                 lastUpdate: Move | Percept4,
                                 numDeaths: Int,
                                 numGolds: Int):
    def possibleMoves: Set[Move] = lastUpdate match {
      case m: Move => assert(false, "No possible moves from an action node.")
      case o: Percept4 =>
        val validFront = !outOfBounds(agentOrientation.forwardFrom(agentPosition))
        val validLeft = !outOfBounds(agentOrientation.leftFrom(agentPosition))
        val validRight = !outOfBounds(agentOrientation.rightFrom(agentPosition))
        val validBack = !outOfBounds(agentOrientation.backFrom(agentPosition))
        val inclusionCondition: Map[Move, Boolean] = Move.values.map(m => m -> (m match {
          case Move.GoForward => validFront
          case Move.GoLeft => validLeft
          case Move.GoRight => validRight
          case Move.GoBack => validBack
          case Move.Shoot => hasArrow && validFront && o.stench
          case Move.ShootLeft => hasArrow && validLeft && o.stench
          case Move.ShootRight => hasArrow && validRight && o.stench
          case Move.NoOp => true
          case Move.Grab => o.glitter
        })).toMap
        Move.values.toSet filter inclusionCondition
    }

    private def possibleObsValues(obsCondition: Unobservable => Boolean): Set[Boolean] =
      Set(belief exists obsCondition, belief forall obsCondition)

    def possibleObservations: Set[Percept4] =
      possibleObsValues(u => u.pits exists (neighborsOf(agentPosition) contains)) flatMap (breeze =>
        possibleObsValues({
          case u: UnobservableWithWumpus => neighborsOf(agentPosition) contains u.wumpus
          case u: UnobservableSansWumpus => false
        }) flatMap (stench =>
          possibleObsValues(_.goldPos == agentPosition) flatMap (glitter =>
            possibleObsValues(u => lastUpdate match {
              case m: Move => u.isInstanceOf[UnobservableSansWumpus] && (Set(Move.Shoot, Move.ShootLeft, Move.ShootRight) contains m)
              case o: Percept4 => assert(false, "No possible observations from an observation node.")
            }) map (scream =>
              Percept4(breeze, stench, glitter, scream)
            )
          )
        )
      ) filterNot (o => o.stench && o.scream)

    private def wumpusInLineOfFire(u: UnobservableWithWumpus, lineOfFire: Orientation): Boolean =
      val (xA, yA) = agentPosition
      val (xW, yW) = u.wumpus
      lineOfFire match {
        case Orientation.North => xW == xA && yW > yA
        case Orientation.South => xW == xA && yW < yA
        case Orientation.East => xW > xA && yW == yA
        case Orientation.West => xW < xA && yW == yA
      }

    infix def transition(m: Move): BeliefState =
      require(possibleMoves contains m, "Not a possible move.")
      val (ap, ao, ha, posterior) = m match {
        case Move.GoForward => (agentOrientation.forwardFrom(agentPosition), agentOrientation, hasArrow, belief)
        case Move.GoLeft => (agentOrientation.leftFrom(agentPosition), agentOrientation.turnLeft, hasArrow, belief)
        case Move.GoRight => (agentOrientation.rightFrom(agentPosition), agentOrientation.turnRight, hasArrow, belief)
        case Move.GoBack => (agentOrientation.backFrom(agentPosition), agentOrientation.turnBack, hasArrow, belief)
        case Move.Shoot => (agentPosition, agentOrientation, false, belief map {
          case u: UnobservableWithWumpus =>
            if wumpusInLineOfFire(u, agentOrientation) then u toSans
            else u
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus but hasArrow?!")
        })
        case Move.ShootLeft => (agentPosition, agentOrientation.turnLeft, false, belief map {
          case u: UnobservableWithWumpus =>
            if wumpusInLineOfFire(u, agentOrientation.turnLeft) then u toSans
            else u
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus but hasArrow?!")
        })
        case Move.ShootRight => (agentPosition, agentOrientation.turnRight, false, belief map {
          case u: UnobservableWithWumpus =>
            if wumpusInLineOfFire(u, agentOrientation.turnRight) then u toSans
            else u
          case u: UnobservableSansWumpus => assert(false, "Sans wumpus but hasArrow?!")
        })
        case _ => (agentPosition, agentOrientation, hasArrow, belief)
      }

      val (deaths, nonterminalAndGolds) = posterior partition (_.danger contains agentPosition)
      val (golds, nonterminal) = nonterminalAndGolds partition (_.goldPos == agentPosition && m == Move.Grab)

      BeliefState(ap, ao, ha, nonterminal, m, numDeaths + deaths.size, numGolds + golds.size)

    private def particleFilterOnObs(obs: Boolean, prior: Set[Unobservable], obsCondition: Unobservable => Boolean): Set[Unobservable] =
      prior filter (u => obsCondition(u) == obs)

    infix def observe(o: Percept4): BeliefState =
      copy(belief = particleFilterOnObs(
        o.breeze, particleFilterOnObs(
          o.stench, particleFilterOnObs(
            o.glitter, particleFilterOnObs(
              o.scream, belief, u => lastUpdate match {
                case m: Move => (Set(Move.Shoot, Move.ShootLeft, Move.ShootRight) contains m) && u.isInstanceOf[UnobservableSansWumpus]
                case o: Percept4 => assert(false, "Cannot observe from an observation node.")
              }
            ), _.goldPos == agentPosition
          ), {
            case u: UnobservableWithWumpus => neighborsOf(agentPosition) contains u.wumpus
            case u: UnobservableSansWumpus => false
          }
        ), _.pits exists (neighborsOf(agentPosition) contains)
      ), lastUpdate = o)

    def eval: Double = (-1000 * numDeaths) + (1000 * numGolds) - belief.size

    def isTerminal: Boolean = belief isEmpty

  private def outOfBounds(sq: Position): Boolean =
    val (x, y) = sq
    x < 1 || x > 4 || y < 1 || y > 4

  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filterNot outOfBounds

  private def manhattanDistance(sq1: Position, sq2: Position): Int =
    val (x1, y1) = sq1
    val (x2, y2) = sq2
    (y2 - y1).abs + (x2 - x1).abs

  private def initialBeliefState: BeliefState =
    val allSquares: Set[Position] = ((1 to 4) flatMap (x => (1 to 4) map (y => (x, y)))).toSet
    val possiblePitCombinations: Set[(Position, Position)] = (allSquares - (1 -> 1)).toList.combinations(2).map(l => (l.head, l.tail.head)).toSet
    val possibleWumpusPositions: Set[Position] = allSquares - (1 -> 1)
    val possibleGoldLocations: Set[Position] = allSquares

    val belief: Set[Unobservable] =
      possiblePitCombinations flatMap (pits =>
        possibleWumpusPositions flatMap (wumpus =>
          possibleGoldLocations map (gold =>
            UnobservableWithWumpus(wumpus, gold, pits._1, pits._2)
          )
        )
      )

    BeliefState(1 -> 1, Orientation.East, true, belief, Move.NoOp, 0, 0)

  private var currentBeliefState: BeliefState = initialBeliefState

  private object FullWidthPlanning:
    private final val TIME_HORIZON: Int = 5
    private final val DISCOUNT: Double = 0.8

    private case class Node(beliefState: BeliefState,
                            value: Double,
                            children: Map[Move | Percept4, Node],
                            depth: Int)

    infix def plan: Move =
      val tree = buildTree(Node(currentBeliefState, 0, Map.empty, 0), mutable.Map.empty)
      tree.children.maxBy((m, n) =>
        println(f"$m: ${n.value%.4f}")
        n.value
      )._1.asInstanceOf[Move]

    private def buildTree(root: Node, memo: mutable.Map[BeliefState, Double]): Node =
      val b: BeliefState = root.beliefState
      val d: Int = root.depth
      val obsNode: Boolean = b.lastUpdate.isInstanceOf[Percept4]
      if obsNode then // observation node (cannot be a leaf)
        memo.get(b) match {
          case Some(value) => Node(b, value, Map.empty, d)
          case None =>
            val successors: Map[Move | Percept4, Node] =
              b.possibleMoves.map(m => m ->
                buildTree(Node(b transition m, 0, Map.empty, d + 1), memo)
              ).toMap
            val utility: Double = b.eval + (DISCOUNT * successors.values.maxBy(_.value).value)
            memo += (b -> utility)
            Node(b, utility, successors, d)
        }
      else if (root.depth == TIME_HORIZON) || b.isTerminal then // leaf action node
        Node(b, b.lastUpdate.asInstanceOf[Move].reward + b.eval, Map.empty, d)
      else // interior action node
        val successors: Map[Move | Percept4, Node] =
          b.possibleObservations.map(o => o ->
            buildTree(Node(b observe o, 0, Map.empty, d), memo)
          ).toMap
        val utility: Double =
          b.eval + b.lastUpdate.asInstanceOf[Move].reward + (DISCOUNT * successors.values.map(_.value).sum / successors.size)
        Node(b, utility, successors, d)

  override def reset(): Unit =
    currentBeliefState = initialBeliefState
    actionQueue.clear()

  override def process(tp: TransferPercept): Int =
    val percept = Percept4(tp.getBreeze, tp.getStench, tp.getGlitter, tp.getScream)
    if actionQueue isEmpty then
      currentBeliefState = currentBeliefState observe percept
      val bestMove = FullWidthPlanning.plan
      currentBeliefState = currentBeliefState transition bestMove
      actionQueue enqueueAll bestMove.toActionSeq()
    // once I get a NO_OP from the forward search, all subsequent actions
    // are going to be a NO_OP because the forward search is deterministic
    // and a NO_OP doesn't change the belief state. So optimize for giving up.
    if actionQueue.front == Action.NO_OP then actionQueue enqueue Action.NO_OP
    actionQueue dequeue
