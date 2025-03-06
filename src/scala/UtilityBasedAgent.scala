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
import scala.math.abs

object UtilityBasedAgent extends AgentFunctionImpl:
  // A node in the search tree
  private case class Node(beliefState: BeliefState,
                          depth: Int,
                          move: Option[Move],
                          parent: Option[Node]):
    def isCutoff: Boolean = beliefState.isTerminal || depth >= TIME_HORIZON

    def successor(move: Move): Node = Node(
      beliefState.transition(move), depth + 1, Some(move), Some(this)
    )

  private enum Orientation:
    case North, South, East, West

    def right: Orientation = this match {
      case North => East
      case South => West
      case East => South
      case West => North
    }

    def left: Orientation = this match {
      case North => West
      case South => East
      case East => North
      case West => South
    }

    def opposite: Orientation = this match {
      case North => South
      case South => North
      case East => West
      case West => East
    }
  
  private enum Move(val toActionSeq: () => Seq[Int]):
    case GoForward extends Move(() => Seq(Action.GO_FORWARD))
    case GoLeft extends Move(() => Seq(Action.TURN_LEFT, Action.GO_FORWARD))
    case GoRight extends Move(() => Seq(Action.TURN_RIGHT, Action.GO_FORWARD))
    case GoBack extends Move(() => {
      val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
      Seq(randomTurn, randomTurn, Action.GO_FORWARD)
    })
    case Shoot extends Move(() => Seq(Action.SHOOT))
    case ShootLeft extends Move(() => Seq(Action.TURN_LEFT, Action.SHOOT))
    case ShootRight extends Move(() => Seq(Action.TURN_RIGHT, Action.SHOOT))
    case NoOp extends Move(() => Seq(Action.NO_OP))
    case Grab extends Move(() => Seq(Action.GRAB))

  private case class Unobservable(gold: Position, wumpus: Option[Position], pit1: Position, pit2: Position)

  private case class BeliefState(agentPosition: Position,
                                  agentOrientation: Orientation,
                                  hasArrow: Boolean,
                                  belief: Map[Unobservable, Probability]):
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

    def isTerminal: Boolean = belief.keySet.forall(u =>
      u.pit1 == agentPosition || u.pit2 == agentPosition || (u.wumpus.isDefined && u.wumpus.get == agentPosition)
    )

    def goldDist: Rational = belief.foldLeft(Rational(0, 1)) {
      case (acc, (u, p)) => acc + (p * Rational(manhattanDistance(agentPosition, u.gold), 1))
    }

    def wumpusDeadScore: Rational = belief.foldLeft(Rational(0, 1)) {
      case (acc, (u, p)) => acc + (p * Rational(if u.wumpus.isDefined then 0 else 15, 1))
    }

    def eval: Rational = belief.foldLeft(Rational(0, 1)) {
      case (acc, (u, p)) =>
        val goldDist = manhattanDistance(agentPosition, u.gold)
        val wumpusDist = u.wumpus match {
          case Some(wumpus) => manhattanDistance(agentPosition, wumpus)
          case None => 0
        }
        val pitDist = Rational(manhattanDistance(agentPosition, u.pit1) + manhattanDistance(agentPosition, u.pit2), 2)
        val wumpusDeadScore = if u.wumpus.isDefined then 0 else 15
        // weighted sum of params
        acc + (p * (Rational((5 * goldDist) - (2 * wumpusDist) + wumpusDeadScore, 1) - pitDist)) // utility heuristic
    }

    def transition(move: Move): BeliefState =
      require(!this.isTerminal, "Attempt to transition from a terminal state.")
      val (x, y) = agentPosition
      move match {
        case Move.GoForward =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x, y + 1)
            case Orientation.South => (x, y - 1)
            case Orientation.East => (x + 1, y)
            case Orientation.West => (x - 1, y)
          })
        case Move.GoRight =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x + 1, y)
            case Orientation.South => (x - 1, y)
            case Orientation.East => (x, y - 1)
            case Orientation.West => (x, y + 1)
          }, agentOrientation = agentOrientation.right)
        case Move.GoLeft =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x - 1, y)
            case Orientation.South => (x + 1, y)
            case Orientation.East => (x, y + 1)
            case Orientation.West => (x, y - 1)
          }, agentOrientation = agentOrientation.left)
        case Move.GoBack =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x, y - 1)
            case Orientation.South => (x, y + 1)
            case Orientation.East => (x - 1, y)
            case Orientation.West => (x + 1, y)
          }, agentOrientation = agentOrientation.opposite)
        // if I have the arrow, the wumpus is certainly not dead
        case Move.Shoot if hasArrow =>
          copy(belief = belief.groupMapReduce((u, _) =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus.get
            u.copy(wumpus = agentOrientation match {
              case Orientation.North if xW == xA && yW > yA => None
              case Orientation.South if xW == xA && yW < yA => None
              case Orientation.East if xW > xA && yW == yA => None
              case Orientation.West if xW < xA && yW == yA => None
              case _ => u.wumpus
            }))(_._2)(_ + _), hasArrow = false
          )
        case Move.ShootRight if hasArrow =>
          copy(hasArrow = false, belief = belief.groupMapReduce((u, _) =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus.get
            u.copy(wumpus = agentOrientation match {
              case Orientation.North if xW > xA && yW == yA => None
              case Orientation.South if xW < xA && yW == yA => None
              case Orientation.East if xW == xA && yW < yA => None
              case Orientation.West if xW == xA && yW > yA => None
              case _ => u.wumpus
            }))(_._2)(_ + _), agentOrientation = agentOrientation.right)
        case Move.ShootLeft if hasArrow =>
          copy(hasArrow = false, belief = belief.groupMapReduce((u, _) =>
            val (xA, yA) = agentPosition
            val (xW, yW) = u.wumpus.get
            u.copy(wumpus = agentOrientation match {
              case Orientation.North if xW < xA && yW == yA => None
              case Orientation.South if xW > xA && yW == yA => None
              case Orientation.East if xW == xA && yW > yA => None
              case Orientation.West if xW == xA && yW < yA => None
              case _ => u.wumpus
            }))(_._2)(_ + _), agentOrientation = agentOrientation.left)
        case _ => copy()
      }

    // the percept is observed at agentPosition (in the current state)
    def bayesianUpdateGiven(percept: TransferPercept): BeliefState =
      val neighbors = neighborsOf(agentPosition)

      if percept.getStench then
        // if I'm getting a stench, then wumpus is alive => u.wumpus is defined
        copy(belief = {
          val pStench = belief.filter((u, _) => u.wumpus.isDefined && neighbors.contains(u.wumpus.get)).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if u.wumpus.isDefined && neighbors.contains(u.wumpus.get) then 1 else 0, 1)
            (u, (likelihood * p) / pStench)
          ).filterNot(_._2 == Probability(0, 1))
        })
      else
        copy(belief = {
          val pNoStench = belief.filterNot((u, _) => u.wumpus.isDefined && neighbors.contains(u.wumpus.get)).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if u.wumpus.isDefined && neighbors.contains(u.wumpus.get) then 0 else 1, 1)
            (u, (likelihood * p) / pNoStench)
          ).filterNot(_._2 == Probability(0, 1))
        })

      if percept.getBreeze then
        // if I'm getting a stench, then wumpus is alive => u.wumpus is defined
        copy(belief = {
          val pBreeze = belief.filter((u, _) => neighbors.contains(u.pit1) || neighbors.contains(u.pit2)).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if neighbors.contains(u.pit1) || neighbors.contains(u.pit2) then 1 else 0, 1)
            (u, (likelihood * p) / pBreeze)
          ).filterNot(_._2 == Probability(0, 1))
        })
      else
        copy(belief = {
          val pNoBreeze = belief.filterNot((u, _) => neighbors.contains(u.pit1) || neighbors.contains(u.pit2)).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if neighbors.contains(u.pit1) || neighbors.contains(u.pit2) then 0 else 1, 1)
            (u, (likelihood * p) / pNoBreeze)
          ).filterNot(_._2 == Probability(0, 1))
        })

      if percept.getGlitter then
        // if I'm getting a stench, then wumpus is alive => u.wumpus is defined
        copy(belief = {
          val pGlitter = belief.filter(_._1.gold == agentPosition).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if agentPosition == u.gold then 1 else 0, 1)
            (u, (likelihood * p) / pGlitter)
          ).filterNot(_._2 == Probability(0, 1))
        })
      else
        copy(belief = {
          val pNoGlitter = belief.filterNot((u, _) => agentPosition == u.gold).values
            .foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if agentPosition == u.gold then 0 else 1, 1)
            (u, (likelihood * p) / pNoGlitter)
          ).filterNot(_._2 == Probability(0, 1))
        })

      if percept.getScream then
        // all beliefs wherein wumpus is alive are wrong
        copy(belief = {
          val pScream = belief.filterNot(_._1.wumpus.isDefined).values.foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if u.wumpus.isDefined then 0 else 1, 1)
            (u, (likelihood * p) / pScream)
          ).filterNot(_._2 == Probability(0, 1))
        })
      else
        // all beliefs wherein wumpus is dead are wrong
        copy(belief = {
          val pNoScream = belief.filter(_._1.wumpus.isDefined).values.foldLeft(Probability(0, 1))((acc, p) => acc + p)
          belief.map((u, p) =>
            val likelihood = Probability(if u.wumpus.isDefined then 1 else 0, 1)
            (u, (likelihood * p) / pNoScream)
          ).filterNot(_._2 == Probability(0, 1))
        })

      // bumps don't matter

    def execute(action: Int): BeliefState =
      if action == Action.GO_FORWARD then transition(Move.GoForward)
      else if action == Action.TURN_LEFT then copy(agentOrientation = agentOrientation.left)
      else if action == Action.TURN_RIGHT then copy(agentOrientation = agentOrientation.right)
      else if action == Action.SHOOT then transition(Move.Shoot)
      else if action == Action.NO_OP then transition(Move.NoOp)
      else transition(Move.Grab)

  private final val TIME_HORIZON = 5
  private final val DISCOUNT = Rational(1, 1)
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  private var currentState: BeliefState = BeliefState((1, 1), Orientation.East, true, initialBeliefPrior)

  override def reset(): Unit = currentState = BeliefState((1, 1), Orientation.East, true, initialBeliefPrior)

  private def initialBeliefPrior: Map[Unobservable, Probability] =
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
        possibleWumpusPositions.flatMap(wumpus => possibleGoldLocations.map(gold => (
          Unobservable(gold, Some(wumpus), pit1, pit2),
          Probability(1, 105 * 15 * 16)
        )))
    }.toMap

  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def manhattanDistance(sq1: Position, sq2: Position): Int =
    val (x1, y1) = sq1
    val (x2, y2) = sq2
    abs(y2 - y1) + abs(x2 - x1)

  private def heuristic(stateBefore: BeliefState, move: Move): Rational =
    val stateAfter: BeliefState = stateBefore.transition(move)
    // designed in the potential style φ(s')-φ(s)
    // favor decreasing manhattan distance to the gold (h1)
    val weightedGoldDistDelta = Rational(-5, 1) * (stateAfter.goldDist - stateBefore.goldDist)
    // favor killing the wumpus (h2)
    val wumpusKilledScore = stateAfter.wumpusDeadScore - stateBefore.wumpusDeadScore
    weightedGoldDistDelta + wumpusKilledScore

  private def reward(stateBefore: BeliefState, move: Move): Rational =
    val stateAfter = stateBefore.transition(move)
    Rational(move match {
      case Move.GoForward =>
        if stateAfter.isTerminal then -1000
        else -1
      case Move.GoLeft | Move.GoRight =>
        if stateAfter.isTerminal then -1001
        else -2
      case Move.GoBack =>
        if stateAfter.isTerminal then -1002
        else -3
      case Move.Shoot =>
        if stateBefore.hasArrow && !stateAfter.hasArrow then -10
        else -1
      case Move.ShootLeft | Move.ShootRight =>
        if stateBefore.hasArrow && !stateAfter.hasArrow then -11
        else -2
      case Move.Grab =>
        if stateAfter.goldDist == Rational(0, 1) then 1000
        else -1
      case Move.NoOp => 0
    }, 1)

  /**
   * THE MAXIMAX SEARCH
   * @param node initial node
   * @return node with the maximum utility and the corresponding utility value
   */
  private def maximax(node: Node): (Node, Rational) = node.beliefState.possibleMoves.map(m =>
    val child = node.successor(m)
    (child, reward(node.beliefState, m) + (DISCOUNT *
      (if node.isCutoff then node.beliefState.eval else maximax(child)._2)))
  ).maxBy(_._2)

  override def process(tp: TransferPercept): Int =
    if actionQueue.isEmpty then
      // evolution via Bayesian update
      currentState = currentState.bayesianUpdateGiven(tp)

      val (maxUtilityChild, maxUtilityValue) = maximax(Node(currentState, 0, None, None))
      println(s"Max utility: $maxUtilityValue")

      maxUtilityChild.move match {
        case Some(m) => actionQueue.enqueueAll(m.toActionSeq())
        case None => assert(false, "Every descendant node must have a non-None move.")
      }

    val action = actionQueue.dequeue
    currentState = currentState.execute(action)
    action
