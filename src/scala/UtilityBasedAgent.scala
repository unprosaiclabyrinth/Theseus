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
    def successors: Set[Node] = possibleMoves(beliefState).map(
      m => Node(transition(beliefState, m), depth + 1, Some(m), Some(this))
    )

    def isCutoff: Boolean = isTerminal(beliefState) || depth >= TIME_HORIZON

  private case class ObservableState(var agentPosition: Position,
                                     var agentOrientation: Orientation,
                                     var hasArrow: Boolean,
                                     var wumpusIsAlive: Boolean):
    def execute(action: Int): Int =
      // Make a dummy state out of the current observable state
      val dummy: State = State(currentState.agentPosition, currentState.agentOrientation, currentState.hasArrow,
        currentState.wumpusIsAlive, (0, 0), None, (0, 0), (0, 0))
      // update the observable state based on the action
      if action == Action.GO_FORWARD then agentPosition = dummy.transition(Move.GoForward).agentPosition
      else if action == Action.TURN_RIGHT then agentOrientation = dummy.transition(Move.GoLeft).agentOrientation
      else if action == Action.TURN_LEFT then agentOrientation = dummy.transition(Move.GoRight).agentOrientation
      else if action == Action.SHOOT then hasArrow = dummy.transition(Move.Shoot).hasArrow
      action

  private case class State(agentPosition: Position,
                           agentOrientation: Orientation,
                           hasArrow: Boolean,
                           wumpusIsAlive: Boolean,
                           gold: Position,
                           wumpus: Option[Position],
                           pit1: Position,
                           pit2: Position):
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
      if isTerminal then Set.empty
      else Move.values.toSet filter {
        case Move.GoForward | Move.Shoot => directions2Neighbors.contains(Direction.Forward)
        case Move.GoLeft | Move.ShootLeft => directions2Neighbors.contains(Direction.Left)
        case Move.GoRight | Move.ShootRight => directions2Neighbors.contains(Direction.Right)
        case Move.GoBack => directions2Neighbors.contains(Direction.Back)
        case _ => true
      }
    
    def isTerminal: Boolean = Set(pit1, pit2).contains(agentPosition) || {
      wumpus match {
        case Some(wumpus) => wumpus == agentPosition
        case None => false // wumpus is dead
      }
    }

    def heuristic(move: Move): Int =
      val stateAfter = transition(move)
      // designed in the potential style φ(s')-φ(s)
      // favor decreasing manhattan distance to the gold (h1)
      val goldDistBefore = manhattanDistance(agentPosition, gold)
      val goldDistAfter = manhattanDistance(stateAfter.agentPosition, stateAfter.gold)
      val weightedGoldDistDelta = -5 * (goldDistAfter - goldDistBefore)
      // favor killing the wumpus (h2)
      val wumpusKilledScore = if wumpusIsAlive && !stateAfter.wumpusIsAlive then 15 else 0
      weightedGoldDistDelta + wumpusKilledScore
      
    def reward(move: Move): Int =
      val stateAfter = transition(move)
      move match {
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
          if hasArrow && !stateAfter.hasArrow then -10
          else -1
        case Move.ShootLeft | Move.ShootRight =>
          if hasArrow && !stateAfter.hasArrow then -11
          else -2
        case Move.Grab =>
          if agentPosition == gold then 1000
          else -1
        case Move.NoOp => 0
      }
      
    def eval: Int =
      val goldDist = manhattanDistance(agentPosition, gold)
      val wumpusDist = wumpus match {
        case Some(wumpus) => manhattanDistance(agentPosition, wumpus)
        case None => 0
      }
      val pitDist = (manhattanDistance(agentPosition, pit1) + manhattanDistance(agentPosition, pit2)) / 2
      val wumpusDeadScore = if wumpusIsAlive then 0 else 15
      // weighted sum of params
      (5 * goldDist) - (2 * wumpusDist) - pitDist + wumpusDeadScore // utility heuristic

    def transition(move: Move): State =
      if isTerminal then return this
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
          }, agentOrientation = agentOrientation match {
            case Orientation.North => Orientation.East
            case Orientation.South => Orientation.West
            case Orientation.East => Orientation.South
            case Orientation.West => Orientation.North
          })
        case Move.GoLeft =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x - 1, y)
            case Orientation.South => (x + 1, y)
            case Orientation.East => (x, y + 1)
            case Orientation.West => (x, y - 1)
          }, agentOrientation = agentOrientation match {
            case Orientation.North => Orientation.West
            case Orientation.South => Orientation.East
            case Orientation.East => Orientation.North
            case Orientation.West => Orientation.South
          })
        case Move.GoBack =>
          copy(agentPosition = agentOrientation match {
            case Orientation.North => (x, y - 1)
            case Orientation.South => (x, y + 1)
            case Orientation.East => (x - 1, y)
            case Orientation.West => (x + 1, y)
          }, agentOrientation = agentOrientation match {
            case Orientation.North => Orientation.South
            case Orientation.South => Orientation.North
            case Orientation.East => Orientation.West
            case Orientation.West => Orientation.East
          })
        case Move.Shoot if hasArrow =>
          wumpus match {
            case Some((xW, yW)) =>
              copy (wumpusIsAlive = agentOrientation match {
                case Orientation.North => xW == x && yW > y
                case Orientation.South => xW == x && yW < y
                case Orientation.East => xW > x && yW == y
                case Orientation.West => xW < x && yW == y
              }, hasArrow = false)
            case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
          }
        case Move.ShootRight if hasArrow =>
          wumpus match {
            case Some((xW, yW)) =>
              copy(wumpusIsAlive = agentOrientation match {
                case Orientation.North => xW > x && yW == y
                case Orientation.South => xW < x && yW == y
                case Orientation.East => xW == x && yW < y
                case Orientation.West => xW == x && yW > y
              }, hasArrow = false)
            case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
          }
        case Move.ShootLeft if hasArrow =>
          wumpus match {
            case Some((xW, yW)) =>
              copy(wumpusIsAlive = agentOrientation match {
                case Orientation.North => xW < x && yW == y
                case Orientation.South => xW > x && yW == y
                case Orientation.East => xW == x && yW > y
                case Orientation.West => xW == x && yW < y
              }, hasArrow = false)
            case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
          }
        case _ => copy()
      }

  // typedef for belief state
  private type BeliefState = Map[State, Probability]
  
  private enum Orientation:
    case North, South, East, West
  
  private enum Move(val actionSeq: Seq[Int]):
    case GoForward extends Move(Seq(Action.GO_FORWARD))
    case GoLeft extends Move(Seq(Action.TURN_LEFT, Action.GO_FORWARD))
    case GoRight extends Move(Seq(Action.TURN_RIGHT, Action.GO_FORWARD))
    case GoBack extends Move({
      val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
      Seq(randomTurn, randomTurn, Action.GO_FORWARD)
    })
    case Shoot extends Move(Seq(Action.SHOOT))
    case ShootLeft extends Move(Seq(Action.TURN_LEFT, Action.SHOOT))
    case ShootRight extends Move(Seq(Action.TURN_RIGHT, Action.SHOOT))
    case NoOp extends Move(Seq(Action.NO_OP))
    case Grab extends Move(Seq(Action.GRAB))

  private final val TIME_HORIZON = 5
  private final val DISCOUNT = 1
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  private var currentState: ObservableState = ObservableState((1, 1), Orientation.East, true, true)
  private val visitedSquares: mutable.Set[Position] = mutable.Set.empty
  private val stenchSquares: mutable.Set[Position] = mutable.Set.empty // stench is observed here
  private val breezeSquares: mutable.Set[Position] = mutable.Set.empty // breeze is observed here
  private val wumpusFreeSquares: mutable.Set[Position] = mutable.Set.empty // no wumpus here 100%
  private val pitFreeSquares: mutable.Set[Position] = mutable.Set.empty // no pit here 100%

  override def reset(): Unit =
    currentState = ObservableState((1, 1), Orientation.East, true, true)
    visitedSquares.clear()
    stenchSquares.clear()
    breezeSquares.clear()
    wumpusFreeSquares.clear()
    pitFreeSquares.clear()

  private def currentBeliefState: BeliefState =
    val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet

    // 1. Pit possibilities
    val possiblePitCombinations: Set[(Position, Position)] = (allSquares -- pitFreeSquares).toList.combinations(2).toSet.map {
      case Seq(p1, p2) => (p1, p2)
      case _ => assert(false, "Wtf?! 2-combinations should have size 2.")
    } filter {
      case (p1, p2) => breezeSquares subsetOf (neighbors(p1) union neighbors(p2))
    }

    // 2. Wumpus possibilities
    val possibleWumpusPositions: Set[Option[Position]] =
      if currentState.wumpusIsAlive then (allSquares -- wumpusFreeSquares).filter(
        stenchSquares subsetOf neighbors(_)
      ).map(Some(_))
      else Set(None)

    // 3. Gold possibilities
    // we're trusting the algorithm to always GRAB on glitter here
    val possibleGoldLocations: Set[Position] =
      allSquares -- (visitedSquares diff Set(currentState.agentPosition))

    // A belief state is characterized by the positions of the gold, wumpus and both the pits
    possiblePitCombinations.flatMap {
      case (pit1, pit2) =>
        possibleWumpusPositions.flatMap(wumpus => possibleGoldLocations.map(gold => (
            State(currentState.agentPosition, currentState.agentOrientation, currentState.hasArrow,
              currentState.wumpusIsAlive, gold, wumpus, pit1, pit2),
            Probability(1, possiblePitCombinations.size * possibleWumpusPositions.size * possibleGoldLocations.size)
        )))
    }.toMap

  private def neighbors(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def isTerminal(beliefState: BeliefState): Boolean = beliefState.keySet.forall(_.isTerminal)

  private def possibleMoves(beliefState: BeliefState): Set[Move] = beliefState.flatMap(_._1.possibleMoves).toSet

  private def manhattanDistance(sq1: Position, sq2: Position): Int =
    val (x1, y1) = sq1
    val (x2, y2) = sq2
    abs(y2 - y1) + abs(x2 - x1)
  
  private def reward(beliefStateBefore: BeliefState, move: Move): Rational =
    // reward + heuristic
    beliefStateBefore.foldLeft(Rational(0, 1)) {
      case (acc, (s, p)) => acc + (p * Rational(s.reward(move) + s.heuristic(move), 1))
    }

  /**
   * Evaluation function that estimates the utility of a belief state.
   * @param beliefState some belief state
   * @return a rational utility heuristic
   */
  private def eval(beliefState: BeliefState): Rational =
    beliefState.foldLeft(Rational(0, 1)) {
      case (acc, (s, p)) => acc + (p * Rational(s.eval, 1))
    }
  
  private def transition(beliefState: BeliefState, move: Move): BeliefState =
    beliefState.groupMapReduce(_._1.transition(move))(_._2)(_ + _)
  
  /**
   * THE MAXIMAX SEARCH
   * @param node initial node
   * @return node with the maximum utility and the corresponding utility value
   */
  private def maximax(node: Node): (Node, Rational) =
    node.successors.map(child =>
      if child.isCutoff then (child, eval(child.beliefState)) else maximax(child)
    ).maxBy(_._2)

  override def process(tp: TransferPercept): Int =
    if actionQueue.isEmpty then
      // update knowledge
      currentState = currentState.copy(wumpusIsAlive = currentState.wumpusIsAlive && !tp.getScream)
      visitedSquares += currentState.agentPosition
      wumpusFreeSquares += currentState.agentPosition
      pitFreeSquares += currentState.agentPosition
      if tp.getStench then stenchSquares += currentState.agentPosition
      else wumpusFreeSquares ++= neighbors(currentState.agentPosition)
      if tp.getBreeze then breezeSquares += currentState.agentPosition
      else pitFreeSquares ++= neighbors(currentState.agentPosition)

      val (maxUtilityChild, maxUtilityValue) = maximax(Node(currentBeliefState, 0, None, None))
      println(s"Max utility: $maxUtilityValue")

      maxUtilityChild.move match {
        case Some(m) => actionQueue.enqueueAll(m.actionSeq)
        case None => assert(false, "Every descendant node must have a non-None move.")
      }
    currentState.execute(actionQueue.dequeue)
