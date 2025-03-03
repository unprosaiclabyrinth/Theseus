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

object UtilityBasedAgent extends AgentFunctionImpl:
  // A node in the search tree
  private class Node

  // expands on agent's actions
  private case class DecisionNode() extends Node

  // expands on possible percepts
  private case class ChanceNode() extends Node

  private case class State(agentPosition: Position,
                           agentDirection: Direction,
                           hasArrow: Boolean,
                           wumpusIsAlive: Boolean,
                           gold: Position,
                           wumpus: Position,
                           pit1: Position,
                           pit2: Position)

  // typedef for belief state
  private type BeliefState = Map[State, Probability]

  private final val TIME_HORIZON = 5
  private final val DISCOUNT = 1
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  // A belief state is characterized by the positions of the gold, wumpus and both the pits
  private var beliefState: BeliefState = initialBeliefState

  private def initialBeliefState: BeliefState =
    val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet
    // C(15, 2) = 105 of these
    val pitCombinations: Set[(Position, Position)] = (allSquares - ((1, 1))).toList.combinations(2).toSet.map {
      case Seq(p1, p2) => (p1, p2)
      case _ => assert(false, "Wtf?! 2-combinations should have size 2.")
    }
    pitCombinations.flatMap {
      case (pit1, pit2) =>
        (allSquares - ((1, 1))).flatMap(wumpus => allSquares.map(gold =>
          (State((1, 1), Direction.East, true, true, gold, wumpus, pit1, pit2), Probability(1, 16 * 15 * 105))
        ))
    }.toMap

  private def neighborsMap(sq: Position): Map[Position, Direction] =
    val (x, y) = sq
    Map((x, y + 1) -> Direction.North, (x, y - 1) -> Direction.South,
      (x + 1, y) -> Direction.East, (x - 1, y) -> Direction.West).filter {
      case ((x, y), _) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def neighborsSet(sq: Position): Set[Position] = neighborsMap(sq).keySet

  private def turnTowardNeighbor(agentPosition: Position, agentDirection: Direction, neighbor: Position): Unit =
    require(neighborsSet(agentPosition).contains(neighbor), "Tried to turn toward a non-neighbor.")
    val d: Direction = neighborsMap(agentPosition)(neighbor)
    val randomTurn = randomElem(List(Action.TURN_RIGHT, Action.TURN_LEFT))
    (agentDirection, d) match {
      case (Direction.North, Direction.North) => /* nothing to be done */
      case (Direction.North, Direction.South) => actionQueue.enqueue(randomTurn, randomTurn)
      case (Direction.North, Direction.East) => actionQueue.enqueue(Action.TURN_RIGHT)
      case (Direction.North, Direction.West) => actionQueue.enqueue(Action.TURN_LEFT)
      case (Direction.South, Direction.North) => actionQueue.enqueue(randomTurn, randomTurn)
      case (Direction.South, Direction.South) => /* nothing to be done */
      case (Direction.South, Direction.East) => actionQueue.enqueue(Action.TURN_LEFT)
      case (Direction.South, Direction.West) => actionQueue.enqueue(Action.TURN_RIGHT)
      case (Direction.East, Direction.North) => actionQueue.enqueue(Action.TURN_LEFT)
      case (Direction.East, Direction.South) => actionQueue.enqueue(Action.TURN_RIGHT)
      case (Direction.East, Direction.East) => /* nothing to be done */
      case (Direction.East, Direction.West) => actionQueue.enqueue(randomTurn, randomTurn)
      case (Direction.West, Direction.North) => actionQueue.enqueue(Action.TURN_RIGHT)
      case (Direction.West, Direction.South) => actionQueue.enqueue(Action.TURN_LEFT)
      case (Direction.West, Direction.East) => actionQueue.enqueue(randomTurn, randomTurn)
      case (Direction.West, Direction.West) => /* nothing to be done */
    }

//  private def updateAgent(action: Int): Int =
//    if action == Action.GO_FORWARD then
//      val (x, y) = agentPosition
//      agentPosition = agentDirection match {
//        case Direction.North => (x, y + 1)
//        case Direction.South => (x, y - 1)
//        case Direction.East => (x + 1, y)
//        case Direction.West => (x - 1, y)
//      }
//    else if action == Action.TURN_RIGHT then
//      agentDirection = agentDirection match {
//        case Direction.North => Direction.East
//        case Direction.South => Direction.West
//        case Direction.East => Direction.South
//        case Direction.West => Direction.North
//      }
//    else if action == Action.TURN_LEFT then
//      agentDirection = agentDirection match {
//        case Direction.North => Direction.West
//        case Direction.South => Direction.East
//        case Direction.East => Direction.North
//        case Direction.West => Direction.South
//      }
//    else if action == Action.SHOOT then
//      hasArrow = false
//    else if action == Action.GRAB then
//      {/* do nothing */}
//    else if action == Action.NO_OP then
//      {/* do nothing */}
//    else assert(false, "No such action.")
//    action

  override def reset(): Unit = beliefState = initialBeliefState

  override def process(tp: TransferPercept): Int = Action.NO_OP

// TODO: typedef Fraction = Probability, define reward function, utility function for state and belief state
// TODO: Internalize heuristic for unexplored squares in U, choose search algorithm: IDA* or RBFS
// TODO: Figure out where to store knowledge stenchSquares, breezeSquares, wumpusFree, pitFree, etc.: node or state or global??