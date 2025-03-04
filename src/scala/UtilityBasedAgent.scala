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

  private case class ObservableState(agentPosition: Position,
                                     agentOrientation: Orientation,
                                     hasArrow: Boolean,
                                     wumpusIsAlive: Boolean)

  private case class State(agentPosition: Position,
                           agentOrientation: Orientation,
                           hasArrow: Boolean,
                           wumpusIsAlive: Boolean,
                           gold: Position,
                           wumpus: Option[Position],
                           pit1: Position,
                           pit2: Position)

  // typedef for belief state
  private type BeliefState = Map[State, Probability]
  
  private enum Orientation:
    case North, South, East, West
    
  private enum Direction:
    case Forward, Right, Left, Back

  private enum Move:
    case GoForward, GoLeft, GoRight, GoBack, Shoot, ShootLeft, ShootRight, NoOp, Grab

  private final val TIME_HORIZON = 5
  private final val DISCOUNT = 1
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  private var currentState: ObservableState = ObservableState((1, 1), Orientation.East, true, true)
  private val exploredPositionCounts: mutable.Map[Position, Int] = mutable.Map.empty // pos -> count
  private val stenchSquares: mutable.Set[Position] = mutable.Set.empty // stench is observed here
  private val breezeSquares: mutable.Set[Position] = mutable.Set.empty // breeze is observed here
  private val wumpusFreeSquares: mutable.Set[Position] = mutable.Set.empty // no wumpus here 100%
  private val pitFreeSquares: mutable.Set[Position] = mutable.Set.empty // no pit here 100%

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
      allSquares -- (exploredPositionCounts.keySet diff Set(currentState.agentPosition))

    possiblePitCombinations.flatMap {
      case (pit1, pit2) =>
        possibleWumpusPositions.flatMap(wumpus => possibleGoldLocations.map(gold => (
            State(currentState.agentPosition, currentState.agentOrientation, currentState.hasArrow,
              currentState.wumpusIsAlive, gold, wumpus, pit1, pit2),
            Probability(1, possiblePitCombinations.size * possibleWumpusPositions.size * possibleGoldLocations.size)
        )))
    }.toMap

  private def toActionSeq(move: Move): Seq[Int] =
    val randomTurn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
    move match {
      case Move.GoForward => Seq(Action.GO_FORWARD)
      case Move.GoLeft => Seq(Action.TURN_LEFT, Action.GO_FORWARD)
      case Move.GoRight => Seq(Action.TURN_RIGHT, Action.GO_FORWARD)
      case Move.GoBack => Seq(randomTurn, randomTurn, Action.GO_FORWARD)
      case Move.Shoot => Seq(Action.SHOOT)
      case Move.ShootLeft => Seq(Action.TURN_LEFT, Action.SHOOT)
      case Move.ShootRight => Seq(Action.TURN_RIGHT, Action.SHOOT)
      case Move.NoOp => Seq(Action.NO_OP)
      case Move.Grab => Seq(Action.GRAB)
    }

  private def directionsToNeighbors(state: State): Map[Direction, Position] =
    val (x, y) = state.agentPosition
    {
      state.agentOrientation match {
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

  private def neighbors(state: State): Set[Position] = directionsToNeighbors(state).values.toSet

  private def neighbors(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def isTerminal(state: State): Boolean =
    Set(state.pit1, state.pit2).contains(state.agentPosition) || {
      state.wumpus match {
        case Some(wumpus) => wumpus == state.agentPosition
        case None => false // wumpus is dead
      }
    }

  private def possibleMoves(state: State): Set[Move] =
    if isTerminal(state) then Set.empty
    else Move.values.toSet filter {
      case Move.GoForward | Move.Shoot => directionsToNeighbors(state).contains(Direction.Forward)
      case Move.GoLeft | Move.ShootLeft => directionsToNeighbors(state).contains(Direction.Left)
      case Move.GoRight | Move.ShootRight => directionsToNeighbors(state).contains(Direction.Right)
      case Move.GoBack => directionsToNeighbors(state).contains(Direction.Back)
      case _ => true
    }

  private def possibleMoves(beliefState: BeliefState): Set[Move] = beliefState.flatMap(
    (s, p) => possibleMoves(s)
  ).toSet

  private def heuristic(stateBefore: State, move: Move): Int =
    val stateAfter = transition(stateBefore, move)
    // designed in the potential style φ(s')-φ(s)
    // maximize exploration by favoring it
    val explorationFactor = 5
    val explorationValue = -explorationFactor * (exploredPositionCounts.getOrElse(stateAfter.agentPosition, 0) -
      exploredPositionCounts.getOrElse(stateBefore.agentPosition, 0)) // potential gradient towards new squares
    // favor killing the wumpus
    val wumpusDeadValue = if stateBefore.wumpusIsAlive && !stateAfter.wumpusIsAlive then 100 else 0
    explorationValue + wumpusDeadValue

  private def reward(stateBefore: State, move: Move): Int =
    val stateAfter = transition(stateBefore, move)
    move match {
      case Move.GoForward =>
        if isTerminal(stateAfter) then -1000
        else -1
      case Move.GoLeft | Move.GoRight =>
        if isTerminal(stateAfter) then -1001
        else -2
      case Move.GoBack =>
        if isTerminal(stateAfter) then -1002
        else -3
      case Move.Shoot =>
        if stateBefore.hasArrow && !stateAfter.hasArrow then -10
        else -1
      case Move.ShootLeft | Move.ShootRight =>
        if stateBefore.hasArrow && !stateAfter.hasArrow then -11
        else -2
      case Move.Grab =>
        if stateBefore.agentPosition == stateBefore.gold then 1000
        else -1
      case Move.NoOp => 0
    }

  private def reward(beliefStateBefore: BeliefState, move: Move): Rational =
    // reward + heuristic
    beliefStateBefore.foldLeft(Rational(0, 1)) {
      case (acc, (s, p)) => acc + (p * Rational(reward(s, move) + heuristic(s, move), 1))
    }

  private def transition(state: State, move: Move): State =
    val (x, y) = state.agentPosition
    move match {
      case Move.GoForward =>
        state.copy(agentPosition = state.agentOrientation match {
          case Orientation.North => (x, y + 1)
          case Orientation.South => (x, y - 1)
          case Orientation.East => (x + 1, y)
          case Orientation.West => (x - 1, y)
        })
      case Move.GoRight =>
        state.copy(agentPosition = state.agentOrientation match {
          case Orientation.North => (x + 1, y)
          case Orientation.South => (x - 1, y)
          case Orientation.East => (x, y - 1)
          case Orientation.West => (x, y + 1)
        }, agentOrientation = state.agentOrientation match {
          case Orientation.North => Orientation.East
          case Orientation.South => Orientation.West
          case Orientation.East => Orientation.South
          case Orientation.West => Orientation.North
        })
      case Move.GoLeft =>
        state.copy(agentPosition = state.agentOrientation match {
          case Orientation.North => (x - 1, y)
          case Orientation.South => (x + 1, y)
          case Orientation.East => (x, y + 1)
          case Orientation.West => (x, y - 1)
        }, agentOrientation = state.agentOrientation match {
          case Orientation.North => Orientation.West
          case Orientation.South => Orientation.East
          case Orientation.East => Orientation.North
          case Orientation.West => Orientation.South
        })
      case Move.GoBack =>
        state.copy(agentPosition = state.agentOrientation match {
          case Orientation.North => (x, y - 1)
          case Orientation.South => (x, y + 1)
          case Orientation.East => (x - 1, y)
          case Orientation.West => (x + 1, y)
        }, agentOrientation = state.agentOrientation match {
          case Orientation.North => Orientation.South
          case Orientation.South => Orientation.North
          case Orientation.East => Orientation.West
          case Orientation.West => Orientation.East
        })
      case Move.Shoot if state.hasArrow =>
        state.wumpus match {
          case Some((xW, yW)) =>
            state.copy (wumpusIsAlive = state.agentOrientation match {
              case Orientation.North => xW == x && yW > y
              case Orientation.South => xW == x && yW < y
              case Orientation.East => xW > x && yW == y
              case Orientation.West => xW < x && yW == y
            }, hasArrow = false)
          case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
        }
      case Move.ShootRight if state.hasArrow =>
        state.wumpus match {
          case Some((xW, yW)) =>
            state.copy(wumpusIsAlive = state.agentOrientation match {
              case Orientation.North => xW > x && yW == y
              case Orientation.South => xW < x && yW == y
              case Orientation.East => xW == x && yW < y
              case Orientation.West => xW == x && yW > y
            }, hasArrow = false)
          case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
        }
      case Move.ShootLeft if state.hasArrow =>
        state.wumpus match {
          case Some((xW, yW)) =>
            state.copy(wumpusIsAlive = state.agentOrientation match {
              case Orientation.North => xW < x && yW == y
              case Orientation.South => xW > x && yW == y
              case Orientation.East => xW == x && yW > y
              case Orientation.West => xW == x && yW < y
            }, hasArrow = false)
          case None => assert(false, "hasArrow and not wumpusIsAlive is not possible.")
        }
      case _ => state
    }

  private def transition(beliefState: BeliefState, move: Move): BeliefState =
    beliefState.groupMapReduce((s, p) => transition(s, move))(_._2)(_ + _)

  private def execute(action: Int): Int =
    // Make a dummy state out of the current observable state
    val dummy: State = State(currentState.agentPosition, currentState.agentOrientation, currentState.hasArrow,
      currentState.wumpusIsAlive, (0, 0), None, (0, 0), (0, 0))
    // update the observable state based on the action
    currentState =
      if action == Action.GO_FORWARD then
        currentState.copy(agentPosition = transition(dummy, Move.GoForward).agentPosition)
      else if action == Action.TURN_RIGHT then
        currentState.copy(agentOrientation = transition(dummy, Move.GoLeft).agentOrientation)
      else if action == Action.TURN_LEFT then
        currentState.copy(agentOrientation = transition(dummy, Move.GoRight).agentOrientation)
      else if action == Action.SHOOT then
        currentState.copy(hasArrow = transition(dummy, Move.Shoot).hasArrow)
      else currentState
    action

  override def reset(): Unit =
    currentState = ObservableState((1, 1), Orientation.East, true, true)
    exploredPositionCounts.clear()
    stenchSquares.clear()
    breezeSquares.clear()
    wumpusFreeSquares.clear()
    pitFreeSquares.clear()

  override def process(tp: TransferPercept): Int =
    if actionQueue.isEmpty then
      // update knowledge
      currentState = currentState.copy(wumpusIsAlive = currentState.wumpusIsAlive && !tp.getScream)
      exploredPositionCounts.updateWith(currentState.agentPosition)(_.map(_ + 1) orElse Some(1))
      wumpusFreeSquares += currentState.agentPosition
      pitFreeSquares += currentState.agentPosition
      if tp.getStench then stenchSquares += currentState.agentPosition
      else wumpusFreeSquares ++= neighbors(currentState.agentPosition)
      if tp.getBreeze then breezeSquares += currentState.agentPosition
      else pitFreeSquares ++= neighbors(currentState.agentPosition)

      // A belief state is characterized by the positions of the gold, wumpus and both the pits
      val beliefState = currentBeliefState
      // TODO: IMPLEMENT MAXIMAX SEARCH (including the Node and everything)
    else execute(actionQueue.dequeue)
    ???
