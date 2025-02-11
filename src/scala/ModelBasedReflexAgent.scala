import scala.collection.mutable

/*
 * Agent function for a model-based reflex agent in the wumpus environment
 * f_MRA: P* -> A
 *
 * Project 2 - Model-Based Reflex Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
object ModelBasedReflexAgent extends AgentFunctionImpl:
  private enum Direction:
    case North, South, East, West

  private enum UturnStage:
    case Zero, Half, Full

  private enum WumpusStatus:
    case Unknown, Located, Dead

  private type Position = (Int, Int) // x, y

  private var agentPosition: Position = (1, 1)
  private var agentDirection: Direction = Direction.East

  private var hasArrow: Boolean = true
  private var wumpusStatus: WumpusStatus = WumpusStatus.Unknown

  private var turningBackStage: UturnStage = UturnStage.Zero
  private var turnAndShootInProgress: Boolean = false
  private var turnAndGoInProgress: Boolean = false

  private var safeSquares: Set[Position] = Set.empty
  private var unsafeSquares: Set[Position] = Set.empty
  private var exploredSquares: Set[Position] = Set.empty
  private var breezeSquares: Set[Position] = Set.empty
  private var stenchSquares: Set[Position] = Set.empty

  private def allSquares: Set[Position] =
    (1 to 4).flatMap(x => (1 to 4).flatMap(y => Set((x, y)))).toSet

  private def neighborsMap(square: Position): Map[Position, Int] =
    val (x, y) = square
    val allNeighbors = agentDirection match {
      case Direction.North => Map((x - 1, y) -> Action.TURN_LEFT, (x + 1, y) -> Action.TURN_RIGHT,
        (x, y - 1) -> Action.NO_OP, (x, y + 1) -> Action.GO_FORWARD)
      case Direction.South => Map((x - 1, y) -> Action.TURN_RIGHT, (x + 1, y) -> Action.TURN_LEFT,
        (x, y - 1) -> Action.GO_FORWARD, (x, y + 1) -> Action.NO_OP)
      case Direction.East => Map((x, y + 1) -> Action.TURN_LEFT, (x, y - 1) -> Action.TURN_RIGHT,
        (x - 1, y) -> Action.NO_OP, (x + 1, y) -> Action.GO_FORWARD)
      case Direction.West => Map((x, y - 1) -> Action.TURN_LEFT, (x, y + 1) -> Action.TURN_RIGHT,
        (x - 1, y) -> Action.GO_FORWARD, (x + 1, y) -> Action.NO_OP)
    }
    allNeighbors.filter {
      case ((x, y), _) => x >= 1 && x <= 4 && y >= 1 && y <= 4 && !unsafeSquares.contains((x, y))
    }

  private def neighborsSet(square: Position): Set[Position] = neighborsMap(square).keySet

  private def updateAgent(action: Int): Int =
    exploredSquares ++= Set(agentPosition) // remember the square as explored
    val (x, y) = agentPosition
    if action == Action.GO_FORWARD then
      agentDirection match {
        case Direction.North =>
          agentPosition = if y < 4 then (x, y + 1) else (x, y)
        case Direction.South =>
          agentPosition = if y > 1 then (x, y - 1) else (x, y)
        case Direction.East =>
          agentPosition = if x < 4 then (x + 1, y) else (x, y)
        case Direction.West =>
          agentPosition = if x > 1 then (x - 1, y) else (x, y)
      }
    else if action == Action.TURN_LEFT then
      agentDirection match {
        case Direction.North => agentDirection = Direction.West
        case Direction.South => agentDirection = Direction.East
        case Direction.East => agentDirection = Direction.North
        case Direction.West => agentDirection = Direction.South
      }
    else if action == Action.TURN_RIGHT then
      agentDirection match {
        case Direction.North => agentDirection = Direction.East
        case Direction.South => agentDirection = Direction.West
        case Direction.East => agentDirection = Direction.South
        case Direction.West => agentDirection = Direction.North
      }
    else if action == Action.SHOOT then hasArrow = false
    action

  private def turnBack: Int = turningBackStage match {
    case UturnStage.Zero =>
      turningBackStage = UturnStage.Half
      updateAgent(Action.TURN_RIGHT)
    case UturnStage.Half =>
      turningBackStage = UturnStage.Full
      updateAgent(Action.TURN_RIGHT)
    case UturnStage.Full =>
      turningBackStage = UturnStage.Zero
      updateAgent(Action.GO_FORWARD)
  }

  private def explore: Int =
    // Mark all neighbors of current position as safe since neither stench nor breeze
    safeSquares ++= neighborsSet(agentPosition)
    if turnAndGoInProgress then
      turnAndGoInProgress = false
      updateAgent(Action.GO_FORWARD)
    else
      // Possible next squares
      val nextSquares = neighborsMap(agentPosition).filter {
        case ((_, _), d) => d != Action.NO_OP
      }
      // Maximize exploration by preferably exploring previously unexplored squares
      val preferableNextSquares = nextSquares.filter {
        case ((x, y), _) => !exploredSquares.contains((x, y))
      }
      val action =
        if preferableNextSquares.isEmpty then randomElem(nextSquares.values.toList)
        else randomElem(preferableNextSquares.values.toList)
      turnAndGoInProgress = action != Action.GO_FORWARD
      updateAgent(action)

  private def huntWumpus: Int =
    // Try to locate wumpus (I need at most 2 stench squares, of which one is this)
    val wumpusPositions: Set[Position] =
      stenchSquares.foldLeft(neighborsSet(stenchSquares.head))(
        (acc: Set[Position], sq: Position) =>
          acc intersect neighborsSet(sq)
      ) diff safeSquares

    if wumpusPositions.size == 1 then // wumpus is pin-pointed
      // Since I'm getting a stench here, it's possible to turn and shoot
      // I won't have to turn back since I just came from there.
      // Initiate turn-and-shoot (TAS) protocol!
      if turnAndShootInProgress then
        updateAgent(Action.SHOOT)
      else
        val (wx, wy) = wumpusPositions.head
        val action = neighborsMap(agentPosition).filter {
          case ((x, y), _) => wx == x && wy == y
        }.head._2
        turnAndShootInProgress = action != Action.GO_FORWARD
        updateAgent(action)
    else if wumpusPositions.size == 2 then // use arrow to pin-point wumpus
      // This is not an SRA, so I shoot deterministically to get information
      // If I'm facing a wall, then shooting straight is a waste, I must turn and shoot
      if turnAndShootInProgress then
        turnAndShootInProgress = false
        updateAgent(Action.SHOOT)
      else if facingWall then
        turnAndShootInProgress = true
        updateAgent(probabilisticChoice(Map(
          Action.TURN_LEFT -> 0.5,
          Action.TURN_RIGHT -> 0.5
        )))
      else if hasArrow then updateAgent(Action.SHOOT)
      // If I shot, then I used up my arrow, but if I still get here
      // then I still got a stench, i.e. I have used up my arrow and the
      // wumpus is not dead. I only ever shoot straight, hence the wumpus is not there
      else
        // filter out the square in front to pin-point the wumpus
        unsafeSquares ++= wumpusPositions.filter((x, y) =>
          neighborsMap(agentPosition)(x, y) != Action.GO_FORWARD
        )
        wumpusStatus = WumpusStatus.Located
        updateAgent(Action.GO_FORWARD)
    // if wumpus cannot be pin-pointed, turn back since that is definitely a safe square
    // provably with percepts <none,none,none,none,none>
    // initiate turn-back protocol!
    else turnBack

  private def facingWall: Boolean =
    !neighborsMap(agentPosition).values.toSet.contains(Action.GO_FORWARD)

  override def process(tp: TransferPercept): Int =
    // condition-action rules
    (tp.getBump, tp.getGlitter, tp.getBreeze, tp.getStench, tp.getScream) match {
      case (_, true, _, _, _) /* <_,glitter,_,_,_> */ =>
        Action.GRAB // win and quit
      case (_, false, false, true, _) /* <_,none,none,stench,_> */ =>
        stenchSquares ++= Set(agentPosition)
        if wumpusStatus == WumpusStatus.Unknown then huntWumpus
        // else if located status (dead+stench is not possible
        // since the neighbors function already avoids neighbors), just do what
        // I'd do in case of a none percept, unless there's a breeze...
        else
          safeSquares ++= neighborsSet(agentPosition)
          explore
      case (_, false, true, true, _) /* <_,none,breeze,stench,_> */ =>
        stenchSquares ++= Set(agentPosition)
        if wumpusStatus == WumpusStatus.Unknown then huntWumpus
        else ??? // TODO: focus on the breeze now...
      case (_, false, false, false, true) /* <_,none,none,none,scream> */ =>
        wumpusStatus = WumpusStatus.Dead
        safeSquares ++= neighborsSet(agentPosition)
        explore
      case (_, false, true, false, true) /* <_,none,breeze,none,scream> */ =>
        wumpusStatus = WumpusStatus.Dead
        ??? // TODO: focus on the breeze now...
      case (_, false, true, false, false) /* <_,none,breeze,none,none> */ =>
        // Randomly choose between going forward and turning as with wumpus stench.
        // TODO: handle breeze...
        updateAgent(probabilisticChoice(Map(
          Action.GO_FORWARD -> 0.5,
          Action.TURN_LEFT -> 0.5
        )))
      case (_, false, false, false, false) /* <_,none,none,none,none> */ =>
        // I will provably NEVER get a bump, so no need to handle it
        explore
    }

