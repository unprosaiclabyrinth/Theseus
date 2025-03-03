/*
 * Agent function for a model-based reflex agent in the wumpus environment
 * f_MRA: P* -> A
 *
 * Project 2 - Model-Based Reflex Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.collection.mutable

object ModelBasedReflexAgent extends AgentFunctionImpl:
  // North is on top
  enum Direction:
    case North, South, East, West

  // Tags for special unsafe squares
  private enum UnsafeTag:
    case Wumpus, Pit

  // The give up point is when the agent concludes that the gold is unreachable and gives up,
  // that is just keeps NO_OPing. If the agent reaches a particular orientation more than this
  // number of times, then most likely it is stuck in a loop and the gold is unreachable.
  // This is akin to a photographic draw in chess. Perhaps, I should set it to 3 in that spirit.
  private final val GIVE_UP_POINT = 3

  private var agentPosition: Position = (1, 1) // the agent's current position
  private var agentDirection: Direction = Direction.East // the agent's current direction

  private var hasArrow: Boolean = true // does the agent have the arrow?
  private var numFoundPits: Int = 0 // number of pits whose positions have been found; can be 0, 1, or 2
  private var givenUp: Boolean = false // has the agent given up?

  private val unsafeSquares: mutable.Set[(Position, UnsafeTag)] = mutable.Set.empty // pit/wumpus positions that are found
  private val exploredOrientationCounts: mutable.Map[(Position, Direction), Int] = mutable.Map.empty // pos, dir -> count
  private val stenchSquares: mutable.Set[Position] = mutable.Set.empty // stench is observed here
  private val breezeSquares: mutable.Set[Position] = mutable.Set.empty // breeze is observed here
  private val wumpusFreeSquares: mutable.Set[Position] = mutable.Set.empty // no wumpus here 100%
  private val pitFreeSquares: mutable.Set[Position] = mutable.Set.empty // no pit here 100%
  private var pitCombinations: Set[Set[Position]] = Set.empty // set of 2-tuples of possible positions of 2 pits

  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty // queues actions to execute

  /**
   * Respawn the agent. Reset the model. Forget all information and knowledge.
   */
  override def reset(): Unit =
    agentPosition = (1, 1)
    agentDirection = Direction.East
    hasArrow = true
    numFoundPits = 0
    givenUp = false
    unsafeSquares.clear()
    exploredOrientationCounts.clear()
    stenchSquares.clear()
    breezeSquares.clear()
    wumpusFreeSquares.clear()
    pitFreeSquares.clear()
    pitCombinations = Set.empty

  /**
   * Compute all "neighbors" of a given square along with an action that will get the agent there
   * from the given square. A "neighbor" of a square is a square adjacent to it but not diagonally
   * adjacent. If a square satisfying the adjacency condition is known to be unsafe with 100% certainty
   * then it is not considered a neighbor. Hence, a square can have 1, 2, 3, or 4 neighbors in general.
   * If the neighbor is:-
   * (1) to the North of the given square, then the corresponding action is GO_FORWARD,
   * (2) to the East of the given square, then the corresponding action is TURN_RIGHT,
   * (3) to the West of the given square, then the corresponding action is TURN_LEFT,
   * (4) to the South of the given square, then the corresponding action is NO_OP.
   * A square cannot have 0 neighbors since I came to this square from some other square and this
   * previous square is definitely safe since I'm still alive! (https://www.youtube.com/watch?v=qM0zINtulhM)
   * @param square a position (x,y) in the world.
   * @return a map (position -> action) that maps each neighbor to an action as described.
   */
  private def neighborsMap(square: Position): Map[Position, Int] =
    val (x, y) = square
    require(x >= 1 && x <= 4 && y >= 1 && y <= 4, "Position out of bounds.")

    val allNeighbors = agentDirection match {
      case Direction.North =>
        Map((x - 1, y) -> Action.TURN_LEFT, (x + 1, y) -> Action.TURN_RIGHT,
          (x, y - 1) -> Action.NO_OP, (x, y + 1) -> Action.GO_FORWARD)
      case Direction.South =>
        Map((x - 1, y) -> Action.TURN_RIGHT, (x + 1, y) -> Action.TURN_LEFT,
          (x, y - 1) -> Action.GO_FORWARD, (x, y + 1) -> Action.NO_OP)
      case Direction.East =>
        Map((x, y + 1) -> Action.TURN_LEFT, (x, y - 1) -> Action.TURN_RIGHT,
          (x - 1, y) -> Action.NO_OP, (x + 1, y) -> Action.GO_FORWARD)
      case Direction.West =>
        Map((x, y - 1) -> Action.TURN_LEFT, (x, y + 1) -> Action.TURN_RIGHT,
          (x - 1, y) -> Action.GO_FORWARD, (x + 1, y) -> Action.NO_OP)
    }

    allNeighbors.filter {
      case ((x, y), _) =>
        x >= 1 && x <= 4 && y >= 1 && y <= 4 && !unsafeSquares.map(_._1).contains(x, y)
    }

  /**
   * Compute all neighbors of a given square.
   * @param square a position (x, y) in the world.
   * @return the set of all neighbors of the given square.
   */
  private def neighborsSet(square: Position): Set[Position] = neighborsMap(square).keySet

  /**
   * Private helper to translate an action in the neighborsMap into an actual movement sequence.
   * @param action an action from the neighborsMap of some square.
   * @return nothing; but enqueues the correct sequence of actions to execute the desired movement.
   */
  private def actionToMovementSequence(action: Int): Unit =
    require(
      Set(Action.GO_FORWARD, Action.TURN_LEFT, Action.TURN_RIGHT, Action.NO_OP).contains(action),
      "Invalid action to translate to movement sequence."
    )
    if action == Action.NO_OP then
      val turn = randomElem(List(Action.TURN_LEFT, Action.TURN_RIGHT))
      actionQueue.enqueue(turn, turn, Action.GO_FORWARD)
    else if action != Action.GO_FORWARD then
      actionQueue.enqueue(action, Action.GO_FORWARD) // action is a turn
    else actionQueue.enqueue(action) // action = GO_FORWARD

  /**
   * Private helper to maximize the agent's exploration of the wumpus world by choosing the least
   * explored neighbor. If there are multiple least explored neighbors, then prefer edge squares.
   * @param nextSquares a pre-computed collection of potential next squares which the agent can go to.
   */
  private def maximizeExploration(nextSquares: Map[Position, Int]): Unit =
    require(nextSquares.nonEmpty, "No possible next squares.")
    // Remove turn-back option
    val forwardNextSquares = nextSquares.filter((_, action) => action != Action.NO_OP)
    if forwardNextSquares.nonEmpty then
      // Given a position, returns the number of times it has been explored
      def timesExplored(pos: Position): Int =
        exploredOrientationCounts.collect { case (((x, y), _), c) if (x, y) == pos => c }.sum
      // Find the minimum exploration count from given nextSquares
      val minCount = forwardNextSquares.keySet.map(timesExplored).min
      // Filter for argmins. This is definitely nonempty
      val leastExplored = forwardNextSquares.keySet.filter(timesExplored(_) == minCount)
      // Prefer edge squares to center squares for systematic and non-wasteful exploration
      def isCenterSquare(pos: Position): Boolean = Set((2, 2), (2, 3), (3, 2), (3, 3)).contains(pos)
      val edgeSquares = leastExplored.filterNot(isCenterSquare)
      if edgeSquares.nonEmpty then actionToMovementSequence(forwardNextSquares(randomElem(edgeSquares.toList)))
      else actionToMovementSequence(forwardNextSquares(randomElem(leastExplored.toList)))
    else actionToMovementSequence(Action.NO_OP)

  /**
   * Explore the world freely. Basically, don't turn back and maximize exploration.
   */
  private def explore(): Unit =
    // neighbors func will not return unsafe squares
    wumpusFreeSquares ++= neighborsSet(agentPosition)
    pitFreeSquares ++= neighborsSet(agentPosition)
    maximizeExploration(neighborsMap(agentPosition))

  /**
   * Identify potential wumpus positions using the current knowledge of `stenchSquares` and
   * `wumpusFreeSquares`. Since there is only one wumpus, the intersection of neighborsSets of
   * all `stenchSquares` minus all the `wumpusFreeSquares` should give potential wumpus positions.
   * @return a set of potential wumpus positions.
   */
  private def maybeWumpusSquares: Set[Position] =
    stenchSquares.foldLeft(neighborsSet(stenchSquares.head))(
      (acc: Set[Position], sq: Position) =>
        acc intersect neighborsSet(sq)
    ) diff wumpusFreeSquares

  /**
   * Private helper that is called when:-
   * (1) exactly 2 potential wumpus positions have been identified, one of which is in front;
   * (2) and the agent has shot in front and missed.
   * Hence, flag the "other" square as unsafe since that is 100% the wumpus position.
   */
  private def flagWumpusUnsafe(): Unit =
    maybeWumpusSquares.find(neighborsMap(agentPosition)(_) != Action.GO_FORWARD) match {
      case Some(wumpusPos) =>
        println(s"\"Yay! WUMPUS found @ $wumpusPos.\"")
        unsafeSquares += ((wumpusPos, UnsafeTag.Wumpus))
      case None => assert(false, "This is not possible") // should be dead code
    }

  /**
   * Private helper that checks for the existence of an unsafe square tagged Wumpus.
   * @return a boolean value indicating whether the wumpus has been found.
   */
  private def wumpusFound: Boolean = unsafeSquares.exists((_, tag) => tag == UnsafeTag.Wumpus)

  /**
   * `huntWumpus` is called when:-
   * (1) the agent observes a stench (i.e. the wumpus is alive),
   * (2) the wumpus is not found.
   * Compute the potential wumpus positions, which could be 1, 2, or 3 in number. If exactly 1 potential wumpus
   * position is computed, then the wumpus is pinpointed and is in an agent's neighboring square. Hence, shoot
   * with a 100% hit rate. Else if exactly 2 potential wumpus positions are computed, then don't take a chance
   * of wasting the arrow the first time that stench square is encountered and go back unless it is the starting
   * square. If the stench square is being encountered after the first time, then pinpoint the wumpus by shooting
   * at one of the 2 potential positions and noting the result: hit or miss. If the arrow hit (and a scream was
   * observed), then the wumpus is dead. Yay! If the arrow missed, then eliminate the position that was shot at
   * and conclude that the wumpus is in the other position. Hence, find the wumpus and flag that square as
   * unsafe. The downside here is that the arrow is used up in the case of a miss, and the wumpus can never be
   * killed despite being found. Else (if exactly 3 potential wumpus positions are computed), then don't take any
   * chances and simply turn back and go that way.
   * @param withBreeze a boolean value indicating whether there is breeze while hunting the wumpus
   */
  private def huntWumpus(withBreeze: Boolean): Unit =
    val wumpusPositions = maybeWumpusSquares // compute potential wumpus positions
    wumpusPositions.size match {
      case 1 => // case: only 1 potential wumpus position
        val action = neighborsMap(agentPosition)(wumpusPositions.head)
        // if the wumpus is not in front, then turn and shoot
        if action != Action.GO_FORWARD then
          actionQueue.enqueue(action, Action.SHOOT)
        else // if the wumpus is in front, simply SHOOT
          actionQueue.enqueue(Action.SHOOT)
      case 2 => // case: exactly 2 potential wumpus positions
        // just started? use random shooting strategy
        if exploredOrientationCounts.isEmpty || exploredOrientationCounts.filter {
          case ((sq, _), _) => sq == agentPosition
        }.values.sum >= 2 then
          if hasArrow then
             // choose a random potential wumpus position to shoot at
            val action = neighborsMap(agentPosition)(randomElem(wumpusPositions.toList))
            if action == Action.GO_FORWARD then
              actionQueue.enqueue(Action.SHOOT) // shoot in front
            else actionQueue.enqueue(action, Action.SHOOT) // turn and shoot
          // If I get here, then I do not have the arrow (since hasArrow is false),
          // but still observe a stench i.e. the wumpus is alive. Hence, I shot and missed.
          // The square in front can be eliminated, since the agent always shoots in front.
          // Also, there are exactly 2 potential wumpus positions since this is that case. Hence,
          // flag the other square as unsafe.
          else
            flagWumpusUnsafe()
            if !withBreeze then explore()
            else actionQueue.enqueue(Action.NO_OP) // execute NO_OP
        else actionToMovementSequence(Action.NO_OP) // go back
      case _ => // case: exactly 3 potential wumpus positions
        // if the wumpus cannot be pinpointed, don't take chances and
        // turn back since that is definitely a wumpus-free square
        actionToMovementSequence(Action.NO_OP) // go back
    }

  /**
   * Compute the probability of a pit being in a given square as the number of times the given square
   * appears in the pitCombinations divided by the total number of pitCombinations.
   * @param sq a position (x, y) in the world.
   * @return P(P_xy) where P_xy is the proposition that there is a pit in (x, y).
   */
  private def pitProbability(sq: Position): Probability =
    val (x, y) = sq
    require(x <= 4 && x >= 1 && y <= 4 && y >= 1, "Position out of bounds.")
    Probability(pitCombinations.count(_.contains(sq)), pitCombinations.size)

  /**
   * Normalize the given list of probabilities using the fact that they should add to one.
   * This function is called only in the case where the given list of probabilities contains
   * pit probabilities of neighbors of a breeze square. Hence, they should add to 1 (or 0 if
   * pit + wumpus case).
   * @param probabilities a list of pit probabilities for neighbors of a breeze square.
   * @return the list of normalized probabilities.
   */
  private def normalizeProbabilities(probabilities: List[Probability]): List[Probability] =
    val totalProbability = probabilities.foldLeft(Probability(0, 1))((acc, p) => acc + p)
    if totalProbability == Probability(0, 1) then probabilities
    else
      val normalizationConst = Probability(1, 1) / totalProbability
      probabilities.map(_ * normalizationConst)

  /**
   * Pretty print pit probabilities for all neighbors of a given breeze square.
   * @param sq a breeze square
   */
  private def printNeighborProbabilities(sq: Position): Unit =
    val neighbors = neighborsSet(sq).toList
    val probabilities = neighbors.map(pitProbability)
    val normalized = normalizeProbabilities(probabilities)
    neighbors.zip(normalized).foreach((neighbor, p) =>
      if p == Probability(1, 1) then println(s"$neighbor: $p (misleading)")
      else println(s"$neighbor: $p")
    )

  /**
   * Update possible `pitCombinations` based on `pitFreeSquares` and `breezeSquares` knowledge.
   */
  private def updatePitCombinations(): Unit =
    val allSquares = (1 to 4).flatMap(x => (1 to 4).flatMap(y => Set((x, y)))).toSet
    // any square that is not yet known to be pit-free can possibly have a pit (is a candidate)
    val candidateSquares: Set[Position] = allSquares diff pitFreeSquares
    // all 2-combinations of candidate squares are candidate pit combinations
    val allCombinations: Set[Set[Position]] = candidateSquares.toList
      .combinations(2).toSet.map(_.toSet)
    // compute set of all pit positions that have been found
    val allFoundPits: Set[Position] = unsafeSquares.filter(_._2 == UnsafeTag.Pit).map(_._1).toSet

    // filter allCombinations to find possible pitCombinations based on the condition that:-
    // breezeSquares is a subset of the union of the neighborSets of the 2 squares and the
    // 2-tuple contains all pit positions that are already found
    pitCombinations = allCombinations.filter(
      combination => (
        breezeSquares.isEmpty || breezeSquares.subsetOf(combination.flatMap(neighborsSet))
      ) && (
        allFoundPits.isEmpty || combination.exists(allFoundPits.contains)
      )
    )

    // compute individual pit positions from possible pitCombinations
    // by computing the intersection of all possible pitCombinations
    val pits: Set[Position] = pitCombinations.foldLeft(pitCombinations.head)(
      (acc: Set[Position], combination) => acc intersect combination
    )
    pits.size match {
      case 2 => // if the intersection of all 2-tuples in a set of 2-tuples is a 2-tuple
        // then there must be only one 2-tuple to begin with: both of them are pits
        pitCombinations.head.foreach(pitPos =>
          if !unsafeSquares.map(_._1).toSet.contains(pitPos) then
            println(s"\"Yay! PIT found @ $pitPos.\"")
            unsafeSquares += ((pitPos, UnsafeTag.Pit))
        )
        numFoundPits = 2
      case 1 => // if one position is common between all 2-tuples, that is definitely a pit
        // be sure that the common position is not an already-found pit
        val pitPos = pits.head
        if !unsafeSquares.map(_._1).toSet.contains(pitPos) then
          println(s"\"Yay! PIT found @ $pitPos.\"")
          unsafeSquares += ((pitPos, UnsafeTag.Pit))
          numFoundPits += 1
        else { /* nothing to be done */ }
      case _ => /* nothing to be done */
    }

  /**
   * Compute pit probabilities for all neighbors, and choose the next square out of the
   * neighbors by applying multiple filters.
   *
   * Note:- In general, if there is a unique max probability then it is not necessarily a pit.
   * But if there is a unique max probability and another nonzero probability, then the max
   * probability is assumed to be a pit.
   *
   * The filters:- neighbors -> keep least likely pits (since turning back is an option,
   * there is at least one 0-probability square) -> remove the option of turning back ->
   * maximize exploration by choosing unique pit positions and orientations.
   */
  private def goToZeroLikelyPit(): Unit =
    val neighbors = neighborsSet(agentPosition)
    val probabilities = neighbors.toList.map(pitProbability)
    printNeighborProbabilities(agentPosition)

    // Compute nonzero probabilities and check for possible pits based on the existence of
    // a unique max and another nonzero value. There is a chance of false positives here
    // which almost always leads to DEATH, but the benefit is higher than the cost
    // associated with that chance.
    probabilities.filter(_ != Probability(0, 1)).minOption match {
      case Some(min) =>
        val maxVal = probabilities.max
        if probabilities.count(_ == maxVal) == 1 && maxVal > min then
          neighbors.find(pitProbability(_) == maxVal) match {
            case Some(pitPos) =>
              println(s"\"Yay! PIT found @ $pitPos.\"")
              unsafeSquares += ((pitPos, UnsafeTag.Pit))
              numFoundPits += 1
            case None => /* continue */
          }
      case None => /* continue */
    }

    // filter for the squares that are 100% safe
    val nextSquares = neighborsMap(agentPosition).filter(
      (neighbor, _) => pitProbability(neighbor) == Probability(0, 1)
    )
    // nextSquares is definitely nonempty since there is always an option of
    // turning back with 0 pit probability so simply maximize exploration
    maximizeExploration(nextSquares)

  /**
   * Handle the breeze case.
   */
  private def handleBreeze(): Unit =
    if numFoundPits == 2 then explore()
    // If pit found @ (2,2) and I am stuck in that little infinite loop
    else if unsafeSquares.contains((2, 2), UnsafeTag.Pit) &&
      exploredOrientationCounts.keySet.map(_._1) == Set((1, 1), (1, 2), (2, 1)) then
      // Take a chance and go forward
      actionQueue.enqueue(Action.GO_FORWARD)
    else
      updatePitCombinations()
      goToZeroLikelyPit()

  /**
   * Private helper that updates the agent's model (position, direction, `hasArrow`) based on an action.
   * @param action an action executed by the agent.
   * @return the same action after updating the agent's model.
   */
  private def updateAgent(action: Int): Int =
    if Set(Action.GO_FORWARD, Action.TURN_LEFT, Action.TURN_RIGHT).contains(action) then
      exploredOrientationCounts += (
        (agentPosition, agentDirection) -> (
          exploredOrientationCounts.getOrElse((agentPosition, agentDirection), 0) + 1
        )
      )
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

  /**
   * Private helper that Checks the obvious general condition that tells us that the gold
   * is unreachable. It DOES NOT check for corner cases like the pits "guarding" the gold,
   * etc. The "ForSure" part is added since the function will never return a false positive
   * but could possibly return false negatives, in which case the agent is simply "not sure".
   * @return a boolean value indicating whether the gold is unreachable for sure
   */
  private def goldUnreachableForSure: Boolean =
    val allExploredPositions = exploredOrientationCounts.keySet.map(_._1)
    (wumpusFound && numFoundPits == 2 && allExploredPositions.size == 13) ||
      (!wumpusFound && numFoundPits == 2 && allExploredPositions.size == 14)

  /**
   * Compute the action to be executed by the MRA given the percepts. f_MRA: P* -> A.
   * @param tp the percepts.
   * @return the action to be executed by the model-based reflex agent.
   */
  override def process(tp: TransferPercept): Int =
    // the current position is ofc safe
    wumpusFreeSquares += agentPosition
    pitFreeSquares += agentPosition

    // Give up if GIVE_UP_POINT is reached or gold is unreachable for sure
    givenUp = {
      val b = goldUnreachableForSure ||
        exploredOrientationCounts.getOrElse((agentPosition, agentDirection), 0) > GIVE_UP_POINT
      if !givenUp && b then println("\"That's it. I give up.\"")
      givenUp || b
    }

    // condition-action rules
    if actionQueue.isEmpty && !givenUp then
      (tp.getBump, tp.getGlitter, tp.getBreeze, tp.getStench, tp.getScream) match {
        case (_, true, _, _, _) /* <_,glitter,_,_,_> */ =>
          actionQueue.enqueue(Action.GRAB) // win
        case (_, false, false, true, _) /* <_,none,none,stench,_> */ =>
          pitFreeSquares ++= neighborsSet(agentPosition)
          stenchSquares += agentPosition
          if wumpusFound then explore()
          else huntWumpus(false) // hunt without breeze
        case (_, false, true, true, _) /* <_,none,breeze,stench,_> */ =>
          // The only square without a 0-pit-probability neighbor is the starting square.
          // If I get a breeze at the first time step itself, then give up. I know this is
          // the first time step, since there are no explored orientations yet.
          if exploredOrientationCounts.isEmpty then
            println("\"That's it. I give up.\"")
            givenUp = true
            actionQueue.enqueue(Action.NO_OP)
          else
            breezeSquares += agentPosition
            stenchSquares += agentPosition
            if wumpusFound then handleBreeze()
            else huntWumpus(true) // hunt with breeze
        case (_, false, false, false, true) /* <_,none,none,none,scream> */ =>
          explore()
        case (_, false, true, false, _) /* <_,none,breeze,none,_> */ =>
          // the only square without a 0-pit-probability neighbor is the starting square
          // if I get a breeze at the first time step itself, then give up. I know this is
          // the first time step, since there are no explored orientations yet.
          if exploredOrientationCounts.isEmpty then
            println("\"That's it. I give up.\"")
            givenUp = true
            actionQueue.enqueue(Action.NO_OP)
          else
            wumpusFreeSquares ++= neighborsSet(agentPosition)
            breezeSquares += agentPosition
            handleBreeze()
        case (_, false, false, false, false) /* <_,none,none,none,none> */ =>
          // I will provably NEVER get a bump, so no need to handle it
          explore()
      }
    else if actionQueue.isEmpty && givenUp then actionQueue.enqueue(Action.NO_OP) // give up
    updateAgent(actionQueue.dequeue)
