/*
 * Agent function for a simple reflex agent in the wumpus environment
 * f_SRA: P -> A
 *
 * Project 1 - Simple Reflex Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 * 
 */

object SimpleReflexAgent extends AgentFunctionImpl:
  /**
   * Nothing to be done to reset the SRA.
   */
  override def reset(): Unit = {/* nothing to be done */}
  
  /**
   * This function returns the action to be executed by the SRA given the percepts.
   * f_SRA: P -> A
   * @param tp percepts
   * @return action to be executed by the simple reflex agent
   */
  override def process(tp: TransferPercept): Int =
    // match percepts with cases like a switch case
    (tp.getBump, tp.getGlitter, tp.getBreeze, tp.getStench, tp.getScream) match {
      // Use only left turns since a right turn is 3 left turns. Reduce wastage
      // of moves by preventing agent from going left-right-left-right-left...
      // Bias towards left since I start facing east. Hence, maximize exploration
      // and score
      case (_, true, _, _, _) /* <_,glitter,_,_,_> */ => Action.GRAB
      case (true, false, _, _, _) /* <bump,none,_,_,_> */ => Action.TURN_LEFT
      case (false, false, _, true, _) /* <none,none,_,stench,_> */ =>
        // If I shoot deterministically, then I get a stench on the next time step,
        // and I shoot, and I get a stench, and I shoot, and so on...the optimal
        // strategy would be to turn either way and shoot but two consecutive actions
        // cannot be determined. Weigh SHOOT more than TURN and GO since shooting
        // must be the primary action on a stench. Weigh turning and going forwards
        // equally signifying a random choice (and enabling further exploration).
        probabilisticChoice(Map(
          Action.SHOOT -> Probability(13, 20),     // 13/20 = 65%
          Action.GO_FORWARD -> Probability(7, 40), // 7/40 = 17.5%
          Action.TURN_LEFT -> Probability(7, 40)   // 7/40 = 17.5%
        ))
      case (false, false, true, false, _) /* <none,none,breeze,none,_> */ =>
        // Randomly choose between going forward and turning as with wumpus stench.
        probabilisticChoice(Map(
          Action.GO_FORWARD -> Probability(1, 2), // 1/2 = 50%
          Action.TURN_LEFT -> Probability(1, 2)   // 1/2 = 50%
        ))
      case (false, false, false, false, _) /* <none,none,_,none,_> */ =>
        // Weigh going forward more than in case of a breeze since turning too much
        // and not going forwards hinders exploration and decreases score. In case of
        // a breeze, turning increases chances of evading the pit (conjecture).
        probabilisticChoice(Map(
          Action.GO_FORWARD -> Probability(13, 20), // 13/20 = 65%
          Action.TURN_LEFT -> Probability(7, 20),   // 7/20 = 35%
        ))
    }