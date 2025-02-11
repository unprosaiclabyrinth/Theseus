/*
 * Template for the implementation of the agent function
 *
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.annotation.tailrec
import scala.util.Random.shuffle

trait AgentFunctionImpl:
  /**
   * Private helper method that chooses an element randomly from a list by doing a
   * random shuffle and choosing the first element. The shuffle is a Fisher-Yates
   * shuffle (Knuth shuffle) as implemented in the scala.util.Random library.
   * @param l list of elements of some type
   * @tparam T type of element
   * @return a random element from the list effectively simulating a random choice
   */
  private def randomElem[T](l: List[T]): T = shuffle(l).head

  /**
   * Private helper method that converts a given probability distribution over
   * actions to a list such that a random choice from the list simulates a
   * probabilistic choice according to the given probability distribution
   * @param m a map of actions mapped to their probabilities; hence a probability
   *          distribution over actions
   * @return a list of actions populated with the appropriate number of each action
   *         in the given map (determined by its probability)
   */
  @tailrec
  private def probabilityDistributionToList(m: Map[Int, BigDecimal]): List[Int] =
    if m.exists((k, v) => v.toInt != v) then // if fractional probabilities exist
      // keep multiplying probabilities by 10 until all probabilities are integers
      probabilityDistributionToList(m.map((k, v) => (k, v * 10)))
    else
      // populate the list with N instances of action A where N is the (integer)
      // probability of A in the given map
      m.flatMap((k, v) => List.fill(v.toInt)(k)).toList

  /**
   * Provides API to probabilistically choose an action given a probability
   * distribution over the actions.
   * @param weightedActions a map of actions mapped to their probabilities; hence
   *                        a probability distribution over actions
   * @return an action as per a probabilistic choice
   */
  def probabilisticChoice(weightedActions: Map[Int, BigDecimal]): Int =
    // Check that the probability distribution is normalized i.e. sums to 1
    // If yes, return probabilistic choice
    weightedActions.foldLeft(BigDecimal(0)) {
      case (acc, (_, v)) => acc + v
    }.toInt match {
      case 1 => randomElem(probabilityDistributionToList(weightedActions))
      case _ => throw IllegalStateException("Probability distribution should sum to 1.")
    }

  // Method that returns an action given the percepts. Inherited by agents.
  def process(tp: TransferPercept): Int