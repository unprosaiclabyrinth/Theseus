/*
 * Template for the implementation of the agent function
 *
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.annotation.{tailrec, targetName}
import scala.util.Random.shuffle

trait AgentFunctionImpl:
  /**
   * Compute the GCD.
   * @param a a positive integer.
   * @param b a positive integer.
   * @return the greatest common divisor of a and b using the Euclidean algorithm.
   */
  @tailrec
  private def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

  /**
   * Compute the LCM.
   * @param a a positive integer.
   * @param b a positive integer.
   * @return the lowest common multiple of a and b using lcm = ab/gcd.
   */
  private def lcm(a: Int, b: Int): Int = (a * b) / gcd(a, b)

  /**
   * A custom implementation of rational probabilities to make computation simpler.
   * @param n numerator.
   * @param d denominator.
   */
  case class Probability(n: Int, d: Int) extends Ordered[Probability]:
    require(d > 0)

    // Reduce the probability fraction to the lowest terms and
    // make the new numerator and denominator available publicly
    private val hcf: Int = gcd(n, d)
    val numer: Int = n / hcf
    val denom: Int = d / hcf

    /**
     * Allow equality-checking using `==` and `!=` of Probabilities.
     * @param obj the other Probability.
     * @return a boolean whether this and that are equal..
     */
    override def equals(obj: Any): Boolean = obj match {
      case that: Probability =>
        this.numer == that.numer && this.denom == that.denom
      case _ => false
    }

    /**
     * Allow addition using `+` of Probabilities.
     * @param that the other addend.
     * @return a Probability with value (this + that).
     */
    @targetName("addProbabilities")
    def +(that: Probability): Probability = Probability(
      (this.numer * that.denom) + (that.numer * this.denom),
      this.denom * that.denom
    )

    /**
     * Allow multiplication using `*` of Probabilities.
     * @param that the multiplier.
     * @return a Probability with value (this * that).
     */
    @targetName("multiplyProbabilities")
    def *(that: Probability): Probability = Probability(
      this.numer * that.numer,
      this.denom * that.denom
    )

    /**
     * Allow division using `/` of Probabilities.
     * @param that the divisor.
     * @return a Probability with the value (this / that).
     */
    @targetName("divideProbabilites")
    def /(that: Probability): Probability =
      require(that != Probability(0, 1), "Division by 0.")
      Probability(this.numer * that.denom, this.denom * that.numer)

    /**
     * Allow comparison using `<`/`>`(`=`), of Probabilities and, in turn, sorting.
     * @param that the other Probability.
     * @return the boolean result of the comparison.
     */
    override def compare(that: Probability): Int =
      (this.numer * that.denom) - (that.numer * this.denom)

    /**
     * Allow hashing of Probabilities.
     * @return the hash code of the Probability object.
     */
    override def hashCode: Int = (numer, denom).##

    /**
     * Allow Probabilities to be printed on screen.
     * @return the string representation of the Probability.
     */
    override def toString: String = f"${100 * numer.toFloat/denom.toFloat}%.3f" + "%"

  // North is on top
  enum Direction:
    case North, South, East, West

  // Tags for special unsafe squares
  enum UnsafeTag:
    case Wumpus, Pit

  // A position in the wumpus world grid
  type Position = (Int, Int) // x, y

  /**
   * Private helper method that chooses an element randomly from a list by doing a
   * random shuffle and choosing the first element. The shuffle is a Fisher-Yates
   * shuffle (Knuth shuffle) as implemented in the scala.util.Random library.
   * @param l list of elements of some type
   * @tparam T type of element
   * @return a random element from the list effectively simulating a random choice
   */
  def randomElem[T](l: List[T]): T = shuffle(l).head

  /**
   * Provides API to probabilistically choose an action given a probability
   * distribution over the actions.
   * @param weightedActions a map of actions mapped to their probabilities; hence
   *                        a probability distribution over actions
   * @return an action as per a probabilistic choice
   */
  def probabilisticChoice(weightedActions: Map[Int, Probability]): Int =
    // Check that the probability distribution is normalized i.e. sums to 1
    // If yes, return probabilistic choice
    require(
      weightedActions.values.toList.foldLeft(Probability(0, 1))((acc, p) => acc + p) == Probability(1, 1),
      "Probabilities are not normalized."
    )
    val theLcm: Int = weightedActions.values.toList.foldLeft(1)((acc, p) => lcm(acc, p.denom))
    randomElem(
      weightedActions.flatMap((action, p) => List.fill(theLcm * p.numer / p.denom)(action)).toList
    )

  // Method that resets the agent in case of multiple trials
  def reset(): Unit
  
  // Method that returns an action given the percepts. Implemented by agents.
  def process(tp: TransferPercept): Int