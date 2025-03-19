/*
 * Template for the implementation of the agent function
 *
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.annotation.targetName
import scala.util.Random.shuffle

trait AgentFunctionImpl:
  /**
   * Compute the LCM.
   * @param a a positive integer.
   * @param b a positive integer.
   * @return the lowest common multiple of a and b using lcm = ab/gcd.
   */
  private def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / a.gcd(b)

  /**
   * A custom implementation of rational numbers to avoid floating-point imprecision.
   * @param n numerator.
   * @param d denominator.
   */
  class Rational(n: BigInt, d: BigInt) extends Ordered[Rational]:
    require(d != 0, "Denominator = 0.")

    // Reduce the fraction to the lowest terms and
    // make the new numerator and denominator available publicly
    private val hcf: BigInt = n gcd d
    val numer: BigInt = if d < 0 then -n / hcf else n / hcf
    val denom: BigInt = d / hcf

    /**
     * Allow equality-checking using `==` and `!=` of Rationals.
     * @param obj the other Rational.
     * @return a boolean whether this and that are equal.
     */
    override def equals(obj: Any): Boolean = obj match {
      case that: Rational =>
        this.numer == that.numer && this.denom == that.denom
      case _ => false
    }

    /**
     * Allow addition using `+` of Rationals.
     * @param that the other addend.
     * @return a Rational with value (this + that).
     */
    @targetName("addRationals")
    def +(that: Rational): Rational = Rational(
      (this.numer * that.denom) + (that.numer * this.denom),
      this.denom * that.denom
    )

    /**
     * Allow subtraction using `-` of Rationals.
     * @param that the other subtrahend.
     * @return a Rational with value (this - that).
     */
    @targetName("subtractRationals")
    def -(that: Rational): Rational = Rational(
      (this.numer * that.denom) - (that.numer * this.denom),
      this.denom * that.denom
    )

    /**
     * Allow multiplication using `*` of Rationals.
     * @param that the multiplier.
     * @return a Rational with value (this * that).
     */
    @targetName("multiplyRationals")
    def *(that: Rational): Rational = Rational(
      this.numer * that.numer,
      this.denom * that.denom
    )

    /**
     * Allow division using `/` of Rationals.
     * @param that the divisor.
     * @return a Rational with the value (this / that).
     */
    @targetName("divideRationals")
    def /(that: Rational): Rational =
      require(that != Rational(0, 1), "Division by 0.")
      Rational(this.numer * that.denom, this.denom * that.numer)

    /**
     * Allow comparison using `<`/`>`(`=`), of Rationals and, in turn, sorting.
     * @param that the other Rational.
     * @return the boolean result of the comparison.
     */
    override def compare(that: Rational): Int =
      (this.numer * that.denom) compareTo (that.numer * this.denom)

    /**
     * Allow hashing of Rationals.
     * @return the hash code of the Probability object.
     */
    override def hashCode: Int = (numer, denom).##

    /**
     * Allow Rationals to be printed on screen.
     * @return the string representation of the Rational.
     */
    override def toString: String = f"${numer.toFloat/denom.toFloat}%.3f"

    /**
     * Convert this to a BigDecimal.
     * @return the decimal value corresponding to this Rational as a BigDecimal.
     */
    def toBigDecimal: BigDecimal = BigDecimal(numer)/BigDecimal(denom)

  // Class of rational probabilities
  case class Probability(n: BigInt, d: BigInt) extends Rational(n, d):
    /**
     * Allow addition using `+` of Probabilities to return a Probability.
     * @param that the other addend.
     * @return a Probability with value (this + that).
     */
    @targetName("addProbabilities")
    def +(that: Probability): Probability = Probability(
      (this.numer * that.denom) + (that.numer * this.denom),
      this.denom * that.denom
    )

    /**
     * Allow multiplication using `*` of Probabilities to return a Probability.
     * @param that the multiplier.
     * @return a Probability with value (this * that).
     */
    @targetName("multiplyProbabilities")
    def *(that: Probability): Probability = Probability(
      this.numer * that.numer,
      this.denom * that.denom
    )

    /**
     * Allow division using `/` of Probabilities to return a Probability.
     * @param that the divisor.
     * @return a Probability with the value (this / that).
     */
    @targetName("divideProbabilities")
    def /(that: Probability): Probability =
      require(that != Probability(0, 1), "Division by 0.")
      Probability(this.numer * that.denom, this.denom * that.numer)

    /**
     * Allow Probabilities to be printed on screen.
     * @return the string representation of the Rational.
     */
    override def toString: String = f"${100 * numer.toFloat / denom.toFloat}%.3f" + "%"

  // A position in the wumpus world grid
  type Position = (Int, Int) // x, y

  /**
   * A custom iterative infinite (within Int bounds) counter implementation.
   * @param from the starting integer to count up from.
   */
  case class Counter(from: Int):
    private var count: Int = from
    
    def next: Int =
      count += 1
      count
      
    def get: Int = count
    
    def reset(): Unit = count = from

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
  def probabilisticChoice[T](weightedActions: Map[T, Probability]): T =
    // Check that the probability distribution is normalized i.e. sums to 1
    // If yes, return probabilistic choice
    require(
      weightedActions.values.toList.foldLeft(Probability(0, 1))((acc, p) => acc + p) == Probability(1, 1),
      "Probabilities are not normalized."
    )
    val theLcm = weightedActions.values.toList.foldLeft(BigInt(1))((acc, p) => lcm(acc, p.denom))
    randomElem(
      weightedActions.flatMap((action, p) => List.fill((theLcm * p.numer / p.denom).intValue)(action)).toList
    )

  // Method that resets the agent in case of multiple trials
  def reset(): Unit
  
  // Method that returns an action given the percepts. Implemented by agents.
  def process(tp: TransferPercept): Int