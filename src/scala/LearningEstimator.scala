/** Maximum likelihood over the three supported environments, in log space. */
object LearningEstimator:
  def estimate(experience: List[Boolean]): BigDecimal =
    require(experience.nonEmpty, "Cannot learn without observations.")
    require(experience.count(!_) <= 2, "Learning stops after two slips.")
    val candidates = List(BigDecimal(1), BigDecimal("0.8"), BigDecimal(1) / 3)
    def logLikelihood(p: BigDecimal): Double =
      var leftStart = false
      experience.foldLeft(0.0) { (logP, bump) =>
        val bumpProbability = if leftStart then p else (1 + p) / 2
        val probability = if bump then bumpProbability else 1 - bumpProbability
        if !bump then leftStart = true
        logP + math.log(probability.toDouble)
      }
    candidates.maxBy(logLikelihood)
