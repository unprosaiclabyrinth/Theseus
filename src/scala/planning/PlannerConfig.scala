/** Macro-action search settings. Defaults preserve the previously corrected planner. */
case class PlannerConfig(
    simulations: Int = 1000,
    horizon: Int = 15,
    discount: Double = 0.2,
    exploration: Double = math.sqrt(2),
    treePolicy: TreePolicyKind = TreePolicyKind.HeuristicUCT,
    rollout: RolloutKind = RolloutKind.Informed,
    shaping: ShapingKind = ShapingKind.Potential
):
  require(simulations > 0, "Planner simulations must be positive.")
  require(horizon > 0 && horizon <= 100, "Planner horizon must be in [1,100].")
  require(discount.isFinite && discount >= 0 && discount <= 1, "Discount must be in [0,1].")
  require(exploration.isFinite && exploration >= 0, "Exploration coefficient must be finite and nonnegative.")

enum TreePolicyKind:
  case CanonicalUCT, HeuristicUCT

enum RolloutKind:
  case Uniform, Informed

enum ShapingKind:
  case None, Legacy, Potential

object PlannerOptions:
  val names = Set("--simulations", "--horizon", "--discount", "--exploration", "--tree-policy", "--rollout", "--shaping")
  def accepts(name: String): Boolean = names.contains(name)
  def parse(options: java.util.Map[String, String]): PlannerConfig =
    def value(key: String, default: String) = Option(options.get(key)).getOrElse(default)
    PlannerConfig(
      value("--simulations", "1000").toInt,
      value("--horizon", "15").toInt,
      value("--discount", "0.2").toDouble,
      value("--exploration", math.sqrt(2).toString).toDouble,
      value("--tree-policy", "heuristic") match {
        case "canonical" => TreePolicyKind.CanonicalUCT
        case "heuristic" => TreePolicyKind.HeuristicUCT
        case _ => throw new IllegalArgumentException("Tree policy must be canonical or heuristic.")
      },
      value("--rollout", "informed") match {
        case "uniform" => RolloutKind.Uniform
        case "informed" => RolloutKind.Informed
        case _ => throw new IllegalArgumentException("Rollout must be uniform or informed.")
      },
      value("--shaping", "potential") match {
        case "none" => ShapingKind.None
        case "legacy" => ShapingKind.Legacy
        case "potential" => ShapingKind.Potential
        case _ => throw new IllegalArgumentException("Shaping must be none, legacy, or potential.")
      }
    )
