/** Seed all agent-side sampling independently of the simulator's movement RNG. */
object AgentRandom:
  def seed(value: Long): Unit = scala.util.Random.setSeed(value)
