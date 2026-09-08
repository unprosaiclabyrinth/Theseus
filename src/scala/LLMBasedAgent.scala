/** LLM agent with explicit, bounded HTTP requests and trial-local history. */
object LLMBasedAgent extends AgentFunctionImpl:
  private var client: Option[CompletionClient] = None
  private var probability = 1.0
  private var messages = ujson.Arr()

  def configure(forwardProbability: Double): Unit =
    val key = sys.env.getOrElse("GOOGLE_API_KEY", "")
    require(key.nonEmpty, "Set GOOGLE_API_KEY before running the LLM agent.")
    configureClient(new GeminiClient(key, sys.env.getOrElse("GOOGLE_MODEL", "gemini-2.5-flash")), forwardProbability)

  /** Dependency injection for offline tests and alternative completion services. */
  def configureClient(completionClient: CompletionClient, forwardProbability: Double): Unit =
    require(forwardProbability.isFinite && forwardProbability >= 0 && forwardProbability <= 1)
    stop()
    probability = forwardProbability
    client = Some(completionClient)
    reset()

  override def reset(): Unit =
    messages = ujson.Arr(ujson.Obj("role" -> "system", "content" ->
      s"""You are Theseus in a 4x4 Wumpus world, starting at (1,1), facing east.
         |There are two distinct pits, one Wumpus, and one gold. These may overlap;
         |the starting square has no hazard. Breeze/stench means a pit/live Wumpus
         |is orthogonally adjacent. Glitter means gold is here. A scream means the
         |Wumpus died. A bump means movement was blocked by a wall.
         |Forward moves ahead with probability $probability; otherwise it slips
         |left or right equally without changing orientation. All other actions
         |are deterministic. Entering a hazard ends the trial with -1000.
         |Moving or turning costs 1; shooting your single arrow costs 10, and the
         |arrow travels to the wall ahead. Grabbing gold ends the trial with +1000;
         |an unsuccessful grab costs 1. Doing nothing costs 0.
         |Choose exactly one action: forward, left, right, shoot, grab, nothing.
         |Return JSON with best_action and belief_state_after_action.
         |Maximize the expected total score. Observations and executed actions
         |follow as messages; infer your position from that history.
         |""".stripMargin))

  def stop(): Unit =
    val old = client
    client = None
    old.foreach(_.close())

  override def process(tp: TransferPercept): Int =
    val active = client.getOrElse(throw new IllegalStateException("LLM agent is not configured."))
    messages.value += ujson.Obj("role" -> "user", "content" ->
      s"bump=${tp.getBump}, glitter=${tp.getGlitter}, breeze=${tp.getBreeze}, stench=${tp.getStench}, scream=${tp.getScream}")
    val (action, belief) = LlmResponse.decode(active.query(messages))
    println(belief)
    val name = LlmResponse.actions.find(_._2 == action).get._1
    messages.value += ujson.Obj("role" -> "assistant", "content" -> s"Executed action: $name")
    action
