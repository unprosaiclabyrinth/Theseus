import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

/** Enforce spacing before requests using monotonic elapsed time. */
final class RequestRateLimiter(intervalMillis: Long = 4000,
                               now: () => Long = () => System.nanoTime(),
                               sleep: Long => Unit = millis => Thread.sleep(millis)):
  require(intervalMillis >= 0)
  private var previous: Option[Long] = None
  def acquire(): Unit =
    previous.foreach { start =>
      val elapsed = (now() - start) / 1000000L
      val remaining = math.max(0L, intervalMillis - elapsed)
      if remaining > 0 then sleep(remaining)
    }
    previous = Some(now())

trait CompletionClient extends AutoCloseable:
  def query(messages: ujson.Arr): String

/** The key is sent only to Google's fixed HTTPS endpoint, never a shell or a log. */
final class GeminiClient(apiKey: String, model: String) extends CompletionClient:
  require(apiKey.nonEmpty, "GOOGLE_API_KEY is required for the LLM agent.")
  require(model.nonEmpty, "GOOGLE_MODEL must not be empty.")
  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()
  private val limiter = new RequestRateLimiter()

  override def query(messages: ujson.Arr): String =
    limiter.acquire()
    val body = ujson.Obj(
      "model" -> model,
      "messages" -> messages,
      "max_tokens" -> 2048,
      "response_format" -> ujson.Obj("type" -> "json_schema", "json_schema" -> ujson.Obj(
        "name" -> "agent_action", "strict" -> true,
        "schema" -> ujson.Obj("type" -> "object", "additionalProperties" -> false,
          "properties" -> ujson.Obj(
            "best_action" -> ujson.Obj("type" -> "string", "enum" -> ujson.Arr.from(LlmResponse.actions.keys.toList.sorted)),
            "belief_state_after_action" -> ujson.Obj("type" -> "string")
          ),
          "required" -> ujson.Arr("best_action", "belief_state_after_action")
        )
      ))
    )
    val request = HttpRequest.newBuilder(URI.create("https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"))
      .timeout(Duration.ofSeconds(30))
      .header("Authorization", "Bearer " + apiKey)
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body.toString)).build()
    val response = http.send(request, HttpResponse.BodyHandlers.ofString())
    if response.statusCode() != 200 then
      throw new IllegalStateException(s"Gemini request failed (HTTP ${response.statusCode()}). Check model access, quota, and credentials.")
    val content = ujson.read(response.body())("choices")(0)("message")("content").str
    content

  override def close(): Unit = http.close()

object LlmResponse:
  val actions: Map[String, Int] = Map(
    "forward" -> Action.GO_FORWARD, "right" -> Action.TURN_RIGHT,
    "left" -> Action.TURN_LEFT, "shoot" -> Action.SHOOT,
    "grab" -> Action.GRAB, "nothing" -> Action.NO_OP
  )
  def decode(json: String): (Int, String) =
    val response = ujson.read(json)
    val name = response("best_action").str
    val action = actions.getOrElse(name,
      throw new IllegalArgumentException("LLM returned an unsupported action."))
    (action, response("belief_state_after_action").str)
