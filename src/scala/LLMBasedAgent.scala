/*
 * Agent function for an LLM-based agent in the wumpus environment.
 * f_LBA: Ω* -> A
 * At each time step, the agent queries an LLM for an action based on the current state.
 * LLM: Google Gemini 2.0 Flash 
 * A Google Gemini API key is required to run the agent. Set the GOOGLE_API_KEY environment variable to the API key.
 *
 * Project 5 - LLM-Based Agent
 * for CS 511: Artificial Intelligence II
 * at the University of Illinois Chicago
 *
 */

import theseus511.LimerickWeatherBot
import theseus511.TheseusBot
import play.api.libs.json.{JsValue, Json}

import scala.language.postfixOps

/**
 * Requires `GOOGLE_API_KEY` environment variable to be set.
 */
object LLMBasedAgent extends AgentFunctionImpl:
  private var llmClientInitialized: Boolean = false
  private var intervalStartTime: Long = -1

  private val string2Action: Map[String, Int] = Map(
    "forward" -> Action.GO_FORWARD,
    "right" -> Action.TURN_RIGHT,
    "left" -> Action.TURN_LEFT,
    "shoot" -> Action.SHOOT,
    "grab" -> Action.GRAB,
    "nothing" -> Action.NO_OP
  )

  private final val actionPrompt: String =
    "Choose one action to execute out of go forward, turn left, turn right, shoot, grab, or do nothing."

  override def reset(): Unit = TheseusBot.reset()

  def stop(): Unit =
    if llmClientInitialized then
      println(LimerickWeatherBot.query("Chicago"))
      LimerickWeatherBot.stop()
      TheseusBot.stop()

  private def makeObsString(tp: TransferPercept): String =
    val s: String = "You observe" + (
      if tp.getGlitter then " "
      else " no "
    ) + "glitter" + (
      if tp.getStench then ", a "
      else ", no "
    ) + "stench" + (
      if tp.getBreeze then ", a "
      else ", no "
    ) + "breeze" + (
      if tp.getScream then ", and a scream!"
      else "."
    )
    if tp.getBump then "Oh no! You went out of bounds. Try again. " + s
    else s

  private def makeActionString(action: Int): String =
    "You " + (
      if action == Action.GO_FORWARD then "went forward."
      else if action == Action.TURN_RIGHT then "turned right."
      else if action == Action.TURN_LEFT then "turned left."
      else if action == Action.SHOOT then "shot."
      else if action == Action.GRAB then "grabbed."
      else "did nothing."
    )

  private def throttleLLMClient(): Unit =
    val millisSinceLastQuery =
      if intervalStartTime == -1 then 4000
      else System.currentTimeMillis - intervalStartTime
    Thread.sleep(4000 - millisSinceLastQuery)
    intervalStartTime = System.currentTimeMillis

  private def prepareLLMClient(): Unit =
    if !llmClientInitialized then
      llmClientInitialized = {
        try {
          theseus511.TheseusBot
          true
        }
        catch {
          case e: ExceptionInInitializerError =>
            println("*** GOOGLE_API_KEY environment variable is not set.")
            System.exit(1)
            false
        }
      }

  override def process(tp: TransferPercept): Int =
    prepareLLMClient()
    TheseusBot.appendToSystemMessage(makeObsString(tp))
    val responseJson: JsValue = TheseusBot.query(actionPrompt)
    throttleLLMClient()
    val actionString: String = (responseJson \ "best_action").as[String]
    println(actionString)
    val action = string2Action.keySet
      .find(actionString contains)
      .flatMap(string2Action get)
      .getOrElse(Action.NO_OP)
    println((responseJson \ "belief_state_after_action").as[String])
    TheseusBot.appendToSystemMessage(makeActionString(action))
    action
