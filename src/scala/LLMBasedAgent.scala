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
 
/**
 * Requires `GOOGLE_API_KEY` environment variable to be set.
 */
object LLMBasedAgent extends AgentFunctionImpl:
  private val apiKeyIsSet: Boolean = {
    try {
      theseus511.LimerickWeatherBot
      true
    }
    catch {
      case e: ExceptionInInitializerError => false
    }
  }

  override def reset(): Unit = {}

  override def process(tp: TransferPercept): Int =
    if !apiKeyIsSet then
      println("*** GOOGLE_API_KEY environment variable is not set.")
      System.exit(1)
    println(LimerickWeatherBot.query("Chicago"))
    LimerickWeatherBot.stop()
    Action.NO_OP