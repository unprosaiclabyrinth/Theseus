import scala.collection.mutable
import UbaModel.*

/** Runtime adapter for the exact-belief, macro-action POMCP-style planner. */
object UtilityBasedAgent extends AgentFunctionImpl:
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty
  private var planner = new POMCP(PlannerConfig())

  def configure(config: PlannerConfig): Unit =
    planner = new POMCP(config)
    actionQueue.clear()

  override def reset(): Unit =
    planner.reset()
    actionQueue.clear()

  override def process(tp: TransferPercept): Int =
    if tp.getGlitter then
      actionQueue.clear()
      return Action.GRAB
    if actionQueue.isEmpty then
      planner.pruneTree(Percept4(tp.getStench, tp.getBreeze, tp.getGlitter, tp.getScream))
      val move = planner.plan
      planner.pruneTree(move)
      actionQueue.enqueueAll(move.toActionSeq())
    actionQueue.dequeue()
