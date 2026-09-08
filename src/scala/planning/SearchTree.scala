import scala.collection.mutable
import UbaModel.*

/** Alternating history and action nodes; action means guide selection. */
class SearchTree(config: PlannerConfig):
  /**
   * Model a node in the Monte Carlo search tree (MCST)
   * @param beliefState the corresponding belief state.
   * @param parent the parent node index.
   * @param children a map from updates that have occurred at this node to the resulting children node indices.
   * @param visitedCount the number of times this node has been visited.
   * @param value the expected utility value of this node.
   */
  case class Node(beliefState: BeliefState,
                          parent: Option[Int],
                          children: mutable.Map[Move | Percept4, Int],
                          visitedCount: Int,
                          value: BigDecimal)

  var root = -1 // current root index
  private var nextId = 0 // a counter that generates node indices on demand

  // a map from indices to MCST nodes
  private val initialBeliefState = BeliefState((1, 1), Orientation.East, true, initialBeliefPrior, History.empty)
  private val nodes: mutable.Map[Int, Node] = mutable.Map(-1 -> Node(
    initialBeliefState, None, mutable.Map.empty, 0, 0.0
  ))

  /**
   * Reset the MCST
   */
  def reset(): Unit =
    root = -1
    nextId = 0
    nodes.clear()
    nodes += (-1 -> Node(
      initialBeliefState, None, mutable.Map.empty, 0, 0.0
    ))

  /**
   * Expand from a node in the MCST to obtain a child via the given update.
   * Add the child node to the parent's children set as well as to the MCST's nodes map.
   * @param parent the node to expand from
   * @param update the update to expand via
   */
  def expandFrom(parent: Int, update: Move | Percept4): Unit =
    val newBeliefState = update match {
      case m: Move => nodes(parent).beliefState.transition(m)
      case o: Percept4 => nodes(parent).beliefState.observe(o)
    }
    val id = nextId
    nextId += 1
    nodes += (id -> Node(newBeliefState, Some(parent), mutable.Map.empty, 0, 0.0))
    nodes(parent).children += (update -> id)

  /**
   * Checks whether the given node has not been visited since that is the leaf condition
   * @param n a node index
   * @return is the node with index n a leaf in the MCST?
   */
  def isLeaf(n: Int): Boolean =
    require(nodes contains n, "isLeaf: No such node.")
    nodes(n).children.isEmpty

  /**
   * Retrieves the index of the child of the parent that is obtained via a given observation.
   * If no such child exists, then the child is created and its index is returned.
   * @param n a node in the MCST
   * @param o a percept
   * @return index of the child obtained on observing pecept o @ node n
   */
  def getObsNode(n: Int, o: Percept4): Int =
    require(nodes contains n, "getObsNode: Invalid node index.")
    if !(nodes(n).children contains o) then expandFrom(n, o)
    nodes(n).children(o)

  /**
   * Private helper that prunes the tree recursively to keep the subtree rooted at the new root.
   * @param root the current root
   * @param newRoot the new root where the pruned tree should be rooted
   */
  private def pruneRecursively(root: Int, newRoot: Int): Unit =
    if root != newRoot then
      nodes(root).children foreach ((_, n) => pruneRecursively(n, newRoot))
      nodes.subtractOne(root)

  /**
   * Computes the child of the current root that is obtained from the given update, makes
   * it the new root and deletes everything else. Essentially traverses the tree one step
   * based on a real update (move or percept) from the real world.
   * @param update a move or percept from the real world.
   */
  def prune(update: Move | Percept4): Unit =
    val children = nodes(root).children
    val newRoot = update match {
      case m: Move =>
        require(children contains update, "prune: No such child.")
        children(m)
      case o: Percept4 => getObsNode(root, o)
    }
    val oldRoot = root
    pruneRecursively(oldRoot, newRoot)
    root = newRoot
    nodes(root) = nodes(root).copy(parent = None)

  private def actionChildren(n: Int): Vector[(Move, Int)] =
    nodes(n).children.toVector.collect { case (m: Move, c) => (m, c) }.sortBy(_._1.ordinal)

  def selectionPolicy(n: Int): (Move, Int) =
    val actions = actionChildren(n)
    val candidates = actions.map { (move, id) =>
      ActionEstimate(move, nodes(id).value, nodes(id).visitedCount)
    }
    val chosen = TreePolicies.select(candidates, nodes(n).visitedCount,
      nodes(n).beliefState, config)
    (chosen, actions.find(_._1 == chosen).get._2)

  // Mean value first, visit count second, stable move order last; no UCB bonus.
  def bestMove(n: Int): Move = TreePolicies.best(actionChildren(n).map { (m, id) =>
    ActionEstimate(m, nodes(id).value, nodes(id).visitedCount)
  })

  def snapshot: Map[Int, Node] = nodes.toMap.map { (id, n) => id -> n.copy(children = n.children.clone()) }

  /**
   * Retrieve the belief state at a node in the MCST
   * @param n a node index in the MCST
   * @return the corresponding belief state
   */
  def beliefStateAt(n: Int): BeliefState =
    require(nodes contains n, "beliefStateAt: No such node.")
    nodes(n).beliefState

  /**
   * "Visit" a node by incrementing its visitedCount
   * @param n the node index of the node to visit in the MCST
   */
  def visit(n: Int): Unit =
    require(nodes contains n, "visit: No such node.")
    nodes.get(n).foreach(curr => nodes(n) = curr.copy(visitedCount = curr.visitedCount + 1))

  /**
   * Update the mean value of a node given a new utility
   * @param n a node index in the MCST
   * @param u a new utility value
   */
  def updateMeanValue(n: Int, u: BigDecimal): Unit =
    require(nodes contains n, "valueOf: No such node.")
    val m = nodes(n).visitedCount
    require(m > 0, "Visit a node before updating its mean.")
    val v = nodes(n).value
    nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = v + (u - v)/m))

  /**
   * Overwrite the value of a node with a new value
   * @param n a node index in the MCST
   * @param newValue a new utility value
   */
  def setValue(n: Int, newValue: BigDecimal): Unit =
    require(nodes contains n, "visit: No such node.")
    nodes.get(n).foreach(curr => nodes(n) = curr.copy(value = newValue))

  /**
   * A debugging helper that recursively dumps the MCST to stdout.
   * (WARNING: don't use unless on the verge of dying of frustration while debugging)
   * @param node a node index in the MCST
   * @param depth dumps the node at this depth
   */
  def dump(node: Int = root, depth: Int = 0): Unit =
    require(nodes contains node, s"dump: No such node $node")

    val prefix = "  " * depth
    val n = nodes(node)
    val parentStr = n.parent.map(_.toString).getOrElse("None")

    println(s"$prefix- Node $node")
    println(s"$prefix  | Parent: $parentStr")
    println(s"$prefix  | Value: ${n.value}")
    println(s"$prefix  | Visits: ${n.visitedCount}")
    println(s"$prefix  | Children: ${n.children.keys.mkString(", ")}")

    n.children.values.foreach(child => dump(child, depth + 1))

