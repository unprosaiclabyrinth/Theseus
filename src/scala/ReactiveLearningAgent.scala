/*
 * Agent function for a reactive learning agent in the unknown wumpus environment
 * f_RLA: Ω* -> A
 *
 * Project 4 - Learning Agent
 * for CS 511: Artificial Intelligence II, Spring 2025
 * at the University of Illinois Chicago
 *
 */
import scala.collection.mutable
import scala.language.postfixOps
import scala.math.{abs, pow, sqrt}
import scala.util.Random

object ReactiveLearningAgent extends AgentFunctionImpl:
  private val actionQueue: mutable.Queue[Int] = mutable.Queue.empty

  private case class Percept(bump: Boolean, breeze: Boolean, stench: Boolean, glitter: Boolean, scream: Boolean):
    def isNone: Boolean = !bump && ! breeze && !stench && !glitter && !scream

  private sealed trait Unobservable

  // Unobservables with the wumpus position and without it are modeled and handled separately.
  private case class UnobservableWithWumpus(agent: Position, gold: Position, wumpus: Position,
                                            pit1: Position, pit2: Position, shouldBump: Boolean) extends Unobservable:
    def toSans: UnobservableSansWumpus = UnobservableSansWumpus(agent, gold, pit1, pit2, shouldBump)

    def pits: Set[Position] = Set(pit1, pit2)

  private case class UnobservableSansWumpus(agent: Position, gold: Position,
                                            pit1: Position, pit2: Position, shouldBump: Boolean) extends Unobservable:
    def pits: Set[Position] = Set(pit1, pit2)

  private sealed trait State:
    def isTerminal: Boolean

    private def transition(m: Move): BeliefState =
      (this match {
        case s: StateWithWumpus => BeliefState(s.agentOrientation, s.hasArrow, Map(s.u -> 1), Move.NoOp)
        case s: StateSansWumpus => BeliefState(s.agentOrientation, false, Map(s.u -> 1), Move.NoOp)
      }).transition(m)


    def reward(m: Move): Int =
      val successor = transition(m)
      {
        if successor.isTerminal then -1000
        else m match {
          case Move.GoForward | Move.TurnLeft | Move.TurnRight => -1
          case Move.Shoot => -10
          case Move.NoOp => 0
          case Move.Grab => this match {
            case s: StateWithWumpus if s.u.agent == s.u.gold => 1000
            case s: StateSansWumpus if s.u.agent == s.u.gold => 1000
            case _ => -1
          }
        }
      }

    def heuristic(m: Move): Int =
      val successor = transition(m)

      // The distance to the gold is a big one
      def goldDist(state: State): Int =
        state match {
          case s: StateWithWumpus => manhattanDistance(s.u.gold, s.u.agent)
          case s: StateSansWumpus => manhattanDistance(s.u.gold, s.u.agent)
        }

      val h1 = -4 * (goldDist(successor.sampleState) - goldDist(this))

      // Assign a "wumpus score"
      def wumpusScore(state: State): Int =
        state match {
          case s: StateWithWumpus => 0
          case s: StateSansWumpus => 9
        }

      val h2 = wumpusScore(successor.sampleState) - wumpusScore(this)
      h1 + h2

  // States with wumpus alive and those with wumpus dead are modeled and handled separately
  // They inherit from State.
  private case class StateWithWumpus(agentOrientation: Orientation, hasArrow: Boolean,
                                     u: UnobservableWithWumpus) extends State:
    override def isTerminal: Boolean = Set(u.wumpus, u.pit1, u.pit2) contains u.agent

  private case class StateSansWumpus(agentOrientation: Orientation,
                                     u: UnobservableSansWumpus) extends State:
    override def isTerminal: Boolean = Set(u.pit1, u.pit2) contains u.agent

  private case class BeliefState(agentOrientation: Orientation,
                                 hasArrow: Boolean,
                                 belief: Map[Unobservable, BigDecimal],
                                 prevAction: Move):
    def u2State(u: Unobservable): State =
      require(belief contains u, "u2State: I don't believe this!")
      u match {
        case uWithWumpus: UnobservableWithWumpus => StateWithWumpus(agentOrientation, hasArrow, uWithWumpus)
        case uSansWumpus: UnobservableSansWumpus => StateSansWumpus(agentOrientation, uSansWumpus)
      }

    def possibleMoves: Set[Move] =
      if isTerminal then return Set.empty
      Move.values.toSet filter {
        case Move.Shoot => hasArrow
        case _ => true
      }

    def isTerminal: Boolean = belief.isEmpty

    private def weightedParticleFilter(prior: Map[Unobservable, BigDecimal], obs: Boolean,
                               condition: Unobservable => Boolean): Map[Unobservable, BigDecimal] =
      // if obs then condition should be true else should be false
      val posterior = prior.filter((u, _) => condition(u) == obs)

      // normalize
      val t = posterior.values.sum
      val sz = posterior.size
      posterior.map((u, p) => u match {
        case u: UnobservableWithWumpus => u -> p/(if t == 0 then sz else t)
        case u: UnobservableSansWumpus => u -> p/(if t == 0 then sz else t)
      })

    def observe(percept: Percept): BeliefState =
      val posterior =
        weightedParticleFilter( // bump update
          weightedParticleFilter( // glitter update
            weightedParticleFilter( // breeze update
              weightedParticleFilter( // stench update
                weightedParticleFilter( // scream update
                  belief, percept.scream, u => prevAction match {
                    case Move.Shoot => u.isInstanceOf[UnobservableSansWumpus]
                    case _ => percept.scream // no filtering, essentially identity function, keep belief as is
                  }
                ), percept.stench, {
                  case uWithWumpus: UnobservableWithWumpus =>
                    neighborsOf(uWithWumpus.agent) contains uWithWumpus.wumpus
                  case _ => false
                }
              ), percept.breeze, {
                case u: UnobservableWithWumpus =>
                  u.pits.exists(neighborsOf(u.agent).contains)
                case u: UnobservableSansWumpus =>
                  u.pits.exists(neighborsOf(u.agent).contains)
              }
            ), percept.glitter, {
              case u: UnobservableWithWumpus => u.agent == u.gold
              case u: UnobservableSansWumpus => u.agent == u.gold
            }
          ), percept.bump, {
            case u: UnobservableWithWumpus =>
               u.shouldBump && !(Set((2, 2), (2, 3), (3, 2), (3, 3)) contains u.agent)
            case u: UnobservableSansWumpus =>
               u.shouldBump && !(Set((2, 2), (2, 3), (3, 2), (3, 3)) contains u.agent)
          }
        )

      // normalize the posterior
      copy(belief = posterior.map((u, p) => (u match {
        case u: UnobservableWithWumpus => u.copy(shouldBump = false)
        case u: UnobservableSansWumpus => u.copy(shouldBump = false)
      }) -> p))

    def transition(move: Move): BeliefState =
      val (ao, ha, posterior) = move match {
        case Move.GoForward => (agentOrientation, hasArrow,
          belief.toList.flatMap((u, p) => u match {
            case u: UnobservableWithWumpus =>
              val f: Position = agentOrientation.forwardFrom(u.agent)
              val uf = u.copy(agent = if hitsWall(f) then u.agent else f, shouldBump = hitsWall(f))
              val l: Position = agentOrientation.leftFrom(u.agent)
              val ul = u.copy(agent = if hitsWall(l) then u.agent else l, shouldBump = hitsWall(l))
              val r: Position = agentOrientation.rightFrom(u.agent)
              val ur = u.copy(agent = if hitsWall(r) then u.agent else r, shouldBump = hitsWall(r))
              List((uf, p * forwardProbability), (ul, p * slipProbability), (ur, p * slipProbability))
            case u: UnobservableSansWumpus =>
              val f: Position = agentOrientation.forwardFrom(u.agent)
              val uf = u.copy(agent = if hitsWall(f) then u.agent else f, shouldBump = hitsWall(f))
              val l: Position = agentOrientation.leftFrom(u.agent)
              val ul = u.copy(agent = if hitsWall(l) then u.agent else l, shouldBump = hitsWall(l))
              val r: Position = agentOrientation.rightFrom(u.agent)
              val ur = u.copy(agent = if hitsWall(r) then u.agent else r, shouldBump = hitsWall(r))
              List((uf, p * forwardProbability), (ul, p * slipProbability), (ur, p * slipProbability))
          }).groupMapReduce(_._1)(_._2)(_ + _)
        )
        case Move.TurnRight => (agentOrientation.turnRight, hasArrow, belief)
        case Move.TurnLeft => (agentOrientation.turnLeft, hasArrow, belief)
        // move = Shoot only when hasArrow is true as dictated by possibleMoves
        case Move.Shoot => (agentOrientation, false,
          belief.map((u, p) => (u match {
            case u: UnobservableWithWumpus =>
              val (xA, yA) = u.agent
              val (xW, yW) = u.wumpus
              agentOrientation match {
                case Orientation.North if xW == xA && yW > yA => u.toSans
                case Orientation.South if xW == xA && yW < yA => u.toSans
                case Orientation.East if xW > xA && yW == yA => u.toSans
                case Orientation.West if xW < xA && yW == yA => u.toSans
                case _ => u
              }
            case u: UnobservableSansWumpus => assert(false, "Sans wumpus before shooting?!")
          }, p))
        )
        case _ => (agentOrientation, hasArrow, belief)
      }

      // Filter out states in which agent is dead
      val alive = posterior.filter ((u, _) => u match {
        case u: UnobservableWithWumpus => !StateWithWumpus(ao, ha, u).isTerminal
        case u: UnobservableSansWumpus => !StateSansWumpus(ao, u).isTerminal
      })

      // normalize
      val t = alive.values.sum
      val sz = alive.size
      BeliefState(ao, ha, alive.map((u, p) => u -> p/(if t == 0 then sz else t)), prevAction = move)

    def sampleState: State =
      val sorted = belief.toList.sortBy(_._2)(Ordering[BigDecimal].reverse).toMap

      val totalProbability = sorted.values.sum
      require(totalProbability > 0, "Total probability must be positive.")

      val r = BigDecimal(Random nextDouble) * totalProbability

      u2State(
        sorted.toList.foldLeft((sorted.head._1, BigDecimal(0), false)) {
          case (acc, (u, p)) =>
            val (sample, cumulative, isSet) = acc
            if isSet then (sample, cumulative, isSet)
            else if cumulative + p >= r then (u, cumulative + p, true)
            else (u, cumulative + p, isSet)
        }._1
      )

    def chanceOfDeath: Map[Orientation, BigDecimal] =
      belief.foldLeft (Map(
        Orientation.North -> BigDecimal(0),
        Orientation.South -> BigDecimal(0),
        Orientation.East -> BigDecimal(0),
        Orientation.West -> BigDecimal(0)
      )) {
        case (acc, (u, p)) => acc.map((ref, mort) =>
          ref -> (u match {
            case u: UnobservableWithWumpus =>
              val danger = u.pits ++ Set(u.wumpus)
              if ref == agentOrientation then
                mort + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnLeft then
                mort + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnRight then
                mort + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else
                mort + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
            case u: UnobservableSansWumpus =>
              val danger = u.pits
              if ref == agentOrientation then
                mort + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnLeft then
                mort + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnRight then
                mort + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.forwardFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
              else
                mort + {
                  if danger contains agentOrientation.backFrom(u.agent) then
                    p * forwardProbability
                  else 0
                } + {
                  if danger contains agentOrientation.leftFrom(u.agent) then
                    p * slipProbability
                  else 0
                } + {
                  if danger contains agentOrientation.rightFrom(u.agent) then
                    p * slipProbability
                  else 0
                }
          })
        )
      }

    def chanceOfGold: Map[Orientation, BigDecimal] =
      belief.foldLeft (Map(
        Orientation.North -> BigDecimal(0),
        Orientation.South -> BigDecimal(0),
        Orientation.East -> BigDecimal(0),
        Orientation.West -> BigDecimal(0)
      )) {
        case (acc, (u, p)) => acc.map((ref, pGold) =>
          ref -> (u match {
            case u: UnobservableWithWumpus =>
              val danger = u.pits ++ Set(u.wumpus)
              if ref == agentOrientation then
                pGold + {
                  val f = agentOrientation.forwardFrom(u.agent)
                  val l = agentOrientation.leftFrom(u.agent)
                  val r = agentOrientation.rightFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnLeft then
                pGold + {
                  val f = agentOrientation.leftFrom(u.agent)
                  val l = agentOrientation.backFrom(u.agent)
                  val r = agentOrientation.forwardFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnRight then
                pGold + {
                  val f = agentOrientation.rightFrom(u.agent)
                  val l = agentOrientation.forwardFrom(u.agent)
                  val r = agentOrientation.backFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else
                pGold + {
                  val f = agentOrientation.backFrom(u.agent)
                  val l = agentOrientation.rightFrom(u.agent)
                  val r = agentOrientation.leftFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
            case u: UnobservableSansWumpus =>
              val danger = u.pits
              if ref == agentOrientation then
                pGold + {
                  val f = agentOrientation.forwardFrom(u.agent)
                  val l = agentOrientation.leftFrom(u.agent)
                  val r = agentOrientation.rightFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnLeft then
                pGold + {
                  val f = agentOrientation.leftFrom(u.agent)
                  val l = agentOrientation.backFrom(u.agent)
                  val r = agentOrientation.forwardFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else if ref == agentOrientation.turnRight then
                pGold + {
                  val f = agentOrientation.rightFrom(u.agent)
                  val l = agentOrientation.forwardFrom(u.agent)
                  val r = agentOrientation.backFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
              else
                pGold + {
                  val f = agentOrientation.backFrom(u.agent)
                  val l = agentOrientation.rightFrom(u.agent)
                  val r = agentOrientation.leftFrom(u.agent)
                  if u.gold == f && !(danger contains f) then
                    p * forwardProbability
                  else if (u.gold == l && !(danger contains l)) ||
                    (u.gold == r && !(danger contains r)) then
                    p * slipProbability
                  else 0
                }
          })
        )
      }

  private enum Orientation:
    case North, South, East, West

    def forwardFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y + 1)
        case South => (x, y - 1)
        case East => (x + 1, y)
        case West => (x - 1, y)
      }

    def rightFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x + 1, y)
        case South => (x - 1, y)
        case East => (x, y - 1)
        case West => (x, y + 1)
      }

    def leftFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x - 1, y)
        case South => (x + 1, y)
        case East => (x, y + 1)
        case West => (x, y - 1)
      }

    def backFrom(pos: Position): Position =
      val (x, y) = pos
      this match {
        case North => (x, y - 1)
        case South => (x, y + 1)
        case East => (x - 1, y)
        case West => (x + 1, y)
      }

    def turnRight: Orientation = this match {
      case North => East
      case South => West
      case East => South
      case West => North
    }

    def turnLeft: Orientation = this match {
      case North => West
      case South => East
      case East => North
      case West => South
    }

    def turnBack: Orientation = this match {
      case North => South
      case South => North
      case East => West
      case West => East
    }

  private enum Move(val action: Int):
    case GoForward extends Move(Action.GO_FORWARD)
    case TurnRight extends Move(Action.TURN_RIGHT)
    case TurnLeft extends Move(Action.TURN_LEFT)
    case Shoot extends Move(Action.SHOOT)
    case NoOp extends Move(Action.NO_OP)
    case Grab extends Move(Action.GRAB)

  private def neighborsOf(sq: Position): Set[Position] =
    val (x, y) = sq
    Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)) filter {
      case (x, y) => x >= 1 && x <= 4 && y >= 1 && y <= 4
    }

  private def manhattanDistance(pos1: Position, pos2: Position): Int =
    val (x1, y1) = pos1
    val (x2, y2) = pos2
    (x2 - x1).abs + (y2 - y1).abs

  private def hitsWall(sq: Position): Boolean =
    val (x, y) = sq
    x < 1 || x > 4 || y < 1 || y > 4

  private var forwardProbability: BigDecimal = 0
  private def slipProbability: BigDecimal = (1 - forwardProbability) / 2

  // keep a partial model to initialize helper agents with
  private var hasArrow: Boolean = true
  private val pitFree: mutable.Set[Position] = mutable.Set((1, 1))
  private var wumpus = (0, 1)
  private val agentPosPrior: mutable.Set[Position] = mutable.Set((1, 1))
  private var lastAction: Move = Move.NoOp

  private object Learning:
    private enum LearningState:
      case Start, GivenUp, Running, Stop

    private val maxLearningIters = 15
    private val learningExperience: mutable.ListBuffer[Boolean] = mutable.ListBuffer.empty
    private var state: LearningState = LearningState.Start

    def learned: BigDecimal =
      // Return maximum likelihood result
      val failureCount = learningExperience.count(!_)
      if failureCount == 0 then 1
      else if failureCount == 1 then 0.8
      else
        val n: Double = learningExperience.takeWhile(identity).length
        val m: Double = learningExperience.reverse.tail.takeWhile(identity).length
        val discriminant: Double = sqrt(pow(2*m + n + 2, 2) - 8*n)
        val pMle: Double = (n - 2 + discriminant) / (2 * (n + m + 2))
        if abs(pMle - 0.8) > abs(pMle - 1D/3D) then 1D/3D
        else 0.8

    def notDone: Boolean = state != LearningState.Stop

    def isDone: Boolean = !notDone

    def reset(): Unit =
      state = LearningState.Start
      learningExperience.clear()

    object StateMachine:
      def transition(o: Percept): Unit =
        state = state match {
          case LearningState.Start =>
            if o.breeze then
              actionQueue enqueue Action.NO_OP
              LearningState.GivenUp
            else
              pitFree ++= Set((1, 2), (2, 1))
              if o.stench then
                if hasArrow then
                  hasArrow = false
                  actionQueue enqueue Action.SHOOT
                  LearningState.Start
                else
                  wumpus = (1, 2)
                  actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                  LearningState.Running
              else if o.scream then
                wumpus = (0, 0)
                actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                LearningState.Running
              else
                actionQueue enqueue (Action.TURN_RIGHT, Action.GO_FORWARD)
                LearningState.Running
          case LearningState.GivenUp =>
            actionQueue enqueue Action.NO_OP
            LearningState.GivenUp
          case LearningState.Running =>
            learningExperience += o.bump
            val failureCount = learningExperience.count(!_)
            if !o.bump && failureCount == 1 then
              agentPosPrior -= ((1, 1)) += ((2, 1))
            if learningExperience.length == maxLearningIters || failureCount == 2 then
              if failureCount == 2 then
                agentPosPrior -= ((2, 1)) ++= Set((1, 1), (3, 1))
              actionQueue enqueue Action.TURN_LEFT
              LearningState.Stop
            else if o.breeze then
              actionQueue enqueue Action.NO_OP
              LearningState.GivenUp
            else
              if agentPosPrior.toSet == Set((2, 1)) then
                pitFree ++= Set((2, 2), (3, 1))
              if o.stench then
                if hasArrow then
                  hasArrow = false
                  actionQueue enqueue (Action.TURN_LEFT, Action.SHOOT, Action.TURN_RIGHT, Action.GO_FORWARD)
                else
                  if agentPosPrior.toSet == Set((2, 1)) then wumpus = (2, 2)
                  actionQueue enqueue Action.GO_FORWARD
              else
                if o.scream then wumpus = (0, 0)
                actionQueue enqueue Action.GO_FORWARD
              LearningState.Running
          case LearningState.Stop => LearningState.Stop
        }

  override def reset(): Unit =
    hasArrow = true
    pitFree.clear()
    pitFree += 1 -> 1
    wumpus = (0, 1)
    agentPosPrior.clear()
    agentPosPrior += 1 -> 1
    lastAction = Move.NoOp
    forwardProbability = 0
    Learning.reset()
    ModelBasedReflexAgent.reset()

  // some random value
  private var globB: BeliefState = BeliefState(Orientation.East, false, Map.empty, Move.NoOp)

  private def initializeGlobalBeliefState(): Unit = globB = { // proper initialization here
    require(Learning isDone, "Learning is not yet complete.")

    val allSquares: Set[Position] = (1 to 4).flatMap(x => (1 to 4).map(y => (x, y))).toSet

    val possiblePitCombinations = (allSquares diff pitFree).toList.combinations(2).toSet.map {
      case Seq(p1, p2) => (p1, p2)
      case _ => assert(false, "Wtf?! 2-combinations should have size 2.")
    }

    val possibleWumpusPositions =
      if wumpus == (0, 1) then allSquares diff pitFree
      else if wumpus == (0, 0) then Set.empty[Position]
      else Set(wumpus)

    val possibleGoldLocations = allSquares diff
      (if forwardProbability == 1 then Set((1, 1)) else Set((1, 1), (2, 1)))

    val unweighted: Set[Unobservable] =
      if possibleWumpusPositions nonEmpty then
        possiblePitCombinations flatMap {
          case (p1, p2) => possibleWumpusPositions flatMap (w =>
            possibleGoldLocations flatMap (g => agentPosPrior map (a =>
              UnobservableWithWumpus(a, g, w, p1, p2, false)
              ))
            )
        }
      else possiblePitCombinations flatMap {
        case (p1, p2) => possibleGoldLocations flatMap (g =>
          agentPosPrior map (a => UnobservableSansWumpus(a, g, p1, p2, false))
          )
      }

    val s = unweighted.size
    val weighted: Map[Unobservable, BigDecimal] = unweighted.map(u => u -> BigDecimal(1) / BigDecimal(s)).toMap

    BeliefState(Orientation.East, hasArrow, weighted, lastAction)
  }

  private object SimpleReflexHelper:
    def process(o: Percept, init: Boolean = false): Move =
      if init then actionQueue enqueue Action.TURN_LEFT
      if o.breeze || o.stench then Move.NoOp
      else Move.GoForward

  private object StochasticModelBasedHelper:
    private def difference(b: BeliefState): Map[Orientation, BigDecimal] =
      val death = b.chanceOfDeath
      val gold = b.chanceOfGold
      gold.map((ref, pGold) => ref -> (pGold - death(ref)))
        .toList.sortBy(_._2).reverse.toMap

    def process(o: Percept): Move =
      val d = difference(globB)
      d foreach ((ref, p) => println(f"$ref: $p%.5f"))
      val move =
        if d forall (_._2 < 0) then
          if o.stench && globB.hasArrow then
            val df = difference(globB.transition(Move.Shoot))
            val dl = difference(globB.transition(Move.TurnLeft).transition(Move.Shoot))
            val dr = difference(globB.transition(Move.TurnRight).transition(Move.Shoot))
            if df exists (_._2 > 0) then
              actionQueue enqueue Action.SHOOT
              val goldenRef = df.head._1
              if globB.agentOrientation == goldenRef then Move.GoForward
              else if globB.agentOrientation.turnRight == goldenRef then
                actionQueue enqueue Action.TURN_RIGHT
                Move.GoForward
              else if globB.agentOrientation.turnLeft == goldenRef then
                actionQueue enqueue Action.TURN_LEFT
                Move.GoForward
              else
                if Random nextBoolean then
                  actionQueue enqueue (Action.TURN_RIGHT, Action.TURN_RIGHT)
                else
                  actionQueue enqueue (Action.TURN_LEFT, Action.TURN_LEFT)
                Move.GoForward
            else if dl exists (_._2 > 0) then
              actionQueue enqueue (Action.TURN_LEFT, Action.SHOOT)
              val goldenRef = df.head._1
              if globB.agentOrientation == goldenRef then
                actionQueue enqueue Action.TURN_RIGHT
                Move.GoForward
              else if globB.agentOrientation.turnRight == goldenRef then
                if Random nextBoolean then
                  actionQueue enqueue (Action.TURN_RIGHT, Action.TURN_RIGHT)
                else
                  actionQueue enqueue (Action.TURN_LEFT, Action.TURN_LEFT)
                Move.GoForward
              else if globB.agentOrientation.turnLeft == goldenRef then Move.GoForward
              else
                actionQueue enqueue Action.TURN_RIGHT
                Move.GoForward
            else if dr exists (_._2 > 0) then
              actionQueue enqueue (Action.TURN_RIGHT, Action.SHOOT)
              val goldenRef = df.head._1
              if globB.agentOrientation == goldenRef then
                actionQueue enqueue Action.TURN_LEFT
                Move.GoForward
              else if globB.agentOrientation.turnRight == goldenRef then Move.GoForward
              else if globB.agentOrientation.turnLeft == goldenRef then
                if Random nextBoolean then
                  actionQueue enqueue (Action.TURN_RIGHT, Action.TURN_RIGHT)
                else
                  actionQueue enqueue (Action.TURN_LEFT, Action.TURN_LEFT)
                Move.GoForward
              else
                actionQueue enqueue Action.TURN_LEFT
                Move.GoForward
            else Move.NoOp
          else Move.NoOp
        else
          val goldenRef = d.head._1
          if globB.agentOrientation == goldenRef then Move.GoForward
          else if globB.agentOrientation.turnRight == goldenRef then
            actionQueue enqueue Action.TURN_RIGHT
            Move.GoForward
          else if globB.agentOrientation.turnLeft == goldenRef then
            actionQueue enqueue Action.TURN_LEFT
            Move.GoForward
          else
            if Random nextBoolean then
              actionQueue enqueue(Action.TURN_RIGHT, Action.TURN_RIGHT)
            else
              actionQueue enqueue(Action.TURN_LEFT, Action.TURN_LEFT)
            Move.GoForward
      move

  override def process(tp: TransferPercept): Int =
    val percept = Percept(tp getBump, tp getBreeze, tp getStench, tp getGlitter, tp getScream)
    if actionQueue isEmpty then
      if percept.glitter then
        actionQueue enqueue Action.GRAB
      else if Learning notDone then
        Learning.StateMachine.transition(percept)
      else if forwardProbability == 0 then
        forwardProbability = Learning.learned
        initializeGlobalBeliefState()
        globB.observe(percept)
        if forwardProbability == 1 then
          ModelBasedReflexAgent.reset()
          ModelBasedReflexAgent.rlaInit(hasArrow)
          actionQueue enqueue ModelBasedReflexAgent.process(tp)
        else if forwardProbability == 0.8 then
          actionQueue enqueue SimpleReflexHelper.process(percept, true).action
//          actionQueue enqueue StochasticModelBasedHelper.process(percept).action
        else
//          actionQueue enqueue SimpleReflexHelper.process(percept, true).action
          actionQueue enqueue StochasticModelBasedHelper.process(percept).action
      else
        // adaptive update
        if forwardProbability == 1 && percept.bump then forwardProbability = 0.8
        globB = globB.observe(percept)
        if forwardProbability == 1 then
          actionQueue enqueue ModelBasedReflexAgent.process(tp)
        else if forwardProbability == 0.8 then
          actionQueue enqueue SimpleReflexHelper.process(percept).action
//          actionQueue enqueue StochasticModelBasedHelper.process(percept).action
        else
//          actionQueue enqueue SimpleReflexHelper.process(percept).action
          actionQueue enqueue StochasticModelBasedHelper.process(percept).action
    val action = actionQueue.dequeue
    val move =
      if action == Action.GO_FORWARD then Move.GoForward
      else if action == Action.TURN_LEFT then Move.TurnLeft
      else if action == Action.TURN_RIGHT then Move.TurnRight
      else if action == Action.GRAB then Move.Grab
      else if action == Action.SHOOT then Move.Shoot
      else Move.NoOp
    if Learning notDone then lastAction = move
    else globB = globB.transition(move)
    action
