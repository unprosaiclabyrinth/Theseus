/*
 * Class that defines the simulation environment.
 * 
 * Written by James P. Biagioni (jbiagi1@uic.edu)
 * for CS511 Artificial Intelligence II
 * at The University of Illinois at Chicago
 * 
 * Last modified 3/5/07 
 * 
 * DISCLAIMER:
 * Elements of this application were borrowed from
 * the client-server implementation of the Wumpus
 * World Simulator written by Kruti Mehta at
 * The University of Texas at Arlington.
 * 
 */

import java.io.BufferedWriter;

class Simulation {
	private int currScore = 0;
	private static final int actionCost = -1;
	private static final int deathCost = -1000;
	private static final int shootCost = -10;
    private int lastAction = 0;
	
	private boolean simulationRunning;
	
	private final Agent agent;
	private final Environment environment;
	private final TransferPercept transferPercept;
	private final BufferedWriter outputWriter;
	
	public Simulation(Environment wumpusEnvironment, int maxSteps, BufferedWriter outWriter, double forwardProbability) {
		// start the simulator
		simulationRunning = true;
		
		outputWriter = outWriter;
		transferPercept = new TransferPercept(wumpusEnvironment);
		environment = wumpusEnvironment;
		
		agent = new Agent(environment, transferPercept, forwardProbability);
		
		environment.placeAgent(agent);
		environment.printEnvironment();
		
		printCurrentPerceptSequence();
		
		try {
			System.out.println("Current score: " + currScore);
			outputWriter.write("Current score: " + currScore + "\n");

            int stepCounter = 0;
            while (simulationRunning && stepCounter < maxSteps) {
				System.out.println("Last action: " + Action.printAction(lastAction));
				outputWriter.write("Last action: " + Action.printAction(lastAction) + "\n");
				
				System.out.println("Time step: " + stepCounter);
				outputWriter.write("Time step: " + stepCounter + "\n");
				
				handleAction(agent.chooseAction());
				wumpusEnvironment.placeAgent(agent);
				
				environment.printEnvironment();
				printCurrentPerceptSequence();
								
				System.out.println("Current score: " + currScore);
				outputWriter.write("Current score: " + currScore + "\n");
				
				//Scanner in = new Scanner(System.in);
				//in.next();
				
				stepCounter += 1;
				
				if (stepCounter == maxSteps || !simulationRunning) {
					System.out.println("Last action: " + Action.printAction(lastAction));
					outputWriter.write("Last action: " + Action.printAction(lastAction) + "\n");
					
					System.out.println("Time step: " + stepCounter);
					outputWriter.write("Time step: " + stepCounter + "\n");
					
					lastAction = Action.END_TRIAL;
				}
				
				if (agent.getHasGold()) {
					System.out.println("\n" + agent.getName() + " found the GOLD!!");
					outputWriter.write("\n" + agent.getName() + " found the GOLD!!\n");
				}
				if (agent.getIsDead()) {
					System.out.println("\n" + agent.getName() + " is DEAD!!");
					outputWriter.write("\n" + agent.getName() + " is DEAD!!\n");	
				}
			}
		}
		catch (Exception e) {
			System.out.println("An exception was thrown: " + e);
		}

		printEndWorld();
	}
	
	public void printEndWorld() {
		try {
			environment.printEnvironment();
			
			System.out.println("Final score: " + currScore);
			outputWriter.write("Final score: " + currScore + "\n");
			
			System.out.println("Last action: " + Action.printAction(lastAction));
			outputWriter.write("Last action: " + Action.printAction(lastAction) + "\n");
		}
		catch (Exception e) {
			System.out.println("An exception was thrown: " + e);	
		}
	}
	
	public void printCurrentPerceptSequence() {
		try {
			System.out.print("Percept: <");	
			outputWriter.write("Percept: <");
			
			if (transferPercept.getBump()) {
				System.out.print("bump,");
				outputWriter.write("bump,");
			} else if (!transferPercept.getBump()) {
				System.out.print("none,");
				outputWriter.write("none,");
			}

			if (transferPercept.getGlitter()) {
				System.out.print("glitter,");
				outputWriter.write("glitter,");
			} else if (!transferPercept.getGlitter()) {
				System.out.print("none,");
				outputWriter.write("none,");
			}

			if (transferPercept.getBreeze()) {
				System.out.print("breeze,");
				outputWriter.write("breeze,");
			} else if (!transferPercept.getBreeze()) {
				System.out.print("none,");
				outputWriter.write("none,");
			}

			if (transferPercept.getStench()) {
				System.out.print("stench,");
				outputWriter.write("stench,");
			} else if (!transferPercept.getStench()) {
				System.out.print("none,");
				outputWriter.write("none,");
			}

			if (transferPercept.getScream()) {
				System.out.print("scream>\n");
				outputWriter.write("scream>\n");
			} else if (!transferPercept.getScream()) {
				System.out.print("none>\n");
				outputWriter.write("none>\n");
			}
		}
		catch (Exception e) {
			System.out.println("An exception was thrown: " + e);
		}
	}
	
	public void handleAction(int action) {
		try {
			if (action == Action.GO_FORWARD) {
				if (environment.getBump()) environment.setBump(false);
				
				agent.goForward();
				environment.placeAgent(agent);
				
				if (environment.checkDeath()) {
					currScore += deathCost;
					simulationRunning = false;
					
					agent.setIsDead(true);
				} else {
					currScore += actionCost;
				}
				
				if (environment.getScream()) environment.setScream(false);
				
				lastAction = Action.GO_FORWARD;
			} else if (action == Action.TURN_RIGHT) {
				currScore += actionCost;
				agent.turnRight();
				environment.placeAgent(agent);
				
				if (environment.getBump()) environment.setBump(false);
				if (environment.getScream()) environment.setScream(false);
				
				lastAction = Action.TURN_RIGHT;
			} else if (action == Action.TURN_LEFT) {
				currScore += actionCost;
				agent.turnLeft();
				environment.placeAgent(agent);
				
				if (environment.getBump()) environment.setBump(false);
				if (environment.getScream()) environment.setScream(false);
				
				lastAction = Action.TURN_LEFT;
			} else if (action == Action.GRAB) {
				if (environment.grabGold()) {
					currScore += 1000;
					simulationRunning = false;
					
					agent.setHasGold(true);
				} else currScore += actionCost;
				
				environment.placeAgent(agent);
				
				if (environment.getBump()) environment.setBump(false);
				if (environment.getScream()) environment.setScream(false);
				
				lastAction = Action.GRAB;
			} else if (action == Action.SHOOT) {
				if (agent.shootArrow()) {
					if (environment.shootArrow()) environment.setScream(true);
					currScore += shootCost;
				} else {
					if (environment.getScream()) environment.setScream(false);
					currScore += actionCost;
				}
				
				environment.placeAgent(agent);
				
				if (environment.getBump()) environment.setBump(false);
				lastAction = Action.SHOOT;
			} else if (action == Action.NO_OP) {
				environment.placeAgent(agent);
				
				if (environment.getBump()) environment.setBump(false);
				if (environment.getScream()) environment.setScream(false);
				
				lastAction = Action.NO_OP;
			}
		}
		catch (Exception e) {
			System.out.println("An exception was thrown: " + e);
		}
	}
	
	public int getScore() {
		return currScore;
	}
}