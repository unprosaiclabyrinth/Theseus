/*
 * Class that defines the agent function.
 * 
 * Written by James P. Biagioni (jbiagi1@uic.edu)
 * for CS511 Artificial Intelligence II
 * at The University of Illinois at Chicago
 * 
 * Last modified 2/19/07 
 * 
 * DISCLAIMER:
 * Elements of this application were borrowed from
 * the client-server implementation of the Wumpus
 * World Simulator written by Kruti Mehta at
 * The University of Texas at Arlington.
 * 
 */

class AgentFunction {
	public int process(TransferPercept tp) {
		// return action to be performed
		return LLMBasedAgent.process(tp); // specify agent
	}
	
	// public method to return the agent's name
	// do not remove this method
	public String getAgentName() {
		return "Theseus511";
	}
}
