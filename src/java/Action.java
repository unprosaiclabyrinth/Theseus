/*
 * Class that defines agent actions.
 * 
 * Written by James P. Biagioni (jbiagi1@uic.edu)
 * for CS511 Artificial Intelligence II
 * at The University of Illinois at Chicago
 * 
 * Last modified 1/31/07 
 * 
 * DISCLAIMER:
 * Elements of this application were borrowed from
 * the client-server implementation of the Wumpus
 * World Simulator written by Kruti Mehta at
 * The University of Texas at Arlington.
 * 
 */

class Action {
//	public static int START_TRIAL = 0;
	public static int GO_FORWARD = 1;
	public static int TURN_RIGHT = 2;
	public static int TURN_LEFT = 3;
	public static int GRAB = 4;
	public static int SHOOT = 5;
	public static int NO_OP = 6;
	public static int END_TRIAL = 7;
	
	public Action() {
		// nothing to construct...
	}
	
	public static String printAction(int action) {
		return switch (action) {
			case 0 -> "START_TRIAL";
			case 1 -> "GO_FORWARD";
			case 2 -> "TURN_RIGHT";
			case 3 -> "TURN_LEFT";
			case 4 -> "GRAB";
			case 5 -> "SHOOT";
			default -> "END_TRIAL";
		};
	}
}
