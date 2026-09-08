/*
 * Wumpus-Lite, version 0.18 alpha
 * A lightweight Java-based Wumpus World Simulator
 * 
 * Written by James P. Biagioni (jbiagi1@uic.edu)
 * for CS511 Artificial Intelligence II
 * at The University of Illinois at Chicago
 * 
 * Thanks to everyone who provided feedback and
 * suggestions for improving this application,
 * especially the students from Professor
 * Gmytrasiewicz's Spring 2007 CS511 class.
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
import java.io.*;
import java.nio.file.*;
import java.util.Random;

class WorldApplication {
    record Config(String agent, int trials, int steps, double probability, int seed,
                  Path output, Path scores, boolean quiet, boolean mixed, PlannerConfig planner) {}

    static Config parse(String[] args) {
        String agent = "uba";
        int trials = 1, steps = 50, seed = new Random().nextInt();
        double probability = 1;
        Path output = Path.of("wumpus_out.txt"), scores = null;
        boolean quiet = false, mixed = false;
        var plannerOptions = new java.util.HashMap<String, String>();
        for (int i = 0; i < args.length; i++) {
            String option = args[i];
            if (option.equals("--quiet")) { quiet = true; continue; }
            if (option.equals("--mixed")) { mixed = true; continue; }
            if (!PlannerOptions.accepts(option) && !java.util.Set.of("--agent", "-d", "-a", "-t", "-s", "-r", "-n", "-f", "--scores").contains(option))
                throw new IllegalArgumentException("Unknown option: " + option);
            if (++i == args.length) throw new IllegalArgumentException("Missing value for " + option);
            String value = args[i];
            if (PlannerOptions.accepts(option)) { plannerOptions.put(option, value); continue; }
            switch (option) {
                case "--agent" -> agent = value;
                case "-t" -> trials = Integer.parseInt(value);
                case "-s" -> steps = Integer.parseInt(value);
                case "-r" -> seed = Integer.parseInt(value);
                case "-n" -> probability = Double.parseDouble(value);
                case "-f" -> output = Path.of(value);
                case "--scores" -> scores = Path.of(value);
                case "-d" -> {
                    if (Integer.parseInt(value) != 4) throw new IllegalArgumentException("Bundled agents require a 4x4 world.");
                }
                case "-a" -> {
                    if (!value.equals("false")) throw new IllegalArgumentException("Bundled agents require the fixed start: use -a false.");
                }
            }
        }
        if (!java.util.Set.of("sra", "mra", "uba", "rla", "lba").contains(agent))
            throw new IllegalArgumentException("Unknown agent: " + agent);
        if (trials < 1 || steps < 1) throw new IllegalArgumentException("Trials and steps must be positive.");
        if (!Double.isFinite(probability) || probability < 0 || probability > 1)
            throw new IllegalArgumentException("-n must be a finite probability in [0,1].");
        if ((agent.equals("mra") || agent.equals("uba")) && probability != 1)
            throw new IllegalArgumentException("MRA and UBA require -n 1.");
        if (mixed && !agent.equals("rla")) throw new IllegalArgumentException("--mixed requires --agent rla.");
        if (agent.equals("rla") && !mixed && probability != 1 && probability != 0.8 && Math.abs(probability - 1.0/3) > 0.0001)
            throw new IllegalArgumentException("RLA supports -n 1, 0.8, or 0.3333333333333333.");
        if (!plannerOptions.isEmpty() && !agent.equals("uba"))
            throw new IllegalArgumentException("Planner options require --agent uba.");
        PlannerConfig planner = PlannerOptions.parse(plannerOptions);
        if (scores == null) scores = output.resolveSibling(output.getFileName() + ".scores.csv");
        if (output.toAbsolutePath().normalize().equals(scores.toAbsolutePath().normalize()))
            throw new IllegalArgumentException("Trace and score files must be different.");
        return new Config(agent, trials, steps, probability, seed, output, scores, quiet, mixed, planner);
    }

    public static void main(String[] args) {
        if (java.util.Arrays.asList(args).contains("--help")) {
            System.out.println("Usage: --agent sra|mra|uba|rla|lba [-t trials] [-s steps] [-r seed] [-n probability]\n"
                + "       [-f trace.txt] [--scores scores.csv] [--quiet] [--mixed]\n"
                + "UBA: [--simulations N] [--horizon N] [--discount 0..1] [--exploration C]\n"
                + "     [--tree-policy canonical|heuristic] [--rollout uniform|informed] [--shaping none|legacy|potential]\n"
                + "Defaults: uba, 1 trial, 50 steps, probability 1, 4x4 world, fixed start.\n"
                + "--mixed evaluates RLA across 1, 0.8 and 1/3; --quiet omits step traces.\n"
                + "Output files are overwritten. The score file defaults to <trace filename>.scores.csv.");
            return;
        }
        int status = run(args);
        if (status != 0) System.exit(status);
    }

    static int run(String[] args) {
        try {
            execute(parse(args));
            return 0;
        } catch (Exception e) {
            System.err.println("Run failed: " + e.getMessage());
            return 1;
        }
    }

    static void execute(Config c) throws Exception {
        if (Files.exists(c.output) && Files.exists(c.scores) && Files.isSameFile(c.output, c.scores))
            throw new IllegalArgumentException("Trace and score files must be different.");
        if (c.agent.equals("lba")) LLMBasedAgent.configure(c.probability);
        if (c.agent.equals("uba")) UtilityBasedAgent.configure(c.planner);
        AgentFunction function = new AgentFunction(c.agent);
        PrintStream console = System.out;
        try (BufferedWriter output = Files.newBufferedWriter(c.output);
             BufferedWriter scores = Files.newBufferedWriter(c.scores);
             BufferedWriter discard = new BufferedWriter(Writer.nullWriter());
             PrintStream quietConsole = new PrintStream(OutputStream.nullOutputStream())) {
            String metadata = "Theseus agent=" + c.agent + " seed=" + c.seed + " trials=" + c.trials
                + " steps=" + c.steps + " probability=" + (c.mixed ? "mixed" : c.probability)
                + (c.agent.equals("uba") ? " planner=" + c.planner : "");
            console.println(metadata);
            output.write(metadata + "\n");
            scores.write("trial,seed,agent,forward_probability,score\n");
            long total = 0;
            for (int trial = 0; trial < c.trials; trial++) {
                int trialSeed = c.seed + trial;
                double probability = c.mixed ? switch (trial % 3) { case 0 -> 1; case 1 -> 0.8; default -> 1.0/3; } : c.probability;
                AgentRandom.seed(trialSeed);
                function.reset();
                int score;
                try {
                    if (c.quiet) System.setOut(quietConsole);
                    BufferedWriter trace = c.quiet ? discard : output;
                    if (!c.quiet) trace.write("Trial " + (trial + 1) + " seed=" + trialSeed + "\n");
                    Environment world = new Environment(4, generateRandomWumpusWorld(trialSeed, 4, false), trace);
                    score = new Simulation(world, c.steps, trace, probability, function,
                        new Random(((long)trialSeed) ^ 0x5DEECE66DL)).getScore();
                } finally {
                    System.setOut(console);
                    function.reset();
                }
                total += score;
                scores.write((trial + 1) + "," + trialSeed + "," + c.agent + "," + probability + "," + score + "\n");
                scores.flush(); // completed trials survive an error in a later trial
                output.write("Trial " + (trial + 1) + " score: " + score + "\n");
            }
            String summary = "Total Score: " + total + "\nAverage Score: " + ((double)total / c.trials);
            output.write(summary + "\n");
            console.println(summary);
        } finally {
            System.setOut(console);
            if (c.agent.equals("lba")) LLMBasedAgent.stop();
        }
    }
	public static char[][][] generateRandomWumpusWorld(int seed, int size, boolean randomlyPlaceAgent) {
        if (size < 2) throw new IllegalArgumentException("World size must be at least 2.");
		char[][][] newWorld = new char[size][size][4];
		boolean[][] occupied = new boolean[size][size];
		
		int x, y;
		
		Random randGen = new Random(seed);

		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				for (int k = 0; k < 4; k++) {
					newWorld[i][j][k] = ' '; 
				}
			}
		}
		
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				occupied[i][j] = false;
			}
		}
	     
		int pits = 2;
		
		// default agent location
		// and orientation
		int agentXLoc = 0;
		int agentYLoc = 0;
		char agentIcon = '>';
		
		// randomly generate agent
		// location and orientation
		if (randomlyPlaceAgent) {
			agentXLoc = randGen.nextInt(size);
			agentYLoc = randGen.nextInt(size);

			agentIcon = switch (randGen.nextInt(4)) {
				case 0 -> 'A';
				case 1 -> '>';
				case 2 -> 'V';
				case 3 -> '<';
				default -> agentIcon;
			};
		}
		
		// place agent in the world
		newWorld[agentXLoc][agentYLoc][3] = agentIcon;

		// Pit generation
		// Random
		for (int i = 0; i < pits; i++) {
			do {
				x = randGen.nextInt(size);
				y = randGen.nextInt(size);
			} while ((x == agentXLoc && y == agentYLoc) | occupied[x][y]);

			occupied[x][y] = true;
			newWorld[x][y][0] = 'P';
		}
		// Custom
//		occupied[2][0] = true;
//		newWorld[2][0][0] = 'P';
//
//		occupied[1][1] = true;
//		newWorld[1][1][0] = 'P';

		// Wumpus Generation
		// Random
		do {
			x = randGen.nextInt(size);
			y = randGen.nextInt(size);
		} while (x == agentXLoc && y == agentYLoc);

		occupied[x][y] = true;
		newWorld[x][y][1] = 'W';

		// Custom
//		occupied[0][1] = true;
//		newWorld[0][1][1] = 'W';
		
		// Gold Generation
		// Random
		x = randGen.nextInt(size);
		y = randGen.nextInt(size);

		//while (x == 0 && y == 0) {
		//	x = randGen.nextInt(size);
		//	y = randGen.nextInt(size);
		//}

		occupied[x][y] = true;
		newWorld[x][y][2] = 'G';
		
		// Custom
//		occupied[2][0] = true;
//		newWorld[2][0][2] = 'G';
		
		return newWorld;
	}
}
