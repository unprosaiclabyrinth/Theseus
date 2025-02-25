# Overview

This project implements different intelligent agent architectures for an agent called `Theseus511` that operates in the wumpus world (described in AIMA 4ed with slight variations). The architectures are implemented with the *a priori* knowledge that:-

+ The agent starts in (1, 1), facing east, in a $$4 \times 4$$ grid.
+ There are exactly two pits in the world.
+ Actions are deterministic (e.g.:- a `GO_FORWARD` will cause the agent to move one square forward with 100% certainty, without any chance of the agent turning left instead).
+ A `NO_OP` is a possible action that does nothing. It has no cost, unlike other actions.

The agent aims to maximize the **average** score. The different architectures the project implements are:-

1. Simple reflex agent (SRA)
2. Model-based reflex agent (MRA) 

# Getting Started

All agent architectures are implemented in Scala. The `src` directory contains the source code for the wumpus world simulator and the agent implementation. The project repo contains a Makefile that automates building and running the different agents. The Makefile runs the project with the options `nonDeterministicMode` and `randomAgentLoc` set to `false`. It contains a `check` target that checks the system for the necessary tools (`scala`, `java`). It is recommended that the system is checked for the nexessary tools before running the project. The check command is:-
```zsh
make check
```
The simple reflex agent can be run using:-
```zsh
make sra
```
The model-based reflex agent can be run using:-
```zsh
make mra
```
A custom implementation of the agent function can be run (**note: if [proper protocol](#proper-protocol-for-custom-implementations) or formatting is not followed, or the custom AgentFunction results in an error, the custom run could lead to junk backup files in the `src/java` directory, or could break the `sra` and `mra` targets altogether**) using:-
```zsh
make run
```
The project was tested using:-

+ **Scala Version:** 3.6.3
+ **Java Version:** OpenJDK 22.0.1

## Proper protocol for custom implementations

1. Implement the custom agent in a `src/acala/CustomAgent.scala` object that extends the `AgentFunctionImpl` trait.
2. Override and define the abstract `process` method such that it returns the actions given the percepts. Replace the "specify agent" line (line 21) in `src/java/AgentFunction.java` with:-
```java
return CustomAgent.process(tp) // specify agent
```
3. Run `make run` to run the agent. This will not break the `sra` and `mra` targets.

# Design

The `reports` directory contains documents detailing the agent designs.

# Evaluation

The agent architectures are generally evaluated on their average score after 10,000 runs. The `scores` directory contains the score lists for 10,000 runs for all agents, whose summary statistics are provided in the respective reports. The $$10,\!000$ trials can be run using:-
```zsh
make tenk # this requires Julia
```
Feel free to run your own trials. A helper script called `wumpus_eval.sh` is provided in the project repo for this purpose. The script can be run using `-h` to display a help message. In general, the script takes three arguments: `num_trials` with option`-n`, `output_file` with option `-o`, and input file with option `-i`. The argument `num_trials` specifies the number of times the simulation is run and all the resulting scores are compiled in the file specified by the argument `output_file`. The argument `imput_file` specifies the file from which the script reads the scores, hence it is also the file to which the simulator writes the output. The arguments are optional; the default value is 10 for `num_trials`, "wumpus_eval.txt" for `output_file`, and "wumpus_out.txt" for `input_file`.

The last two lines of the script can be uncommented to make it print out the average score over the `num_trials` simulations run, i.e. the average of all the scores written to `output_file`. **The script uses the Unix shell utility `bc` for the mean computation. Hence, it should be installed for this to work.** The project must be compiled (using `make build`) before running the script for it to work. The usage of the evaluation script can be summarized as:-

```zsh
make build
./wumpus_eval.sh [-n <num_trials>] [-o <output_file>] [-i <input_file>] [-h]
```
