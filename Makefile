# Default target
all:
	@echo "Specify an agent target. Available agent targets: run, sra, mra, uba, hla"

# Simple reflex agent
sra: src/scala/SimpleReflexAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn SimpleReflexAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@if [[ -f src/java/AgentFunction.java.orig ]]; then mv src/java/AgentFunction.java.orig src/java/AgentFunction.java; fi

# Model-based reflex agent
mra: src/scala/ModelBasedReflexAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn ModelBasedReflexAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@if [[ -f src/java/AgentFunction.java.orig ]]; then mv src/java/AgentFunction.java.orig src/java/AgentFunction.java; fi

# Utility-based agent
uba: src/scala/UtilityBasedAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn UtilityBasedAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@if [[ -f src/java/AgentFunction.java.orig ]]; then mv src/java/AgentFunction.java.orig src/java/AgentFunction.java; fi

# Hybrid learning agent
hla: src/scala/HybridLearningAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn HybridLearningAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@if [[ -f src/java/AgentFunction.java.orig ]]; then mv src/java/AgentFunction.java.orig src/java/AgentFunction.java; fi

# Check if required tools are installed
check:
	@echo "Checking for required tools..."

	@command -v java >/dev/null 2>&1 || { echo >&2 "java is required but not installed. Aborting :("; exit 1; }
	@java --version

	@command -v scala >/dev/null 2>&1 || { echo >&2 "scala is required but not installed. Aborting :("; exit 1; }
	@scala --version

	@echo "All required tools are installed :)"

# Build the project
build: clean
	@printf "Building the agent"
	@mkdir -p target & pid=$$!; \
		while kill -0 $$pid 2> /dev/null; do printf "."; sleep 0.5; done; \
		wait $$pid
	@scalac -d target src/java/*.java src/scala/*.scala & pid=$$!; \
		while kill -0 $$pid 2> /dev/null; do printf "."; sleep 0.5; done; \
		wait $$pid
	@javac -d target -cp target src/java/*.java & pid=$$!; \
		while kill -0 $$pid 2> /dev/null; do sleep 0.25; printf "."; sleep 0.25; done; \
		wait $$pid
	@echo

# Run the project
run: build
	@scala run -cp target --main-class WorldApplication -- -n 1.00 -a false

tenk: build
	@echo "Running the agent 10,000 times..."
	@scala run -cp target --main-class WorldApplication -- -n 1.00 -a false -t 10000 > /dev/null 
	@tail -n1 wumpus_out.txt
	@echo "Complete results in wumpus_out.txt"

# Clean the project and junk backup files
# If junk backups exist before build, then they are indeed junk
clean:
	@rm -rf target
	@rm -rf src/java/*.orig

# Phony targets
.PHONY: sra mra uba hla check build run tenk clean
