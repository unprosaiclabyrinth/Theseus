# Default target
all:
	@echo "Specify an agent target. Available agent targets: run, sra, mra"

# Simple reflex agent
sra: src/scala/SimpleReflexAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn SimpleReflexAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@mv src/java/AgentFunction.java.orig src/java/AgentFunction.java

# Model-based reflex agent
mra: src/scala/ModelBasedReflexAgent.scala
	@sed -i '.orig' 's|.*// specify agent|\t\treturn ModelBasedReflexAgent.process(tp); // specify agent|' src/java/AgentFunction.java
	@make run
	@mv src/java/AgentFunction.java.orig src/java/AgentFunction.java

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
	@mkdir -p target
	@scalac -d target src/java/*.java src/scala/*.scala
	@javac -d target -cp target src/java/*.java

# Run the project
run: build
	@scala run -cp target --main-class WorldApplication -- -n false -a false

tenk: build
	@echo "Running the agent 10,000 times..."
	@scala run -cp target --main-class WorldApplication -- -n false -a false -t 10000 > /dev/null
	@echo "Results in wumpus_out.txt"

# Clean the project and junk backup files
# If junk backups exist before build, then they are indeed junk
clean:
	@rm -rf target
	@rm -rf src/java/*.orig

# Phony targets
.PHONY: sra mra check build run tenk clean
