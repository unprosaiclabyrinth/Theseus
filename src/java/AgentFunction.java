/** Select an agent at runtime without rewriting or recompiling source files. */
class AgentFunction {
    private final AgentFunctionImpl implementation;
    private final String name;

    AgentFunction() { this("uba"); }

    AgentFunction(String name) {
        this.name = name;
        implementation = switch (name) {
            case "sra" -> SimpleReflexAgent$.MODULE$;
            case "mra" -> ModelBasedReflexAgent$.MODULE$;
            case "uba" -> UtilityBasedAgent$.MODULE$;
            case "rla" -> ReactiveLearningAgent$.MODULE$;
            case "lba" -> LLMBasedAgent$.MODULE$;
            default -> throw new IllegalArgumentException("Unknown agent: " + name);
        };
    }

    // Allows custom agents and test doubles without editing the simulator.
    AgentFunction(String name, AgentFunctionImpl implementation) {
        this.name = name;
        this.implementation = implementation;
    }

    public int process(TransferPercept percept) { return implementation.process(percept); }
    public void reset() { implementation.reset(); }
    public String getAgentName() { return "Theseus (" + name + ")"; }
}
