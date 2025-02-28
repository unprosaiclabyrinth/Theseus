using Base.Threads

println("Running wumpus simulation 10000 times and compiling scores...")

# Run trials and compile scores in parallel
Threads.@sync @threads for i in 1:5000 # 5000 iterations
    num_trials = 2 # 2 trials per iteration
    outfile = "jltlv_wumpus_eval$i.txt"
    infile = "jltlv_wumpus_out$i.txt"
    run(`./wumpus_eval.sh -n $num_trials -i $infile -o $outfile`)
    #println("$(threadid()) -> $i")
end

# Compile all scores
run(`zsh -c "cat jltlv_wumpus_eval*.txt > wumpus_eval.txt"`)
println("Scores written to wumpus_eval.txt")

# Clean up
run(`zsh -c "rm -f jltlv_wumpus_{out,eval}*.txt"`)
