library(ProjectTemplate)
load.project()

# # Playing Many Games
set.seed(10114)
win_optimal <- replicate(1000, play_hand(shoe=shoe,
                        strategy=strategy_optimal,
                        verbose=FALSE))

# write(winnings, file='reports/simulation_results.txt', sep='\t')

set.seed(10114)
win_simple <- replicate(1000, play_hand(shoe=shoe,
                                         strategy=strategy_simple,
                                         verbose=FALSE))

#png("graphs/strategies.png", height=1200, width=1200,res = 150)
plot(density(win_optimal, bw=.25), col='green',
     lwd=2, xlab="Winnings", xlim=c(-3,3), ylim=c(0,.9), main="")
lines(density(win_simple,bw=.25), col='purple', lwd=2)
legend("topright", col=c("green", "purple"), 
       legend=c("Optimal", "Simple"), bty="n", lty=1)
#dev.off()

print(mean(win_optimal))
print(mean(win_simple))