library(ProjectTemplate)
load.project()


win_simple50 = replicate(1000,
                         payoff(50, strategy_simple, shoe))
win_optimal50 = replicate(1000,
                          payoff(50, strategy_optimal, shoe))

df = data.frame(
    value = c(win_simple50[ "avgGain", ],
              win_optimal50[ "avgGain", ]),
    strategy = rep(c("simple", "optimal"), each = 1000))


qplot(value, data = df, geom = "freqpoly", colour = strategy,
      binwidth = 0.05)

ggsave(filename="payoffs.png", device='png', path="graphs")