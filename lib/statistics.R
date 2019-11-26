payoff <- function(n, strategy, shoe) {
    results <- replicate(n, play_hand(shoe=shoe,
                                      strategy=strategy))
    c(avgGain=mean(results), sdGain=sd(results),
      medGain=median(results))
}