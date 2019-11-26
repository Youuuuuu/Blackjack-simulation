# Cheate shoe with fixed seed
seed.shoe <- function(m=1) {
  set.seed(1014)
  sample(deck, m, replace=TRUE)
}

# Initial values
myCards1 <- new_hand(bet=1, cards=c(7,7), seed.shoe)
myCards2 <- new_hand(bet=0, cards=c(1,10), seed.shoe)
myCards3 <- new_hand(bet=5, cards=c(7,8,7), seed.shoe)
myCards4 <- new_hand(bet=5, cards=c(7,7,8), seed.shoe)


context("hit")

expect_equal(hit(myCards1)$cards, c(7,7,6))

expect_equal(length(hit(myCards2)$cards), 3)
expect_equal(length(hit(myCards3)$cards), 4)



context("stand")

expect_equal(stand(myCards1)$cards, c(7, 7))
expect_equal(stand(myCards2)$cards, c(1, 10))
expect_equal(stand(myCards3)$cards, c(7, 8, 7))



context("double-down")

expect_equal(dd(myCards1)$cards, c(7, 7, 6))
expect_equal(dd(myCards1)$bet, 2)
expect_equal(dd(myCards2)$cards, c(1, 10, 6))
# expect_equal(dd(myCards1)$bet, NA)
expect_equal(dd(myCards3)$cards, c(7, 8, 7, 6))
expect_equal(dd(myCards3)$bet, 10)



context("split-pair")

# Two identical cards
firstHand <- structure(list(bet=1, shoe=seed.shoe, cards=c(7, 6)), class="hand")
secondHand <- structure(list(bet=1, shoe=seed.shoe, cards=c(7, 6)), class="hand")
test_hands <- list(firstHand, secondHand)

expect_identical(splitPair(myCards1), test_hands)

# Two notidentical cards
expect_error(splitPair(myCards2), regexp = "Cannot split. Cards are not identical")

# More than two cards
expect_error(splitPair(myCards4), regexp = "Cannot split after hit.")
