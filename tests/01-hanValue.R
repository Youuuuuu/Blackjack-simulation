# Hand Value testing

context("TEST CARDS")

test_cards <- list( c(10,1), c(10,5,6), c(10,1,1),
                    c(7,6,1,5), c(3,6,1,1),
                    c(2,3,4,10), c(5,1,9,1,1),
                    c(5,10,7), c(10,9,1,1,1))

test_cards_val <- c(21.5, 21, 12, 19, 21, 19, 17, 0, 0)

expect_identical(test_cards_val, sapply(test_cards, handValue))
