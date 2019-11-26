# TEST WINNINGS FUNCTION


context("Winnings")

test_vals <- c(0, 16,19,20,21,21.5)

testWinnings <- matrix(
  c(-1,1,1,1,1,1.5,
    -1,0,1,1,1,1.5,
    -1,-1,0,1,1,1.5,
    -1,-1,-1,0,1,1.5,
    -1,-1,-1,-1,0,1.5,
    -1,-1,-1,-1,-1,0),
  nrow=length(test_vals), byrow=TRUE)

dimnames(testWinnings) <- list(dealer=test_vals,
                               player=test_vals)
check = testWinnings
check[] = NA

for (i in seq_along(test_vals)) {
  for (j in seq_along(test_vals)) {
    check[i, j] <- winnings(test_vals[i], test_vals[j])
  }
}

expect_identical(check, testWinnings)
