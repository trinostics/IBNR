#CoinDice.r
#set.seed(12345)
coin.space <- c(0L,1L)
theta.coin <- 0.5 # this is a fair coin
N <- 9 # we want to flip a coin 20 times

flips <- sample(coin.space, 
                size = N, 
                replace = TRUE, 
                prob = c(theta.coin, 1 - theta.coin))
flips
x <- flips + 1L
y <- numeric(length(x))

die.space <- 1:6
theta.die <- rep(1/6, 6)

xis1 <- x == 1L
N1 <- sum(xis1)
N2 <- N - N1

y[xis1] <- sample(die.space, size = N1,
                  replace = TRUE,
                  prob = theta.die)
y[!xis1] <- sample(die.space, size = N2, 
                   replace = TRUE, prob = theta.die) +
  sample(die.space, size = N2, 
         replace = TRUE, prob = theta.die)

round(sum(y) / sum(x), 3)