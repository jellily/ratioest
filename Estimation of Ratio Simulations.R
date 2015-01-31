library(MASS)
library(ggplot2)
setwd("~/Documents/ratioest")

# Read in real data
true.dat <- read.csv("results.csv", header = FALSE, row.names = 1)

# Bootstrap for real data
res <- c()
set.seed(98481)
for(i in 1:5000)
{
  cont <- sample(as.numeric(true.dat[24,]), 50, replace = TRUE)
  treat <- sample(as.numeric(true.dat[26,]), 50, replace = TRUE)
  
  res <- c(res, abs(mean(treat) - mean(cont))/mean(cont))
}
mt <- mean(as.numeric(true.dat[26,]))
mc <- mean(as.numeric(true.dat[24,]))

hist(res)
abline(v = abs(mt-mc)/mc)


# Simulations ---
# function to do one simulation
sim.one <- function(r, cont.mean = 94)
{
  # simulate time
  #cont <- rnegbin(n = 5, mu = cont.mean, theta = 20)
  #treat <- rnegbin(n = 5, mu = r*cont.mean, theta = 20)
  cont <- rnorm(n = 50, mean = cont.mean, sd = 4)
  treat <- rnorm(n = 50, mean = r*cont.mean, sd = 4)
  
  # calculate the estimator
  est <- abs(mean(treat) - mean(cont))/mean(cont)
  return(est)
}


# code to replicate simulations across true ratios of means
true.ratio <- seq(from = 1, to = 20, length.out = 100)

set.seed(64684)
out <- cbind(true.ratio[1], t(replicate(1000, sim.one(true.ratio[1]))))

for(i in 2:length(true.ratio))
{
  res <- replicate(1000, sim.one(true.ratio[i]))
  out <- rbind(out, cbind(true.ratio[i], t(res)))  
}

out <- data.frame(out)
out[,1] <- out[,1] - 1
bias.out <- out
for(i in 1:nrow(out))
{
  bias.out[i, 2:ncol(out)] <- (out[i,2:ncol(out)] - out[i,1])^2
  out[i, 2:ncol(out)] <- (out[i,2:ncol(out)] - out[i,1])
}


results <- data.frame(out[,1], rowMeans(out[,2:1001]), apply(out[,2:1001], 2, min), apply(out[,2:1001], 2, max))
names(results) <- c("truth", "estimate", "min", "max")
plot <- ggplot(results, aes(truth, estimate)) + geom_point() + theme_bw()
plot <- plot + geom_errorbar(aes(x = truth, ymin = min, ymax = max))
plot


results <- data.frame(bias.out[,1], rowMeans(bias.out[,2:1001]), apply(bias.out[,2:1001], 2, min), apply(out[,2:1001], 2, max))
names(results) <- c("truth", "estimate", "min", "max")
plot <- ggplot(results, aes(truth, estimate)) + geom_point() + theme_bw()
plot <- plot + geom_errorbar(aes(x = truth, ymin = min, ymax = max))
plot