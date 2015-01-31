library(MASS)
library(ggplot2)
setwd("~/Documents/Scott/ratioest")

# Read in real data
true.dat <- read.csv("results.csv", header = FALSE)
hist(as.numeric(true.dat[2,]))

res <- c()
set.seed(98481)
for(i in 1:5000)
{
  cont <- sample(as.numeric(true.dat[11,]), 50, replace = TRUE)
  treat <- sample(as.numeric(true.dat[13,]), 50, replace = TRUE)
  
  res <- c(res, abs(mean(treat) - mean(cont))/mean(cont))
}
mt <- mean(as.numeric(true.dat[13,]))
mc <- mean(as.numeric(true.dat[11,]))

hist(res)
abline(v = abs(mt-mc)/mc)

# function to do one simulation
sim.one <- function(r, cont.mean = 10)
{
  # simulate time
  cont <- rnegbin(n = 5, mu = cont.mean, theta = 20)
  treat <- rnegbin(n = 5, mu = r*cont.mean, theta = 20)
  
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
  res <-replicate(1000, sim.one(true.ratio[i]))
  out <- rbind(out, cbind(true.ratio[i], t(res)))  
}

out <- data.frame(out)

results <- data.frame(out[,1], rowMeans(out[,2:1001]))
names(results) <- c("true.ratio", "estimate")


results$truth <- dat$true.ratio - 1

plot <- ggplot(results, aes(truth, estimate)) + geom_point() + theme_bw()
plot <- plot + geom_abline(intercept = 0, slope = 1)
plot