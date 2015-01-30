library(MASS)
library(ggplot2)
# function to do one simulation
sim.one <- function(r, cont.mean = 10)
{
  # simulate time
  cont <- rnegbin(n = 50, mu = cont.mean, theta = 20)
  treat <- rnegbin(n = 50, mu = r*cont.mean, theta = 20)
  
  # calculate the estimators
  est <- abs(mean(treat) - mean(cont))/mean(cont)
  est.log <- log(mean(treat)) - log(mean(cont))
  
  return(c(est, est.log))
}


# code to replicate simulations across true ratios of means
true.ratio <- seq(from = 1, to = 20, length.out = 100)

set.seed(64684)
out <- cbind(
  rep(true.ratio[1], 2),
             replicate(1000, sim.one(true.ratio[1])))

for(i in 2:length(true.ratio))
{
  res <-replicate(1000, sim.one(true.ratio[i]))
  out <- rbind(out,
               cbind(rep(true.ratio[i], 2), res))  
}

out <- data.frame(out)
out$method <- rep(c("ratio", "log"), 100)

results <- data.frame(out[,1], out$method, rowMeans(out[,2:1001]))
names(results) <- c("true.ratio", "method", "estimate")

dat <- subset(results, method == "ratio")
dat$truth <- dat$true.ratio - 1

plot <- ggplot(dat, aes(truth, estimate)) + geom_point() + theme_bw()
plot <- plot + geom_abline(intercept = 0, slope = 1)
plot