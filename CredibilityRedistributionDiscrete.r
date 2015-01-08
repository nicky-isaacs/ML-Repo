#x11()  # opens a graphics window on linux machines
par(mfrow=c(10,5), oma=c(1,1,0,0), mar=c(1,1,1,0))  # graphics settings to fit 50 graphs
prior = rep(1/101, 101)  # set a uniform prior

label = c(0, cumsum(rep(1/100, 100)))  # x-axis labels, 1% increments

plot(label, prior)  # plot initial prior

randFlip = runif(49)  # get 49 random values between 0 and 1
tailsProb = .35  # set coin bias for tails

for (isHeads in (randFlip > tailsProb)) {  # loop through random values as boolean for heads
  if (isHeads) {  # if is heads likelihoods will be value of the x-axis, otherwise (1 - x-axis)
    likelihood = label
  } else {
    likelihood = 1 - label
  }
  
  posterior = likelihood * prior / sum(likelihood * prior)  # solve bayes
  plot(label, posterior)
  prior = posterior  # prior for next flip is this posterior
}

print(mean(randFlip > tailsProb))  # heads bias exhibited in the 49 flips