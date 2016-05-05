## chisq.test(c(input values separated by commas), p=c(probabilities under H0, separated by commas))

### So if you had a data set with 6 A's, 3 B's, 4 C's, and 8 D's, and you wanted to test if they are all
### equally distributed in the population, you would enter the line:

freqs <- c(6,3,4,8)
probs <- c(0.25, 0.25, 0.25, 0.25)

expected <- sum(freqs) * probs
expected
chisq.test(freqs, p=probs)



