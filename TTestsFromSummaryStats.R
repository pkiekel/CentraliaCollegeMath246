install.packages("BSDA")

library(BSDA)

### tsum.test(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL, n.y = NULL, alternative = "two.sided", mu = 0, var.equal = FALSE, conf.level = 0.95)

### For this specific example, copy and paste this syntax into the R Script window:
tsum.test(mean.x=5068, s.x = 4777, n.x = 35, mean.y = 5466, s.y = 8191, n.y = 35, alternative = "less", mu = 0, var.equal = FALSE, conf.level = 0.95)


zsum.test(mean.x, sigma.x = NULL, n.x = NULL, mean.y = NULL,
sigma.y = NULL, n.y = NULL, alternative = "two.sided", mu = 0,
conf.level = 0.95)

### "greater", "less" or "two.sided"
