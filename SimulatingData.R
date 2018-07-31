df <- data.frame(dive=factor(sample(c("dive1","dive2"),10,replace=TRUE)),speed=runif(10))

dfnormal <- data.frame(dive=factor(sample(c("dive1","dive2"),10,replace=TRUE)),speed=rnorm(10000,5,1))
hist(dfnormal$speed)
summary(lm(dfnormal$speed ~ dfnormal$dive))

dfnormal$speed2 <- ""
dfnormal$speed2[dfnormal$dive == "dive1"] <- rnorm(10000,9,100)
dfnormal$speed2[dfnormal$dive == "dive2"] <- rnorm(10000,10,100)
summary(lm(dfnormal$speed2 ~ dfnormal$dive))


FakeMess <- read.csv("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Math246IntermedStats_B564/Data/FakeMessyData.csv")

scatterplot(FakeMess$Outcome ~ FakeMess$Input)

FakeMess$OutLog <- log(FakeMess$Outcome)

scatterplot(FakeMess$OutLog ~ FakeMess$Input)


fit <- lm(FakeMess$OutLog ~ FakeMess$Input)
summary(fit)
plot(fit)

LogInt <- fit$coefficients[1]
LogSlope <- fit$coefficients[2]
Stdev <- sd(FakeMess$OutLog, na.rm = TRUE)
## yhat = b0 + b1 * x + random normal (mean = 0, stdev = sigma)


newsim <- data.frame(Input = runif(1000,1,7))
newsim$Input <- round(newsim$Input,0)
newsim$Input[newsim$Input<1] <- 1
newsim$Input[newsim$Input>7] <- 7
newsim$OutLog <- LogInt + LogSlope * newsim$Input + rnorm(1000,0,Stdev)
newsim$Output <- round(exp(1)^newsim$OutLog,0)

newfit <- lm(newsim$OutLog ~ newsim$Input)
summary(newfit)
# plot(fit)

expfit <- lm(newsim$Output ~ newsim$Input)
summary(expfit)
scatterplot(newsim$Outcome ~ newsim$Input)
scatterplot(FakeMess$Outcome ~ FakeMess$Input)



fit.av <- lm(FakeMess$OutLog ~ FakeMess$Factor)
summary(fit.av)
plot(fit.av)


fit.av2 <- aov(FakeMess$OutLog ~ FakeMess$Factor)
summary(fit.av2)
plot(fit.av2)

boxplot(FakeMess$OutLog~FakeMess$Factor)
