FakeMess <- read.csv("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Math246IntermedStats_B564/Data/FakeMessyData.csv")
FakeMess$OutLog <- log(FakeMess$Outcome)

fit <- lm(FakeMess$OutLog ~ FakeMess$Input)
summary(fit)
plot(fit)

fit.av <- lm(FakeMess$OutLog ~ FakeMess$Factor)
summary(fit.av)
plot(fit.av)


fit.av2 <- aov(FakeMess$OutLog ~ FakeMess$Factor)
summary(fit.av2)
plot(fit.av2)

boxplot(FakeMess$OutLog~FakeMess$Factor)
