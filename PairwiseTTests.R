hsb2<-read.table("http://www.ats.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)
attach(hsb2)

tapply(write, ses, mean)
tapply(write, ses, sd)

a1 <- aov(write ~ ses)
summary(a1)

pairwise.t.test(write, ses, p.adj = "none")
pairwise.t.test(write, ses, p.adj = "bonf")
TukeyHSD(a1)

a2 <- aov(write ~ ses + female)
summary(a2)

TukeyHSD(a2, "ses")
