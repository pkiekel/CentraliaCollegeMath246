ClassWork <- readxl::read_excel("C:/Users/Anndi/Documents/Stats/Data/DataFromStatSurvey_B782_descripstats.xlsx")

variable.names(ClassWork)
str(ClassWork)
head(ClassWork)

Spdcount.mean <- mean(ClassWork$SpudCount, na.rm=TRUE)
Spdcount.mean

Spdcount.sd <- sd(ClassWork$SpudCount, na.rm=TRUE)
Spdcount.sd

Spdcount.summary <- summary(ClassWork$SpudCount, na.rm = TRUE)
Spdcount.summary

Exercise.mean <- mean(ClassWork$Exercise, na.rm=TRUE)
Exercise.mean

Exercise.sd <- sd(ClassWork$Exercise, na.rm=TRUE)
Exercise.sd

Exercise.summary <- summary(ClassWork$Exercise, na.rm = TRUE)
Exercise.summary

Height.mean <- mean(ClassWork$Height, na.rm=TRUE)
Height.mean

Height.sd <- sd(ClassWork$Height, na.rm=TRUE)
Height.sd

Height.summary <- summary(ClassWork$Height, na.rm = TRUE)
Height.summary

Navel.mean <- mean(ClassWork$Navel, na.rm=TRUE)
Navel.mean

Navel.sd <- sd(ClassWork$Navel, na.rm=TRUE)
Navel.sd

Navel.summary <- summary(ClassWork$Navel, na.rm = TRUE)
Navel.summary

Nose.mean <- mean(ClassWork$Nose, na.rm=TRUE)
Nose.mean

Nose.sd <- sd(ClassWork$Nose, na.rm=TRUE)
Nose.sd

Nose.summary <- summary(ClassWork$Nose, na.rm = TRUE)
Nose.summary

Hand.mean <- mean(ClassWork$Hand, na.rm=TRUE)
Hand.mean

Hand.sd <- sd(ClassWork$Hand, na.rm=TRUE)
Hand.sd

Hand.summary <- summary(ClassWork$Hand, na.rm = TRUE)
Hand.summary

cat(summary(Classy$SpudCount, na.rm = TRUE)[4], sd(Classy$SpudCount, na.rm=TRUE),summary(Classy$SpudCount, na.rm = TRUE)[1:3],summary(Classy$SpudCount, na.rm = TRUE)[5:6])

dat.data <- Classy$SpudCount
dat.xbar = summary(dat.data, na.rm = TRUE)[4]
dat.sd   = sd(dat.data, na.rm=TRUE)
dat.q0 = summary(dat.data, na.rm = TRUE)[1]
dat.q1 = summary(dat.data, na.rm = TRUE)[2]
dat.q2 = summary(dat.data, na.rm = TRUE)[3]
dat.q3 = summary(dat.data, na.rm = TRUE)[5]
dat.q4 = summary(dat.data, na.rm = TRUE)[6]
dat.iqr = dat.q3 - dat.q1
dat.lowerfence = dat.q1 - 1.5*dat.iqr
dat.upperfence = dat.q3 + 1.5*dat.iqr
infence <- dat.data[which(dat.data<=dat.upperfence & dat.data>=dat.lowerfence)]
dat.rightbox <- max(infence)
dat.leftbox <- min(infence)
dat.n <- sum(!is.na(dat.data))
dat.nmiss <- sum(is.na(dat.data))

dat.summary.arith <- array(c(dat.xbar,dat.sd,dat.n,dat.nmiss))
dimnames(dat.summary.arith) <- list(c("xbar","sd","n","nmiss"))

dat.summary.rank <- array(c(dat.q0,dat.q1,dat.q2,dat.q3,dat.q4,dat.iqr,dat.lowerfence,dat.upperfence,dat.leftbox,dat.rightbox))
dimnames(dat.summary.rank) <- list(c("min","q1","med","q3","max","iqr","lf","uf","leftbox","rightbox"))

dat.summary.arith
dat.summary.rank

################################
install.packages("moments")

dat.summary <- function(dat.data){
  library(moments)
  dat.xbar = summary(dat.data, na.rm = TRUE)[4]
  dat.sd   = sd(dat.data, na.rm=TRUE)
  dat.skew = skewness(dat.data, na.rm=TRUE)
  dat.kurt = kurtosis(dat.data, na.rm=TRUE)
  dat.q0 = summary(dat.data, na.rm = TRUE)[1]
  dat.q1 = summary(dat.data, na.rm = TRUE)[2]
  dat.q2 = summary(dat.data, na.rm = TRUE)[3]
  dat.q3 = summary(dat.data, na.rm = TRUE)[5]
  dat.q4 = summary(dat.data, na.rm = TRUE)[6]
  dat.range = dat.q4 - dat.q0
  dat.iqr = dat.q3 - dat.q1
  dat.lowerfence = dat.q1 - 1.5*dat.iqr
  dat.upperfence = dat.q3 + 1.5*dat.iqr
  infence <- dat.data[which(dat.data<=dat.upperfence & dat.data>=dat.lowerfence)]
  dat.rightwhisk <- max(infence)
  dat.leftwhisk  <- min(infence)
  dat.n <- sum(!is.na(dat.data))
  dat.nmiss <- sum(is.na(dat.data))
  
  dat.summary.arith <- array(c(dat.xbar,dat.sd,dat.skew,dat.kurt,dat.n,dat.nmiss))
  dimnames(dat.summary.arith) <- list(c("xbar","sd","skewness","kurtosis","n","nmiss"))
  
  dat.summary.rank1 <- array(c(dat.q0,dat.q1,dat.q2,dat.q3,dat.q4))
  dimnames(dat.summary.rank1) <- list(c("min","q1","med","q3","max"))
  
  dat.summary.rank2 <- array(c(dat.range,dat.iqr,dat.lowerfence,dat.upperfence,dat.leftwhisk,dat.rightwhisk))
  dimnames(dat.summary.rank2) <- list(c("range","iqr","lf","uf","leftwhisk","rightwhisk"))
  
  output <- list(dat.summary.arith,dat.summary.rank1,dat.summary.rank2)
  my_names <- c("Arithmetic","Five Number Summary","Ranking Spread")
  names(output) <- my_names
  boxplot(dat.data)
  hist(dat.data)
  
  return(output)
}

dat.summary(ClassWork$Height)
dat.summary(ClassWork$SpudCount)
dat.summary(ClassWork$Navel)
dat.summary(ClassWork$Hand)
dat.summary(ClassWork$Nose)
dat.summary(ClassWork$Exercise)
