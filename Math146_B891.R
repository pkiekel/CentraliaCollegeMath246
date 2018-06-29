Classy <- read.csv("D:/Users/preston.kiekel/Documents/DataFromStatSurvey_B782_cleaned.csv")

variable.names(Classy)
str(Classy)
head(Classy)

mean(Classy$SpudCount, na.rm=TRUE)
sd(Classy$SpudCount, na.rm=TRUE)
summary(Classy$SpudCount, na.rm = TRUE)

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
  histogram(dat.data)
  
  return(output)
}

dat.summary(Classy$Height)
histogram(Classy$Height)

MyFunction <- function(dat.data){
  histogram(dat.data)
}

MyFunction(Classy$Height)
