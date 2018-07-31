install.packages("ClustOfVar")
install.packages("XLConnectJars")
library(ClustOfVar, pos=16)

library(XLConnectJars, pos=14)
library(XLConnect, pos=14)
.Workbook <- 
  loadWorkbook("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Math246IntermedStats_B564/Data/Data_MashedPotatoes.xls")
MashedPotatoes <- readWorksheet(.Workbook, "Sheet1", header=TRUE, 
                                rownames=NULL)
remove(.Workbook)

MashedPotatoes <- read.csv("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Math246IntermedStats_B564/Data/Data_MashedPotatoes.csv")
MashedPotatoes[, c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
                   19, 20, 39)] <- lapply(MashedPotatoes[, c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 
                                                             13, 14, 15, 16, 17, 18, 19, 20, 39), drop=FALSE], as.factor)

MashedPotatoes


DavidClusters <- read.csv("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Math246IntermedStats_B564/Module04_ExploratoryDataAnalysis/Data_MashedPotatoes_DavidClusters.csv")
# First we need to get rid of the missing values and/or bad values in the data set.
DavidClusters$DavidClus[DavidClusters$DavidClus != "Yes" & DavidClusters$DavidClus != "No"] <- ""
DavidClusters$DavidClus <- factor(DavidClusters$DavidClus)
table(DavidClusters$DavidClus)
.Table <- table(DavidClusters$ServantLeaderDichot, DavidClusters$DavidClus)
.Table
chisq.test(.Table)
chisq.test(.Table)$expected

DavidClusters$BigDave <- ""
DavidClusters$BigDave[DavidClusters$BigCluster >= 2] <- "Yes"
DavidClusters$BigDave[DavidClusters$BigCluster < 2 ] <- "No"

.Table <- table(DavidClusters$ServantLeaderDichot, DavidClusters$BigDave)
.Table
chisq.test(.Table)$expected
chisq.test(.Table)
chisq.test(.Table)$residuals^2





dat.graph <- function(varcat, varquant){
  .Var <- varcat
  .Var2 <- factor(.Var[.Var != ""])
  print(levels(.Var2))
  .Var3 <- factor(.Var2, levels = c("Very unimportant","Somewhat unimportant","I feel neutral","Somewhat important","Very important"))
  # .Var2 <- na.omit(.Var[])
  .Table <- table(.Var3)
  .Table
  barplot(.Table)
  boxplot(varquant)
  hist(varquant)
}

################################
# install.packages("moments")

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
  
  return(output)
}

dat.summary(MashedPotatoes$FreedomForDifficultSituations)
dat.graph(MashedPotatoes$Gives.others.the.freedom.to.handle.difficult.situations.in.the.way.they.feel.is.best,
     MashedPotatoes$FreedomForDifficultSituations)


dat.graph(MashedPotatoes$Strong.independent.leader,
     MashedPotatoes$StrongIndLeader)






######################################
## Cluster Analysis ##
######################################
library(ClustOfVar)

cols <- c(21:30,32,34)
leadervars <- c("StrongIndLeader", "TwoWayComm", "FreedomForDifficultSituations", "RecognizeOthersFeelingDown", "CaresAboutOthersSuccessMoreThanOwn", "CaresAboutOthersWellBeing", "InterestedInOthersReachingGoals", "UnderstandsOrgAndItsGoals", "ProvidesWorkExperience", "SacrificesOwnInt", "WantsToKnowAboutCareerGoals", "DecisionsWithoutConsultingOthers")

leader <- hclustvar(MashedPotatoes[,leadervars])
plot(leader)
summary(leader)

#choice of the number of clusters
stability(leader,B=40)
part <- cutreevar(leader,4)
print(part)
summary(part)


###############################################################
##  Cluster Analysis with ALL vars (reverse the bad ones)    ##
##  to test the hypothesized clustering                      ##
###############################################################
cols <- c(22:33,35:36)
leadervars <- c('TwoWayComm','FreedomForDifficultSituations','RecognizeOthersFeelingDown','CaresAboutOthersSuccessMoreThanOwn','CaresAboutOthersWellBeing','InterestedInOthersReachingGoals','UnderstandsOrgAndItsGoals','ProvidesWorkExperience','SacrificesOwnInt','NotCompromiseEthics','WantsToKnowAboutCareerGoals','HonestyOverProfits','StrongIndLeader_R','DecisionsWithoutConsultingOthers_R')

leader <- hclustvar(MashedPotatoes[,leadervars])
plot(leader)
summary(leader)

#choice of the number of clusters
stability(leader,B=40)
part <- cutreevar(leader,4)
print(part)
summary(part)


#### Now make averages to create the clusters we got. ###
MashedPotatoes$BigCluster <- NA
for (i in 1:nrow(MashedPotatoes)) {
  MashedPotatoes$BigCluster[i] <- mean(MashedPotatoes$CaresAboutOthersSuccessMoreThanOwn[i],MashedPotatoes$NotCompromiseEthics[i],MashedPotatoes$ProvidesWorkExperience[i],MashedPotatoes$CaresAboutOthersWellBeing[i],MashedPotatoes$InterestedInOthersReachingGoals[i],MashedPotatoes$HonestyOverProfits[i],MashedPotatoes$TwoWayComm[i],MashedPotatoes$UnderstandsOrgAndItsGoals[i], na.rm = TRUE)
}
MashedPotatoes$BigCluster

dat.summary(MashedPotatoes$BigCluster)
