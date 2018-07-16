# dir.create("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/AirPollution")
setwd("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/AirPollution")

dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip")

list.files("specdata")
andy <- read.csv("diet_data/Andy.csv")
head(andy)
length(andy$Day)
dim(andy)
str(andy)
summary(andy)
names(andy)

andy[1, "Weight"]
andy[30, "Weight"]
andy[which(andy$Day == 30), "Weight"]
andy[which(andy[,"Day"] == 30), "Weight"]
subset(andy$Weight, andy$Day==30)

andy_start <- andy[1, "Weight"]
andy_end <- andy[30, "Weight"]
andy_loss <- andy_start - andy_end
andy_loss

files <- list.files("diet_data")
files

files[1]
files[2]
files[3:5]

head(read.csv(files[3]))
files_full <- list.files("diet_data", full.names=TRUE)
files_full
head(read.csv(files_full[3]))

andy_david <- rbind(andy, read.csv(files_full[2]))
head(andy_david)
tail(andy_david)
day_25 <- andy_david[which(andy_david$Day == 25), ]
day_25

for (i in 1:5) {print(i)}
for (i in 1:5) {
  dat <- rbind(dat, read.csv(files_full[i]))
}
dat <- data.frame()

for (i in 1:5) {
  dat <- rbind(dat, read.csv(files_full[i]))
}
str(dat)

for (i in 1:5) {
  dat2 <- data.frame()
  dat2 <- rbind(dat2, read.csv(files_full[i]))
}
str(dat2)
head(dat2)

median(dat$Weight)
median(dat$Weight, na.rm=TRUE)
dat_30 <- dat[which(dat[, "Day"] == 30),]
dat_30
median(dat_30$Weight)


weightmedian <- function(directory, day)  {
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
  for (i in 1:5) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[, "Day"] == day),]  #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm=TRUE)      #identifies the median weight 
  #while stripping out the NAs
}


weightmedian(directory = "diet_data", day = 20)
weightmedian("diet_data", 4)
weightmedian("diet_data", 17)



# Creation of output object

summary(files_full)
tmp <- vector(mode = "list", length = length(files_full))
summary(tmp)

for (i in seq_along(files_full)) {
  tmp[[i]] <- read.csv(files_full[[i]])
}
str(tmp)

str(lapply(files_full, read.csv))
str(tmp[[1]])
head(tmp[[1]][,"Day"])

output <- do.call(rbind, tmp)
str(output)
