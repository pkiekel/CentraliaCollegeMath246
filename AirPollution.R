# dir.create("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/AirPollution")
setwd("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/AirPollution")

dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip")

list.files("specdata")
d001 <- read.csv("specdata/001.csv")
head(d001)
tail(d001)
dim(d001)
d001.clean <- d001[,]

pollutantmean <- function(directory, pollutant, id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either 'sulfate' or 'nitrate'.
  
  ## 'id' is an integer vector indicating the monitor id numbers
  ## to be used.
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the results!
  
  ## go through each file, and append it to a working file
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  # files_list
  dat <- data.frame()                                #creates an empty data frame
  for (i in id) {
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  
  outty <- mean(dat[, pollutant], na.rm=TRUE)      #computes the mean of the pollutant 
  # outty
  #while stripping out the NAs

  return(outty)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor id numbers
  ## to be used.
  
  ## Returns a dataframe of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor id number, and 'nobs' is the
  ## number of complete complete cases
  
  ## We need to remove a row if either sulfate OR nitrate are missing
  
  ## go through each file, find n, and append it to a working file
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  # files_list
  dat <- data.frame()                                #creates an empty data frame
  for (i in id) {
    #loops through the files, find how many non-missing are in each, append that
    current <- read.csv(files_list[i])
    notna <- nrow(na.omit(current))
    # notna
    addrow <- c(i, notna)
    dat <- rbind(dat, addrow)
  }
  colnames(dat) <- c("id", "nobs")

  return(dat)
}


complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)
complete("specdata", 3)


corr <- function(directory, threshold = 1){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV file
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 1
  
  ## Return a numeric vector of correlations
  ## NOTE: do not round the result!
  
  # Create a list of file names
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  corvec <- vector(mode = 'numeric', length = 0)          #creates an empty vector
  for (i in 1:332) {
    #loops through the files, find how many non-missing are in each
    # make a temporary data file
    current <- read.csv(files_list[i])

    # let 'cp' be the number of complete observations (no missing values)
    cp <- complete(directory,i)[,2]

    # if cp >= threshold, compute cor and append to corvec
    # if cp < threshold, skip it and move to i+1
    if (cp >= threshold) {
      # create a dataset that removes missing values
      nomiss <- na.omit(current)
      # newcor <- cor(nomiss$sulfate, nomiss$nitrate)
      # corvec <- rbind(corvec, newcor)
      corvec <- c(corvec, cor(nomiss$sulfate, nomiss$nitrate))
    }
    # colnames(corvec) <- c("correlation")
  }

  return(corvec)  
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

