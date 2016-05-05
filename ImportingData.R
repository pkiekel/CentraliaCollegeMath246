
#####Read Data From Text File#####
Dataset <-   read.table("C:/Teaching/Math146Stats_B451/DataFromStatSurvey_B451_cleaned.csv",
   header=TRUE, sep=",", na.strings=c("", "NA"), dec=".", fill=TRUE, 
  quote="\"", strip.white=TRUE)
Dataset <- 
  read.table("C:/Teaching/Math146Stats_B451/DataFromStatSurvey_B451_cleaned.csv",
   header=TRUE, sep=",", na.strings=c("", "NA"), dec=".", fill=TRUE, 
  quote="\"", strip.white=TRUE)


#####Import from Excel, Access or dBase data set#####
DatasetXLS <- sqlQuery(channel = 1, select * from [Sheet1$])



#####Import from Excel, Access or dBase data set#####
DatasetXLSX <- sqlQuery(channel = 2, select * from [Sheet1$])
