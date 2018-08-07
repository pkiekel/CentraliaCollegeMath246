Emissions <- readXL("/home/pkiekel/Desktop/Teaching/Math245Math246_B891/Data/EmissionsData.xlsx", 
                    rownames=FALSE, header=TRUE, na="", sheet="ALL IN 1", stringsAsFactors=TRUE)

integrate <- Emissions$AllSec_Integrate

thing <- t.test(Emissions$AllSec_Integrate, alternative='two.sided', mu=0.0, conf.level=0.95)
thing

thing2 <- t.test(Emissions$ALL_SECTORS ~ Emissions$Before2008)
thing2

