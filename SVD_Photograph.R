#####Load data set#####
load("C:/Teaching/Math246IntermedStats_B564/Module04_ExploratoryDataAnalysis/face.rda")


faceData <- as.data.frame(faceData)
image(t(faceData)[,nrow(faceData):1])

# This is the stdev and mean of the original mx, to rescale
stdevy <- sqrt(diag(cov(faceData)))
meany <- colMeans(faceData)
meanmx <- matrix(meany,nrow=nrow(faceData),ncol=length(meany),byrow=TRUE)
covy <- sqrt(diag(cov(faceData)))

svd1 <- svd(scale(faceData))
## NOTE that %#% is matrix multiplication
plot(svd1$d^2/sum(svd1$d^2), pch=19, xlab="Singular vector", ylab="Variance explained")

# Here svd1$d[1] is a constant
approx1 <- (svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]) %*% diag(stdevy) + meanmx

# In these examples we need to make the diagonal matrix out of d
approx2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2]) %*% diag(stdevy) + meanmx
approx3 <- svd1$u[,1:3] %*% diag(svd1$d[1:3]) %*% t(svd1$v[,1:3]) %*% diag(stdevy) + meanmx
approx4 <- svd1$u[,1:4] %*% diag(svd1$d[1:4]) %*% t(svd1$v[,1:4]) %*% diag(stdevy) + meanmx
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]) %*% diag(stdevy) + meanmx
approx6 <- svd1$u[,1:6] %*% diag(svd1$d[1:6]) %*% t(svd1$v[,1:6]) %*% diag(stdevy) + meanmx
approx7 <- svd1$u[,1:7] %*% diag(svd1$d[1:7]) %*% t(svd1$v[,1:7]) %*% diag(stdevy) + meanmx
approx8 <- svd1$u[,1:8] %*% diag(svd1$d[1:8]) %*% t(svd1$v[,1:8]) %*% diag(stdevy) + meanmx
approx9 <- svd1$u[,1:9] %*% diag(svd1$d[1:9]) %*% t(svd1$v[,1:9]) %*% diag(stdevy) + meanmx
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]) %*% diag(stdevy) + meanmx
approx11 <- svd1$u[,1:11] %*% diag(svd1$d[1:11]) %*% t(svd1$v[,1:11]) %*% diag(stdevy) + meanmx
approx12 <- svd1$u[,1:12] %*% diag(svd1$d[1:12]) %*% t(svd1$v[,1:12]) %*% diag(stdevy) + meanmx
approx13 <- svd1$u[,1:13] %*% diag(svd1$d[1:13]) %*% t(svd1$v[,1:13]) %*% diag(stdevy) + meanmx
approx14 <- svd1$u[,1:14] %*% diag(svd1$d[1:14]) %*% t(svd1$v[,1:14]) %*% diag(stdevy) + meanmx
approx15 <- svd1$u[,1:15] %*% diag(svd1$d[1:15]) %*% t(svd1$v[,1:15]) %*% diag(stdevy) + meanmx
approx16 <- svd1$u[,1:16] %*% diag(svd1$d[1:16]) %*% t(svd1$v[,1:16]) %*% diag(stdevy) + meanmx
approx17 <- svd1$u[,1:17] %*% diag(svd1$d[1:17]) %*% t(svd1$v[,1:17]) %*% diag(stdevy) + meanmx
approx18 <- svd1$u[,1:18] %*% diag(svd1$d[1:18]) %*% t(svd1$v[,1:18]) %*% diag(stdevy) + meanmx
approx19 <- svd1$u[,1:19] %*% diag(svd1$d[1:19]) %*% t(svd1$v[,1:19]) %*% diag(stdevy) + meanmx
approx20 <- svd1$u[,1:20] %*% diag(svd1$d[1:20]) %*% t(svd1$v[,1:20]) %*% diag(stdevy) + meanmx
approx21 <- svd1$u[,1:21] %*% diag(svd1$d[1:21]) %*% t(svd1$v[,1:21]) %*% diag(stdevy) + meanmx
approx22 <- svd1$u[,1:22] %*% diag(svd1$d[1:22]) %*% t(svd1$v[,1:22]) %*% diag(stdevy) + meanmx
approx23 <- svd1$u[,1:23] %*% diag(svd1$d[1:23]) %*% t(svd1$v[,1:23]) %*% diag(stdevy) + meanmx
approx24 <- svd1$u[,1:24] %*% diag(svd1$d[1:24]) %*% t(svd1$v[,1:24]) %*% diag(stdevy) + meanmx
approx25 <- svd1$u[,1:25] %*% diag(svd1$d[1:25]) %*% t(svd1$v[,1:25]) %*% diag(stdevy) + meanmx
approx26 <- svd1$u[,1:26] %*% diag(svd1$d[1:26]) %*% t(svd1$v[,1:26]) %*% diag(stdevy) + meanmx
approx27 <- svd1$u[,1:27] %*% diag(svd1$d[1:27]) %*% t(svd1$v[,1:27]) %*% diag(stdevy) + meanmx
approx28 <- svd1$u[,1:28] %*% diag(svd1$d[1:28]) %*% t(svd1$v[,1:28]) %*% diag(stdevy) + meanmx
approx29 <- svd1$u[,1:29] %*% diag(svd1$d[1:29]) %*% t(svd1$v[,1:29]) %*% diag(stdevy) + meanmx
approx30 <- svd1$u[,1:30] %*% diag(svd1$d[1:30]) %*% t(svd1$v[,1:30]) %*% diag(stdevy) + meanmx
approx31 <- svd1$u[,1:31] %*% diag(svd1$d[1:31]) %*% t(svd1$v[,1:31]) %*% diag(stdevy) + meanmx
approx32 <- svd1$u[,1:32] %*% diag(svd1$d[1:32]) %*% t(svd1$v[,1:32]) %*% diag(stdevy) + meanmx
## approx <- svd1$u[,1:32] %*% diag(svd1$d[1:32]) %*% t(svd1$v[,1:32])
image(t(approx)[, nrow(approx):1], main = "full reproduction")


# par(mfrow = c(2,4))
#par(mfrow = c(1,1))
image(t(approx1)[, nrow(approx1):1], main = "1 component")
image(t(approx2)[, nrow(approx2):1], main = "2 components")
image(t(approx3)[, nrow(approx3):1], main = "3 components")
image(t(approx4)[, nrow(approx4):1], main = "4 components")
image(t(approx5)[, nrow(approx5):1], main = "5 components")
image(t(approx6)[, nrow(approx6):1], main = "6 components")
image(t(approx7)[, nrow(approx7):1], main = "7 components")
image(t(approx8)[, nrow(approx8):1], main = "8 components")
image(t(approx9)[, nrow(approx9):1], main = "9 components")
image(t(approx10)[, nrow(approx10):1], main = "10 components")
image(t(approx11)[, nrow(approx11):1], main = "11 components")
image(t(approx12)[, nrow(approx12):1], main = "12 components")
image(t(approx13)[, nrow(approx13):1], main = "13 components")
image(t(approx14)[, nrow(approx14):1], main = "14 components")
image(t(approx15)[, nrow(approx15):1], main = "15 components")
image(t(approx16)[, nrow(approx16):1], main = "16 components")
image(t(approx17)[, nrow(approx17):1], main = "17 components")
image(t(approx18)[, nrow(approx18):1], main = "18 components")
image(t(approx19)[, nrow(approx19):1], main = "19 components")
image(t(approx20)[, nrow(approx20):1], main = "20 components")
image(t(approx21)[, nrow(approx21):1], main = "21 components")
image(t(approx22)[, nrow(approx22):1], main = "22 components")
image(t(approx23)[, nrow(approx23):1], main = "23 components")
image(t(approx24)[, nrow(approx24):1], main = "24 components")
image(t(approx25)[, nrow(approx25):1], main = "25 components")
image(t(approx26)[, nrow(approx26):1], main = "26 components")
image(t(approx27)[, nrow(approx27):1], main = "27 components")
image(t(approx28)[, nrow(approx28):1], main = "28 components")
image(t(approx29)[, nrow(approx29):1], main = "29 components")
image(t(approx30)[, nrow(approx30):1], main = "30 components")
image(t(approx31)[, nrow(approx31):1], main = "31 components")
image(t(approx32)[, nrow(approx32):1], main = "32 components")
image(t(faceData)[, nrow(faceData):1], main = "Original data") ## Original data

