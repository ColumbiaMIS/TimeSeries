
#####TREES#####

TrainData <- read.csv("https://s3.amazonaws.com/cumis/train.csv")
TrainData=na.exclude(TrainData)

TestData <- read.csv("https://s3.amazonaws.com/cumis/test.csv")
TestData=na.exclude(TestData)

#Load rpart package
library(rpart)

#Build the decision tree
myTree <- rpart(Survived ~ Pclass + Sex + Age, data = TrainData, method = "class")

#Visualize the decision tree using plot() and text()
plot(myTree)
text(myTree)

#Installing necessary packages to run rattle
install.packages("RGtk2")
install.packages("rattle")
install.packages("rpart.plot")

#Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(myTree)


myTreeTwo <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                   data = TrainData, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(myTreeTwo)


install.packages('randomForest')
library(randomForest)
fit=randomForest(TrainData[,c(3,5,6,7,8,10)],y=as.factor(TrainData$Survived),ntree=2000)
prediction=predict(fit, TestData[,c(2,4,5,6,7,9)],type = "response")

####K-MEANS####

data(iris)
summary(iris)
k<-kmeans(iris[,1:4],centers=3)
plot(iris[,1], iris[,2], col=as.factor(k$cluster))
plot(iris[,1], iris[,2], col=as.factor(iris[,5]))

plot(iris[,1:4], col=as.factor(iris[,5]))
plot(iris[,1:4], col=as.factor(k$cluster))


install.packages("class")
library(class)
prediction=knn(TrainData[,c(3,6,7,8,10)], TestData[,c(2,5,6,7,9)], TrainData$Survived, k = 5)


install.packages("jpeg")
install.packages("ggplot2")
library(jpeg)
library(ggplot2)
img=readJPEG("~/Downloads/colorful_bird.jpg")
imgDm=dim(img)
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3]))

plotTheme <- function() {theme(panel.background = element_rect(size = 3,colour = "black",fill = "white"),
                                 axis.ticks = element_line(size = 2),panel.grid.major = element_line(colour = "gray80",linetype = "dotted"),
                                 panel.grid.minor = element_line(colour = "gray90",linetype = "dashed"),axis.title.x = element_text(size = rel(1.2),face = "bold"), axis.title.y = element_text(size = rel(1.2),face = "bold"),plot.title = element_text(size = 20,  face = "bold",vjust = 1.5))}

ggplot(data = imgRGB, aes(x = x, y = y)) +geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +labs(title = "Original Image: Colorful Bird") + xlab("x") +ylab("y") +plotTheme()

kClusters <- 3
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) +geom_point(colour = kColours) + labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +xlab("x") +ylab("y") + plotTheme()



#####TIME SERIES#####
f <- decompose(AirPassengers)
plot(f)

install.packages("forecast")
library(forecast)
fit <- auto.arima(AirPassengers)
fore <- predict(fit, n.ahead=24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2),pt.cex=3)




install.packages("dtw")
library(dtw)
mm <- matrix(runif(12),ncol=3)
dm <- dist(mm,method="DTW")

#for asymetric step-patterns
dm <- dist(mm,mm,method="DTW",step=asymmetric)  #non-symmetric matrix
dm <- (dm+t(dm))/2 # symmetrize


install.packages("wavelets")
library(wavelets)

wt=dwt(AirPassengers,filter = "haar", boundary = "periodic",fast = T)
features=unlist(c(wt@W,wt@V[[wt@level]]))

#FeaturesTable=NULL
#for(i in timeSeriesList){
# wt=dwt(i,filter = "haar", boundary = "periodic",fast = T)
# FeaturesTable <- rbind(FeaturesTable, unlist(c(wt@W,wt@V[[wt@level]])))
#}
#FeaturesTable=as.data.frame(FeaturesTable)

#####KAGGLE#####

install.packages("R.matlab")
library(R.matlab)
mat0 <- readMat('~/Documents/Documents/COLLEGE/semester 7/kaggle/train_1/1_1_0.mat')
dat0 = as.data.frame(mat0[[1]][[1]])
mat1 <- readMat('~/Documents/Documents/COLLEGE/semester 7/kaggle/train_1/1_1_1.mat')
dat1 = as.data.frame(mat1[[1]][[1]])

plot(1:1000,dat1[,1][1:1000],type = 'l')
plot(1:1000,apply(dat1,1,mean)[1:1000],type = 'l')

dat1_ts=ts(apply(dat1,1,mean),frequency=400)
dat0_ts=ts(apply(dat0,1,mean),frequency=400)

plot(decompose(dat1_ts))
