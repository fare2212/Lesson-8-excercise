#TEAM : Fares Al Hasan
#Date: 14-1-2016
#predict VCF and compare it with the original one 
#calculate the total error and for each class
setwd("~/git/GeoScripting/Lesson8_Excercise")
getwd()


library(raster)
library(sp)
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/vcfGewata.rda")
plot(vcfGewata)
vcfGewata[vcfGewata > 100] <- NA

alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
plot(alldata[[1:7]], col=bpy.colors())
opar <- par(mfrow=c(1, 3))
pairs(brick(GewataB3,vcfGewata))
pairs(brick(GewataB5,vcfGewata))
pairs(brick(GewataB7,vcfGewata))
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

df <- as.data.frame(getValues(alldata))
str(df)
model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 +  band7, data = df,na.action=na.omit )
summary(model)
VCFpredict <- predict(alldata[[1:6]], model = model, na.rm=TRUE)
names(VCFpredict) <- "VCF"
VCFpredict[VCFpredict < 0] <- NA
VCFpredict[VCFpredict > 100] <- NA
opar <- par(mfrow=c(1, 2))
plot(VCFpredict, col=bpy.colors())
plot(alldata$VCF, col=bpy.colors())

summary(model)$r.squared 

CalcRMSE <- function (x,y){
  # x is Original , y is the Predicted
  minus<-x-y
  square<-minus^2
  mean<-cellStats(square, 'mean')
  RMSE <- sqrt(mean)
  return(RMSE)
}
RMSE <- CalcRMSE(alldata$VCF,VCFpredict)
RMSE

load("data/trainingPoly.rda")
plot(GewataB1)
plot(trainingPoly, add = TRUE)
trainingPoly@data
trainingPoly@data$Class
str(trainingPoly@data$Class)
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
classes <- rasterize(trainingPoly, vcfGewata, field='Code',progress="text")
classes
plot(classes)
trainingPredict <- zonal(VCFpredict,classes)
traininOriginal <- zonal(alldata$VCF,classes)
trainingPredict
traininOriginal
RMSEclass1 <- sqrt(mean((traininOriginal[1,2]-trainingPredict[1,2])^2))
RMSEclass1
RMSEclass2 <- sqrt(mean((traininOriginal[2,2]-trainingPredict[2,2])^2))
RMSEclass2
RMSEclass3 <- sqrt(mean((traininOriginal[3,2]-trainingPredict[3,2])^2))
RMSEclass3
