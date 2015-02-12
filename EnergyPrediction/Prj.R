##The internal design condition were set as following:- 0.6 clo, humidity: 60%, 
##air speed: 0.30 m/s, lighting level: 300. 
##The internal gains were set to sensible (5) and latent (2) W/m2 


# load libraries
library(caret) 
library(kernlab)
library(xlsx)
library(plyr)
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(corrgram)
library(MASS)
library(gvlma)
library(randomForest)
library(corrplot)
library(reshape)
  
## Function that returns Root Mean Square Error //Evaluation criteria

rmser <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns MAPE

maper <- function(errorfr)
{
  (mean(errorfr))*100
}

##  reading the data 

dataD1 <- read.xlsx("D:/Users/Kanu/Desktop/Analytics/Data Science Track/DD/EnergyPrediction/data/ENB2012_data.xlsx",1,as.data.frame=TRUE, header=TRUE, colClasses=NA)
str(dataD1)
head(dataD1)
dim(dataD1)


## data cleaning 
dataD2 <- dplyr::select(dataD1,X1:Y2)
dataD2 <- slice(dataD2, 1:768)
names(dataD2) <- c(X1="Rel.Compactness",X2="Surf.Area",X3="Wall.Area",X4="Roof.Area",X5="Height",
                   X6="Orient",X7="Glaz.Area",X8="Glaz.Area.Distribution",Y1="Heat.Load",Y2="Cool.Load")

summary(dataD2)


## data partition for training and testing

set.seed(998)
inTrain <- createDataPartition(y=dataD2[,9],p=0.75, list=FALSE)
training1 <- dataD2[inTrain,]
testing1 <- dataD2[-inTrain,]
dim(training1)

## centering and scaling 

preProcValues <- preProcess(training1[,-c(9:10)], method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training1[,-c(9:10)])
testTransformed <- predict(preProcValues, testing1[,-c(9:10)])

trainTransformedf <- cbind(trainTransformed,training1[,9:10])
testTransformedf <-  cbind(testTransformed,testing1[,9:10])


##check for normality in data, multicollinearity, homoskedasticity,

# pairwise plot to analyse relationship

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(trainTransformedf, panel = panel.smooth,
      cex = 1.5, pch = 20, bg = "light blue",
      diag.panel = panel.hist, cex.labels = 1, font.labels = 2)


# association  between variables, non parametric method spearman rank

M <- cor(trainTransformedf, use="complete.obs", method="spearman")
corrgram(trainTransformedf, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,cor.method="spearman",
         main="Exploratory Displays for Correlation") 
MM <- data.frame(M)
MM2 <- MM[1:8,1:8]
pal <- colorRampPalette(c("red", "white", "blue"))(n = 40)
heatmap(as.matrix(MM2), col = pal)

corrplot(M, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, 
         tl.col = rgb(0, 0, 0))
mtext("Spearman's Correlation Map", side=3, outer=TRUE, line=-3,cex=1.5) 

# cross validation, fitcontrol is for caret

set.seed(32323)  
folds <-  createFolds(y=trainTransformedf[,9],k=10,list=TRUE,returnTrain=TRUE)
fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)


# Stepwise Regression variable selection

fitStepH <- lm(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution,trainTransformedf)
stepH <- stepAIC(fitStepH, direction="both")
stepH$anova # display results 

fitStepC <- lm(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area,trainTransformedf)
stepC <- stepAIC(fitStepC, direction="both")
stepC$anova # display results 


#Regression analysis and residual Diagnostics

fitH <- lm(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Height+Glaz.Area+Glaz.Area.Distribution, data=trainTransformedf)
fitC <- lm(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Height+Glaz.Area+Glaz.Area.Distribution, data=trainTransformedf)
summary(fitH)
summary(fitC)
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(fitH)
plot(fitC)


# finding outlier, helps in decision of using robust regression
outlierTest(fitH) # Bonferonni p-value for most extreme obs
outlierTest(fitC)
cutoffH <- 4/((nrow(trainTransformedf)-length(fitH$coefficients)-2)) 
cutoffC <- 4/((nrow(trainTransformedf)-length(fitC$coefficients)-2))  


#leverage and influence check
plot(fitH, which=4, cook.levels=cutoffH)
plot(fitC, which=4, cook.levels=cutoffC)

# Influence Plot
#influencePlot(fitH, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#influencePlot(fitC, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Evaluate homoscedasticity
#Non-constant Variance Score Test
ncvTest(fitH)
ncvTest(fitC)

# plot studentized residuals vs. fitted values
spreadLevelPlot(fitH)
spreadLevelPlot(fitC)


# Evaluate Collinearity ,
# variance inflation factors
vif(fitC)
vif(fitH) 

# Evaluate Nonlinearity
# component + residual plot
#par(mar=c(1,1,1,1))
#par(mfrow=c(2,2))
#crPlots(fitH)
#crPlots(fitC)
# Ceres plots
#ceresPlots(fitH)
#ceresPlots(fitC)


# Test for Autocorrelated Errors ,A test that the residuals from a linear
#regression or multiple regression are independent.
durbinWatsonTest(fitH)
durbinWatsonTest(fitC)


anova(fitH) # anova table
vcov(fitH) # covariance matrix for model parameters
anova(fitC) # anova table
vcov(fitC) # covariance matrix for model parameters


# Global test of model assumptions

gvmodelH <- gvlma(fitH)
summary(gvmodelH) 
gvmodelC <- gvlma(fitC)
summary(gvmodelC) 



# First model neural networks

nnetH <- train(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Height+Glaz.Area+Glaz.Area.Distribution,trainTransformedf,
               method = "nnet",tuneLength = 5,
               trControl = fitControl,linout=TRUE,trace=FALSE)

nnetC <- train(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Height+Glaz.Area+Glaz.Area.Distribution,trainTransformedf,
               method = "nnet",tuneLength = 5,
               trControl = fitControl,linout=TRUE,trace=FALSE)



# Second model:- knn 
knnH <- train(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution,trainTransformedf,
              method = "knn",
              tuneLength = 2,
              trControl = fitControl)
          

knnC <- train(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution,trainTransformedf,
              method = "knn",
              tuneLength = 2,
              trControl = fitControl)

# Third model :- Support Vector regression

svmH <- train(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution , data=trainTransformedf, method = "svmRadialCost", trControl = fitControl)
svmC <- train(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution , data=trainTransformedf, method = "svmRadialCost", trControl = fitControl)


# Fourth model:- random forest

rfH <- train(Heat.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution, trainTransformedf,
             method = "rf",importance=TRUE,
             prox=TRUE,
             trControl = fitControl)

rfC <- train(Cool.Load ~ Rel.Compactness+Surf.Area+Wall.Area+Roof.Area+Height+Orient+Glaz.Area+Glaz.Area.Distribution, trainTransformedf,
             method = "rf",
             tuneLength = 2,prox=TRUE,importance=TRUE,
             trControl = fitControl)
print(rfH)
plot(rfH)
print(rfC)
plot(rfC)

dm <- as.matrix(varImp(rfH$finalModel))
dm1 <- melt(dm)
p <- ggplot(dm1, aes(x=X1,y=value)) + geom_histogram(stat="identity")
p <- p+ggtitle("Variable Importance in Prediction") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=20),axis.text.x = element_text(angle = 45, hjust = 1,size=11))
p <- p + theme(axis.title.y = element_text(size = rel(1.4), angle = 90))
p <- p+ylab("Importance on a scale of 0-100") +xlab("")
p
# 5th model:-bagged tree

predictors = data.frame(trainTransformedf[,1:8])
heat=trainTransformedf[,9]
cold=trainTransformedf[,10]
treebagH <- bag(predictors,heat,B=10,bagControl=bagControl(fit=ctreeBag$fit,predict=ctreeBag$pred,aggregate=ctreeBag$aggregate))
treebagC <- bag(predictors,cold,B=10,bagControl=bagControl(fit=ctreeBag$fit,predict=ctreeBag$pred,aggregate=ctreeBag$aggregate))


# prediction based on test/independent data


trainTransformedf1 <- trainTransformedf[,-c(4,6)]
testTransformedf1 <- testTransformedf[,-c(4,6)]

prednnetH <- predict(nnetH,testTransformedf1[,1:6])
prednnetC <- predict(nnetC,testTransformedf1[,1:6])

predknnH <- predict(knnH,testTransformedf[,1:8])
predknnC <- predict(knnC,testTransformedf[,1:8])

predsvrH <- predict(svmH,testTransformedf[,1:8])
predsvrC <- predict(svmC,testTransformedf[,1:8])

predrfH <- predict(rfH,testTransformedf[,1:8])
predrfC <- predict(rfC,testTransformedf[,1:8])

predtreebagH <- predict(treebagH,testTransformedf[,1:8])
predtreebagC <- predict(treebagC,testTransformedf[,1:8])

preDFH <- data.frame(prednnetH,predknnH,predsvrH,predrfH,predtreebagH,Heat.Load=testTransformedf[,9])
preDFC <- data.frame(prednnetC,predknnC,predsvrC,predrfC,predtreebagC,Cool.Load=testTransformedf[,10])

ensmbStackH <- train(Heat.Load~.,method="gam",data=preDFH)
ensmbStackC <- train(Cool.Load~.,method="gam",data=preDFC)

combPredH <- predict(ensmbStackH,preDFH[,1:5])
combPredC <- predict(ensmbStackC,preDFC[,1:5])



best2StackH <- train(Heat.Load~prednnetH+predsvrH,method="gam",data=preDFH)
best2StackC <- train(Cool.Load~prednnetC+predsvrC,method="gam",data=preDFC)

best2PredH <- predict(best2StackH,preDFH)
best2PredC <- predict(best2StackC,preDFC)


print(qplot(combPredH,testTransformedf[,9]))
print(qplot(combPredC,testTransformedf[,9]))



#  train rmse


errornnetHTr <- trainTransformedf1[,7] - predict(nnetH,trainTransformedf1[,1:6])
rmsennetHTr <- rmser(errornnetHTr)

errorknnHTr <- trainTransformedf[,9] - predict(knnH,trainTransformedf[,1:8])
rmseknnHTr <- rmser(errorknnHTr)

errorsvrHTr <- trainTransformedf[,9] - predict(svmH,trainTransformedf[,1:8])
rmsesvrHTr <- rmser(errorsvrHTr)

errorrfHTr <- trainTransformedf[,9] - predict(rfH,trainTransformedf[,1:8])
rmserfHTr <- rmser(errorrfHTr)

errortreebagHTr <- trainTransformedf[,9] - predict(treebagH,trainTransformedf[,1:8])
rmsetreebagHTr <- rmser(errortreebagHTr)


errornnetCTr <- trainTransformedf1[,8] - predict(nnetC,trainTransformedf1[,1:6])
rmsennetCTr <- rmser(errornnetCTr)

errorknnCTr <- trainTransformedf[,10] - predict(knnC,trainTransformedf[,1:8])
rmseknnCTr <- rmser(errorknnCTr)

errorsvrCTr <- trainTransformedf[,10] - predict(svmC,trainTransformedf[,1:8])
rmsesvrCTr <- rmser(errorsvrCTr)

errorrfCTr <- trainTransformedf[,10] - predict(rfC,trainTransformedf[,1:8])
rmserfCTr <- rmser(errorrfCTr)

errortreebagCTr <- trainTransformedf[,10] - predict(treebagC,trainTransformedf[,1:8])
rmsetreebagCTr <- rmser(errortreebagCTr)


# Calculate test data error 
errornnetH <- testTransformedf1[,7] - prednnetH
errornnetC <- testTransformedf1[,8] - prednnetC

errorknnH <- testTransformedf[,9] - predknnH
errorknnC <- testTransformedf[,10] - predknnC

errorsvrH <- testTransformedf[,9] - predsvrH
errorsvrC <- testTransformedf[,10] - predsvrC

errorrfH <- testTransformedf[,9] - predrfH
errorrfC <- testTransformedf[,10] - predrfC

errortreebagH <- testTransformedf[,9] - predtreebagH
errortreebagC <- testTransformedf[,10] - predtreebagC

errorcombPredH <- testTransformedf[,9] - combPredH
errorcombPredC <- testTransformedf[,10] - combPredC

errorbest2PredH <- testTransformedf[,9] - best2PredH
errorbest2PredC <- testTransformedf[,10] - best2PredC


# RMSE of test data / generalisation error
rmsennetH <- rmser(errornnetH)
rmsennetC <- rmser(errornnetC)

rmseknnH <- rmser(errorknnH)
rmseknnC <- rmser(errorknnC)

rmsesvrH <- rmser(errorsvrH)
rmsesvrC <- rmser(errorsvrC)

rmserfH <- rmser(errorrfH)
rmserfC <- rmser(errorrfC)

rmsetreebagH <- rmser(errortreebagH)
rmsetreebagC <- rmser(errortreebagC)

rmsecombPredH <- rmser(errorcombPredH)
rmsecombPredC <- rmser(errorcombPredC)

rmsebest2PredH <- rmser(errorbest2PredH)
rmsebest2PredC <- rmser(errorbest2PredC)


Model.HeatingLoad <- c("ANNTrain","ANNTest","KNNTrain","KNNTest","SVRTrain","SVRTest","RFTrain","RFTest","TREEBAGTrain","TREEBAGTest","Best 2 model","Ensemble")
RMSE.HL <- c(rmsennetHTr,rmsennetH,rmseknnHTr,rmseknnH,rmsesvrHTr,rmsesvrH,rmserfHTr,rmserfH,rmsetreebagHTr,rmsetreebagH,rmsebest2PredH,rmsecombPredH)
evalH <- data.frame(Model.HeatingLoad,RMSE.HL) 

Model.CoolingLoad <- c("ANNTrain","ANNTest","KNNTrain","KNNTest","SVRTrain","SVRTest","RFTrain","RFTest","TREEBAGTrain","TREEBAGTest","Best 2 model","Ensemble")
RMSE.CL <- c(rmsennetCTr,rmsennetC,rmseknnCTr,rmseknnC,rmsesvrCTr,rmsesvrC,rmserfCTr,rmserfC,rmsetreebagCTr,rmsetreebagC,rmsebest2PredC,rmsecombPredC)
evalC <- data.frame(Model.CoolingLoad,RMSE.CL)

print(evalH)
print(evalC)



energyPredH <- function(df){
  
  names(df) <- c("Rel.Compactness","Surf.Area","Wall.Area","Roof.Area","Height",
                 "Orient","Glaz.Area","Glaz.Area.Distribution")  
  
  df1<- predict(preProcValues, df[,1:8])
  df2 <- subset(df1[,-c(4,6)])
  prednnetH1 <- predict(nnetH,df2)
  predknnH1 <- predict(knnH,df1)
  predsvrH1 <- predict(svmH,df1)
  predrfH1 <- predict(rfH,df1)
  predtreebagH1 <- predict(treebagH,df1)
  preDFH1 <- data.frame(prednnetH=prednnetH1,predknnH=predknnH1,predsvrH=predsvrH1,predrfH=predrfH1,predtreebagH=predtreebagH1)
  combPredH1 <- predict(ensmbStackH,preDFH1)
  return(combPredH1)
}
energyPredC <- function(df){
  
  names(df) <- c("Rel.Compactness","Surf.Area","Wall.Area","Roof.Area","Height",
                 "Orient","Glaz.Area","Glaz.Area.Distribution")  
  
  df3<- predict(preProcValues, df[,1:8])
  df4 <- subset(df3[,-c(4,6)])
  prednnetC1 <- predict(nnetC,df4)
  predknnC1 <- predict(knnC,df3)
  predsvrC1 <- predict(svmC,df3)
  predrfC1 <- predict(rfC,df3)
  predtreebagC1 <- predict(treebagC,df3)
  preDFC1 <- data.frame(prednnetC=prednnetC1,predknnC=predknnC1,predsvrC=predsvrC1,predrfC=predrfC1,predtreebagC=predtreebagC1)
  combPredC1 <- predict(ensmbStackC,preDFC1)
  return(combPredC1)
}



# Stop the clock






# 
# # Calculate errorfraction 
# errornnetHfr <- (testTransformedf[,9] - prednnetH)/testTransformedf[,9]
# errornnetCfr <- (testTransformedf[,10] - prednnetC)/testTransformedf[,10]
# 
# errorknnHfr <- (testTransformedf[,9] - predknnH)/testTransformedf[,9]
# errorknnCfr <- (testTransformedf[,10] - predknnC)/testTransformedf[,10]
# 
# errorsvrHfr <- (testTransformedf[,9] - predsvrH)/testTransformedf[,9]
# errorsvrCfr <- (testTransformedf[,10] - predsvrC)/testTransformedf[,10]
# 
# errorglmHfr <- (testTransformedf[,9] - predglmH)/testTransformedf[,9]
# errorglmCfr <- (testTransformedf[,10] - predglmC)/testTransformedf[,10]
# 
# errorrfHfr <- (testTransformedf[,9] - predrfH)/testTransformedf[,9]
# errorrfCfr <- (testTransformedf[,10] - predrfC)/testTransformedf[,10]
# 
# errortreebagHfr <- (testTransformedf[,9] - predtreebagH)/testTransformedf[,9]
# errortreebagCfr <- (testTransformedf[,10] - predtreebagC)/testTransformedf[,10]
# 
# errorcombPredHfr <- (testTransformedf[,9] - combPredH)/testTransformedf[,9]
# errorcombPredCfr <- (testTransformedf[,10] - combPredC)/testTransformedf[,10]
# 
# 
# errorbest2PredHfr <- (testTransformedf[,9] - best2PredH)/testTransformedf[,9]
# errorbest2PredCfr <- (testTransformedf[,10] - best2PredC)/testTransformedf[,10]
# 
# 
# 
# # mape of each model
# mapennetH <- maper(errornnetHfr)
# mapennetC <- maper(errornnetCfr)
# 
# mapeknnH <- maper(errorknnHfr)
# mapeknnC <- maper(errorknnCfr)
# 
# mapesvrH <- maper(errorsvrHfr)
# mapesvrC <- maper(errorsvrCfr)
# 
# mapeglmH <- maper(errorglmHfr)
# mapeglmC <- maper(errorglmCfr)
# 
# maperfH <- maper(errorrfHfr)
# maperfC <- maper(errorrfCfr)
# 
# mapetreebagH <- maper(errortreebagHfr)
# mapetreebagC <- maper(errortreebagCfr)
# 
# mapecombPredH <- maper(errorcombPredHfr)
# mapecombPredC <- maper(errorcombPredCfr)
# 
# mapebest2PredH <- maper(errorbest2PredHfr)
# mapebest2PredC <- maper(errorbest2PredCfr)
#MeanAbsolutePercentageError.HL <- c(mapennetH,mapennetH,mapeknnH,mapeknnH,mapesvrH,mapesvrH,mapeglmH,mapeglmH,maperfH,maperfH,mapetreebagH,mapebest2PredH,mapebest2PredH,mapecombPredH,mapecombPredH)
#,MeanAbsolutePercentageError.HL)
#MeanAbsolutePercentageError.CoolingLoad <- c(mapennetC,mapeknnC,mapesvrC,mapeglmC,maperfC,mapetreebagC,mapebest2PredC,mapecombPredC)

#oolingLoad,MeanAbsolutePercentageError.CoolingLoad)

# # comparing models
# 
# cvValues <- resamples(list(ANN=nnetH ,KNN=knnH,SVM=svmH,GLM=glmH,RandomForest = rfH, Treebag=treebagH, best2M=best2StackH,Ensemble=ensmbStackH))
# summary(cvValues)
# 

