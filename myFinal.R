data2 <- read.csv("data2.csv", header = T)
names(data2)

# Feature Engineering

# transform the claim_score
hist(data2$claim_score)
boxplot(data2$claim_score)
summary(data2$claim_score)

hist(log(data2$claim_score + 1.08 ))
hist(sqrt(data2$claim_score + 1.08 ))

boxplot((log(data2$claim_score )))
boxplot((sqrt(data2$claim_score + 1.08 )))

# display the outliers in the variable
boxplot(data2$claim_score + 1.08)$out
boxplot(claim_score + 1.08, plot = F)$out

data2$claim_score <- sqrt(data2$claim_score + 1.08 )


#Check the data for near zero variance
nzv <- nearZeroVar(, saveMetrics= TRUE)
nzv # result shows that there is no variable with near zero variance

# check for variable with high correlation between the predictors.
descrCor <-  cor(data2)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr # result shows that there are no variables with high correlation between them

#scaling and centre the variable - standardize
pp_hpc <- preProcess(data2, method = c("center", "scale", "YeoJohnson"))
pp_hpc

data3 <- predict(pp_hpc, newdata = data2)
head(data3)

# view the claim_score variable
boxplot(data3$claim_score)
hist(data3$claim_score)

# outlier treatment
# check variables for outliers
# outlier treatment
# outlier treatment


#Outlier Treament using random forest and K-nearest neighbour algorithm
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)
}
# outlierTreament(data3$medium_impact_crime)
apply(data3, 2, function(x) any(is.na(x)))
sapply(data3, function(x) sum(is.na(x)))

numeric_cols<-names(data3)[sapply(data3, is.numeric)]
numeric_data<-data3[,names(data3)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))
Outlier_miss # list of all the variables and the numbers of outliers
sum(is.na(data3))

library(missForest)
data3.imp <- missForest(merged_files1)

dim(data3)
data3.imp
str(data3.imp)

str(merged_files.imp$ximp)
data3.imp$OOBerror
data4 <- data3.imp$ximp
str(data3)

summary(data3)

##########



# check for the 
install.packages("ISLR")
install.packages("leaps")
library(ISLR)
library(leaps)
fit <- regsubsets(claim_score~., data3, nvmax = 20)
summary(fit)
plot(fit, scale = "bic")

# check boruta for variable importance
install.packages("Boruta")
library(Boruta)
boruta_varimp_safedriver = Boruta(claim_score ~ ., data = data3, doTrace = 2) 
plot(boruta_varimp_safedriver, las=2)  
# baruta comfirmed all the variable to be important



# Model selection
# plot a model using all the variables
modelaa <- lm(formula = (data1$claim_score) ~ ., data = data3)
summary(modelaa)
plot(modelaa$residuals, pch=20)


fit <- regsubsets(claim_score~., data4, nvmax = 20)
summary(fit)
plot(fit, scale = "bic")





library(caret)
