# install package and load library
install.packages("missForest")
install.packages("mice")
library(mice)
library(missForest)

#load data
data("iris")
#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# Removing categorical data
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

install.packages("VIM")
library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# impute data missing
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)


# *************************************************************************


# data project
install.packages("missForest")
install.packages("mice")
install.packages("VIM")

library(mice)
library(missForest)
library(VIM)
myfiles <- read.csv('merged_files.csv', header = TRUE)

#check for missing values
str(myfiles)
sum(is.na(myfiles))

# the above shows that there are block of missing values amounting to a total of 
# 12290. 

# Analyse pattern of missing values. 
md.pattern(myfiles)
md.pattern(myfiles, plot = TRUE, rotate.names = FALSE)

# Total missing values
sum(is.na(myfiles))
sum(complete.cases(myfiles))

p <- table(is.na(myfiles))
p <- prop.table(p)
p <- round(p *100)
p


# Find correlation between claim_score and the other variables
nums <- sapply(myfiles, is.numeric)
NUMDATA<-myfiles[,nums]
cormat<-cor(NUMDATA,use="pairwise.complete.obs")
corr_cat <- cor(NUMDATA[-1], myfiles$claim_score) 
corr_catq <- data.frame(corr_cat)
corr_catq


#the showa that about 22% of our data is qmissing which are located in the population variables. 
# the missing values resulted because the population and claim_crime dataframe have different dimensions
# and their common variable which was used to engineer the merging do not exactly match. 
# otherefore the block of missing values in the dataframe. 
# a decision was taken to restore the missing values because deleting 22% of the rows 
# might impact on the integrirty of the Model



mice_plot22 <- aggr(myfiles, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(myfiles), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))



# imputed data using mice
imputed_Data22 <- mice(myfiles, m=10, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data22)

mice_data <-mice(myfiles, m=10, maxit=500, method='cart', seed=500)
mice_data
sum(is.na(mice_data))


# using mi
#imputing missing value with mi
#install package and load library
install.packages("mi")
library(mi)
mi_data <- mi(myfiles, seed = 335)


# multicollinearity


# Boruta feature selection
install.packages('Boruta')
library(Boruta)


