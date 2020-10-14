getwd()
setwd('C:/R Folder')
student <- read.csv('Students.csv', header = T, stringsAsFactors = F)
head(student)
str(student)
colnames(student)
dim(student)

tab1 <- table(student$exercise_level)
tab1[c(4,2,3,1,5,6)]
tab1 <- c(tab1[1:4], tab1[5]+tab1[6])
tab1



