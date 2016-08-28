library(ggplot2)
library(MASS)
library(fANCOVA)
df1=aggregate(Price~Type, data=Cars93, FUN=mean)
barplot(df1$Price, names.arg=df1$Type)


senicdata <- read.csv2("SENIC.csv")
head(senicdata)


mod=loess.as(senicdata$Obs, senicdata$X3, criterion="gcv", degree=2,
             plot=FALSE)
result=predict(mod, se=TRUE)

