# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S

rm(list=ls())
dat<-read.csv('HR.txt',header = TRUE,sep='\t',row.names=1)
boxplot(dat$MonthlyIncome~dat$Gender)
f1<-table(dat$Attrition)
barplot(f1,col=c('red','blue'))
f2<-table(dat$Gender)
barplot(f2,col=rainbow(5))

f3<-table(dat$Attrition,dat$Gender)
colnames(f3)<-c("Male","Female")
barplot(f3,legend=row.names(f3),col=c("blue","red"),main="comparison Barplot")

qqnorm(dat$Age)
qqline(dat$Age)
