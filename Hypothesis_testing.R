rm(list=ls())
setwd("D:/R studio/Statistics for Decision making")
dataset<-read.table("HR.txt",header=T,sep = "\t")
View(dataset)


library("dplyr")
a=group_by(dataset,dataset$Gender)
summarise(a,mean(MonthlyIncome),count=n())


tapply(dataset$MonthlyIncome,dataset$Gender,mean)

#gatherdf=dataset %>% gather(columname1,columname2, range of columns)
# t.test and anova we go with split function
a=split(dataset,dataset$Gender)
male=a$`1`
female=a$`2`
t.test(male$MonthlyIncome,female$MonthlyIncome,alternative = "t",sig.level = 0.99)
f=aov(dataset$MonthlyIncome~dataset$Gender)
summary(f)



#chi-square and two sample z-test
a=table(dataset$Gender,dataset$Attrition)
b=chisq.test(a)
b$observed
b$expected
b$p.value


#Null hypothesis: pie(male)(YES)==pie(female)(YES)
p1=a[1,2]/sum(a[1,1],a[1,2])#p1=x1/n1
p2=a[2,2]/sum(a[2,1],a[2,2])#p2=x2/n2
ppoopled=(a[1,2]+a[2,2])/(sum(a[1,1],a[1,2])+sum(a[2,1],a[2,2]))#ppoopled=x1+x2/n1+n2
n1=sum(a[1,1],a[1,2])#n1=lenght(male)
n2=sum(a[2,1],a[2,2])#n2=length(female)
z_test=zdata<-(p1-p2)/sqrt(ppoopled*(1-ppoopled)*((1/n1)+(1/n2)))

p_value=2*pnorm(abs(z_test),lower.tail = F)