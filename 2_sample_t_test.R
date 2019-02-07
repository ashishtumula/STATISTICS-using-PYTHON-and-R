rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("Cola.csv",stringsAsFactors = F)
View(dataset)

#q1: Comparing New Cola weekly sales from two different End_UP locations
#null hypothesis: mean(produce end-up location)=mean(beverage end-up location)
t.test(dataset$Produce,dataset$Beverage,alternative = "t",conf.level = 0.95)
d=mean(dataset$Beverage)-mean(dataset$Produce)
s=sqrt(((sd(dataset$Beverage)^2)/(length(dataset$Beverage)))+((sd(dataset$Produce)^2)/(length(dataset$Produce))))
power.t.test(n=length(dataset$Beverage),delta =d , sd=s ,alternative = "t",sig.level = 0.05)

#observation: rejecting the Null Hypothesis based on the evidence that p-value is less than 0.05 
#relation: Beverage end-up locations having less sales 
  


#q2: testing the difference between the Mean delivery times
#null hypothesis: mean(local)< = mean(national chain)
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("PizzaTime.csv",stringsAsFactors = F)
View(dataset)
t.test(dataset$Local,dataset$Chain,alternative = "t",conf.level = 0.95)
d=mean(dataset$Local)-mean(dataset$Chain)
s=sqrt(((sd(dataset$Local)^2)/(length(dataset$Local)))+((sd(dataset$Chain)^2)/(length(dataset$Chain))))
power.t.test(n=10,delta=d,sd=s,alternative = "t",sig.level = 0.05)

#observation: reject to fail the null hypothesis based on the evidence of p-value is greater than 0.05
#realtion : local-chain pizza has faster rate in delivering to there customer and we accept the advertisement



#q3: 
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset1<-read.csv("Bank1.csv",stringsAsFactors = F)
dataset2<-read.csv("Bank2.csv",stringsAsFactors = F)

dataset<-cbind(dataset1,dataset2)
colnames(dataset)<-c("waiting_time1","waiting_time2")
t.test(dataset$waiting_time1,dataset$waiting_time2,alternative = "t",conf.level = 0.95)


#q4:
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("FiveYearCDRate.csv",stringsAsFactors = F)

B<-complete.cases(dataset$LA)
dataset$LA[!B]=sample(dataset$LA[B],length(dataset$LA[!B]))

t.test(dataset$NY,dataset$LA,alternative = "t",conf.level = 0.95,paired = T)
n1=length(dataset$NY)
n2=length(dataset$LA)
d=mean(dataset$NY)-mean(dataset$LA)
s=sqrt(((sd(dataset$NY)^2)/(length(dataset$NY)))+((sd(dataset$LA)^2)/(length(dataset$LA))))
power.t.test(n=n1+n2,delta = d,sd=s,alternative = "t",sig.level = 0.05)

#q5
# here patient is same for both the before and after
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("Myeloma.csv",stringsAsFactors = F)

t.test(dataset$Before,dataset$After,alternative = "t",sig.level=0.05,paired = T)

#observation : we are not rejected the ho throght the evidence


