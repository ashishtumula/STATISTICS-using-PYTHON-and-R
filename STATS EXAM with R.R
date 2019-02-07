#1.	Load the data (recruitment_data.csv) file and perform the following,
x<-read.csv("C:/Users/ASHISH/Desktop/STATS EXAM/recruitment_data.csv")
x
head(x)
View(x)
#a)	Delete all missing value rows and report the reduced data frame 
na.omit(x)
#b)Check the variable 'sales_quota_pct', for its normality, justify with suitable plots
x1<-(x$sales_quota_pct)
x1
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
hist(x1,col=rainbow(10),xlab="sales_quota_pct",ylab="count",main = "Histogram of sales quota pct")
barplot(x1,col=rainbow(10),xlab="sales_quota_pct",ylab="count",main = "barplot of sales quota pct")
boxplot(x1,col=rainbow(10),xlab="sales_quota_pct",ylab="count",main = "boxlot of sales quota pct")
ggplot2(x1,col=rainbow(10),xlab="sales_quota_pct",ylab="count",main = "ggplotof sales quota pct")
library("agricolae")
x1_ogive<-hist(x1)
ogive.freq(x1_ogive)
#c)Check whether the sales_quota_pct vary significantly with respect to the mode of requirement (Referral candidates Vs Applied Online)
data<-split(x,x$recruiting_source)
data
#d)Check the proportion of Attrition vary significantly or not, with respect to all mode of recruitment
x2<-table(x$attrition,x$recruiting_source)
pp_appliedonline<-x2[,1]/sum(x2[1,1])
pp_campus<-x2[,1]/sum(x2[1,2])
pp_referral<-x2[,1]/sum(x2[1,3])
pp_search<-x2[,1]/sum(x2[1,4])

#2Load the data (prima-indians-diabetes.csv) file and do the following, [5 Marks]
y<-read.csv("C:/Users/ASHISH/Desktop/STATS EXAM/prima-indians-diabetes+%281%29.csv",header = F)
y
View(y)
#a)
colnames(y)<-c('No. of times Pregnant','glucose concentration','Blood pressure','Skin Thickness',' Insulin','Body Mass Index','Diabetes Pedigree Function','Age','Class')
names(y)
#b)	Class-0 is attached with Healthy, Class-1 is attached with Diabetes. Check whether Body Mass Index is having a significant effect on Diabetes. Justify with statistical evidence.
     #class 0=healthy  class1=diabeter
y1<-table(y$`Body Mass Index`,y$Class)
y2<-y$`Body Mass Index`[y$Class=="0"]
y3<-y$`Body Mass Index`[y$Class=="1"]
t.test(y2,y3,alternative = 't',conf.level = 0.95)
#c)Also, check whether 'Age', factor affecting the Disease, justify with suitable test.
y4<-y$Age[y$Class=="1"]
y4
y5<-y$Age[y$Class=="0"]
y5
t.test(y4,y5,alternative = 't',conf.level = 0.95)

#5)For the given quantitative samples, compute the following.    	[8 Marks]
#a)	First Quartile
#b)	Third Quartile
#c)	Inter Quartile Range (IQR)
#d)	Inner fence 
#e)	Outer fence
data<-c(34,67,40,72,37,33,42,62,49,32,52,40,31,19,68,55,57,54,37,32,54,38,20,50,56,48,35,52,29,56,68,65,45,44,54,39,29,56,43,42)
res<-summary(data)
quantile(data)                   # Finding the quantiles
iqr <- res[5] - res[2]              # Finding the iqr.
iqr
inner <- res[5] + 1.5*iqr           # Finding the inner fences.
inner
outer <- res[5] + 3 * iqr           # Finding the outer layer.
outer

#3)	Load the 'survey' data set from MASS library and test whether the students smoking habit is independent of their exercise level.			[5 Marks]
library(MASS)
data(survey)
a<-survey
a
View(a)
head(a)
str(a)
dim(a)
names(a)
a<-table(a$Smoke,a$Exer)
table<-chisq.test(a)

#4)4.	Load the 'Cars93' data set from MASS library and verify the following    [5 Marks]
#b)	Also check the mean mileage of the car, vary significantly with respect to 'Drive Train'.  
library(MASS)
data(Cars93)
b<-Cars93
b
colnames(b)
head(b)
View(b)
#a)	Considering the 'Price' attribute, check whether the mean price of the car significantly vary with respect to 'Air Bags'. Justify your answer with suitable hypothesis test.
b1<-(b$Price)
b2<-tapply(b$Price,b$AirBags,mean)
summary(b2)
#ANOVA TEST
b3<-aov(b$Price~b$AirBags)
b3
summary(b3)
#b)	Also check the mean mileage of the car, vary significantly with respect to 'Drive Train'.  
b4<-tapply(b$MPG.city,b$DriveTrain,mean)

#10.	Load HR.txt data and verify the following with suitable hypothesis test	[10 Marks]
f<-read.table("C:/Users/ASHISH/Desktop/STATS EXAM/HR.txt",header = TRUE,sep = '\t',row.names = 1)
f
colnames(f)
#a)
#c) 
m1<-f$MonthlyIncome[f$Gender== c("1","2")]
m2<-f$MonthlyIncome[f$Attrition==c("Yes")]
mean(m1)
mean(m2)
m3<-mean(m1)-mean(m2)
m3
str(f)
pooled_sd<-sqrt(m3)
power.t.test(n=1470,delta = m3,sd=pooled_sd,sig.level = 0.05)
