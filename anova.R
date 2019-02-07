rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("ForeignMarket2.csv",stringsAsFactors = F)
View(dataset)
#q1
dataset<-dataset[1:40,1:4]
dataset$Cost.to.export...US..per.container.=gsub(",","",dataset$Cost.to.export...US..per.container.)
dataset$Cost.to.import..US..per.container.=gsub(",","",dataset$Cost.to.import..US..per.container.)
a=aov(dataset$Cost.to.import..US..per.container.~dataset$Region)
a$coefficients
a$residuals
a$
summary(a)
data<-split(dataset,dataset$Region)
View(data$`East Asia & Pacific`)
dataset$Cost.to.export...US..per.container.=as.integer(dataset$Cost.to.export...US..per.container.)
dataset$Cost.to.import..US..per.container.=as.integer(dataset$Cost.to.import..US..per.container.)


#q2
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("Golfball.csv",stringsAsFactors = F)
a=aov(dataset$Design1~dataset$Design2)
summary(a)
b=aov(dataset$Design2~dataset$Design3)

#q3
rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("Granule.csv",stringsAsFactors = F)
View(dataset)
B<-complete.cases(dataset$Vermont)
dataset$Vermont[!B]=sample(dataset$Vermont[B],length(dataset$Vermont[!B]))

t.test(dataset$Boston,dataset$Vermont,alternative = "t",conf.level = 0.95,paired = T)