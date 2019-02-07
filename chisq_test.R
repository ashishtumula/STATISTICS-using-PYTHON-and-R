rm(list=ls())
setwd("D:/R studio/Statistics for Decision making/datasets textbook/Excel Data Files")
dataset<-read.csv("CardioGoodFitness.csv",stringsAsFactors = F)
View(dataset)


#q3: cardiofitness 
#null hypothesis pie(1*rating)=pie(2*rating)=pie(3*rating)=pie(4*rating)=pie(5*rating)
#performing the chisquared test
table<-table(dataset$Marital.Status,dataset$Fitness)
test<-chisq.test(table)
test$observed
test$expected
test$p.value    
test$statistic
#observation: we failed to reject the null hypothesis so there is a dependency in the rating 

#q1:
a<-c(163,154)
b<-c(64,108)
table1<-as.table(rbind(a,b))
rownames(table1)<-c('Yes','NO')
colnames(table1)<-c('Beachcomber','Windsurfer')

# proportion of pie(no)(beachcomber)=pie(no)(windsurfer)
#as there are two categories we go with the z-test
p1=table1[2,1]/(table1[1,1]+table1[2,1])
p2=table1[2,2]/(table1[1,2]+table1[2,2])
ppooled=(table1[2,1]+table1[2,2])/((table1[1,1]+table1[2,1])+(table1[1,2]+table1[2,2]))
z_test=sqrt(ppooled*(1-ppooled)*((1/((table1[1,1]+table1[2,1]))+(1/(table1[1,2]+table1[2,2])))))
p_value=2*pnorm(abs(z_test))


#q3:relation between the identified main oppurtunity and Geographic Region
a<-c(58,58,45,21)
b<-c(60,30,31,15)
c<-c(23,10,12,4)
d<-c(16,18,31,2)
e<-c(5,18,8,3)
table2=as.table(rbind(a,b,c,d,e))
test2<-chisq.test(table2)

#q4: relation between the age group and people primary get news
a<-c(107,119,133)
b<-c(73,102,127)
c<-c(75,97,109)
d<-c(52,79,107)
e<-c(95,83,76)
table3=as.table(rbind(a,b,c,d,e))
test3<-chisq.test(table3)
test3$statistic


