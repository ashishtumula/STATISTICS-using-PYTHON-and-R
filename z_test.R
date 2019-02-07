rm(list = ls())
setwd("D:/R studio/Statistics for Decision making")

x<-read.csv('heart.csv')
ct<-table(x$disease,x$gender)
colnames(ct)<-c('Male','Female')
rownames(ct)<-c('NHD','HD')
# perform a suitable hypothesis test to verify the % of male patient with heart disease wrt to % of female patient

x1<-ct[2,1]
x2<-ct[2,2]
n1<-sum(ct[,1])
n2<-sum(ct[,2])
p1<-x1/n1
p2<-x2/n2
ppooled=(x1+x2)/(n1+n2)
#calculate test statistics
zdata<-(p1-p2)/sqrt(ppooled*(1-ppooled)*((1/n1)+(1/n2)))
pvalue<-2*pnorm(abs(zdata),lower.tail = F)

#exercise doing and not doing

ct<-table(x$disease,x$exercise)
rownames(ct)<-c('NHD','HD')
colnames(ct)<-c('Exercise','No Exercise')
x1<-ct[2,1]
x2<-ct[2,2]
n1<-sum(ct[,1])
n2<-sum(ct[,2])
p1<-x1/n1
p2<-x2/n2
ppooled=(x1+x2)/(n1+n2)
#calculate test statistics
zdata<-(p1-p2)/sqrt(ppooled*(1-ppooled)*((1/n1)+(1/n2)))
pvalue<-2*pnorm(abs(zdata),lower.tail = F)
