# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S

rm(list=ls())
x<-rnorm(70,28,2.5)
val<-hist(x)
points<-ogive.freq(val,ylab = 'Cummilated Relative Freq')

hist(x)
x1<-rnorm(100)
val2<-hist(x1)
points2<-ogive.freq(val2,ylab = 'Cummilated Relative Freq')


x2<-rexp(n=100,rate=3)
hist(x2)
val3<-hist(x2)
points3<-ogive.freq(val3,ylab = 'Cummilated Relative Freq')

