# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S

rm(list=ls())
data(mtcars)
#Covariance
dat<-mtcars
c1<-cov(dat$mpg,dat$wt)
c2<-cov(dat$mpg,dat$disp)

#Correlation coefficient
cc1<-cor(dat$mpg,dat$wt)
cc2<-cor(dat$mpg,dat$disp)

#scatterplot
plot(dat$wt,dat$mpg)
plot(dat$gear,dat$disp)
