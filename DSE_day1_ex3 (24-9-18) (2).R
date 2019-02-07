# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S

rm(list=ls())
library(MASS)
data("Cars93")
x<-Cars93

complete.cases(x) # will check for all the columns

# to check a particular attribute
B<-complete.cases(x$Rear.seat.room)
x$Rear.seat.room[!B]<-mean(x$Rear.seat.room[B])


#Alternate Method 

ind<-0;k<-1
for (i in c(1:length(x$Rear.seat.room)))  #i : 1 to 93
{
  if (is.na(x$Rear.seat.room[i]))
  {
    ind[k]<-i;k<-k+1
  }
}
x$Rear.seat.room[ind]<-mean(na.omit(x$Rear.seat.room))


f<-table(x$DriveTrain)
m<-names(f)[f==max(f)] #calculation of mode

#replace missing value with random sampling
ind2<-0;p<-1
for (k in c(1:length(x$Luggage.room)))
{
  if (is.na(x$Luggage.room[k]))
  {
    ind2[p]<-k;p<-p+1
  }
  
}
x$Luggage.room[ind2]<-mean(na.omit(x$Luggage.room))

BL<-complete.cases(x$Luggage.room)
x$Luggage.room[!BL]
x$Luggage.room[!BL]<-sample(x$Luggage.room[BL],length(x$Luggage.room[!BL]))


x$Luggage.room[ind2]<-sample(na.omit(x$Luggage.room),length(ind2))













