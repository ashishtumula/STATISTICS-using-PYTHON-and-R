# Poisson Distribution

# Probability that exactly 4 customers arrive at a given minute
dpois(4,lambda=3)

# Probability that exactly 3 customers arrive at every given minute

dpois(3,lambda = 3)

#Construct poisson distribution

poisx = c(dpois(0,lambda = 3),
          dpois(1,lambda = 3),
          dpois(2,lambda = 3),
          dpois(3,lambda = 3),
          dpois(4,lambda = 3),
          dpois(5,lambda = 3),
          dpois(6,lambda = 3),
          dpois(7,lambda = 3),
          dpois(8,lambda = 3),
          dpois(9,lambda = 3),
          dpois(10,lambda = 3),
          dpois(11,lambda = 3),
          dpois(12,lambda = 3))

# Perform above action using FOR loop - Class Activity




#====================================================
poisx = c()
x = c(0,1,2,3,4,5,6,7,8,9,10,11,12)

for (i in x) {
  poisx = c(poisx,
    dpois(i,lambda = 3)
  )
}
#====================================================


x = c(0,1,2,3,4,5,6,7,8,9,10,11,12)

plot(x,poisx,type="b")

# Plot cumulative poisson distribution

cpoisx = c(ppois(0,lambda = 3),
           ppois(1,lambda = 3),
           ppois(2,lambda = 3),
           ppois(3,lambda = 3),
           ppois(4,lambda = 3),
           ppois(5,lambda = 3),
           ppois(6,lambda = 3),
           ppois(7,lambda = 3),
           ppois(8,lambda = 3),
           ppois(9,lambda = 3),
           ppois(10,lambda = 3),
           ppois(11,lambda = 3),
           ppois(12,lambda = 3))
  

plot(x,cpoisx,type="b")



#If there are twelve cars crossing a bridge 
#per minute on average, find the probability 
#of having seventeen or more cars crossing the 
#bridge in a particular minute.