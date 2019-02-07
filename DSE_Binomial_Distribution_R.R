# Binomial Distribution

# Compute the Binomial Distribution Probability values
dbinom(0,size=7,prob=0.6)
dbinom(1,size=7,prob=0.6)
dbinom(2,size=7,prob=0.6)
dbinom(3,size=7,prob=0.6)
dbinom(4,size=7,prob=0.6)
dbinom(5,size=7,prob=0.6)
dbinom(6,size=7,prob=0.6)
dbinom(7,size=7,prob=0.6)

# Compute cumulative probability for binomial distribution

pbinom(0, size=7, prob=0.6)
pbinom(1, size=7, prob=0.6)
pbinom(2, size=7, prob=0.6)
pbinom(3, size=7, prob=0.6)
pbinom(4, size=7, prob=0.6)
pbinom(5, size=7, prob=0.6)
pbinom(6, size=7, prob=0.6)
pbinom(7, size=7, prob=0.6)


# Plot the 2 charts

#Plot the probability distributions

px = c(dbinom(0,size=7,prob=0.6),
       dbinom(1,size=7,prob=0.6),
       dbinom(2,size=7,prob=0.6),
       dbinom(3,size=7,prob=0.6),
       dbinom(4,size=7,prob=0.6),
       dbinom(5,size=7,prob=0.6),
       dbinom(6,size=7,prob=0.6),
       dbinom(7,size=7,prob=0.6))

x = c(0,1,2,3,4,5,6,7)

plot(x,px,type="b")


# Plot the cumulative probability distributions


cpx = c(pbinom(0, size=7, prob=0.6),
        pbinom(1, size=7, prob=0.6),
        pbinom(2, size=7, prob=0.6),
        pbinom(3, size=7, prob=0.6),
        pbinom(4, size=7, prob=0.6),
        pbinom(5, size=7, prob=0.6),
        pbinom(6, size=7, prob=0.6),
        pbinom(7, size=7, prob=0.6))

plot(x,cpx,type="b")



