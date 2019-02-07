#One sample t test

bottles <- c(484.11,459.49,471.38,512.01,494.48,528.63,493.6,448.03,473.88,501.59,502.85,538.08,465.68,495.03,475.32,529.41,518.13,464.32,449.08,489.27)
bottles

#Summary statistics
str(bottles)
summary(bottles)
mean(bottles)
sd(bottles)
var(bottles)

#Visual summary
hist(bottles)
boxplot(bottles)

#Conducting the one sample t test
?t.test

t.test(bottles, mu=500, alternative="t", conf.level=0.95)
t.test(bottles, mu=500, alternative="g", conf.level=0.95)
t.test(bottles, mu=500, alternative="l", conf.level=0.95)



#Now, we will use power t test to calculate Power of test

#Calculate delta value
mean(bottles) - 500

#Calculate Standard Deviation
sd(bottles)

#Calculate power of test at 5% significance level
power.t.test(n=20,delta=-10.28,sd=26.62018, alternative="two.sided",sig.level=0.05)
?power.t.test

#Calculate the number of observations needed for power of 80% minimum
power.t.test(power=.8,delta=-10.28,sd=26.62018, alternative="two.sided",sig.level=0.05)

###################################


#Let us use sleep dataset to understand paired two sample t test

library(help="datasets")

?sleep
sleep

#Subsetting the data into 2 column values

group1 <- sleep[1:10,]
group1

group2 <- sleep[11:20,]
group2

#Calculate variance and standard deviation
mean(group1$extra)
var(group1$extra)
sd(group1$extra)

mean(group2$extra)
var(group2$extra)
sd(group2$extra)

#two sample t test - Using sleep dataset as is it
with(sleep,t.test(extra[group==1],extra[group==2],paired=T, alternative = "g"))


#two sample t test - Using the subset datasets group1 & group2
t.test(group1$extra, group2$extra,paired=T)


#Note - for unpaired t test, do not use the paired parameter in t.test

#Calculate the power of test

#Calculate delta value
mean(group1$extra) - mean(group2$extra)

#Calculate pooled SD
pooledSD <- (((10-1)*(1.78901^2)+(10-1)*(2.0023^2))/(10+10-2))^0.5
pooledSD


#Calculate power of test
?power.t.test
power.t.test(n=10, delta = -1.58, sd=1.898652,
             sig.level = 0.05,type = "paired",
             alternative = "two.sided" )

#Calculate number of observations required for power 80% or .80
power.t.test(power=.80, delta = -1.58, sd=1.898652,
             sig.level = 0.05,type = "p",
             alternative = "two.sided" )

