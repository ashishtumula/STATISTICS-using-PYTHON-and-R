#Perform z-test - When SD of population is known

install.packages("BSDA")
library(BSDA)

#IQ Test for students studying in class 10 is to be identified.
#A random sample of 10 students are chosen and IQ scores are calculated.
#The expected IQ levels for the entire class is 75
#The standard deviation in IQ for population is expected to be 4
#Need to verify if this sample represents true population mean of 75

IQ.Scores <- c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)

#Summary statistics
str(IQ.Scores)
summary(IQ.Scores)
mean(IQ.Scores)
sd(IQ.Scores)
var(IQ.Scores)

#Visual summary
hist(IQ.Scores)
boxplot(IQ.Scores)

z.test(x=IQ.Scores, mu=75, sigma.x= 4, alternative = "two.sided" )

#P-value is less than 0.05, hence we reject null hypothesis
#The average IQ scores of the population is not equal to 75 based on this sample

################
################
################
################

#Two sample z-test

#Heights of Italians and germans are compared
#SD of Italians height is known to be 2
#SD of Germans height is known to be 3

I.Height <- c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
G.Height <- c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)

#Summary statistics - Italians
str(I.Height)
summary(I.Height)
mean(I.Height)
sd(I.Height)
var(I.Height)

#Visual summary - Italians
hist(I.Height)
boxplot(I.Height)

#Summary statistics - Germans
str(G.Height)
summary(G.Height)
mean(G.Height)
sd(G.Height)
var(G.Height)

#Visual summary - Germans
hist(G.Height)
boxplot(G.Height)

#Perform 2 sample z-test

z.test(x=I.Height, y=G.Height, sigma.x=2,sigma.y = 3,alternative = "two.sided")

#p-value is less than 0.05, hence we reject null hypothesis
#Mean height of Italians is not the same as mean height of Germans

################
################
################
################

#One sample t test

bottles <- c(499.11,498.49,499.38,501.01,499.48,502.63,497.6,496.03,498.88,501.59,502.85,503.08,496.68,498.03,499.32,504.41,501.13,499.32,499.08,499.27)
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

t.test(bottles, mu=500, alternative="t", conf.level=0.95)

#p-value is greater than 0.05, hence do not reject null hypothesis

################
################
################
################


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


#two sample t test - Using the subset datasets group1 & group2
t.test(group1$extra, group2$extra,paired=T)

#p-value is greater than 0.05, hence do not reject null hypothesis

################
################
################
################

# We will use the built in "survey" dataset to check if smoking habits 
# and exercise frequency variables are independent of each other

survey

str(survey)
summary(survey)

# We will create a contingency table for the test
tbl <- table(survey$Smoke, survey$Exer)

# View the table
tbl


#Run ch-square test
chisq.test(tbl)

# We received a warning message as the cell values are too small 
# in their value. So, let us now combine none and some 
# categories under the exercise variable

ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 

#View the newly combined contingency table
ctbl

#Perform the chi-square test for the new table again.
chisq.test(ctbl)

# As the p-value 0.4828 is greater than the .05 significance level, 
# we do not reject the null hypothesis that the smoking habit is
# independent of the exercise level of the students.
