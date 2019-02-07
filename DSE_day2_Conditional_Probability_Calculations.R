# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S
rm(list=ls())
x<-read.table('HR.txt',header = TRUE,sep='\t',row.names = 1)
#Marginal Probability calculation
CT<-table(x$Attrition,x$Gender)
colnames(CT)<-c('MALE','FEMALE')
#1.What is the probability that a randomly selected employee 
#in the organisation is a Female?
p_female<-CT[1,2]/nrow(x)
#2.What is the probability that a randomly selected employee 
#leaving an organisation? [ Attrition probability]
p_leaving<-sum(CT[2,])/nrow(x)
#3.What is the probability that a randomly selected employee 
#is a Male & also leaving an organisation?
p_male_leaving<-CT[2,1]/nrow(x)
#4. An employee selected at random is found to be left the organisation,
#what is the probability that the employee is a Male?
#[Conditional Probability: P(Male/Yes)]
p_male_bar_yes<-CT[2,1]/sum(CT[2,])


