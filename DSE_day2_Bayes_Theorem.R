# Statistical Methods for Decision Making [DSE-Hyd]
# Prepared by Faculty: Mahesh Anand.S
rm(list=ls())
x<-read.csv('heart.csv')
#Label-1: No Heart Disease
#Label-2: Heart Disease
CT<-table(x$exercise,x$disease)
#Label-1: No Exercise
#Label-0: Exercise
colnames(CT)<-c('NHD','HD')
rownames(CT)<-c('Exercise','No Exercise')
#Using Baye's Theorem, calculate P(HeartDisease/Exercise)
# P(Exercise/HD)*P(HD)
# --------------------
#       P(Exercise)
p_hd_bar_exercise<-(CT[1,2]/sum(CT[,2]) * sum(CT[,2]))/sum(CT[1,])
print(p_hd_bar_exercise)

