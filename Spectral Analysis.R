detach(data) #this stops using any previously called data

filename<-"1610oct10.csv" #determines the name of the file with the relevant data
data<-read.csv(filename,header=T) #reads the data from the specified file, says that the first row is column headings
attach(data) #associates the data with the headings
library(MASS) #loads specified library for data analysis

a=60; #sets lower bound for histogram
b=80; #sets upper bound for histogram

bins=c(64,66,68,70,72,74,76,78); #bins define edges of histogram calsses
p=length(bins); #finds total number of bins
data.cut<-cut(PosLevel,bins); #cut the noise levels into bins

distr_est<-fitdistr(PosLevel,"normal") #fit the noise level to a normal distribution
mean_est<-distr_est$estimate[1] #find the mean of the data
sd_est<-distr_est$estimate[2] #find the standard deviation of the data

plot.vector<-bins[1:p] #plot the axes of the data
hist(PosLevel,breaks=bins,xlim=c(a,b),main=filename,ylab="Probability Density",freq=FALSE) #plot a histogram of the data
curve(dnorm(x,mean_est,sd_est),a,b,add=TRUE, col = "red", lwd = 2) #add the line to show how the data should be distributed

fit<-glm(PosLevel~MidRain+Freq) #fits the data to a normal model with the Midrain and Freq variables
summary(fit) #displays results of model