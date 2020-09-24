#CLEARING ENVIRONMENT
rm(list = ls())

# LOAD PACKAGE
library(qcc)

# DATA LOADING
data <- read.table('/Users/pc/iCloudDrive/EDX/Introduction to Analytics Modelling/Fall2020hw3/data 6.2/temps.txt',stringsAsFactors = FALSE, header = T) 

# QUICK LOOK FOR LOADED DATA
head(data)

#FILTER OUT FIRST COLUMN (DATE COLUMN)
data <- data[,2:21]
head(data)

#CONVERT LIST OBJECT TO MATRIX FOR EASIER MANIPULATION
datam <- matrix(data)
datav <- as.vector(unlist(datam))
#COVERT DATA TO TIME SERIES
datas <- ts(datav, start=1996, frequency = nrow(data))
plot(datas)

#AVERAGE TEMP DURING JULY-AUGUST 
julaugav <- rep(1:ncol(data))
for(i in 1:ncol(data)){
    julaugav[i] <- mean(data[1:61,i])}

#AVERAGE AND STANDARD DEVIATION OF JULY/AUGUST TEMPERATURES FOR ALL YEARS
julaugav_all <- mean(julaugav[1:ncol(data)])
julaugav_all
julaugsd_all <- sd(julaugav[1:ncol(data)])
julaugsd_all

# AVERAGE DAILY TEMP FOR 1996-2015  
ADT <- rep(1:nrow(data))
for(i in 1:nrow(data)){
  ADT <- rowMeans(data)}


#CUSUM IMPLEMENTAION
Mu <- julaugav_all #TEMP AVERAGE JULY/AUGUST
C <- julaugsd_all #STANDARD DEV. JULY/AUGUST TEMP
Tr <- julaugsd_all*2 #ASSUMED THRESHOLD TEMP CHANGE, 3 TIMES OF STANDARD DEV TEMP
Mu
C
Tr

#RESULTS: 
ST <- cusum(data[1:65,], center=Mu, std.dev = C, head.start = 0, decision.interval = Tr, se.shift = 3, plot=TRUE)



