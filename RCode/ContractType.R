
### read files
contract1 <- read.csv("ContractTypeId=1.csv")
contract2 <- read.csv("ContractTypeId=2.csv")
contract3 <- read.csv("ContractTypeId=3.csv")
contract4 <- read.csv("ContractTypeId=4.csv")
contract5 <- read.csv("ContractTypeId=5.csv")
contract6 <- read.csv("ContractTypeId=6.csv")
contract8 <- read.csv("ContractTypeId=8.csv")

### time elapsed 
contract2$sessionStart = as.POSIXct(contract2$sessionStart)
contract2$sessionEnd = as.POSIXct(contract2$sessionEnd)

### create new column for duration
contract2["duration"] = (contract2$sessionEnd - contract2$sessionStart)
contract2$duration <- as.numeric(contract2$duration)

### plots
### 1
require(ggplot2)
ggplot(aes(x=as.POSIXct(modTime), y=imageCount), data = contract2) + geom_line()

### 2
logTrans<-log(as.numeric(contract2$duration))
plot(logTrans, contract2$ImageCount)
hist(logTrans)

### 3
hist(log(contract2$imageCount), main="Histogram of the Logarithm of Image Count", xlab = "Log(ImageCount)")

### 4 - barplot
barplot(c(nrow(contract1), nrow(contract2),nrow(contract3), nrow(contract4), nrow(contract5),
          nrow(contract6), nrow(contract8)), col="red", main="Number of Instances vs. Contract Type ID",
        xlab="Contract Type ID", ylab="Number of Instances", names.arg=c(1, 2, 3, 4, 5, 6, 8))

### time series testing
#install.packages('xts')
#require(xts)
#pcp <- rnorm(24)
#PCP <- ts(pcp, frequency = 12, start = 2014)
#plot(as.xts(PCP), major.format = "%b-%y")

#a<-ts(rnorm(150),start=c(2002,7),freq=12);a
#plot(a, type="l", lwd=2, col="red", ylab= "% return",xlim=c(2014,2016),axes=F)
#axis(1,at=2002:2014,labels=2002:2014);axis(2);box()


### plot of time series data
table(format(rdate,"%Y-%m-%d"))

# CT 1
rdate1 <- as.Date(contract1$modTime, "%Y-%m-%d")
byday1 <- aggregate(imageCount~rdate1, data=contract1,FUN=sum)

# CT 8
rdate8 <- as.Date(contract8$modTime, "%Y-%m-%d")
byday8 <- aggregate(imageCount~rdate8, data=contract8,FUN=sum)

plot(subset1$rdate1, subset1$imageCount, type="l", col="red", main="Contract Type 1 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count", breaks = "days")

plot(d1, subset1$imageCount, type="l", col="red", main="Contract Type 1 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count", breaks = "days")

plot(byday8$rdate8, byday8$imageCount, type="l", col="blue", main="Contract Type 8 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count")

subset1 <- subset(byday1, rdate1 > as.Date("2016-11-19"))

subset1 <- subset(d1, rdate1 > as.Date("2016-11-19"))
d1 <- as.POSIXct(contract1$modTime)
test1 <- aggregate(imageCount~d1, data=contract1,FUN=sum)

df1 <- subset(test1, d1 > as.POSIXct('2016-11-19 23:59'))
plot(df1$d1, df1$imageCount, type="l", col="red", main="Contract Type 1: 2016-11-20", 
     xlab="Date", ylab="Image Count")

# CT 2
rdate2 <- as.Date(contract2$modTime, "%Y-%m-%d")
byday2 <- aggregate(imageCount~rdate2, data=contract2,FUN=sum)

plot(byday2$rdate2, byday2$imageCount, type="l", col="dark green", main="Contract Type 2 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count")

# CT 3
rdate3 <- as.Date(contract3$modTime, "%Y-%m-%d")
byday3 <- aggregate(imageCount~rdate3, data=contract3,FUN=sum)

plot(byday3$rdate3, byday3$imageCount, type="l", main="Contract Type 3 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count")

# CT 4
rdate4 <- as.Date(contract4$modTime, "%Y-%m-%d")
byday4 <- aggregate(imageCount~rdate4, data=contract4,FUN=sum)

plot(byday4$rdate4, byday4$imageCount, type="p", main="Contract Type 4 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count")

# CT 5
rdate5 <- as.Date(contract5$modTime, "%Y-%m-%d")
byday5 <- aggregate(imageCount~rdate5, data=contract5,FUN=sum)
subset5 <- subset(byday5, rdate5 > as.Date("2015-12-31"))

plot(subset5$rdate5, subset5$imageCount, type="l", main="Contract Type 5 Total Image Count Per Day (2016)", 
     xlab="Date", ylab="Image Count", col="darkorchid4")

# CT 6
rdate6 <- as.Date(contract6$modTime, "%Y-%m-%d")
byday6 <- aggregate(imageCount~rdate6, data=contract6,FUN=sum)
subset6 <- subset(byday6, rdate6 > as.Date("2015-12-31"))

plot(byday6$rdate6, byday6$imageCount, type="l", main="Contract Type 6 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count", col="orange")

# CT 8
rdate8 <- as.Date(contract8$modTime, "%Y-%m-%d")
byday8 <- aggregate(imageCount~rdate8, data=contract8,FUN=sum)
subset8 <- subset(byday8, rdate8 > as.Date("2015-12-31"))

plot(byday8$rdate8, byday8$imageCount, type="l", main="Contract Type 8 Total Image Count Per Day", 
     xlab="Date", ylab="Image Count", col="blue")
