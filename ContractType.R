
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

### 4 - not working
contract2$modTime <- as.Date(contract2$modTime, '%m/%d/%Y')
require(ggplot2)
ggplot(data = contract2, aes(as.POSIXct(modTime), clientId)) + geom_line()

### 5 - not working
install.packages('xts')
require(xts)
pcp <- rnorm(24)
PCP <- ts(pcp, frequency = 12, start = 2014)
plot(as.xts(PCP), major.format = "%b-%y")


a<-ts(rnorm(150),start=c(2002,7),freq=12);a
plot(a, type="l", lwd=2, col="red", ylab= "% return",xlim=c(2002,2014),axes=F)
axis(1,at=2002:2014,labels=2002:2014);axis(2);box()


barplot(c(nrow(contract1), nrow(contract2),nrow(contract3), nrow(contract4), nrow(contract5),
          nrow(contract6), nrow(contract8)), col="red", main="Number of Instances vs. Contract Type ID",
        xlab="Contract Type ID", ylab="Number of Instances", names.arg=c(1, 2, 3, 4, 5, 6, 8))


