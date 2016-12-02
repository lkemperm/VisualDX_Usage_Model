### load data 
VisualDx.Data <- read.csv("clientSession(2014-2016).csv")

### subset initial ClientSession table
keep <- c("productId", "agent", "sessionStart", "sessionEnd", 
          "currentLicenseCount", "imageCount", "diagnosisCount")
subset1 <- VisualDx.Data[keep]

### separate agent type: mobile vs. nonmobile
x <- c("mobile", "apple", "android")
mobile <- subset1[grep(paste(x, collapse = "|"), subset1$agent), ]

PC <- subset1[-(grep(paste(x, collapse = "|"), subset1$agent)), ]

### mobile time elapsed
mobile$sessionStart = as.POSIXct(mobile$sessionStart)
mobile$sessionEnd = as.POSIXct(mobile$sessionEnd)

### create new column for duration
mobile["duration"] = (mobile$sessionEnd-mobile$sessionStart)

### make duration numeric
mobile$duration<-as.numeric(mobile$duration)

### plot duration
library("ggplot2")
ggplot(aes(x=as.POSIXct(mobile$modTime), y=imageCount), data = mobile) + geom_line() 

p<-ggplot(mobile, aes(x = duration))
p + geom_histogram(stat = "bin", binwidth=1000)

### barplot
barplot(c(nrow(mobile), nrow(PC)), names.arg=c("Mobile", "PC"), main="Mobile and PC Usage",
        ylab="Number of Instances", col="dark green")

