### load data 
VisualDx.Data <- read.csv("data/clientSession_2.csv")

### subset initial ClientSession table
keep <- c("productId", "agent", "sessionStart", "sessionEnd", 
          "currentLicenseCount", "imageCount", "diagnosisCount")
clientSession <- VisualDx.Data[keep]

### separate agent type: mobile vs. nonmobile
x <- c("mobile", "apple", "android")
mobile <- subset1[grep(paste(x, collapse = "|"), subset1$agent), ]

PC <- subset1[-(grep(paste(x, collapse = "|"), subset1$agent)), ]

### mobile time elapsed
mobile$sessionStart = as.POSIXct(mobile$sessionStart)
mobile$sessionEnd = as.POSIXct(mobile$sessionEnd)

### PC time elapsed
PC$sessionStart = as.POSIXct(PC$sessionStart)
PC$sessionEnd = as.POSIXct(PC$sessionEnd)

### create new column for duration
mobile["duration"] = (mobile$sessionEnd-mobile$sessionStart)
PC["duration"] = (PC$sessionEnd-PC$sessionStart)

### make duration numeric
mobile$duration<-as.numeric(mobile$duration)
PC$duration<-as.numeric(PC$duration)

### plot duration
library("ggplot2")
ggplot(aes(x=as.POSIXct(mobile$modTime), y=imageCount), data = mobile) + geom_line() 

p<-ggplot(mobile, aes(x = duration))
p + geom_histogram(stat = "bin", binwidth=1000)

### barplot
barplot(c(nrow(mobile), nrow(PC)), names.arg=c("Mobile", "PC"), main="Mobile and PC Usage",
        ylab="Number of Instances", col="dark green")

### differences in mobile/PC data
mobilesub <- subset(mobile, duration < 5000)
hist(mobilesub$duration)

PCsub <- subset(PC, duration < 5000)
hist(PCsub$duration)

### log transformation histograms
hist(log(mobile$duration))
plot(log(mobile$duration), mobile$imageCount)

### convert time
#look at x and y location scales
### MOBILE
mobilesub <- as.Date(mobile$sessionStart, "%Y-%m-%d")
mobiletot <- aggregate(imageCount~mobilesub, data=mobile, FUN=sum)
mobile2016 <- subset(mobiletot, mobilesub > as.Date("2015-12-31"))

plot(mobile2016$mobilesub, mobile2016$imageCount, type="l", main="Mobile Usage by Month", 
     xlab="Date", ylab="Image Count")

### PC
PCsub <- as.Date(PC$sessionStart, "%Y-%m-%d")
PCtot <- aggregate(imageCount~PCsub, data=PC, FUN=sum)
PC2016 <- subset(PCtot, PCsub > as.Date("2015-12-31"))

plot(PC2016$PCsub, PC2016$imageCount, type="l", main="Mobile Usage by Month", 
     xlab="Date", ylab="Image Count")

### OVERLAY
### 2016 (monthly)
plot(mobile2016$mobilesub, mobile2016$imageCount, type="l", main="Mobile and PC Image Count", 
     xlab="Month", ylab="Image Count", col="red", ylim=c(1946,230000), lwd=1.75)
lines(PC2016$PCsub, PC2016$imageCount, type="l", col="blue", lwd=1.75)
legend('topright', legend=c("Mobile", "PC") , lty=1, col=c('red', 'blue'), 
       bty='y', cex=.75, box.lty=1)

### 1 Day
mT <- as.POSIXct(mobile$sessionStart)
PCT <- as.POSIXct(PC$sessionStart)

mDay <- aggregate(imageCount~mT, data=mobile,FUN=sum)
mOneDay <- subset(mDay, mT <= as.POSIXct('2016-10-25 23:59') & mT >= as.POSIXct('2016-10-25 00:00'))

PCDay <- aggregate(imageCount~PCT, data=PC,FUN=sum)
PCOneDay <- subset(PCDay, PCT <= as.POSIXct('2016-10-29 23:59') & PCT >= as.POSIXct('2016-10-29 00:00'))

plot(mOneDay$PCT, mOneDay$imageCount, type="l", main="Mobile and PC Image Count", 
     xlab="Time", ylab="Image Count", col="red", lwd=1.75)
lines(PCOneDay$PCT, PCOneDay$imageCount)

### density plot of one day 11/1/16
### MULTIPLOT
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

require(ggplot2)
m <- ggplot(mOneDay, aes(mT)) + geom_density(kernel = "gaussian") + 
  xlab("Time of Day") + ylab("Density") + labs(title = "Mobile Density Plot: 11/01/16")
pc <- ggplot(PCOneDay, aes(PCT)) + geom_density(kernel = "gaussian") + 
  xlab("Time of Day") + ylab("Density") + labs(title = "PC Density Plot: 11/01/16")

multiplot(m, pc)

### aggregate by normal day
mDay["Date"] <- as.Date(mDay$mT)
mHoliday <- subset(mDay, Date == "2014-01-01" | Date == "2015-01-01" 
                   | Date == "2016-01-01" | Date == "2014-12-31" 
                   | Date == "2015-12-31" | Date == "2014-07-04"
                   | Date == "2015-07-04" | Date == "2016-07-04"
                   | Date == "2014-12-25" | Date == "2015-12-25")

df <- aggregate(imageCount~Date, data=mHoliday,FUN=sum)