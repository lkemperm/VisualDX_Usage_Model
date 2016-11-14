### load data 
VisualDx.Data <- read.csv("VisualDx Data.csv")

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
