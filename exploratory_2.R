df <- read.csv("visualDX_table.csv")
names <- colnames(df)

# drop columns that we are not interested in for now (columns that just have ids...)
df <- subset(df, select = -c(eventId, controlId, sessionId, clientId, clientId.1, siteId, inquiryId, eventId.1, serverName.1, serverName.2))

# make histogram of eventTypeIds 
hist(df$eventTypeId)

# histogram of image count 
hist(df$imageCount)

# histogram of diagnosis count 
hist(df$diagnosisCount)

# plot image count vs. diagnosis count 
plot(df$imageCount, df$diagnosisCount)

# histogram of event type ID 
hist(df$eventTypeId)

# histogram of inquiry type ID 
hist(df$inquiryTypeId)

# nonzero_image_count<-which(df$imageCount != 0)
df$sessionStart = as.POSIXct(df$sessionStart)
df$sessionEnd = as.POSIXct(df$sessionEnd)
# create new column for duration
df["duration"] = (df$sessionEnd-df$sessionStart)
plot(df$duration, df$imageCount)
df$duration <- as.numeric(df$duration)
hist(df$duration)
boxplot(df$duration, horizontal = TRUE)

require(ggplot2)
ggplot(aes(x=as.POSIXct(modTime), y=imageCount), data = df) + geom_line()

# require(ggplot2)
# df<-df[1:20,]
# ggplot(df, aes(modTime, imageCount))+
#   geom_line()
# 
# qplot(modTime, imageCount, data=df, geom="path")



