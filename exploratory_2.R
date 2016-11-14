merged_table <- read.csv("visualDX_table.csv")

# drop columns that we are not interested in for now (columns that just have ids...)
merged_table <- subset(merged_table, select = -c(eventId, controlId, sessionId, clientId, clientId.1, siteId, inquiryId, eventId.1, serverName.1, serverName.2))

# make histogram of eventTypeIds 
hist(merged_table$eventTypeId)

# histogram of image count 
hist(merged_table$imageCount)

# histogram of diagnosis count 
hist(merged_table$diagnosisCount)

# plot image count vs. diagnosis count 
plot(merged_table$imageCount, df$diagnosisCount)

# histogram of event type ID 
hist(merged_table$eventTypeId)

# histogram of inquiry type ID 
hist(merged_table$inquiryTypeId)

# nonzero_image_count<-which(df$imageCount != 0)
merged_table$sessionStart = as.POSIXct(merged_table$sessionStart)
merged_table$sessionEnd = as.POSIXct(merged_table$sessionEnd)
# create new column for duration
merged_table["duration"] = (merged_table$sessionEnd-merged_table$sessionStart)
plot(merged_table$duration, merged_table$imageCount)
merged_table$duration <- as.numeric(merged_table$duration)
hist(merged_table$duration)
boxplot(merged_table$duration, horizontal = TRUE)

require(ggplot2)
ggplot(aes(x=as.POSIXct(modTime), y=imageCount), data = merged_table) + geom_line()

# require(ggplot2)
# df<-df[1:20,]
# ggplot(df, aes(modTime, imageCount))+
#   geom_line()
# 
# qplot(modTime, imageCount, data=df, geom="path")

# load inquiry data 
inquiry_table <- read.csv('visualDX_data/merged_inquiries.csv')
# drop columns that we are not interested in for now from inquiry table 
inquiry_table <- subset(inquiry_dat, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))

