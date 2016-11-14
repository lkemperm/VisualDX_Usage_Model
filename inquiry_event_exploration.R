# load inquiry data 
inquiry_table <- read.csv('data/merged_inquiries.csv')
# drop columns that we are not interested in for now from inquiry table 
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))
# create column for duration 
inquiry_table$startTime = as.POSIXct(inquiry_table$startTime)
merged_table$endTime = as.POSIXct(merged_table$endTime)
inquiry_table["duration"] = inquiry_table

# load events data 
events_table <- read.csv('data/merged_events.csv')
# drop columns that we are not interested in for now from events table 
events_table <- subset(events_table, select = -c(eventId, moduleId, diagnosisId, sessionId, serverName))
# count na image IDs to see if this is useful, i.e. if there is a relationship between image ID being null and another feature
sum((events_table$imageId) == "NULL")
# replace null values with 0, otherwise 1 
events_table[events_table$imageId == "NULL"]<-0
events_table[events_table$imageId != "NULL"]<-1
# make sure it worked 
levels(events_table$imageId)
