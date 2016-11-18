# load inquiry data 
inquiry_table <- read.csv('data/merged_inquiries.csv', stringsAsFactors = FALSE)
# drop columns that we are not interested in for now from inquiry table 
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))
# this cuts off the time 
inquiry_table$startTime <- format(inquiry_table$startTime, format="%m-%d-%Y-%R%p")
inquiry_table$endTime <- format(inquiry_table$endTime, format="%m-%d-%Y-%R%p")
# create column for duration 
inquiry_table["duration"] = (inquiry_table$endTime - inquiry_table$startTime)
# replace null with none for search queries 
inquiry_table$searchQuery[inquiry_table$searchQuery == "NULL"] <- "NONE"
# histogram of inquiry duration after turning data into numeric
inquiry_table$duration <- as.numeric(inquiry_table$duration)
hist(inquiry_table$duration)
# try: making word cloud for search queries 
# simplify text data 
library(tm)
library(SnowballC)
library(wordcloud)
searchVector <- inquiry_table[, "searchQuery", drop = FALSE]
topQueries <- sort(table(searchVector), decreasing = TRUE)[1:50]
topQueries <- topQueries[-1]
topVector <- rep(names(topQueries), topQueries[names(topQueries)])
# remove "NONE" observations 
searchQueryCorpus <- Corpus(VectorSource(topVector))
searchQueryCorpus <- tm_map(searchQueryCorpus, PlainTextDocument)
searchQueryCorpus <- tm_map(searchQueryCorpus, removePunctuation)
searchQueryCorpus <- tm_map(searchQueryCorpus, removeWords, stopwords('english'))
searchQueryCorpus <- tm_map(searchQueryCorpus, stemDocument)
pal <- brewer.pal(8, "Dark2")
wordcloud(searchQueryCorpus, max.words = 100, random.order = FALSE, min.freq = 1, colors = pal)

# load events data 
events_table <- read.csv('data/merged_events.csv', stringsAsFactors = FALSE)
# drop columns that we are not interested in for now from events table 
events_table <- subset(events_table, select = -c(eventId, moduleId, sessionId, serverName))
# count null image IDs, diagnosis IDS to see if this is useful
# i.e. if there is a relationship between image ID being null and another feature
sum((events_table$imageId) == "NULL")
sum((events_table$diagnosisId) == "NULL")
# replace null values with 0, otherwise 1 (8502938 null objects)
events_table$imageId[events_table$imageId != "NULL"] <- 1
events_table$imageId[events_table$imageId == "NULL"] <- 0
events_table$imageId <- as.numeric(events_table$imageId)
# do the same for diagnosis ID 
events_table$diagnosisId[events_table$diagnosisId != "NULL"] <- 1
events_table$diagnosisId[events_table$diagnosisId == "NULL"] <- 0
events_table$diagnosisId <- as.numeric(events_table$diagnosisId)
# many null controlIds: may be easier to interpret as 0's 
events_table$controlId[events_table$controlId == "NULL"]<- 0
events_table$controlId <- as.numeric(events_table$controlId)
events_table$eventTypeId <- as.numeric(events_table$eventTypeId)
# nulls in active view ID also should be interpreted as 0's 
events_table$activeViewId[events_table$activeViewId == "NULL"] <- 0
events_table$activeViewId <- as.numeric(events_table$activeViewId)
# convert time to date object
events_table$time <- as.POSIXct(events_table$time)
# test length and length with na.omit 
dim(events_table)
test<- na.omit(events_table)
dim(test)
# this removed coerced NA values in controlId when converted to numeric... OK 
events_table <- na.omit(events_table)
# try: plot relationship between imageID and diagnosisID 
plot(events_table$imageId, events_table$diagnosisId)
# too slow! try another solution: histogram of counts where we have both an imageID and diagnosisID
# try all possible combinations 
sum(events_table$imageId == 1 & events_table$diagnosisId == 1)
sum(events_table$imageId == 0 & events_table$diagnosisId == 0)
sum(events_table$imageId == 1 & events_table$diagnosisId == 0)
sum(events_table$imageId == 0 & events_table$diagnosisId == 1)
with(events_table, table(imageId, diagnosisId))
hist(sum(events_table[events_table$imageId == 1 & events_table$diagnosisId ==1,]), 
     main = 'Image ID and diagnosis ID of 1')