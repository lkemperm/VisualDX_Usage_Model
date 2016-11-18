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
searchText <- c("hello VisualDX project test", "project test", "project test test ")
searchQueryCorpus <- Corpus(VectorSource(searchText))
searchVector <- inquiry_table[, inquiry_table$searchQuery]
testCorpus <- Corpus(VectorSource(searchMatrix))
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

