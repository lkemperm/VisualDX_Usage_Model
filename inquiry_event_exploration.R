# load inquiry data 
inquiry_table <- read.csv('data/merged_inquiries.csv', stringsAsFactors = FALSE)
# drop columns that we are not interested in for now from inquiry table 
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))
inquiry_table$startTime <- as.POSIXct(as.numeric(inquiry_table$startTime), origin = "1970-01-01", na.rm = TRUE)
inquiry_table$endTime <- as.POSIXct(as.numeric(inquiry_table$endTime), origin = "1970-01-01", na.rm = TRUE)
# create column for duration 
inquiry_table["duration"] = (inquiry_table$endTime - inquiry_table$startTime)
# replace null with none for search queries 
inquiry_table$searchQuery[inquiry_table$searchQuery == "NULL"] <- "NONE"
# try: making word cloud for search queries 
# simplify text data 
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
# searchText <- c("hello VisualDX project test", "project test", "project test test ")
# searchQueryCorpus <- Corpus(VectorSource(searchText))
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
# do the same for diagnosis ID 
events_table$diagnosisId[events_table$diagnosisId != "NULL"] <- 1
events_table$diagnosisId[events_table$diagnosisId == "NULL"] <- 0