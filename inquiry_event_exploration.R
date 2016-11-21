# load inquiry data 
inquiry_table <- read.csv('data/merged_inquiries.csv', stringsAsFactors = FALSE)
# drop columns that we are not interested in for now from inquiry table 
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))
# turn inquiry type ID column into numeric data 
inquiry_table$inquiryTypeId <- as.numeric(inquiry_table$inquiryTypeId)
inquiry_table$ddxModuleId[inquiry_table$ddxModuleId == "NULL"] <- "NONE"

inquiry_table$diagnosisLookupId[inquiry_table$diagnosisLookupId == "NULL"] <- "NONE"
# look at possible values for application answer ID 
unique(unlist(inquiry_table$applicationAnswerId, use.names = FALSE))
# convert to numeric data 
inquiry_table$applicationAnswerId <- as.numeric(inquiry_table$applicationAnswerId)
# same for outcome answer ID 
unique(unlist(inquiry_table$outcomeAnswerId, use.names = FALSE))
inquiry_table$outcomeAnswerId <- as.numeric(inquiry_table$outcomeAnswerId)
# remove NA values
inquiry_table <- na.omit(inquiry_table)
inquiry_table$startTime <- as.POSIXct(inquiry_table$startTime)
inquiry_table$endTime <- as.POSIXct(inquiry_table$endTime)
# create column for duration 
inquiry_table["duration"] = (inquiry_table$endTime - inquiry_table$startTime)
# replace null with none for search queries 
inquiry_table$searchQuery[inquiry_table$searchQuery == "NULL"] <- "NONE"
# histogram of inquiry duration after turning data into numeric
inquiry_table$duration <- as.numeric(inquiry_table$duration)
hist(inquiry_table$duration)
# get range in seconds, split range into sub intervals 
range(inquiry_table$duration)
breaks = seq(0, 24000, by=250)
duration.cut = cut(inquiry_table$duration, breaks, right=FALSE)
duration.freq = table(duration.cut)
cbind(duration.freq)
hist(log(duration.freq))
barplot(duration.freq, main = "Barplot of duration intervals")
# skewed towards very short durations: try logarithm 
hist(log(inquiry_table$duration))
# boxplot of duration 
boxplot(log(inquiry_table$duration), main = "Distribution of log inquiry duration")

# try: making word cloud for search queries 
# simplify text data 
library(tm)
library(SnowballC)
library(wordcloud)
searchVector <- inquiry_table[, "searchQuery", drop = FALSE]
topQueries <- sort(table(searchVector), decreasing = TRUE)[1:50]
# remove "NONE" observations 
topQueries <- topQueries[-1]
topVector <- rep(names(topQueries), topQueries[names(topQueries)])
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
# test length and length with na.omit 
dim(events_table)
test<- na.omit(events_table)
dim(test)
# this removed coerced NA values in controlId when converted to numeric... OK 
events_table <- na.omit(events_table)
# convert time to date object
events_table$time <- as.POSIXct(events_table$time)
# try: plot relationship between imageID and diagnosisID 
plot(events_table$imageId, events_table$diagnosisId)
# too slow! try another solution: histogram of counts where we have both an imageID and diagnosisID
# table of these combinations
table<- with(events_table, table(imageId, diagnosisId))
mosaicplot(table, col = hcl(c(240, 120)),
           off = c(5, 5, 5, 5), main = "Diagnosis ID and Image ID")
avID_imgID <- with(events_table, table(imageId, activeViewId))
addmargins(avID_imgID)
av_comp <- prop.table(avID_imgID, 1)
img_comp <- prop.table(avID_imgID, 2)