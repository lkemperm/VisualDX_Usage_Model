inquiry_table <- read.csv('data/merged_inquiries.csv', stringsAsFactors = FALSE)
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))

inquiry_table$inquiryTypeId <- as.numeric(inquiry_table$inquiryTypeId)
inquiry_table$ddxModuleId[inquiry_table$ddxModuleId == "NULL"] <- "NONE"
inquiry_table$diagnosisLookupId[inquiry_table$diagnosisLookupId == "NULL"] <- "NONE"
unique(unlist(inquiry_table$applicationAnswerId, use.names = FALSE))
inquiry_table$applicationAnswerId <- as.numeric(inquiry_table$applicationAnswerId)
unique(unlist(inquiry_table$outcomeAnswerId, use.names = FALSE))
inquiry_table$outcomeAnswerId <- as.numeric(inquiry_table$outcomeAnswerId)

inquiry_table <- na.omit(inquiry_table)
inquiry_table$startTime <- as.POSIXct(inquiry_table$startTime)
inquiry_table$endTime <- as.POSIXct(inquiry_table$endTime)

inquiry_table["duration"] = (inquiry_table$endTime - inquiry_table$startTime)

inquiry_table$searchQuery[inquiry_table$searchQuery == "NULL"] <- "NONE"
inquiry_table$duration <- as.numeric(inquiry_table$duration)
hist(inquiry_table$duration)

range(inquiry_table$duration)
breaks = seq(0, 24000, by=3000)
duration.cut = cut(inquiry_table$duration, breaks, right=FALSE)
duration.freq = table(duration.cut)
cbind(duration.freq)
hist(log(duration.freq))
barplot(duration.freq, main = "Barplot of duration intervals")

hist(log(inquiry_table$duration))
boxplot(log(inquiry_table$duration), main = "Distribution of log inquiry duration")
# explore relationship between other features 
dm_id <- inquiry_table$ddxModuleId
dl_id <- inquiry_table$diagnosisLookupId
aa_id <- inquiry_table$applicationAnswerId
oa_id <- inquiry_table$outcomeAnswerId
d <- data.frame(dm_id, dl_id, aa_id, oa_id)
# check out this table where the diagnosis lookup ID is NONE 
d2 <- d[which(dl_id == "NONE"),]
# correlation of numeric features 
cor(aa_id, oa_id)
# check out table where both ddx module id and diagnosis lookup ID are NONE 
d3 <- d[which(dl_id == "NONE" && dm_id == "NONE"),]
# check out table where application answer ID is not 0 (apparent most common value)
d4 <- d[which(aa_id != 0),]
d5 <- d[which(oa_id != 0),]
d6 <- d[which(aa_id == 1),]
d7 <- d[which(dm_id != "NONE" && dl_id != "NONE"),]
nrow(d5)
nrow(d4)
nrow(d)
# look at distribution of times 
boxplot(inquiry_table$startTime)
hist(inquiry_table$startTime, breaks = "days")
# split up by year 
hist(inquiry_table$startTime, breaks = "years")

library(ggplot2)
ggplot(inquiry_table, aes(startTime)) +
  geom_density()
ggplot(inquiry_table, aes(startTime, duration) ) +
  geom_jitter()

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
events_table <- subset(events_table, select = -c(eventId, moduleId, sessionId, serverName))

events_table$imageId[events_table$imageId != "NULL"] <- 1
events_table$imageId[events_table$imageId == "NULL"] <- 0
events_table$imageId <- as.numeric(events_table$imageId)

events_table$diagnosisId[events_table$diagnosisId != "NULL"] <- 1
events_table$diagnosisId[events_table$diagnosisId == "NULL"] <- 0
events_table$diagnosisId <- as.numeric(events_table$diagnosisId)

events_table$controlId[events_table$controlId == "NULL"]<- 0
events_table$controlId <- as.numeric(events_table$controlId)
events_table$eventTypeId <- as.numeric(events_table$eventTypeId)

events_table$activeViewId[events_table$activeViewId == "NULL"] <- 0
events_table$activeViewId <- as.numeric(events_table$activeViewId)

events_table <- na.omit(events_table)
events_table$time <- as.POSIXct(events_table$time)
hist(events_table$eventTypeId)

ggplot(events_table, aes(diagnosisId, fill=eventTypeId), col = c(1:10)) +
  geom_bar(position="fill")

ggplot(events_table, aes(eventTypeId)) + geom_bar() +
  facet_wrap(~ diagnosisId)

cor(events_table$imageId, events_table$diagnosisId)

ggplot(events_table, aes(eventTypeId)) +
  geom_freqpoly(aes(group = diagnosisId, colour = diagnosisId))

ggplot(events_table, aes(time)) +
  geom_freqpoly(aes(group = eventTypeId, colour = eventTypeId))

ggplot(events_table, aes(eventTypeId)) +
  geom_freqpoly(aes(group = activeViewId, colour = activeViewId))

cor(events_table$eventTypeId, events_table$activeViewId)

ggplot(events_table,
       aes(diagnosisId, eventTypeId )) +
  geom_point() + geom_boxplot()

plot(events_table$imageId, events_table$diagnosisId)
table<- with(events_table, table(imageId, diagnosisId))
mosaicplot(table, col = hcl(c(240, 120)),
           off = c(5, 5, 5, 5), main = "Diagnosis ID and Image ID")
avID_imgID <- with(events_table, table(imageId, activeViewId))
addmargins(avID_imgID)
av_comp <- prop.table(avID_imgID, 1)
img_comp <- prop.table(avID_imgID, 2)