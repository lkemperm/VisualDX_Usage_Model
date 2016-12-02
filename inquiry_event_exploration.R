# read table and subset data 
inquiry_table <- read.csv('data/merged_inquiries.csv', stringsAsFactors = FALSE)
inquiry_table <- subset(inquiry_table, select = -c(inquiryId, certificateId, serverName, eventId, clientId, answerDate))

# replace null calues 
inquiry_table$ddxModuleId[inquiry_table$ddxModuleId == "NULL"] <- "NONE"
inquiry_table$diagnosisLookupId[inquiry_table$diagnosisLookupId == "NULL"] <- 0
inquiry_table$searchQuery[inquiry_table$searchQuery == "NULL"] <- "NONE"

# convert numeric columns to numeric
inquiry_table$inquiryTypeId <- as.numeric(inquiry_table$inquiryTypeId)
inquiry_table$diagnosisLookupId <- as.numeric(inquiry_table$diagnosisLookupId)
inquiry_table$applicationAnswerId <- as.numeric(inquiry_table$applicationAnswerId)
inquiry_table$outcomeAnswerId <- as.numeric(inquiry_table$outcomeAnswerId)

# exclude NA values (2)
inquiry_table <- na.omit(inquiry_table)

# convert start and end time to dates
inquiry_table$startTime <- as.POSIXct(inquiry_table$startTime)
inquiry_table$endTime <- as.POSIXct(inquiry_table$endTime)

# create new column for duration and convert to numeric
inquiry_table["duration"] = (inquiry_table$endTime - inquiry_table$startTime)
inquiry_table$duration <- as.numeric(inquiry_table$duration)
# histogram of duration 
hist(log(inquiry_table$duration))

# create date intervals to look at different subsets
library(lubridate)
date1 <- as.POSIXct("2015-01-01 00:00:00")
date2 <- as.POSIXct("2016-01-01 00:00:00")
int <- interval(date1, date2)

# split up inquiry table by inquiry type IDs 
i1 <- inquiry_table[inquiry_table$inquiryTypeId == 1,]
i2 <- inquiry_table[inquiry_table$inquiryTypeId == 2,]
i2$diagnosisLookupId
i3 <- inquiry_table[inquiry_table$inquiryTypeId == 3,]
i4 <- inquiry_table[inquiry_table$inquiryTypeId == 4,]

# split up by year 
# i1_2016<- i1[i1$startTime %within% int,]
# i2_2016<- i2[i2$startTime %within% int,]

# compare durations 
range(i1$duration)
range(i2$duration)
range(i3$duration)
range(i4$duration)

# use ggplot to see if attributes vary with duration  
library(ggplot2)
# a) test outcome answer ID 
ggplot(aes(x=duration, y=outcomeAnswerId), data = inquiry_table)+ geom_point()
# b) test inquiryTypeId
ggplot(aes(x=duration, y=inquiryTypeId), data = inquiry_table)+ geom_point()
# c) test ddxModuleId 
ggplot(aes(x=duration, y=ddxModuleId), data = inquiry_table)+ geom_point()
# d) test applicationAnswerId 
ggplot(aes(x=duration, y=applicationAnswerId), data = inquiry_table)+ geom_point()
# e) test diagnosisLookupId 
ggplot(aes(x=duration, y=diagnosisLookupId), data = inquiry_table)+ geom_point()

# inquiry time series for duration 
inquiry.ts <- xts(as.numeric(inquiry_table$duration), order.by=inquiry_table$startTime, frequency = 12)
plot(inquiry.ts)
periodicity(inquiry.ts)

fit <- stl(inquiry.ts, s.window="period")
decompose(inquiry.ts, frequency = c(876572,1080603,1453601))
dAvg <- apply.daily(inquiry.ts, function(x) apply(x, 2, mean)) 
plot(dAvg, main = "Daily average", col = 2)
dMax <- apply.daily(inquiry.ts, function(x) apply(x, 2, max))  # daily max
lines(dMax, col = 3)

hrAvg <- period.apply(inquiry.ts, endpoints(inquiry.ts, on = "hours", 1), function(x) apply(x, 2, mean))
# break duration into sub-intervals of size 3000 
range(inquiry_table$duration)
breaks = seq(0, 24000, by=3000)
duration.cut = cut(inquiry_table$duration, breaks, right=FALSE)
duration.freq = table(duration.cut)
cbind(duration.freq)
hist(log(duration.freq))
barplot(duration.freq, main = "Barplot of duration intervals")

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

# look at distribution of inquiries over time 
boxplot(inquiry_table$startTime)
hist(inquiry_table$startTime, breaks = "days")
# split up by year 
hist(inquiry_table$startTime, breaks = "years")

# density graph of time 
library(ggplot2)
ggplot(inquiry_table, aes(startTime)) +
  geom_density()
ggplot(inquiry_table, aes(startTime, duration) ) +
  geom_jitter()

# make word cloud of search queries 
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

# change nulls to 0's, others to 1's for imageId and diagnosisId 
events_table$imageId[events_table$imageId != "NULL"] <- 1
events_table$imageId[events_table$imageId == "NULL"] <- 0
events_table$diagnosisId[events_table$diagnosisId != "NULL"] <- 1
events_table$diagnosisId[events_table$diagnosisId == "NULL"] <- 0
events_table$controlId[events_table$controlId == "NULL"]<- 0
events_table$activeViewId[events_table$activeViewId == "NULL"] <- 0
events_table$activeViewId[events_table$activeViewId == "NULL"] <- 0

# transform variables to numeric data
events_table$imageId <- as.numeric(events_table$imageId)
events_table$diagnosisId <- as.numeric(events_table$diagnosisId)
events_table$controlId <- as.numeric(events_table$controlId)
events_table$eventTypeId <- as.numeric(events_table$eventTypeId)
events_table$activeViewId <- as.numeric(events_table$activeViewId)

# omit NA observations (2)
events_table <- na.omit(events_table)

# convert time to date object
events_table$time <- as.POSIXct(events_table$time)
hist(events_table$eventTypeId)

# compare diagnosis ID of 1 to diagnosis ID of 0 
no_diagnosis <- events_table[events_table$diagnosisId == 0 ,]
yes_diagnosis <- events_table[events_table$diagnosisId == 1 ,]

require(plotrix)
l <- list(yes_diagnosis$eventTypeId, no_diagnosis$eventTypeId)
multhist(l)

# compare image IDS 
l2 <- list(yes_diagnosis$imageId, no_diagnosis$imageId)
test <- unique(yes_diagnosis$imageId)
multhist(l2)

l3 <- list(yes_diagnosis$controlId, no_diagnosis$controlId)
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

ggplot(data=events_table, aes(x=time, y=eventTypeId, fill=diagnosisId)) +
  geom_bar(stat="identity", position=position_dodge())

plot(events_table$imageId, events_table$diagnosisId)
table<- with(events_table, table(imageId, diagnosisId))
mosaicplot(table, col = hcl(c(240, 120)),
           off = c(5, 5, 5, 5), main = "Diagnosis ID and Image ID")
avID_imgID <- with(events_table, table(imageId, activeViewId))
addmargins(avID_imgID)
av_comp <- prop.table(avID_imgID, 1)
img_comp <- prop.table(avID_imgID, 2)

library(TTR)
require(xts)
events.ts <- xts(events_table, order.by=as.POSIXct(events_table$time))
# event_timeseries_comp <- decompose(events.ts)
axTicksByTime(events.ts, ticks.on='months')
events.ts.test <- events.ts['2014-03']