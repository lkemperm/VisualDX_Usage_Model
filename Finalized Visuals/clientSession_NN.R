library(e1071)
library(rpart)
library(neuralnet)
library(chron)
library(timeDate)

# convert sessionStart to POSIXct
clientSession$sessionStart <- as.POSIXct(clientSession$sessionStart)

# convert year month day to date object (used in aggregate function)
toDate <- function(year, month, day){
  ISOdate(year, month, day)
}

# aggregate outcome by hour 
aggregateByHour <- function(x, start, y){
  time <- x[[start]]
  mo <- strftime(time, "%m")
  yr <- strftime(time, "%Y")
  day <- strftime(time, "%d")
  hour <- strftime(time, "%H")
  outcome <- x[[y]]
  new_df <- data.frame(hour, day, mo, yr, outcome)
  agg <- aggregate(outcome ~ hour + day + mo + yr, FUN = sum)
  agg$day <- as.numeric(agg$day)
  agg$hour <- as.numeric(agg$hour)
  agg["holiday"] <- ifelse(isHoliday(timeDate(toDate(agg$yr, agg$mo, agg$day))), 0, 1)
  agg["businessDay"] <- ifelse(isBizday(timeDate(toDate(agg$yr, agg$mo, agg$day))), 0, 1)
  agg["peakHour"] <- ifelse(((agg$hour >= 11) & (agg$hour <= 14)), 1, 0)
  agg["residencyProgram"] <- ifelse(((agg$mo == "06") & (agg$day>=15)), 1 , 0)
  pred_df <- subset(agg, select = -c(mo, day, hour, yr))
return(pred_df)
}

# aggregate data frames
clientSession.ts <- aggregateByHour(clientSession, "sessionStart", "imageCount")
inquiry.ts <- aggregateByHour(inquiry_table, "startTime", "duration")

# function to get test and train indicies 
get_train_ind<- function(pred){
  smp_size <- floor(0.75*nrow(pred))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(pred)), size = smp_size)
return(train_ind)
}

# get indicies for clientSession and inquiry
client_index <- get_train_ind(clientSession.ts)
inquiry_index <- get_train_ind(inquiry.ts)

# get client train and test sets 
client_train <- clientSession.ts[client_index,]
client_test <- clientSession.ts[-client_index,]

# get inquiry train and test sets 
inquiry_train <- inquiry.ts[inquiry_index,]
inquiry_test <- inquiry.ts[-inquiry_index,]

# fit linear model to compare to NN (inquiry)
lm.inquiry <- glm(outcome~., data = inquiry_train)
summary(lm.inquiry)

pr.inquiry <- predict(lm.inquiry, inquiry_test)
MSE.inquiry <- sum((pr.inquiry-inquiry_test$outcome)^2)/nrow(inquiry_test)

# same for clientSession 
lm.clientSession <- glm(outcome~., data = client_train)
summary(lm.clientSession)
pr.clientSession <- predict(lm.clientSession, client_test)
MSE.clientSession <- sum((pr.clientSession-client_test$outcome)^2)/nrow(client_test)

# scale data in preparation for neural network fit 
scale_data <- function(pred){
  maxs <- apply(pred, 2, max)
  mins <- apply(pred, 2, min)
  scaled <- as.data.frame(scale(pred, center=mins, scale = maxs-mins))
return(scaled)
}

# get scaled data for inquiry and clientSession 
scaled_inquiry <- scale_data(inquiry.ts)
scaled_client <- scale_data(clientSession.ts)

# use scaled data to get new training and test sets for nn 
inquiry_train_ <- scaled_inquiry[inquiry_index,]
inquiry_test_ <- scaled_inquiry[-inquiry_index,]
client_train_ <- scaled_client[client_index,]
client_test_ <- scaled_client[-client_index,]

# fit neural network to the data to predict hourly outcome 
fit_nn <- function(train, hiddenVal){
  m <- model.matrix(~outcome + holiday + businessDay + peakHour + residencyProgram, data = train)
  nn <- neuralnet(outcome ~ holiday + businessDay + peakHour + residencyProgram, data = train, 
                  hidden = hiddenVal, linear.output = T)
  return(nn)
}

# nn's for clientSession and inquiry 
clientSession_nn <- fit_nn(client_train_, 3)
inquiry_nn <- fit_nn(inquiry_train_, 3)

# try on test data set 
test_nn <- function(test, pred, nn){
  m2 <- model.matrix(~outcome + holiday + businessDay + peakHour + residencyProgram, data = test)
  cols <- c("holiday", "businessDay", "peakHour", "residencyProgram")
  cov <- subset(m2, select = cols)
  pr.nn <- compute(nn, cov)
  pr.nn_ <- pr.nn$net.result* (max(pred$outcome)-
                                 min(pred$outcome))+min(pred$outcome)
  test.r <- (test$outcome) * (max(pred$outcome)-
                             min(pred$outcome))+min(pred$outcome)
  MSE.nn <- sum((test.r-pr.nn_)^2)/nrow(test)
return(MSE.nn)
}

# MSE for nn's
MSE.nn.CS <- test_nn(client_test_, clientSession.ts, clientSession_nn)
MSE.nn.INQ <- test_nn(inquiry_test_, inquiry.ts, inquiry_nn)
t <- as.data.frame(c(paste(MSE.inquiry, MSE.nn.i), paste(MSE.clientSession, MSE.nn.CS)), row.names = 
                     c("MSE for inquiry duration", "MSE for clientSession imageCount"))


# try random forest 
library(randomForest)
client.fit <- randomForest(outcome~., data=client_train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(client.fit, main = "Importance of features for predicting hourly imageCount")
client.pred <- predict(client.fit, client_test)
table(client.pred, client_test$outcome)
inquiry.fit <- randomForest(outcome~., data = inquiry_train, 
                            importance=TRUE,
                            ntree=2000)
varImpPlot(inquiry.fit, main = "Importance of features for predicting hourly inquiry duration")
