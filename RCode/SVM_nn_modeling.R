library(e1071)
library(rpart)
library(neuralnet)
library(chron)
library(timeDate)

clientSession$sessionStart <- as.POSIXct(clientSession$sessionStart)

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

clientSession.ts <- aggregateByHour(clientSession, "sessionStart", "imageCount")
inquiry.ts <- aggregateByHour(inquiry_table, "startTime", "duration")

get_train_ind<- function(pred){
  smp_size <- floor(0.75*nrow(pred))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(pred)), size = smp_size)
return(train_ind)
}

# get indicies to split test and train data 
client_index <- get_train_ind(clientSession.ts)
inquiry_index <- get_train_ind(inquiry.ts)

# client train and test sets 
client_train <- clientSession.ts[client_index,]
client_test <- clientSession.ts[-client_index,]

# inquiry train and test sets 
inquiry_train <- inquiry.ts[inquiry_index,]
inquiry_test <- inquiry.ts[-inquiry_index,]

# fit linear model to compare to NN 
lm.inquiry <- glm(outcome~., data = inquiry_train)
summary(lm.inquiry)
pr.inquiry <- predict(lm.inquiry, inquiry_test)
MSE.inquiry <- sum((pr.inquiry-inquiry_test$outcome)^2)/nrow(inquiry_test)

lm.clientSession <- glm(outcome~., data = client_train)
summary(lm.clientSession)
pr.clientSession <- predict(lm.clientSession, client_test)
MSE.clientSession <- sum((pr.clientSession-client_test$outcome)^2)/nrow(client_test)

scale_data <- function(pred){
  maxs <- apply(pred, 2, max)
  mins <- apply(pred, 2, min)
  scaled <- as.data.frame(scale(pred, center=mins, scale = maxs-mins))
return(scaled)
}
# scaled data -- use to get new train and test data 
scaled_inquiry <- scale_data(inquiry.ts)
scaled_client <- scale_data(clientSession.ts)

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

m <- model.matrix(~dur + holiday + businessDay + peakHour + residencyProgram, data = train_)
nn <- neuralnet(dur ~ holiday + businessDay + peakHour + residencyProgram, data = m, 
                hidden = 3, linear.output = T)

# try on test data set 
m2 <- model.matrix(~dur + holiday + businessDay + peakHour + residencyProgram, data = test_)
cols <- c("holiday", "businessDay", "peakHour", "residencyProgram")
cov <- subset(m2, select = cols)
pr.nn <- compute(nn, cov)
pr.nn_ <- pr.nn$net.result*(max(inquiry_pred$dur)-
                              min(inquiry_pred$dur))+min(inquiry_pred$dur)
test.r <- (test_$dur) * (max(inquiry_pred$dur)-
                           min(inquiry_pred$dur))+min(inquiry_pred$dur)
MSE.nn <- sum((test.r-pr.nn_)^2)/nrow(test)
print(paste(MSE.lm, MSE.nn))


index <- 1:nrow(inquiry_pred)
testindex <- sample(index, trunc(length(index)/3))
testset <- inquiry_pred[testindex,]
trainset <- inquiry_pred[-testindex,]

# svm 
svm.model <- svm(formula = dur ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-1])

# rpart 
rpart.model <- rpart(formula = dur ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-1])

svm.t <- table(pred = svm.pred, true = testset[,1])
table(pred = rpart.pred, true = testset[,1])
library(caret)
u <- union(rpart.pred, testset[,1])
t <- table(factor(rpart.pred, u), factor(testset[,1], u))
# too big for confusion matrix...
# confusionMatrix(t)
# confusionMatrix(svm.t)

# try random forest 
library(randomForest)
inquiry.rf <- randomForest(inquiry_pred[,-1], inquiry_pred[,1], prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))