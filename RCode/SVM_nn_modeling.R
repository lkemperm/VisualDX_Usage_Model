library(e1071)
library(rpart)
library(neuralnet)

# get hourly duration
x <- inquiry_table$startTime
mo <- strftime(x, "%m")
yr <- strftime(x, "%Y")
day <- strftime(x, "%d")
hour <- strftime(x, "%H")
dur <- inquiry_table$duration
dd <- data.frame(hour, day, mo, yr, dur)
dd.agg <- aggregate(dur ~ hour + day + mo + yr, FUN = sum)

toDate <- function(year, month, day){
  ISOdate(year, month, day)
}
require(chron)
testDate <- dd.agg[2,]
d <- toDate(testDate$yr, testDate$mo, testDate$day)
t<- timeDate(d)
isHoliday(t)

dd.agg["holiday"] <- ifelse(isHoliday(timeDate(toDate(dd.agg$yr, dd.agg$mo, dd.agg$day))), 0, 1)
dd.agg["businessDay"] <- ifelse(isBizday(timeDate(toDate(dd.agg$yr, dd.agg$mo, dd.agg$day))), 0, 1)
dd.agg["peakHour"] <- ifelse((dd.agg$hour ==11 | dd.agg$hour == 12 | dd.agg$hour == 13 | dd.agg$hour == 14), 
                             0, 1)

inquiry_pred <- subset(dd.agg, select = -c(mo, day, hour, yr))
# data = inquiry_pred[, c('holiday', 'businessDay', 'peakHour')]
# split data into test and train
smp_size <- floor(0.75*nrow(inquiry_pred))
set.seed(123)
train_ind <- sample(seq_len(nrow(inquiry_pred)), size = smp_size)
train <- inquiry_pred[train_ind,]
test <- inquiry_pred[-train_ind,]

# fit linear model to test against NN 
lm.fit <- glm(dur~., data = train)
summary(lm.fit)
pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm-test$dur)^2)/nrow(test)

# scale data before fitting NN 
maxs <- apply(inquiry_pred, 2, max)
mins <- apply(inquiry_pred, 2, min)
scaled <- as.data.frame(scale(inquiry_pred, center=mins, scale = maxs-mins))
train_ <- scaled[train_ind,]
test_ <- scaled[-train_ind,]

# fit neural network to the data to predict hourly duration 
m <- model.matrix(~dur + holiday + businessDay + peakHour, data = train_)
nn <- neuralnet(dur ~ holiday + businessDay + peakHour, data = m, hidden = 2, linear.output = T)

# try on test data set 
m2 <- model.matrix(~dur + holiday + businessDay + peakHour, data = test_)
cols <- c("holiday", "businessDay", "peakHour")
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

table(pred = svm.pred, true = testset[,1])
table(pred = rpart.pred, true = testset[,1])