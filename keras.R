install.packages("keras")
library(keras)
install_keras()

dat <- read.csv("", sep = ";")
unique(dat$country_id)
dat <- dat[dat$country_id == 1729, ]
dat <- dat[sample(length(dat$home_result)) ,] #sampling the order of data
dat[, 1:32] <- NULL
dat[,1:7] <- NULL
dat$away_result <- NULL
dat <- dat[, c(1,2,3,4,5,6,7,32,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,8)]
#dat <- dat[, c(1:8, 32)]
dat <- dat[dat$home_result != "draw", ]
#dat <- dat[, c(1:8,32)]

#mnist <- dataset_mnist()
#x_train <- mnist$train$x
#y_train <- mnist$train$y
#x_test <- mnist$test$x
#y_test <- mnist$test$y

set.seed(220)

index <- sample(2, nrow(dat), replace = T, prob=c(.8,.2))
train <- dat[index == 1, ]
test <- dat[index==2, ]


y_train <- train[, 1]
y_test <- test[, 1]

x_train <- train[, 2:25]
x_test <- test[, 2:25]

vec <- rep(0, length(test$diff_Acceleration))
index <- which(test$home_result == "win")
index2 <- which(test$home_result == "draw")
index3 <- which(test$home_result == "loss")
vec[index] <- 1
vec[index2] <- 0
vec[index3] <- 0

y_test <- vec
y_train <- vec



# reshape
#dim(x_train) <- c(nrow(x_train), 784)
#dim(x_test) <- c(nrow(x_test), 784)
# rescale
#x_train <- x_train / 255
#x_test <- x_test / 255

#normalized = (x-min(x))/(max(x)-min(x))

x_train <- scale(x_train)
x_test <- scale(x_test)

y_train <- to_categorical(y_train)
y_test <- to_categorical(y_test)
y_test <- y_test[, 1:2]
y_train <- y_train[, 1:2]

#model <- keras_model_sequential() 
#model %>% 
#  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>% 
#  layer_dropout(rate = 0.4) %>% 
#  layer_dense(units = 128, activation = "relu") %>%
#  layer_dropout(rate = 0.3) %>%
#  layer_dense(units = 10, activation = "softmax")

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 30, input_shape = c(24)) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units=15) %>% 
  layer_dense(units = 2, activation = "sigmoid")

summary(model)

#model %>% compile(
#  loss = "categorical_crossentropy",
#  optimizer = optimizer_rmsprop(),
#  metrics = c("accuracy")
#)


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(lr=0.0001),
  metrics = 'accuracy'
)






#history <- model %>% fit(
#  x_train, y_train, 
#  epochs = 5, batch_size = 128, 
#  validation_split = 0.2
#)

y_train[y_train==0.99] <- 1
y_train2 

history <- model %>% fit(
  as.matrix(x_train), 
  as.matrix(y_train), 
  epochs = 100,
  batch_size = 50, 
  validation_split = 0.3
)


model %>% evaluate(as.matrix(x_test), as.matrix(y_test),verbose = 1)

plot(history)
history$metrics

# Plot the accuracy of the training data 
plot(history$metrics$acc, main="Modell ackuratessen", xlab = "epoch", ylab="ackuratess", col="blue", type="l")

# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col="green")

# Add Legend
legend("bottomright", c("träning","test"), col=c("blue", "green"), lty=c(1,1))


#model %>% predict_classes(as.matrix(x_test))


# Predict the classes for the test data
classes <- model %>% predict_classes(as.matrix(x_test), batch_size = 5)
# Evaluate on test data and labels

# Confusion matrix
library(caret)
library(xtable)
tab <- table(vec, classes) #1 = win, 0 = draw, 2 = loss
a <- confusionMatrix(tab)
xtable(a$table)
xtable(as.data.frame(a$byClass))

#avaluate
score <- model %>% evaluate(as.matrix(x_test), as.matrix(y_test), batch_size = 5)
score
# Print the score
print(score)




