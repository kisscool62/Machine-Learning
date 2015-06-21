###################################
#
# prediction model for pml
# coursera machine leaning project
#
###################################

training_raw <- read.csv('pml-training.csv', dec = '.', na.strings=c(""," ","NA", "#DIV/0!"))
testing_raw <- read.csv('pml-testing.csv', dec = '.', na.strings=c(""," ","NA"))

library(plyr)
library(caret)
library(rpart)
library(randomForest)
library(doParallel)

filterColumns <- function(df, FUN, ...){
        df.dim <- dim(df)[2]
        res <- vector(mode="logical", df.dim)
        
        for(i in 1:df.dim){
                res[i] <- FUN(df[,i], ...)
        }
        
        names(res) <- names(df)
        res
                     
}

isToFewVariance <- function(vector, threshold = 0){
        var(vector) <= threshold
}

isClassANumber <- function(vector, whatIsANumber){
        class(vector) %in% whatIsANumber        
}

isNa <- function(vector, threshold = 1){
        size_vector <- length(vector)
        (1-(size_vector - sum(is.na(vector))) / size_vector) >= threshold
}

createTrainAndTestPartition <- function(df, p){
        len <- dim(df)[1]
        training_rows <- sample(1:len, size = floor(p*len))        
        list(train = df[training_rows,], test = df[-training_rows,])
}

#which(filterColumns(training_without_NA, isToFewVariance, 0.03))
#sum(filterColumns(training_without_NA, isToFewVariance, 0.03))

#filterColumns(training_without_NA, var)

#numericColumns <- filterColumns(training, isClassANumber, c("numeric", "integer"))


# remove all columns with 97% NA because shouldn't impact the model
filtered_columns <- !filterColumns(training_raw, isNa, 0.97)
training_without_NA <- subset(training_raw, select = filtered_columns)
training_without_NA <- subset(training_without_NA, select = -c(1))
training_without_NA <- mutate(training_without_NA, cvtd_timestamp = as.numeric(as.POSIXlt(as.character(cvtd_timestamp)), format = "%d/%m/%Y %H:%M"))

testing_without_NA <- subset(testing_raw, select = filtered_columns)
testing_without_NA <- subset(testing_without_NA, select = -c(1))
testing_without_NA <- mutate(testing_without_NA, cvtd_timestamp = as.numeric(as.POSIXlt(as.character(cvtd_timestamp)), format = "%d/%m/%Y %H:%M"))
levels(testing_without_NA$new_window) <- levels(training_without_NA$new_window)
                             

training_A <- training_without_NA[training_without_NA$classe == "A",]
training_B <- training_without_NA[training_without_NA$classe == "B",]
training_C <- training_without_NA[training_without_NA$classe == "C",]
training_D <- training_without_NA[training_without_NA$classe == "D",]
training_E <- training_without_NA[training_without_NA$classe == "E",]
        

data <- createTrainAndTestPartition(training_A, 0.6) 
training_A_1 <- data$train
testing_A_1 <- data$test

data <- createTrainAndTestPartition(training_B, 0.6) 
training_B_1 <- data$train
testing_B_1 <- data$test

data <- createTrainAndTestPartition(training_C, 0.6) 
training_C_1 <- data$train
testing_C_1 <- data$test

data <- createTrainAndTestPartition(training_D, 0.6) 
training_D_1 <- data$train
testing_D_1 <- data$test

data <- createTrainAndTestPartition(training_E, 0.6) 
training_E_1 <- data$train
testing_E_1 <- data$test

training <- rbind(training_A_1, training_B_1, training_C_1, training_D_1, training_E_1)
testing <- rbind(testing_E_1, testing_D_1, testing_B_1, testing_C_1, testing_A_1)

#too long method...
#model_caret <- train(form = classe ~ ., data = training, method="rf")

model_rf <- randomForest(formula = classe ~ ., data = training)

prediction <- predict(model_rf, subset(testing, select = -c(59)))

## predict with removed problem_id
to_test <- subset(testing_without_NA, select = -c(59))
to_test <- mutate(to_test, 
                  magnet_dumbbell_z = as.numeric(magnet_dumbbell_z),
                  magnet_forearm_y = as.numeric(magnet_forearm_y),
                  magnet_forearm_z = as.numeric(magnet_forearm_z))

to_train <- subset(training_without_NA, select = -c(59))
train_types <- filterColumns(df = to_train, FUN = class)
test_types <- filterColumns(df = to_test, FUN = class) 
train_types == test_types
which(train_types != test_types)

names(to_train) == names(to_test)

final_prediction <- predict(model_rf, newdata = to_test)

class(to_test)
class(training)