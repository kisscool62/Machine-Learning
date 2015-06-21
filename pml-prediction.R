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

###################################
#
# functions
#
###################################

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
        training_rows <- sample(1:len, size = floor(p*len), replace = FALSE)        
        list(train = df[training_rows,], test = df[-training_rows,])
}

# write answers
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
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
## predict with removed problem_id
#to_test is the final data to predict classes. Not for sample error analysis because classe is not known
to_test <- subset(testing_without_NA, select = -c(59))
to_test <- mutate(to_test, 
                  magnet_dumbbell_z = as.numeric(magnet_dumbbell_z),
                  magnet_forearm_y = as.numeric(magnet_forearm_y),
                  magnet_forearm_z = as.numeric(magnet_forearm_z))

                             
## building data to build the model
# data grouped by classe
data_A <- training_without_NA[training_without_NA$classe == "A",]
data_B <- training_without_NA[training_without_NA$classe == "B",]
data_C <- training_without_NA[training_without_NA$classe == "C",]
data_D <- training_without_NA[training_without_NA$classe == "D",]
data_E <- training_without_NA[training_without_NA$classe == "E",]
        
## createTrainAndTestPartition takes randomly 60% in each group to build train data, remaining 40% are test data
data <- createTrainAndTestPartition(data_A, 0.6) 
training_A_1 <- data$train
testing_A_1 <- data$test

data <- createTrainAndTestPartition(data_B, 0.6) 
training_B_1 <- data$train
testing_B_1 <- data$test

data <- createTrainAndTestPartition(data_C, 0.6) 
training_C_1 <- data$train
testing_C_1 <- data$test

data <- createTrainAndTestPartition(data_D, 0.6) 
training_D_1 <- data$train
testing_D_1 <- data$test

data <- createTrainAndTestPartition(data_E, 0.6) 
training_E_1 <- data$train
testing_E_1 <- data$test

## build a training data
training <- rbind(training_A_1, training_B_1, training_C_1, training_D_1, training_E_1)
testing <- rbind(testing_E_1, testing_D_1, testing_B_1, testing_C_1, testing_A_1)

#too long method...
#model_caret <- train(form = classe ~ ., data = training, method="rf")

# build the model with all remaining variables
model_rf <- randomForest(formula = classe ~ ., data = training)

## predict without classe (not needed) variable
prediction <- predict(model_rf, subset(testing, select = -c(59)))

## out of sample error
table(prediction, testing$classe)

sum(prediction == testing$classe)/length(prediction) * 100

## apply the model to the final test data
final_prediction <- predict(model_rf, newdata = to_test)

pml_write_files(final_prediction)

