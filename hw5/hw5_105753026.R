# Perform n-fold cross-validation
# % of training, % of calibration, % of testing= n-2, 1, 1

# system("Rscript hw5_105753026.R -fold 5 -out performance.csv")

library('ROCR')
library('randomForest')
library('rpart')

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_105753026.R -fold n -out performance.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-fold"){
    n<-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("k fold cross-validation :", n))
print(paste("output file:", out_f))

# read data
data <- read.csv("Archaeal_tfpssm.csv", header=F)
#levels(data[,2])
#head(data[,5600:5603])

# k fold
k = n

# accuracy for training, calibration, test set
train <- c()
calibration <- c()
test <- c()

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

# function for k fold
for(i in 1:k){

  training <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  training$id <- sample(1:10, nrow(training), replace=TRUE)
  train_list <- 1:10
  random <- sample(1:10, 1)
  trainingset <- subset(training, id %in% train_list[-random])
  calset <- subset(training, id %in% c(random))
  
  # run a random forest model
  #model <- randomForest(trainingset$V2 ~., data = trainingset, ntree = 100)
  # run a decision tree model
  model <- rpart( trainingset$V2 ~., data = trainingset)
  
  # calculate accuracy score of training, calibration, test set
  train <- c(train, length(which(predict(model, trainingset[,-2], type="class") == trainingset[, 2] )) / nrow(trainingset))
  calibration <- c(calibration, length(which(predict(model, calset[,-2], type="class") == calset[, 2] )) / nrow(calset))
  test <- c(test, length(which(predict(model, testset[,-2], type="class") == testset[, 2] )) / nrow(testset))
}

# output file
set <- c("training", "calibration", "test")
accuracy <- round((c(mean(train), mean(calibration), mean(test))), digits = 2)
out_data <- cbind(set, accuracy)
colnames(out_data) <- c("set", "accuracy")
write.csv(out_data, file=out_f, row.names = F, quote = F)

