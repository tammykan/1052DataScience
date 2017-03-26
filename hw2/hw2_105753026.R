library('ROCR')

# system("Rscript hw2_105753026.R --target male --files method1.csv --out result.csv")

# sensitivity = TP/(TP+FN)
# specificity = TN/(TN+FP)
# percision = TP/(TP+FP)
# f1 = 2*precision*recall/(precision+recall)
# eval <- prediction(d$pred.score,d$reference)
# auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)

getConfusionMatrix <- function(pred, ref, target){
  confusionMatrix <- table(truth = c(pred==ref), prediction = c(pred==target))
  return (confusionMatrix)
}

query_func<-function(query_m, i)
{
  if(query_m == "male"){
    which.max(i)
  }
  else if (query_m == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_105753026.R --target male|female --files file1 file2 ... filen --out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
methods <- c()
sens <- c()
spec <- c()
f1s <- c()
aucs <- c()

for(file in files)
{
  method<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
  # get confusion matrix
  matrix <- getConfusionMatrix(d$prediction, d$reference, query_m)
  
  # get the values of sensitivity, specificity, f1 and auc
  sensitivity <- round(matrix[4] / (matrix[4] + matrix[1]), digits = 2)
  specificity <- round(matrix[2] / (matrix[2] + matrix[3]), digits = 2)
  percision <- round(matrix[4] / (matrix[4] + matrix[3]), digits = 2)
  f1 <- round(2*percision*sensitivity / (percision+sensitivity), digits = 2)
  eval <- prediction(d$pred.score, d$reference)
  auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
  
  sens <- c(sens, sensitivity)
  spec <- c(spec, specificity)
  f1s <- c(f1s, f1)
  aucs <- c(aucs, auc)
  methods <- c(methods, method)
  
}
out_data<-data.frame(method=methods, sensitivity=sens, specificity=spec, 
                     F1=f1s, AUC=aucs, stringsAsFactors = F)
index<-sapply(out_data[,c("sensitivity","specificity", "F1", "AUC")], query_func, query_m=query_m)

# output file
out_data<-rbind(out_data,c("highest",methods[index]))
write.csv(out_data, file=out_f, row.names = F, quote = F)
