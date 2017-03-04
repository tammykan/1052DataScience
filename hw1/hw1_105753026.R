########################
# homework1 example
########################

# command for rstudio: system("Rscript hw1_105753026.R -files test.1.csv -out result.csv")

# get argument form command
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R -files input -out output", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-files"){
    i_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    o_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

# remove file name from extension
input <- sub('\\.csv$', '', i_f)

# read input data
data <- read.table(i_f, header = TRUE, sep = ",")

# get maximum weight and height from column
max_weight <- max(data[,2])
max_height <- max(data[,3])

# two bits factor
result <- data.frame(set = input, weight = round(max_weight, 2), height = round(max_height, 2))

# output data
write.csv(result, file = o_f, row.names = FALSE)
