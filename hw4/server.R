library('ROCR')
library(shiny)
library(ggplot2)
library(plotly)

# get confusion matrix
getConfusionMatrix <- function(pred, ref, target){
  confusionMatrix <- table(truth = c(pred==ref), prediction = c(pred==target))
  return (confusionMatrix)
}

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    # read multiple select input
    files <- input$method
    query <- input$query
    
    sensitivityResult <- c()
    specificityResult <- c()
    
    for(file in files){
      
      method<-read.table(paste0("./methods/", file, ".csv"), header=T,sep=",")
      data<-data.frame(person=method$persons, pred=method$prediction,
                       ref=method$reference, score=method$pred.score)
      
      # get confusion matrix
      matrix <- getConfusionMatrix(data$pred, data$ref, query)
      
      # get the values of sensitivity, specificity, f1 and auc
      sensitivity <- round(matrix[4] / (matrix[4] + matrix[1]), digits = 2)
      specificity <- round(matrix[2] / (matrix[2] + matrix[3]), digits = 2)
      percision <- round(matrix[4] / (matrix[4] + matrix[3]), digits = 2)
      f1 <- round(2*percision*sensitivity / (percision+sensitivity), digits = 2)
      eval <- prediction(data$score, data$ref)
      auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
      
      sensitivityResult <- c(sensitivityResult, sensitivity)
      specificityResult <- c(specificityResult, 1-specificity)
    }
    
    # get the plot graph
    plotData = do.call(rbind.data.frame, Map('c', sensitivityResult, specificityResult))
    plot <- ggplot(data = plotData, 
                   aes(x = specificityResult, y = sensitivityResult)) + 
      labs(x = "1-specificity", y = "sensitivity") + 
      geom_point(aes(text = paste("Method:", gsub(".csv", "", basename(files))))) + 
      ggtitle("The Evaluation Result of Methods")
    
    ggplotly(plot)
    
  })
  
  output$info <- renderTable({

    files <- input$method
    query <- input$query
    
    sensitivityResult <- c()
    specificityResult <- c()
    methods <- c()

    for(file in files){
      
      method<-read.table(paste0("./methods/", file, ".csv"), header=T,sep=",")
      data<-data.frame(person=method$persons, pred=method$prediction,
                       ref=method$reference, score=method$pred.score)
      
      # get confusion matrix
      matrix <- getConfusionMatrix(data$pred, data$ref, query)
      
      # get the values of sensitivity, specificity, f1 and auc
      sensitivity <- round(matrix[4] / (matrix[4] + matrix[1]), digits = 2)
      specificity <- round(matrix[2] / (matrix[2] + matrix[3]), digits = 2)
      sensitivityResult <- c(sensitivityResult, sensitivity)
      specificityResult <- c(specificityResult, 1-specificity)
      methods <- c(methods, file)
      
    }
    
    # get the table information
    table = cbind("Method" = methods, "1-specificity" = specificityResult, "sensitivity" = sensitivityResult)
    table
    
  })
  
}
