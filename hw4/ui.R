library(shiny)
library(ggplot2)
library(plotly)

methodFile = gsub(".csv", "", basename(list.files("./methods/")))

ui <- pageWithSidebar(
  headerPanel('DataScience HW4'),
  sidebarPanel(
    selectInput("method", "Method", 
                choices = methodFile, 
                multiple = TRUE, selected = as.character(methodFile)),
    
    selectInput("query", "Male/Female", choices = c("male","female"))
  ),
  mainPanel(
    plotlyOutput('plot'),
    tableOutput("info")
  )
)