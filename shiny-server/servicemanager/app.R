#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(shinyBS)

# Function to set home directory
defaultDir = '/home/user/cpls'
csf <- function() {
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName)) 
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
      }
    }
  }
}
dir <- tryCatch(dirname(csf()),
                error = function(e) {
                  defaultDir
                }
)
if (is.null(dir) | length(dir) == 0) {
  dir <- defaultDir
}
if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

buttonStartValue <- 0
buttonStopValue <- 0
buttonRunOnceValue <- 0
buttonTestValue <- 0
buttonRefreshValue <-0

source("scripts/funcs.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Service Manager"),
   sidebarLayout(
   # Sidebar with a slider input for number of bins 
   sidebarPanel(
            actionButton('start', 'Start PLS'),br(),
            actionButton('stop', 'Stop PLS'),br(),br(),
            actionButton('runonce', 'Run PLS once'),br(),
            actionButton('test', 'Test PLS'),br(),br(),
            actionButton('refresh','Refresh Status')
            ,width=2),
   mainPanel(
    h2("Service State"), 
    textOutput("status"),br(),
    h2("Running Once/Test State"),
    textOutput("runningoncestate")
   )
   
   )
)
   


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  autoInvalidate <- reactiveTimer(2000)
observe({
 if ((input$refresh > buttonRefreshValue) | (input$refresh == 0)){ 
   buttonRefreshValue <<- input$refresh
   if(cplsRunning()){output$status <- renderText("Running")}else{output$status <- renderText("Not Running")}}
  output$runningoncestate <- renderText("N/A")
  
  if (input$start > buttonStartValue){ 
      buttonStartValue <<- input$start
      if(!cplsRunning()){
          output$status <- renderText("Starting")
          rc <- system(paste("Rscript ",dir,"/cpls.R",sep=''),wait=FALSE)
          if (rc > 0){
            output$status <- renderText("Error")
          }
      }}
  if (input$runonce > buttonRunOnceValue){ 
    buttonRunOnceValue <<- input$runonce
      output$status <- renderText("Starting : Running Once")
      system(paste("Rscript ",dir,"/cpls.R runOnce",sep=''),wait=FALSE)
      output$runningoncestate <- renderText("Completed - Check Logs for Results")
  }
  if (input$test > buttonTestValue){ 
    buttonTestValue <<- input$test
    output$status <- renderText("Starting : Testing")
    system(paste("Rscript ",dir,"/cpls.R test",sep=''),wait=FALSE)
    output$runningoncestate <- renderText("Test Completed - Check Logs for Results")
  }
  if (input$stop > buttonStopValue){ 
    buttonStopValue <<- input$stop
    if(cplsRunning()){
      output$status <- renderText("Stopping")
      system(paste("touch ",dir,"/store/killcpls.proc",sep = ''))
    }}
})
  
  observe({
    autoInvalidate()
    if(cplsRunning()){output$status <- renderText("Running")}else{output$status <- renderText("Not Running")}
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

