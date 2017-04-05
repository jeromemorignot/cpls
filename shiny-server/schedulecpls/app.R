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
dir = paste0(Sys.getenv("HOME"),'/cpls')

if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

startTimes <- list()
if (file.exists('store/startTimes.rda')) {
  load('store/startTimes.rda')
} else
{
  startTimes <- c("06:00","10:00","14:00","18:00")
}

buttonAddValue <- 0
buttonSaveValue <- 0

ui <- fluidPage(
  tags$head(HTML("<meta http-equiv=\"Pragma\" content=\"no-cache\">")),
  titlePanel("Start Times"),
  
  sidebarLayout(
    sidebarPanel(
      timeInput("time_input", "Enter time", value = strptime("06:00", "%R"),seconds = FALSE),
      actionButton('add', 'Add')
      ),
    
    mainPanel(
      fluidRow(
        column(6,
               wellPanel(
                 HTML('PLS is configured to use the Pacific Time Zone to match the Lending Club platform.  Lending Club lists new loans 
            at 06:00, 10:00, 14:00, 18:00 PST.   PLS starts one minute prior the configured times to allow the system 
            enough time to properly load the detected list of new loans. <br><br>'),
                 selectInput('start_times', 'Start Times', 
                             choices=startTimes,
                             multiple=TRUE,
                             selected=startTimes,
                             selectize = TRUE),
                 bsTooltip(id = "times", title = "Select hours to start service in PST time", 
                           placement = "top", trigger = "hover")
               )
        )
      )
    )
  ),
  fluidRow(
    column(5,
           actionButton('save', 'Save'),
           actionButton("help", "Help")
    ),
    column(7,
           bsAlert("alert"),      
           bsModal("helper", "System Configuration", "help", size = "large",
                   HTML(help())
           )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if(input$add > buttonAddValue) {
      
      buttonAddValue <<- input$add
      
      startTimes <- c(input$start_times,strftime(input$time_input, "%R"))
      
      updateSelectizeInput(session,'start_times',choices = c(c("06:00","10:00","14:00","18:00"),input$start_times,strftime(input$time_input, "%R")), selected = startTimes,server=TRUE)
    }
  
  
  if(input$save > buttonSaveValue){
    
  buttonSaveValue <<- input$save
  # Save configuration data

  startTimes <- input$start_times
  
  save(startTimes,file='store/startTimes.rda')
  
  createAlert(session, anchorId = "alert", alertId="a1", 
              content="Configuration saved",
              style = "success",
              append = FALSE)
  
  stopApp()
  
  }
  })
  
  }

shinyApp(ui, server)

