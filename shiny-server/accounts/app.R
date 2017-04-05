#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(gtools)
library(shinyjs)

options(shiny.autoreload = TRUE)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

# Function to set home directory
dir = paste0(Sys.getenv("HOME"),'/cpls')

if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

help <- function() {
  return('
         Ergo Ipsum
         <p></p>
         <b>Frequently Asked Questions</b>
         <p></p>
         <li>Blalalalalalat?<br>
         <p class="tab">
         Blablablabla
         </p>
         <li>And?<br>
         <p class="tab">
         Blablabla
         </p>
         
         
         ')
}

buttonSaveValue <- 0
buttonAddValue <- 0
buttonRemoveValue <- 0

if(!file.exists('store/users.rda')){
  system(paste0('cp ',dir,'/data/users.rda ',dir,'/store'))
}
load('store/users.rda')
load('store/fields.rda')

inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value))
  )
}

ui <- fluidPage(
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode),                      # Add the js code to the page
  wellPanel(
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             textInput(paste0('name', i), paste0('User #', i),
                       ifelse(length(users[[i]]$name)>0,users[[i]]$name,'FirstName LastName'))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             textInput(paste0('email', i), paste0('Email #', i),
                       ifelse(length(users[[i]]$email)>0,users[[i]]$email,'user@domain.com'))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             textInput(paste0('accID', i), paste0('Account #', i),
                       ifelse(length(users[[i]]$accID)>0,users[[i]]$accID,'12345678'))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             textInput(paste0('token', i), paste0('Token #', i),
                       ifelse(length(users[[i]]$token)>0,users[[i]]$token,'xxxxxxxxxxxxxxxxxxxxxxxxx'))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             numericInput(paste0('maxNotesPerOrder', i), paste0('Maximum Number of Notes per Order #', i),
                       ifelse(length(users[[i]]$maxNotesPerOrder)>0,users[[i]]$maxNotesPerOrder,5),min = 0)
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             numericInput(paste0('minCash', i), paste0('Minimum Cash Level #', i),
                          ifelse(length(users[[i]]$minCash)>0,users[[i]]$minCash,0),min = 0)
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             numericInput(paste0('portfolioId', i), paste0('Portfolio Id #', i),
                          ifelse(length(users[[i]]$portfolioId)>0,users[[i]]$portfolioId,0))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             numericInput(paste0('amountPerNote', i), paste0('Amount Per Note #', i),
                          ifelse(length(users[[i]]$amountPerNote)>0,users[[i]]$amountPerNote,25),step = 25,min = 25)
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             selectInput(paste0('sortField', i), paste0('Sort Field #', i),choices = fields,multiple = FALSE,
                          selected = ifelse(length(users[[i]]$sortField)>0,users[[i]]$sortField,'model'))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             selectizeInput(paste0('reportCriteria', i), paste0('Report Criteria #', i),multiple = TRUE,choices = setNames(fields,fields),
                         selected = users[[i]]$reportCriteria, options = list(maxItems = 6))
      )})),
  fluidRow(
    lapply(1:length(users), function(i) {
      HTML(paste0('<b>Customer Filter for User #', i,"</b>"))
      })),
  fluidRow(
    lapply(1:length(users), function(i) {
      column(4,
             inputTextarea(paste0('filterCriteria', i),
             ifelse(length(users[[i]]$filterCriteria)>0,users[[i]]$filterCriteria,"intRate >= 16 & ( grade == 'C' | grade == 'D' | grade == 'E') & delinq2Yrs <= 0 & model >= .85")
             ,20,60 )
      )}))
  ,br(),
  fluidRow(
    lapply(1:length(users), function(i) {
      if (length(users) > 1) 
        column(4,checkboxInput(paste0('remove', i), paste0('User #', i, ' selected for removal'),FALSE))
      }))
  
  ),
    br(),
  wellPanel(
      fluidRow(
        column(5,
               actionButton('save', 'Save'),
               actionButton('remove', 'Remove Selected User(s)'),
               actionButton('add', 'Add User'),
               actionButton("help", "Help")
        ),
        column(7,
               bsAlert("alert"),      
               bsModal("helper", "System Configuration", "help", size = "large",
                       HTML(help())
               )
        ),
        textOutput("console")
      ))
    
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  observe({
    if(input$save > buttonSaveValue) {
      
      buttonSaveValue <<- input$save
      
      closeAlert(session, alertId="a1")
    
      inputs <- reactiveValuesToList(input)
      newusers=list()
      lc=list()
      users <- lapply(1:length(users), function(i) {
            lc$name <<- paste0(inputs[[paste0('name', i)]])
            lc$email <<- paste0(inputs[[paste0('email', i)]])
            lc$accID <<- paste0(inputs[[paste0('accID', i)]])
            lc$token <<- paste0(inputs[[paste0('token', i)]])
            lc$maxNotesPerOrder <<- as.numeric(paste0(inputs[[paste0('maxNotesPerOrder', i)]]))
            lc$minCash <<- as.numeric(paste0(inputs[[paste0('minCash', i)]]))
            lc$portfolioId <<- paste0(inputs[[paste0('portfolioId', i)]])
            lc$amountPerNote <<- as.numeric(paste0(inputs[[paste0('amountPerNote', i)]]))
            lc$sortField <<- paste0(inputs[[paste0('sortField', i)]])
            lc$reportCriteria <<- inputs[[paste0('reportCriteria', i)]]
            lc$filterCriteria <<- inputs[[paste0('filterCriteria', i)]]
            newusers <- append(newusers,lc)
        })
      save(users,file='store/users.rda')

      
      createAlert(session, anchorId = "alert", alertId="a1", 
                  content="Configuration saved",
                  style = "success",
                  append = FALSE)
      
    }
    if(input$remove > buttonRemoveValue) {
      inputs <- reactiveValuesToList(input)
      buttonRemoveValue <<- input$remove
      
      closeAlert(session, alertId="a1")
      if (length(users)>1){
      inputs <- reactiveValuesToList(input)
      newusers=list()
      lc=list()
      users <- lapply(1:length(users), function(i) {
        lc$name <<- paste0(inputs[[paste0('name', i)]])
        lc$email <<- paste0(inputs[[paste0('email', i)]])
        lc$accID <<- paste0(inputs[[paste0('accID', i)]])
        lc$token <<- paste0(inputs[[paste0('token', i)]])
        lc$maxNotesPerOrder <<- as.numeric(paste0(inputs[[paste0('maxNotesPerOrder', i)]]))
        lc$minCash <<- as.numeric(paste0(inputs[[paste0('minCash', i)]]))
        lc$portfolioId <<- paste0(inputs[[paste0('portfolioId', i)]])
        lc$amountPerNote <<- as.numeric(paste0(inputs[[paste0('amountPerNote', i)]]))
        lc$sortField <<- paste0(inputs[[paste0('sortField', i)]])
        lc$reportCriteria <<- inputs[[paste0('reportCriteria', i)]]
        lc$filterCriteria <<- inputs[[paste0('filterCriteria', i)]]
        if (inputs[[paste0('remove',i)]]) {
          lc<- NULL
          }
        newusers <- append(newusers,lc)
      })
#      output$console <- renderPrint(print(paste0(length(users))))
      count=0
      for(i in 1:length(users)){
        if(length(users[[i]]) == 0) count=count+1
      }
      while(count>0){
      for(i in 1:length(users)){
        if(length(users[[i]]) == 0) { 
          users[[i]]<-NULL
          count = count - 1
          break
          }
      }
      }
      if(length(users)>0)
       save(users,file='store/users.rda')
      session$close()
      js$reset()
#      output$console('window.location.href = \'/accounts/#\';')
    }
}
    if(input$add > buttonAddValue) {
      
      buttonAddValue <<- input$add
      lc=list()
      # User first and last name
      lc$name = "FirstName LastName"
      
      # Email address (used to send email report)
      lc$email = "user@domain.com"
      
      # LC Account number
      lc$accID = "12345627"
      
      # API token (paste API token)
      lc$token = "xxxxxxxxxxxxxxxxxxxxxxxxx";
      
      # Maximum notes per order submission (0 for unlimited)
      lc$maxNotesPerOrder = 5
      
      # Minimum cash level to maintain in account (0 for no minimum)
      lc$minCash = 0
      
      # Lending Club Portfolio ID (FALSE for unassigned)
      lc$portfolioId = FALSE
      
      # Investment amount per note (must be multiple of 25)
      lc$amountPerNote = 25
      
      # Note sort from highest to lowest(Prioritize note selection)
      # Popular fields are model and intRate
      lc$sortField = 'model'
      
      # Maximum grade allocation (comment out for no grade allocation)
      # lc$gradeAllocation = c("A" = 0, "B" = .20, "C" = .60, "D" = .60, "E" = .1, "F" = 0, "G" = 0)
      
      # Maximum term allocation (comment out for no term allocation)
      # lc$termAllocation = c("36" = .60, "60" = .60)
      
      # Report criteria (max 6 fields)
      lc$reportCriteria=c('id','intRate','grade','purpose','model')
      
      # Filter Criteria
      lc$filterCriteria <- "
               intRate >= 16 &
                 ( grade == 'C' | grade == 'D' | grade == 'E') &
                 delinq2Yrs <= 0 &
                 model >= .85 "
      users <- append(users,list(lc))
      
      save(users,file='store/users.rda')
      session$close()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)