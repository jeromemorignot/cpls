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
library(mailR)

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
buttonTestEmailValue <- 0

if(!file.exists('store/config.rda')){
  system(paste0('cp ',dir,'/data/config.rda ',dir,'/store'))
}
load('store/config.rda')

ui <- fluidPage(
  
  fluidRow(
    br(),
    wellPanel(
      
      fluidRow(
        column(4,
               numericInput('maxNoteCount', 'Max Note Count', ifelse(length(config$maxNoteCount)>0,config$maxNoteCount,60), 1),
               bsTooltip(id = "maxNoteCount", title = "Maximum number of times to check for new notes per list (roughly once a second), if new notes have not been detected by this number of tries, then fail", 
                         placement = "bottom", trigger = "hover")
        ),
        column(4,
               numericInput('numNotesThresh', 'Number of Notes Threshold', ifelse(length(config$numNotesThresh)>0,config$numNotesThresh,10), 1),
               bsTooltip(id = "numNotesThresh", title = "Number of new notes required for list detection.  When starting cPLS, how many new notes are required for list to be detected.", 
                         placement = "bottom", trigger = "hover")
        )
    ),
  fluidRow(
    column(4,
           textInput('mailFrom', 'Sender Return Address for emails', ifelse(length(config$mailFrom)>0,config$mailFrom,'noreply@peerlendingserver.com')),
           bsTooltip(id = "mailFrom", title = "Sender return address for outgoing email", 
                     placement = "bottom", trigger = "hover")
    )
  ),  
  
  fluidRow(
    column(4,
           textInput('mailHost', 'SMTP Mail Host Server', ifelse(length(config$mailHost)>0,config$mailHost,'smtp.gmail.com')),
           bsTooltip(id = "mailHost", title = "SMTP Mail Host Server for outgoing email", 
                     placement = "bottom", trigger = "hover")
    ),
  
    column(2,
           numericInput('mailPort', 'SMTP Mail Port', ifelse(length(config$mailPort)>0,config$mailPort,465), 1),
           bsTooltip(id = "mailPort", title = "Port for SMTP Mail server for outgoing email", 
                     placement = "bottom", trigger = "hover")
    ),
    column(2,
           checkboxInput('mailSSL', 'SMTP SSL Enabled', ifelse(length(config$mailSSL)>0,config$mailSSL,TRUE)),
           bsTooltip(id = "mailSSL", title = "SSL Connection used for SMTP Mail server for outgoing email", 
                     placement = "bottom", trigger = "hover")
    )
    ),    
  
  fluidRow(
    column(4,
           textInput('mailUserName', 'SMTP Username', ifelse(length(config$mailUserName)>0,config$mailUserName,'youremail@gmail.com')),
           bsTooltip(id = "mailUserName", title = "Username for connecting to the email server", 
                     placement = "bottom", trigger = "hover")
    ),
    column(4,
           passwordInput('mailPassword', 'SMTP Password', ifelse(length(config$mailPassword)>0,config$mailPassword,'password')),
           bsTooltip(id = "mailPassword", title = "Password for connecting to the email server", 
                     placement = "bottom", trigger = "hover")
    )
  ),
  
  fluidRow(
    column(4,
           textInput('testemailaddress', 'Email Address for testing configuration', 'youremail@gmail.com'),
           bsTooltip(id = "testemailaddress", title = "Email Address for testing SMTP configuration", 
                     placement = "bottom", trigger = "hover")
           
    ),
    column(4, br(), actionButton('test', 'Send Test Email') )
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
      
      
      if (invalid(input$maxNoteCount)) {
        createAlert(session, anchorId = "alert", alertId="a1", 
                    content="Please enter a number greater than 0",
                    style = "danger",
                    append = FALSE)
        return()
      }
      
      if (invalid(input$numNotesThresh)) {
        createAlert(session, anchorId = "alert", alertId="a1", 
                    content="Please enter a number greater than 0",
                    style = "danger",
                    append = FALSE)
        return()
      }
      
      closeAlert(session, alertId="a1")
      
      # Save configuration data
      config <- reactiveValuesToList(input)
      
      save(config,file='store/config.rda')
      
      createAlert(session, anchorId = "alert", alertId="a1", 
                  content="Configuration saved",
                  style = "success",
                  append = FALSE)
      
    }
    if(input$test > buttonTestEmailValue){
      
      buttonTestEmailValue <<- input$test
      
      currentconfig <- reactiveValuesToList(input)
      
      if (exists('receipt')) rm(receipt)
      
      closeAlert(session, alertId="a1")
      
      tryCatch({
        receipt <- send.mail(from = currentconfig$mailFrom,
                             to = currentconfig$testemailaddress,
                             # cc = c('jmrpublic@gmail.com'),
                             subject = "PLS8 Test Message",
                             body = "This is just a test message to confirm your configuration",
                             html = TRUE,
                             inline = TRUE,
                             smtp = list(host.name = currentconfig$mailHost, port = currentconfig$mailPort, user.name = currentconfig$mailUserName, passwd = currentconfig$mailPassword, ssl = currentconfig$mailSSL),
                             authenticate = TRUE,
                             send = TRUE)
        createAlert(session, anchorId = "alert", alertId="a1", 
                    content="Email sent",
                    style = "success",
                    append = FALSE)},
        error = function(e) {
          createAlert(session, anchorId = "alert", alertId="a1", 
                      content="Error sending the email",
                      style = "danger",
                      append = FALSE)}
      )
    }
  })

  }

# Run the application 
shinyApp(ui = ui, server = server)

