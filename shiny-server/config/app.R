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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

