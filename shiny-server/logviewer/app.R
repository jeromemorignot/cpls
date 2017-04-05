library('shiny')
library('readr')

# Function to set home directory
dir = paste0(Sys.getenv("HOME"),'/cpls')

if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

ui <- fluidPage(
  titlePanel("System Log Viewer"),
  wellPanel(fluidRow(
    column(12,
           dataTableOutput(outputId="log"),
           br()
    )
  ))
)
server <- function(input, output, session) {
  
  data <- reactiveFileReader(1000, 
                             session, 
                             'store/system.log',
                             read_fwf,
                             skip=0,
                             fwf_positions( c(1,25,30),c(23,29,NA),c('Date','Priority','Message')))
  
  output$log <- renderDataTable({
    log <- data()
    log$Date <- gsub('(\\[|\\])','',log$Date)
    log
  },options = list(pageLength = 100,
                   lengthMenu = c(100, 250, 1000),
                   order = c(0,'desc'),
                   autoWidth = TRUE,
                   columnDefs = list(
                     list(width = "140px", targets = 0),
                     list(width = "60px", targets = 1)
                   )
                   
  ))
}
shinyApp(ui = ui, server = server)
