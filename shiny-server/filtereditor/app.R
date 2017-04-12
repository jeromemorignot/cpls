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
library(dplyr)
require(rCharts)
library(reshape2)

# Function to set home directory
dir = paste0(Sys.getenv("HOME"),'/cpls')

if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

if(!exists('notes')) {
  load('store/filterNotes.rda')
}

load('store/users.rda')

# Obtain total number of notes
totalNotes=nrow(notes)
filterStr <- ''
filterStrProd <- ''

if (length(users[[1]]$filter)>0){
  filter <- users[[1]]$filter
} else {
  filter <- list()
}

# Number formatter function
printNumber <- function(value, digits=0, sep=",", decimal=".") {
  formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal)
}

resetInputs <- function (session,inputs) {
  closeAlert(session, alertId="a1")
  for (name in names(inputs)) {
    if(invalid(inputs[[name]])) {
      next()
    }
    if (grepl('(Min$|Max$|model|customFilter)', name)) {
      updateNumericInput(session, name, value = '')
    } else {
      choices <- eval(parse(text=paste(name,'Choices',sep='')))
      updateSelectizeInput(session, name, choices=choices, selected='')
    }
  }
}

findUserId <- function (username, users) {
  for(i in 1:length(users)){
    if (users[[i]]$name == username) {return(i)}
  }
  return(0)
}

buttonSaveValue <- 0
buttonResetValue <- 0
buttonfilterSocialLender <- 0
buttonfilterBalancedROI <- 0
buttonApplyCustomFilter <- 0
initFilter <- 0
buttonSwitchUser <- 0
selectedUserId <- 1

fn <- ''
customFilterStr <- ''

numInput<-function (inputId, value = "", label='', ...) {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = filter[[inputId]], class='input-small', ...))
}

toolTipMin <- function(id,nas=0) {
  if(nas) {
    msg=paste('Historical min: ',min(notes[[id]],na.rm=TRUE),'Percent NA\'s: ',nas,sep='')
  } else {
    msg=paste('Historical min: ',min(notes[[id]],na.rm=TRUE), sep='')
  }
  bsTooltip(id = paste(id,'Min',sep=''), msg, 
            placement = "bottom", trigger = "hover")
}
toolTipMax <- function(id,nas=0) {
  bsTooltip(id = paste(id,'Max',sep=''), title = paste('Historical max:',max(notes[[id]],na.rm=TRUE)), 
            placement = "bottom", trigger = "hover")
}
setName <- function(str) {
  fn<<-str
  return(NULL)
}

termChoices <- c('36','60')
gradeChoices <- levels(notes$grade)
purposeChoices <- levels(notes$purpose)
homeOwnershipChoices <- levels(notes$homeOwnership)
subGradeChoices <- levels(notes$subGrade)
initialListStatusChoices <- levels(notes$initialListStatus)
addrStateChoices <- levels(notes$addrState)
empLengthChoices <- sort(unique(notes$empLength))
isIncVChoices <- levels(notes$isIncV)
completeChoices <- c(TRUE,FALSE)
reviewStatusSpecChoices <- c('APPROVED','NOT_APPROVED')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(2,
    selectInput('username','',choices = lapply(1:length(users), function(x) {users[[x]]$name}), multiple = FALSE, selected = users[[1]]$name)),
    br()
  ),
  fluidRow(
    column(4,
           wellPanel(  
             bsCollapse(multiple = TRUE, open = NULL, id = "collapse1",
                        
                        
                        bsCollapsePanel("Common", 
                                        
                                        selectInput('term', 'Loan Term', 
                                                    choices=termChoices,
                                                    multiple = TRUE,
                                                    selected = filter$term,
                                                    selectize = TRUE),
                                        
                                        selectInput('grade', 'Grade', 
                                                    choices=gradeChoices,
                                                    multiple = TRUE,
                                                    selected = filter$grade,
                                                    selectize = TRUE),
                                        
                                        setName('intRate'),
                                        "Interest Rate", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn,round(mean(is.na(notes[[fn]])))),
                                        toolTipMax(fn),
                                        
                                        selectInput('purpose', 'Purpose', 
                                                    choices=purposeChoices,
                                                    multiple = TRUE,
                                                    selected = filter$purpose,
                                                    selectize = TRUE),
                                        
                                        selectInput('homeOwnership', 'Home Ownership', 
                                                    choices=homeOwnershipChoices,
                                                    multiple = TRUE,
                                                    selected = filter$homeOwnership,
                                                    selectize = TRUE),
                                        
                                        setName('loanAmount'),
                                        "Loan Amount", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('pubRec'),
                                        "Derogatory Public Records", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('inqLast6Mths'),
                                        "Inquiries Last 6 Months", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('delinq2Yrs'),
                                        "Delinquencies Last 2 Years", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('dti'),
                                        "Debt to Income Ratio", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('revolUtil'),
                                        "Revolving Line Utilization", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('ficoRangeHigh'),
                                        "FICO Range High", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        id="col1", value="test1"),
                        
                        
                        
                        ### Credit History
                        bsCollapsePanel("Credit History",
                                        
                                        setName('mthsSinceLastDelinq'),
                                        "Months Since Last Delinquency", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('mthsSinceLastRecord'),
                                        "Months Since Last Public Record", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('openAcc'),
                                        "Number of Open Credit Lines", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('revolBal'),
                                        "Total Credit Revolving Balance", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('totalAcc'),
                                        "Number of Credit Lines", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('mthsSinceLastMajorDerog'),
                                        "Months Since Last Major Derogatory", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('collections12MthsExMed'),
                                        "1 Year Collections Except Medical", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('ficoRangeLow'),
                                        "FICO Range Low", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),br(),
                                        
                                        
                                        
                                        id="col2", value="test2"),
                        
                        
                        
                        ### Loan Detail
                        bsCollapsePanel("Loan Detail",
                                        
                                        
                                        
                                        
                                        
                                        selectInput('subGrade', 'Sub Grade', 
                                                    choices=subGradeChoices,
                                                    multiple = TRUE,
                                                    selected = filter$subGrade,
                                                    selectize = TRUE),
                                        
                                        selectInput('initialListStatus', 'Initial List Status', 
                                                    choices=initialListStatusChoices,
                                                    multiple = TRUE,
                                                    selected = filter$initialListStatus,
                                                    selectize = TRUE),
                                        
                                        setName('installment'),
                                        "Installment Amount", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),br(),
                                        
                                        id="col3", value="test3"),
                        
                        
                        ### About Borrower
                        bsCollapsePanel("About Borrower",
                                        
                                        selectInput('addrState', 'State', 
                                                    choices=addrStateChoices,
                                                    multiple = TRUE,
                                                    selected = filter$addrState,
                                                    selectize = TRUE),
                                        
                                        
                                        selectInput('empLength', 'Employment Length', 
                                                    choices=empLengthChoices,
                                                    multiple = TRUE,
                                                    selected = filter$empLength,
                                                    selectize = TRUE),
                                        
                                        
                                        
                                        selectInput('isIncV', 'Is Income Verified', 
                                                    choices=isIncVChoices,
                                                    multiple = TRUE,
                                                    selected = filter$isIncV,
                                                    selectize = TRUE),
                                        
                                        setName('annualInc'),
                                        "Annual Income", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),br(),
                                        
                                        
                                        id="col4", value="test4"),
                        
                        
                        
                        
                        
                        
                        bsCollapsePanel("Custom",
                                        
                                        tags$blockquote("Fields provided by PLS"),
                                        
                                        setName('model'),
                                        "Artificial Intelligence Model (1 = 100%)", br(),
                                        numInput(paste(fn,'Min',sep=''), step=0.01, min="0", max="1"),"  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=0.01, min="0", max="1"),br(),p(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        
                                        setName('earliestCrLineMonths'),
                                        "Earliest Credit Line Months", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),p(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('amountTermIncomeRatio'),
                                        "Amount per Term Income Ratio (1=100%)", br(),
                                        numInput(paste(fn,'Min',sep=''), step=0.01, min="0", max="1"), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=0.01, min="0", max="1"),br(),p(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        setName('revolBalAnnualIncRatio'),
                                        "Revolving Balance Income Ratio", br(),
                                        numInput(paste(fn,'Min',sep=''), step=1), "  to  ",
                                        numInput(paste(fn,'Max',sep=''), step=1),br(),p(),
                                        toolTipMin(fn),
                                        toolTipMax(fn),
                                        
                                        tags$label(id='customFilterLabel','Custom Filter - Experts Only'),
                                        tags$textarea(id="customFilter", rows=3, cols=41, filter$customFilter),
                                        bsTooltip(id = 'customFilter', title = 'Note custom filter parameters use slightly different syntax than SQL.  You can use the filter criteria given on the right pane as a guide as well as to obtain the proper field names.  You can use paranthesis to group statements as well as | and & as logical operators.', 
                                                  placement = "top", trigger = "hover"),
                                        
                                        actionButton("applyCustomFilter", "Apply Custom Filter"),
                                        
                                        
                                        id="col5", value="test5"),
                        
                        #             bsCollapsePanel("Special Fields",
                        #               
                        #               tags$blockquote("Fields without history (*), or fields not used in saved filter criteria (-)"),
                        #               
                        #               
                        #               selectInput('complete', 'Completed Notes -', 
                        #                 choices=completeChoices,
                        #                 multiple = TRUE,
                        #                 selected = filter$complete,
                        #                 selectize = TRUE),
                        #               
                        #               
                        #               setName('expDefaultRate'),
                        #               "Expected Default Rate *", br(),
                        #               numInput(paste(fn,'MinNonfilter',sep=''), step=.1), "  to  ",
                        #               numInput(paste(fn,'MaxNonfilter',sep=''), step=.1),
                        #             
                        #               selectInput('reviewStatusSpec', 'Review Status *', 
                        #                 choices=reviewStatusSpecChoices,
                        #                 multiple = TRUE,
                        #                 selected = filter$reviewStatusNonfilter,
                        #                 selectize = TRUE),
                        #               
                        #               setName('investorCount'),
                        #               "Investor Count *", br(),
                        #               numInput(paste(fn,'MinNonfilter',sep=''), step=1), "  to  ",
                        #               numInput(paste(fn,'MaxNonfilter',sep=''), step=1),br(),p(),
                        #               
                        #               
                        #               id="col6", value="test6"),
                        
                        
                        
                        bsCollapsePanel("Preset Filters",
                                        
                                        tags$blockquote("Under construction ..."),
                                        actionButton('filterSocialLender', 'Social Lender'),
                                        actionButton('filterBalancedROI', 'Balanced ROI'),
                                        
                                        id="col7", value="test7")
             ),
             checkboxInput('complete', 'Completed Notes Only', value = FALSE)
           ),
           fluidRow(
             actionButton('save', 'Save'),
             actionButton("reset", "Clear")
#             tags$button( "Close", id="close", type="button", class="btn action-button", onclick="self.close()")
           ),
           fluidRow(
             br(),
             bsAlert(anchorId="alert")
           )
    ),
    column(8,
           textOutput('text'),
           uiOutput('summary'),
           showOutput("gaugeROI", "highcharts"),
           showOutput("roi", "highcharts"),
           showOutput("count", "highcharts"),
           uiOutput("criteria")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  observe({
    if(input$save > buttonSaveValue) {
      buttonSaveValue <<- input$save
      
      #         if (input$name=='') {
      #           createAlert(session, inputId = "alert", alertId="a1", 
      #             message="Please enter full name",
      #             type = "danger",
      #             append = FALSE)
      #           return()
      #         }
      
      
      closeAlert(session, alertId="a1")
      
      # Save configuration data
      filter <- reactiveValuesToList(input)
      
      if(filterStrProd == 'intRate > 0') {
        createAlert(session, anchorId = "alert", alertId="a1", 
                    content ="No filter to save",
                    style = "danger",
                    append = FALSE)
        return()
      }
      
      print(filterStrProd)
      
      load('store/users.rda')
      
      users[[findUserId(input$username,users)]]$filterCriteria <- filterStrProd
      users[[findUserId(input$username,users)]]$filter <- filter
      
      save(users,file='store/users.rda')
      
      createAlert(session, anchorId = "alert", alertId="a1", 
                  content="Configuration saved",
                  style = "success",
                  append = FALSE)
      
    }
  })
  
  # Reset button
  observe({
    if(input$reset > buttonResetValue) {
      buttonResetValue <<- input$reset
      resetInputs(session,inputs)
      customFilterStr<<-''
      #updateNumericInput(session, "intRateMin", value = '')
    } 
  })
  
  # Save custom filter button
  observe({
    if(input$applyCustomFilter > buttonApplyCustomFilter) {
      buttonApplyCustomFilter <<- input$applyCustomFilter
      if (input$customFilter != ''){ 
        customFilterStr <<- gsub("[\r\n]", "", input$customFilter)
      } else {
        customFilterStr <<- ''
      }
    } 
  }, priority=100)
  
  observe({
    if(input$filterSocialLender > buttonfilterSocialLender) {
      buttonfilterSocialLender <<- input$filterSocialLender
      resetInputs(session,inputs)
      updateNumericInput(session, "intRateMin", value = '14')
      updateNumericInput(session, "delinq2YrsMax", value = '0')
      updateNumericInput(session, "installmentIncomeRatioMax", value = '0.15')
      updateNumericInput(session, "modelMin", value = '0.85')
      updateNumericInput(session, "pubRecMax", value = '0')
      updateNumericInput(session, "inqLast6MthsMax", value = '2')
      updateSelectInput(session, 'grade', selected = c('B','C','D','E'))
      #         updateSelectInput(session, 'purpose', selected = c("credit_card", "debt_consolidation", "car", 
      #           "home_improvement", "educational", "house", "moving", "other", "renewable_energy", "major_purchase", 
      #           "vacation", "wedding"))
      # updateSelectInput(session, 'empLength', selected = c('12','24','36','48','60','72','84','96','108','120'))
    } 
  })
  
  observe({
    if(input$filterBalancedROI > buttonfilterBalancedROI) {
      buttonfilterBalancedROI <<- input$filterBalancedROI
      resetInputs(session,inputs)
      updateSelectInput(session, 'grade', selected = c('B','C','D'))
      updateNumericInput(session, "delinq2YrsMax", value = '0')
      updateNumericInput(session, "pubRecMax", value = '0')
      updateNumericInput(session, "modelMin", value = '0.90')
      updateNumericInput(session, "earliestCrLineMonthsMin", value = '120')
      updateNumericInput(session, "collections12MthsExMedMax", value = '0')
      updateSelectInput(session, 'empLength', selected = c('12','24','36','48','60','72','84','96','108','120'))
    } 
  })
  
  observe({
    currentSelectedUserId = findUserId(input$username,users)
  if(selectedUserId != currentSelectedUserId){
      selectedUserId <<- currentSelectedUserId
      resetInputs(session,inputs)
      load('store/users.rda')
      updateSelectInput(session, 'initialListStatus', selected = users[[selectedUserId]]$filter$initialListStatus)
      updateSelectInput(session, 'subGrade', selected = users[[selectedUserId]]$filter$subGrade)
      updateSelectInput(session, 'grade', selected = users[[selectedUserId]]$filter$grade)
      updateSelectInput(session, 'isIncV', selected = users[[selectedUserId]]$filter$isIncV)
      updateNumericInput(session, "revolBalAnnualIncRatioMax", value = users[[selectedUserId]]$filter$revolBalAnnualIncRatioMax)
      updateNumericInput(session, "mthsSinceLastRecordMax", value = users[[selectedUserId]]$filter$mthsSinceLastRecordMax)
      updateNumericInput(session, "mthsSinceLastMajorDerogMax", value = users[[selectedUserId]]$filter$mthsSinceLastMajorDerogMax)
      updateNumericInput(session, "revolUtilMax", value = users[[selectedUserId]]$filter$revolUtilMax)
      updateNumericInput(session, "revolBalMax", value = users[[selectedUserId]]$filter$revolBalMax)
      updateNumericInput(session, "earliestCrLineMonthsMin", value = users[[selectedUserId]]$filter$earliestCrLineMonthsMin)
      updateNumericInput(session, "inqLast6MthsMin", value = users[[selectedUserId]]$filter$inqLast6MthsMin)
      updateNumericInput(session, "intRateMin", value = users[[selectedUserId]]$filter$intRateMin)
      updateNumericInput(session, "delinq2YrsMin", value = users[[selectedUserId]]$filter$delinq2YrsMin)
      updateNumericInput(session, "openAccMin", value = users[[selectedUserId]]$filter$openAccMin)
      updateNumericInput(session, "annualIncMin", value = users[[selectedUserId]]$filter$annualIncMin)
      updateSelectInput(session, 'homeOwnership', selected = users[[selectedUserId]]$filter$homeOwnership)
      updateTextAreaInput(session, 'customFilter', value = users[[selectedUserId]]$filter$customFilter)
      customFilterStr <<- users[[selectedUserId]]$filter$customFilter
      updateSelectInput(session, 'addrState', selected = users[[selectedUserId]]$filter$addrState)
      updateSelectInput(session, 'purpose', selected = users[[selectedUserId]]$filter$purpose)
      updateNumericInput(session, "earliestCrLineMonthsMax", value = users[[selectedUserId]]$filter$earliestCrLineMonthsMax)
      updateNumericInput(session, "pubRecMax", value = users[[selectedUserId]]$filter$pubRecMax)
      updateNumericInput(session, "ficoRangeHighMax", value = users[[selectedUserId]]$filter$ficoRangeHighMax)
      updateNumericInput(session, "openAccMax", value = users[[selectedUserId]]$filter$openAccMax)
      updateNumericInput(session, "intRateMax", value = users[[selectedUserId]]$filter$intRateMax)
      updateNumericInput(session, "mthsSinceLastDelinqMax", value = users[[selectedUserId]]$filter$mthsSinceLastDelinqMax)
      updateNumericInput(session, "totalAccMax", value = users[[selectedUserId]]$filter$totalAccMax)
      updateNumericInput(session, "installmentMax", value = users[[selectedUserId]]$filter$installmentMax)
      updateSelectInput(session, 'empLength', selected = users[[selectedUserId]]$filter$empLength)
      updateNumericInput(session, "dtiMax", value = users[[selectedUserId]]$filter$dtiMax)
      updateSelectInput(session, 'term', selected = users[[selectedUserId]]$filter$term)
      updateNumericInput(session, "revolBalAnnualIncRatioMin", value = users[[selectedUserId]]$filter$revolBalAnnualIncRatioMin)
      updateNumericInput(session, "modelMin", value = users[[selectedUserId]]$filter$modelMin)
      updateNumericInput(session, "mthsSinceLastDelinqMin", value = users[[selectedUserId]]$filter$mthsSinceLastDelinqMin)
      updateNumericInput(session, "revolBalMin", value = users[[selectedUserId]]$filter$revolBalMin)
      updateNumericInput(session, "mthsSinceLastMajorDerogMin", value = users[[selectedUserId]]$filter$mthsSinceLastMajorDerogMin)
      updateNumericInput(session, "collections12MthsExMedMin", value = users[[selectedUserId]]$filter$collections12MthsExMedMin)
#      updateNumericInput(session, "installmentIncomeRatioMax", value = users[[selectedUserId]]$filter$installmentIncomeRatioMax)
      updateNumericInput(session, "amountTermIncomeRatioMax", value = users[[selectedUserId]]$filter$amountTermIncomeRatioMax)
      updateNumericInput(session, "modelMax", value = users[[selectedUserId]]$filter$modelMax)
      updateNumericInput(session, "ficoRangeLowMax", value = users[[selectedUserId]]$filter$ficoRangeLowMax)
      updateNumericInput(session, "annualIncMax", value = users[[selectedUserId]]$filter$annualIncMax)
      updateNumericInput(session, "loanAmountMax", value = users[[selectedUserId]]$filter$loanAmountMax)
      updateNumericInput(session, "collections12MthsExMedMax", value = users[[selectedUserId]]$filter$collections12MthsExMedMax)
      updateNumericInput(session, "inqLast6MthsMax", value = users[[selectedUserId]]$filter$inqLast6MthsMax)
#      updateNumericInput(session, "installmentIncomeRatioMin", value = users[[selectedUserId]]$filter$installmentIncomeRatioMin)
      updateNumericInput(session, "amountTermIncomeRatioMin", value = users[[selectedUserId]]$filter$amountTermIncomeRatioMin)
      updateNumericInput(session, "dtiMin", value = users[[selectedUserId]]$filter$dtiMin)
      updateNumericInput(session, "ficoRangeLowMin", value = users[[selectedUserId]]$filter$ficoRangeLowMin)
      updateNumericInput(session, "installmentMin", value = users[[selectedUserId]]$filter$installmentMin)
      updateNumericInput(session, "revolUtilMin", value = users[[selectedUserId]]$filter$revolUtilMin)
      updateNumericInput(session, "pubRecMin", value = users[[selectedUserId]]$filter$pubRecMin)
      updateNumericInput(session, "ficoRangeHighMin", value = users[[selectedUserId]]$filter$ficoRangeHighMin)
      updateNumericInput(session, "loanAmountMin", value = users[[selectedUserId]]$filter$loanAmountMin)
      updateNumericInput(session, "mthsSinceLastRecordMin", value = users[[selectedUserId]]$filter$mthsSinceLastRecordMin)
      updateNumericInput(session, "totalAccMin", value = users[[selectedUserId]]$filter$totalAccMin)
    } 
  })
  
  dataInput <- reactive({
    # Dynamically obtain field names UI input
    inputsRaw <<- reactiveValuesToList(input)
    nNotes <<- c(names(notes),'customFilter')
    nMin <<- paste(nNotes, "Min", sep="")
    nMax <<- paste(nNotes, "Max", sep="")
    nAll <<- c(nNotes,nMin,nMax)
    inputs <<- inputsRaw[names(inputsRaw) %in% nAll]
    
    
    
    filterStr <<- ''
    filterStrProd <<- ''
    
    for (name in names(inputs)) {
      if(invalid(inputs[[name]])) {
        # print('nope')
        next()
      }
      # print(name)
      if (name == 'complete') {
        if(input$complete==FALSE) next
        field <- name
        # expr <- paste(field,'%in%',deparse(as.logical(inputs[[name]])))
        filterStr <<- paste(filterStr,'(complete==TRUE & (loan_status=="Fully_Paid" | loan_status=="Charged_Off"))',sep=' & ')
      } else if (name == 'customFilter') {
        if (initFilter == 0) {
          customFilterStr <<- gsub("[\r\n]", "", input$customFilter)
          initFilter <<- 1
          if (customFilterStr!=''){
            filterStr <<- paste(filterStr,' & ',customFilterStr,sep='')
            filterStrProd <<- paste(filterStrProd,' & ',customFilterStr, sep='')
          }
        } else{
          next 
        }
        
      } else if (grepl('Min$', name)) {
        field <- substr(name, 1, nchar(name)-3)
        expr <- paste(field,'>=',inputs[[name]])
        filterStr <<- paste(filterStr,expr,sep=' & ')
        filterStrProd <<- paste(filterStrProd,expr,sep=' & ')
      } else if (grepl('Max$', name)) {
        field <- substr(name, 1, nchar(name)-3)
        expr <- paste(field,'<=',inputs[[name]])
        filterStr <<- paste(filterStr,expr,sep=' & ')
        filterStrProd <<- paste(filterStrProd,expr,sep=' & ')
      } else {
        field <- name
        expr <- paste(field,'%in%',deparse(inputs[[name]],width.cutoff=500))
        filterStr <<- paste(filterStr,expr,sep=' & ')
        filterStrProd <<- paste(filterStrProd,expr,sep=' & ')
      }
    }
    
    filterStr <<- gsub('^ & ','',filterStr)
    filterStrProd <<- gsub('^ & ','',filterStrProd)
    
    if (customFilterStr!=''){
      filterStr <<- paste(filterStr,' & ',customFilterStr,sep='')
      filterStrProd <<- paste(filterStrProd,' & ',customFilterStr, sep='')
    }
    
    filterStr <<- gsub('^ & ','',filterStr)
    filterStrProd <<- gsub('^ & ','',filterStrProd)
    
    if (filterStr == '') filterStr <<- 'intRate > 0'
    if (filterStrProd == '') filterStrProd <<- 'intRate > 0'
    
    if(input$complete) {
      totalNotes <<- nrow(filter_(notes, '(complete==TRUE & (loan_status=="Fully_Paid" | loan_status=="Charged_Off"))'))
    } else { 
      totalNotes <<- nrow(notes)
    }
    
    # print(filterStr)
    table <-filter_(notes, filterStr)
    
    if (nrow(table) > 0) {
      table <- group_by(table, grade) %>%
        summarise(averageROI=round(mean(ROI),2),
                  projectedROI=round(mean(projROI),2),
                  noteCount=n(),
                  defCount=sum(isDef),
                  l16Count=sum(isL16),
                  l31Count=sum(isL31),
                  ageMonths=round(mean(ageMths)) ) %>%
        # standardDeviation=round(sd(projROI),2) ) %>%
        mutate(percentNotes = round ( 100 * noteCount/sum(noteCount), 2) ) %>%
        mutate(percentDef = round ( 100 * defCount/noteCount, 2) ) %>%
        mutate(percentL16 = round ( 100 * l16Count/noteCount, 2) ) %>%
        mutate(percentL31 = round ( 100 * l31Count/noteCount, 2) )
      
    }
    table
    
    
  })
  
  output$text <- renderText({ 
    if (nrow(dataInput())==0) {
      'No records found'
    } else {
      ''
      # print(dataInput())
    }
  })
  
  output$roi <- renderChart({ 
    di <- dataInput()
    if (nrow(di) > 0) {
      d=subset(melt(di, id = "grade"), variable %in% c('averageROI','projectedROI','percentDef'))
      h=hPlot(value ~ grade, data = d, group='variable', type = c('column','column','column'), title = "Return by Grade")
      h$plotOptions(column = list(colorByPoint = F, animation = list(duration=500)))
      h$xAxis(title = list(text = "Loan Grade"), categories = unique(d$grade))
      h$yAxis(title = list(text = "ROI"))
      h$addParams(dom = 'roi')
      # print(h)
      return(h)
    } else {
      emptyChart <- rCharts::Highcharts$new()
      emptyChart$chart(type = "gauge",height=1)
      emptyChart$addParams(dom = 'roi')
      return(emptyChart)
    }
  })
  
  output$count <- renderChart({
    di <- dataInput()
    if (nrow(di) > 0) {
      h=hPlot(value ~ grade, data = subset(melt(di, id = "grade"), variable %in% c('percentNotes')), group='variable', type = c('pie'), title = "Percent Distribution by Grade")
      h$plotOptions(pie = list(animation = list(duration=500)))
      h$addParams(dom = 'count')
      return(h)
    } else {
      emptyChart <- rCharts::Highcharts$new()
      emptyChart$chart(type = "gauge",height=1)
      emptyChart$addParams(dom = 'count')
      return(emptyChart)
    }
  })
  
  output$gaugeROI <- renderChart({ 
    di <<- dataInput()
    if (nrow(di)>0) {
      numNotes<<-round(sum(di$noteCount))
      roi<<-round(sum(di$averageROI*di$noteCount)/numNotes,2)
      if (roi<0) {
        color='#DF5353'
      } else {
        color='#55BF3B'
      }
      
      #pct=round(numNotes/totalNotes*100,2)
      proi=round(sum(di$projectedROI*di$noteCount)/numNotes,2)
      if (proi<0) {
        pcolor='#DF5353'
      } else {
        pcolor='#55BF3B'
      }
      
      maxROI = max(c(roi,proi))
      minROI = min(c(roi,proi))
      
      a <- rCharts::Highcharts$new()
      a$chart(type = "gauge",height=180)
      
      a$title(text="Portfolio Performance")
      
      a$pane(
        list(startAngle=-90,endAngle=90,size=220,background=list(shape='arc'),center=list('25%','90%')),
        list(startAngle=-90,endAngle=90,size=220,background=list(shape='arc'),center=list('75%','90%'))
      )
      
      a$plotOptions(gauge = list(dataLabels=FALSE,
                                 animation = list(duration=700)))
      
      a$series(list(
        list(data = list(roi), name = "ROI", yAxis = 0, 
             pivot=list(backgroundColor=color), dial=list(backgroundColor=color)),
        list(data = list(proi), name = "Projected ROI", yAxis = 1,
             pivot=list(backgroundColor='#7cb5ec'), dial=list(backgroundColor=pcolor))))
      
      a$yAxis(list(
        list(title = list(text = "Instant ROI"), min = minROI-5, max=maxROI+5, pane=0),
        list(title = list(text = "Projected ROI"), min=minROI-5, max=maxROI+5, pane = 1)
      ))
      # more(a$print())
      
      a$addParams(dom = 'gaugeROI')
      return(a)
      
    } else {
      emptyChart <- rCharts::Highcharts$new()
      emptyChart$chart(type = "gauge",height=1)
      emptyChart$addParams(dom = 'gaugeROI')
      return(emptyChart)
    }
    
  })
  
  # Filter criteria 
  output$criteria <-renderUI({
    
    di=dataInput()
    
    f <- filterStrProd
    if(filterStrProd=='intRate > 0') f <- 'NA'
    
    fluidRow(
      h4("Filter Criteria",style="font-family: 'verdana'; font-weight: 500; line-height: 1.2;", align='center'),
      wellPanel(
        f
      )       
    )
    
  })
  
  # Summary
  output$summary <-renderUI({
    di=dataInput()
    if (nrow(di)>0) {
      numNotes=round(sum(di$noteCount))
      pct=round(numNotes/totalNotes*100,2)
      roi=round(sum(di$averageROI*di$noteCount)/numNotes,2)
      # print(input$complete)
      proi=round(sum(di$projectedROI*di$noteCount)/numNotes,2)
      age=round(sum(di$ageMonths*di$noteCount)/numNotes)
      def=round(sum(di$percentDef*di$noteCount)/numNotes,2)
      l16=round(sum(di$percentL16*di$noteCount)/numNotes,2)
      l31=round(sum(di$percentL31*di$noteCount)/numNotes,2)
      fluidRow(
        h4("Summary",style="font-family: 'verdana'; font-weight: 500; line-height: 1.2;", align='center'),
        wellPanel(
          fluidRow(
            column(4,paste('Total Notes:',printNumber(totalNotes))),
            column(4,paste('Filtered Notes:',printNumber(numNotes))),
            column(4,paste('Filtered Notes: ',pct,'%',sep=''))
          ),
          fluidRow(
            column(4,paste('Instant ROI: ',roi,'%',sep='')),
            column(4,paste('Projected ROI: ',proi,'%',sep='')),
            column(4,paste('Age Months:',age))
          ),
          fluidRow(
            column(4,paste('Default Percent: ',def,'%',sep='')),
            column(4,paste('Late 16: ',l16,'%',sep='')),
            column(4,paste('Late 31: ',l31,'%',sep=''))
          )
        )       
      )
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

