
# Load trained model
info(log,'Loading machine learning model')
load('data/model.rda')

# Load cpls configuration
globalconfig <- 'store/config.rda'
reqFile(globalconfig)
scheduledtimes <- 'store/startTimes.rda'
reqFile(scheduledtimes)

info(log,'Importing configuration')

if(!file.exists(globalconfig) | !file.exists(scheduledtimes)){
  err('Configuration not available')
}

load(globalconfig)
load(scheduledtimes)

maxNoteCount <- config$maxNoteCount
numNotesThresh <- config$numNotesThresh
mailFrom <- config$mailFrom
host <- config$mailHost
port <- config$mailPort
userName <- config$mailUserName
userPasswd <- config$mailPassword
ssl <- config$mailSSL

checkConfig()



# Error if user name is not updated (indication that config.R not updated correctly)
if(userName=='user@domain.com') {
  err('Default user name not updated in configuration')
}

# Load all users from store sub-directory (must end with .acc extension)
info(log,'Loading user accounts')
users <- list()
ls <- read.csv('data/loans_sample.csv')
#files <- sort(list.files(path="store", pattern="*.acc", full.names=T, recursive=FALSE))
useraccounts <- 'store/users.rda'
if(!file.exists(useraccounts)){
  err('No user account configuration available')
}
load(useraccounts)

for(userid in 1:length(users)){
  
  #Check Filter Criteria
  tmpfilter <- unlist(lapply(1:1,function (i) { paste0('UserFilter <- function (x) {filter(x,',users[[userid]]$filterCriteria,')}')}))
  write(tmpfilter,file = 'tmp/tmpfilter.R')
  source('tmp/tmpfilter.R')
  
  res <- tryCatch({
       ls %>% UserFilter()
      }, error = function(e) {
        err(paste('User (',users[[userid]]$name,') - Filter error',sep=''))
      })
  users[[userid]]$filterCriteria <- UserFilter
  info(log,paste("Importing user: ",users[[userid]]$name,sep=''))
  if(users[[userid]]$portfolioId == "NA") users[[userid]]$portfolioId = FALSE
}
if (length(users)==0) {
  err('No user accounts configured')
}

checkSums <- md5sum(sort(c(useraccounts,globalconfig,scheduledtimes)))
