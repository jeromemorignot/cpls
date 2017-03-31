
# Load trained model
info(log,'Loading machine learning model')
load('data/model.rda')

# Load cpls configuration
globalconfig <- 'store/config.rda'
reqFile(globalconfig)
scheduledtimes <- 'store/startTimes.rda'
reqFile(scheduledtimes)

info(log,'Importing configuration')

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
  err('Default user name not updated in config.R')
}

# Load all users from store sub-directory (must end with .acc extension)
info(log,'Loading user accounts')
users <- list()
ls <- read.csv('data/loans_sample.csv')
#files <- sort(list.files(path="store", pattern="*.acc", full.names=T, recursive=FALSE))
useraccounts <- 'store/users.rda'
load(useraccounts)

#for (file in files) {

  # Error if default user file exists
#  if (grepl("user_name.acc",file)) {
#    err('Default file "store/user_name.acc" exists')
#  }
  
#  lc=list()
#  source(file)
#  checkUser(file,lc)
  
  # Check filter syntax
#  res <- tryCatch({
#    ls %>% lc$filterCriteria()
#  }, error = function(e) {
#    err(paste('User (',lc$name,') - Filter error',sep=''))
#  })
  
  # Error if name is not configured (inidication that file was not updated)
#  if (lc$name == "FirstName LastName") {
#    err(paste('User name not configured in file:',file))
#  }
  
#  users <- append(users,list(lc))
#  info(log,paste("Importing user: ",lc$name,sep=''))
#}
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
}
if (length(users)==0) {
  err('No user accounts configured')
}

checkSums <- md5sum(sort(c(useraccounts,globalconfig,scheduledtimes)))
