# Obtain list of invested loans for each user
for (i in 1:length(users)) {
  investedLoansFlag <- TRUE
  for (attempt in 1:3) {
    users[[i]]$pre$investedloans <- gURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/notes",sep=''),users[[i]]$token)
    
    if ( is.null(users[[i]]$pre$investedloans) | length(users[[i]]$pre$investedloans) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain list of loans already invested in (Null API Response). Attempt: ',attempt,sep=""))
      next
    }
    if ( ! grepl("loanId",users[[i]]$pre$investedloans) ) {
      warn(log,paste('User (',users[[i]]$name,') -  Unable to obtain list of loans already invested in (Invalid API Response). Attempt: ',attempt,sep=""))
      warn(log,paste('User (',users[[i]]$name,') - API Response: ', users[[i]]$pre$investedloans,sep=''))
      next
    }
    users[[i]]$pre$loanIds <- fromJSON(users[[i]]$pre$investedloans)$myNotes$loanId
#    print(users[[i]]$pre$loanIds)
    investedLoansFlag <- FALSE
    info(log,paste('User (',users[[i]]$name,') - Number of already invested notes: ',length(users[[i]]$pre$loanIds),sep=''))
    break
  }
}


if (investedLoansFlag) {
  warn(log,'Unable to get list of loans already invested in')
  next
}
