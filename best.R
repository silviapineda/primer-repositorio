best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ## HOLA
  o <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!state %in% o$State) {
    stop("invalid state")
  }
  
  if(outcome == "heart attack") {
    column <- names(o)[11]
  }else if(outcome == "heart failure") {
    column <-  names(o)[17]
  }else if(outcome=="pneumonia"){
    column <-  names(o)[23]
  } else {
    stop("invalid outcome")
  }
  
  stateOutcome <- o[o$State==state,]
  cleanedOutcome <- stateOutcome[stateOutcome[column]!="Not Available",]
  cleanedOutcome$Hospital.Name[which.min(cleanedOutcome[[column]])]
}
