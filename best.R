best <- function(state, outcome) {
  
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table<-table[,c(2,7,11,17,23)]
  
  if(outcome == "heart attack"){table<-table[table[,3] != "Not Available",c(1,2,3)]}
  if(outcome == "heart failure"){table<-table[table[,3] != "Not Available",c(1,2,4)]}
  if(outcome == "pneumonia"){table<-table[table[,3] != "Not Available",c(1,2,5)]}
  
  table[,3]<-as.numeric(table[,3])
  
  table
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}