best <- function(state, outcome) {
  
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table<-table[,c(2,7,11,17,23)]
  
  if(outcome == "heart attack"){table<-table[table[,3] != "Not Available",c(1,2,3)]}
  else if(outcome == "heart failure"){table<-table[table[,4] != "Not Available",c(1,2,4)]}
  else if(outcome == "pneumonia"){table<-table[table[,5] != "Not Available",c(1,2,5)]}
  else{return("Invalid outcome")}
  
  table[,3]<-as.numeric(table[,3])
  
  if(state %in% unique(table[,2])){table<-table[table[,2]==state,]}
    else{return("Invalid State")}
  
  colnames(table)<- c("Hospital","State","Rate")
  
  table<-table[order(table$Rate,table$Hospital),]
  
  table[1,1]
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}