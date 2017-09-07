rankall <- function(outcome, num = "best") {
  
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table<-table[,c(2,11,17,23)]
  
  if(outcome == "heart attack"){table<-table[table[,2] != "Not Available",c(1,2,3)]}
  else if(outcome == "heart failure"){table<-table[table[,3] != "Not Available",c(1,2,4)]}
  else if(outcome == "pneumonia"){table<-table[table[,4] != "Not Available",c(1,2,5)]}
  else{return("Invalid outcome")}
  
  table[,2]<-as.numeric(table[,3])
  
  colnames(table)<- c("Hospital","State","Rate")
  
  states<-unique(table[,2])

  for(state in states){table2[state]<-table[table[2,]==state,]}
  
  #table2
  
  #table<-table[order(table$State, table$Rate,table$Hospital),]
#  
 # if(num=="best"){table[1,1]}
#  else if(num=="worst"){table[dim(table)[1],1]}
#  else if(num>dim(table)[1]){return(NA)}
#  else{table<-table[num,1]}
  
#  table
  
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
