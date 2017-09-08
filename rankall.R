rankall <- function(outcome, num = "best") {
  #library(foreach)
  
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table<-table[,c(2,7,11,17,23)]
  
  if(outcome == "heart attack"){table<-table[table[,3] != "Not Available",c(1,2,3)]}
  else if(outcome == "heart failure"){table<-table[table[,4] != "Not Available",c(1,2,4)]}
  else if(outcome == "pneumonia"){table<-table[table[,5] != "Not Available",c(1,2,5)]}
  else{return("Invalid outcome")}
  
  table[,3]<-as.numeric(table[,3])
  
  colnames(table)<- c("Hospital","State","Rate")
  
  table<-table[order(table$State, table$Rate, table$Hospital),]
  
  states<-unique(table[,2])
  
  output_data<-data.frame(matrix(ncol = 2, nrow = 0))
  #data.frame(matrix(ncol = 10000, nrow = 0))
  
  colnames(output_data)<- c("Hospital","State")
  
  for(i in 1:length(states)){
    
    partial_data<-subset(table, State ==states[i])
    
    if(num=="best"){partial_data<-partial_data[1,c(1,2)]}
    else if(num=="worst"){partial_data<-partial_data[dim(partial_data)[1],c(1,2)]}
    else if(num>dim(partial_data)[1]){
      partial_data<-data.frame(cbind("NA", states[i]))
      colnames(partial_data)<- c("Hospital","State")}
    else{partial_data<-partial_data[num,c(1,2)]}
    
    output_data<-rbind(output_data, partial_data)
    
  }
  

  
   output_data 
  #table
  
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}