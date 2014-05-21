best <- function(state, outcome){
  
  ##Read the file from the data set
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ##Check the state for validity
  if (sum(data["State"]==state) == 0) {
    ##Stop the execution
    stop("Invalid state")
  }
  
  ##Check the outcome for Validity
  if ((outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia") == FALSE) {
    ##Stop the execution
    stop("Invalid outcome")
  }
  
  #Return the hospital names with lowest outcome
  if (outcome == "heart attack"){
    outdata <- subset(data, State == state)
    result <- min(as.numeric(outdata[,11]), na.rm=TRUE)
    hospname <- subset(outdata, as.numeric(outdata[,11]) == result)["Hospital.Name"]
  }else if (outcome == "heart failure"){
    outdata <- subset(data, State == state)
    result <- min(as.numeric(outdata[,17]), na.rm=TRUE)
    hospname <- subset(outdata, as.numeric(outdata[,17]) == result)["Hospital.Name"]
  }else {
    outdata <- subset(data, State == state)
    result <- min(as.numeric(outdata[,23]), na.rm=TRUE)
    hospname <- subset(outdata, as.numeric(outdata[,23]) == result)["Hospital.Name"]
  }
  
  #If more than one hospital name is obtained, sort the list
  hospname <- as.character(hospname)
  sort <- sort.list(hospname)
  hospname[sort[1]]

}