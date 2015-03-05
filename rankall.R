rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
#  if (all(outcome_of_care_measures$State != state))
# {
#    stop("invalid state")
#  }

  outcomes <- c( "heart attack", "heart failure", "pneumonia")
  if (all(outcomes != outcome))
  {
    stop("invalid outcome")
  }
 
#outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
#outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

statelist <- unique(outcome_of_care_measures$State)
hospitallist <- NA
stateno <- length(statelist)

###heart attach
if(outcome == "heart attack")
{
  outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcome_of_care_measures <- outcome_of_care_measures[complete.cases(outcome_of_care_measures),]
 
  if(num == "best")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
     #   temp <- filter_state[1,c(filter_state$Hospital.Name,filter_state$State)]
        hospitallist[i] <-head(filter_state$Hospital.Name,1)
    
      }
     i <- i+1
  
    }

  }
  else if(num == "worst")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        hospitallist[i] <-tail(filter_state$Hospital.Name,1)
      }
      i <- i+1
    }
  }
  else
  {
    i <- 1
    for(sta in statelist){
     
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        len <- nrow(filter_state)
        n <- as.numeric(num)
        if(n>len)
        {
          hospitallist[i] <- NA
        }
        else{
          filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      #    temp <- filter_state[n,c(filter_state$Hospital.Name,filter_state$State)]
      hospitallist[i] <- filter_state$Hospital.Name[n]
        }
      }
     i<- i+1
    }
    
    
  }
}

###heart failure

if(outcome == "heart failure")
{
  outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcome_of_care_measures <- outcome_of_care_measures[complete.cases(outcome_of_care_measures),]
  
  if(num == "best")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        #   temp <- filter_state[1,c(filter_state$Hospital.Name,filter_state$State)]
        hospitallist[i] <-head(filter_state$Hospital.Name,1)
        
      }
      i <- i+1
      
    }
    
  }
  else if(num == "worst")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        hospitallist[i] <-tail(filter_state$Hospital.Name,1)
      }
      i <- i+1
    }
  }
  else
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        len <- nrow(filter_state)
        n <- as.numeric(num)
        if(n>len)
        {
          hospitallist[i] <- NA
        }
        else{
          filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
          #    temp <- filter_state[n,c(filter_state$Hospital.Name,filter_state$State)]
          hospitallist[i] <- filter_state$Hospital.Name[n]
        }
      }
      i<- i+1
    }
    
    
  }
}

###pneumonia
if(outcome == "pneumonia")
{
  outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  outcome_of_care_measures <- outcome_of_care_measures[complete.cases(outcome_of_care_measures),]
  
  if(num == "best")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        #   temp <- filter_state[1,c(filter_state$Hospital.Name,filter_state$State)]
        hospitallist[i] <-head(filter_state$Hospital.Name,1)
        
      }
      i <- i+1
      
    }
    
  }
  else if(num == "worst")
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        hospitallist[i] <-tail(filter_state$Hospital.Name,1)
      }
      i <- i+1
    }
  }
  else
  {
    i <- 1
    for(sta in statelist){
      
      if(all(outcome_of_care_measures$State != sta)){
        hospitallist[i] <- NA
      }
      else{
        filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == sta,]
        len <- nrow(filter_state)
        n <- as.numeric(num)
        if(n>len)
        {
          hospitallist[i] <- NA
        }
        else{
          filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
          #    temp <- filter_state[n,c(filter_state$Hospital.Name,filter_state$State)]
          hospitallist[i] <- filter_state$Hospital.Name[n]
        }
      }
      i<- i+1
    }
    
    
  }
}
#print(statelist)
#print(statelist)
df <- data.frame(hospital=hospitallist,state=statelist)
df

##end
  
}
