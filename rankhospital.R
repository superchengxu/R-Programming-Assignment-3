rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c( "heart attack", "heart failure", "pneumonia")
  if (all(outcome_of_care_measures$State != state))
  {
    stop("invalid state")
  }
  if (all(outcomes != outcome))
  {
    stop("invalid outcome")
  }
  
  filter_state <- outcome_of_care_measures[outcome_of_care_measures$State == state,]
  if(outcome == "heart attack")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    filter_state <- filter_state[complete.cases(filter_state),]
    filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    len <- nrow(filter_state)
    
    if(num == "best")
    {
      answer <- head(filter_state$Hospital.Name,1)
    }
    else if(num == "worst")
    {
      answer <- tail(filter_state$Hospital.Name,1)
    }
    else
    {
      n <- as.numeric(num)
      if(n<=len)
      {
        answer <- filter_state$Hospital.Name[n]
      }
      else{
        answer <- NA
      }
    }
  }
  if(outcome == "heart failure")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filter_state <- filter_state[complete.cases(filter_state),]
    filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    len <- nrow(filter_state)
    
    if(num == "best")
    {
      answer <- head(filter_state$Hospital.Name,1)
    }
    else if(num == "worst")
    {
      answer <- tail(filter_state$Hospital.Name,1)
    }
    else
    {
      n <- as.numeric(num)
      if(n<=len)
      {
        answer <- filter_state$Hospital.Name[n]
      }
      else{
        answer <- NA
      }
    }
  }
  if(outcome == "pneumonia")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filter_state <- filter_state[complete.cases(filter_state),]
    filter_state <- filter_state[order(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    len <- nrow(filter_state)
    
    if(num == "best")
    {
      answer <- head(filter_state$Hospital.Name,1)
    }
    else if(num == "worst")
    {
      answer <- tail(filter_state$Hospital.Name,1)
    }
    else
    {
      n <- as.numeric(num)
      if(n<=len)
      {
        answer <- filter_state$Hospital.Name[n]
      }
      else{
        answer <- NA
      }
    }
  }
  answer
}

