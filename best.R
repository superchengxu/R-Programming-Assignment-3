best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#  print(outcome_of_care_measures)
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
#print(head(filter_state,1))

  if(outcome == "heart attack")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    m <- min(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm =TRUE)
    n <- filter_state[filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == m,]
    name <- n$Hospital.Name  
    sort_name <- sort(name)
    answer <- sort_name
  }
  if(outcome == "heart failure")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    m <- min(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm =TRUE)
    n <- filter_state[filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == m,]
    name <- n$Hospital.Name  
    sort_name <- sort(name)
    answer <- sort_name
  }
  if(outcome == "pneumonia")
  {
    filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    m <- min(filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm =TRUE)
    n <- filter_state[filter_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == m,]
    name <- n$Hospital.Name  
    sort_name <- sort(name)
    answer <- sort_name
  }
  answer
  
}
