best <- function(state, outcome){
    ## If invalid state entered then stop with condition
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
        
    ## Read outcome data
    dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Filter and rename column for simplify their names from all dataframe
    dataframe <- dataframe[c(2, 7, 11, 17, 23)]
    names(dataframe)[1] <- "name"
    names(dataframe)[2] <- "state"
    names(dataframe)[3] <- "heart attack"
    names(dataframe)[4] <- "heart failure"
    names(dataframe)[5] <- "pneumonia"
    
    ## Check the state string
    outstates <- unique(dataframe[, 2])

    if( state %in% outstates == FALSE ) stop("invalid state")
    
    ## Grab only rows with our state value without NA
    dataframe <- dataframe[dataframe$state==state & dataframe[outcome] != 'Not Available', ]
    values <- dataframe[, outcome]
    #print (which.min(values))
    
    ## Return hospital name in that state with lowest 30-day death rate
    dataframe[which.min(values), ]$name
}

