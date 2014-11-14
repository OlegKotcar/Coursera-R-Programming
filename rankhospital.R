rankhospital <- function (state, outcome, num = "best"){
    dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    dataframe <- dataframe[c(2, 7, 11, 17, 23)]
    names(dataframe)[1] <- "name"
    names(dataframe)[2] <- "state"
    names(dataframe)[3] <- "heart attack"
    names(dataframe)[4] <- "heart failure"
    names(dataframe)[5] <- "pneumonia"
    
    ## If invalid state entered then stop with condition
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
    
    ## Check the state string
    outstates <- unique(dataframe[, 2])
    if( state %in% outstates == FALSE ) stop("invalid state")
    
    ## Validate the num value
    if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
    
    ## Grab only rows with our state value
    dataframe <- dataframe[dataframe$state==state & dataframe[outcome] != 'Not Available', ]
    
    ## Order the data
    dataframe[outcome] <- as.data.frame(sapply(dataframe[outcome], as.numeric))
    dataframe <- dataframe[order(dataframe$name, decreasing = FALSE), ]
    dataframe <- dataframe[order(dataframe[outcome], decreasing = FALSE), ]
    
    ## Process the num argument
    values <- dataframe[, outcome]
    if( num == "best" ) {
        rowNum <- which.min(values)
    } else if( num == "worst" ) {
        rowNum <- which.max(values)
    } else {
        rowNum <- num
    }
    ## Return hospital name in that state with lowest 30-day death rate
    dataframe[rowNum, ]$name
}
