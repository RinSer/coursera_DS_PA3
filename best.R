best <- function(state, outcome) {
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(!(state %in% table[, 7])) {
                stop("invalid state")
        }
        
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        ## Return hosplital name in that state with lowest 30-day death rate
        stateOutcome <- subset(table, State == state)
        
        ## Find the hospitals with minimum death rate
        if(outcome == "heart attack") {
                colnum <- 11
        } else if (outcome == "heart failure") {
                colnum <- 17
        } else {
                colnum <- 23
        }
        
        stateOutcome[, colnum] <- as.numeric(stateOutcome[, colnum])
        
        hospitals <- with(stateOutcome, Hospital.Name[ stateOutcome[, colnum] == min(stateOutcome[, colnum], na.rm=TRUE)])
        
        hospital <- sort(hospitals)
        
        hospital[1]
        
}