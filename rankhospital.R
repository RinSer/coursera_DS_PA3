rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hosplital rank in that state with lowest 30-day death rate
        stateOutcome <- subset(table, State == state)
        if(outcome == "heart attack") {
                colnum <- 11
        } else if (outcome == "heart failure") {
                colnum <- 17
        } else {
                colnum <- 23
        }
        
        stateOutcome[, colnum] <- suppressWarnings(as.numeric(stateOutcome[, colnum]))
        
        ranks <- stateOutcome[order(stateOutcome[, colnum], stateOutcome$Hospital.Name),]
        
        cc <- sum(complete.cases(ranks))
        
        if(num == "best") {
                rank <- ranks$Hospital.Name[1]
        } else if(num == "worst") {
                rank <- ranks$Hospital.Name[cc]
        } else {
                rank <- ranks$Hospital.Name[num]
        }
        
        rank
        
}