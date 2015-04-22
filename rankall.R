rankall <- function(outcome, num = "best") {
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        if(outcome == "heart attack") {
                colnum <- 11
        } else if (outcome == "heart failure") {
                colnum <- 17
        } else {
                colnum <- 23
        }
        ## Return hosplital rank in that state with lowest 30-day death rate
        hospitals <- c()
        states <- c()
        
        for (st in table$State) {
                if(!st %in% states) {
                        states <- c(states, st)
                }
        }
        
        for(state in states) {
                stateOutcome <- subset(table, State == state)
                
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
                
                hospitals <- c(hospitals, rank)
        }
        
        OT <- data.frame(hospital = hospitals, state = states) 
        
        OutcomeTable <- OT[order(OT$state, OT$hospital),]
        
        OutcomeTable
        
}