best <- function (state = character(2), outcome = character()){
        
        ## Read outcome data and filter our unnecessary columns. Also change the outcome data to numeric
        
                outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
                
                outcomes <- subset.data.frame(outcomes,select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)


        ## standardize to uppercase state names and Conditions
        
                validStates <- casefold(unique(outcomes$State),upper=TRUE)
       
                state <- casefold(state,upper=TRUE)
                
                outcome <- casefold(outcome,upper=TRUE)
        
        
        ## Check that state and outcome are valid
        
                if ( !(state  %in% validStates)) {
                        
                        return("ERROR: Invalid State")
                        
                        
                } else {
                        
                        if (!(outcome %in% c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA")))
                        
                        return("ERROR: Invalid Outcome")
                        
                }
                
        ## Filter out hospitals without data on specific condition
                
                        if (outcome=="HEART ATTACK" ){
                                
                                
                                outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) & outcomes$State==state)
                                result <- subset.data.frame(outcomes,outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==  min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))[,1]
                                
                                
                        } else if (outcome=="HEART FAILURE" ) {
                                
                                outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) & outcomes$State==state)
                                result <- subset.data.frame(outcomes,outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==  min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))[,1]
                           
                        } else { ## This can only be Pneumonia
                                
                                
                                outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) & outcomes$State==state)
                                result <- subset.data.frame(outcomes,outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==  min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))[,1]
                                
                        }
                                
                        
                     
                
        
        ## Return hospital name in that state with lowest 30-day death rate. If there are more than one hospitals with minimum rate, choose the first in alphabetical order.
        
                     if(length(result)>1){
                             
                             
                             return(head(sort(result$Hospital.Name,decreasing=TRUE),1))
                             
                     } else{
                             
                        return(result) 
                             
                     }
                                
                                       
        
        
        
}

        
        
        
