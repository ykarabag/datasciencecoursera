rankhospital <- function (state = character(2), outcome = character(), ranking = "BEST"){
                
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
                
                
                ## handle lower case "best" or "worst" inputs and turn them to uppercase
                if (is.character(ranking)){
                        
                        ranking <- casefold(ranking,upper=TRUE)
                }
                
                
                #create a rank column
                outcomes$rank <- integer(1)
                
                
                ## Check that state, ranking and outcome are valid
                
                if ( !(state  %in% validStates)) {
                        
                        return("ERROR: Invalid State")
                        
                        
                } 
                
                if (!(outcome %in% c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA"))){
                                
                                return("ERROR: Invalid Outcome")
                        
                } 
                
                        if (is.character(ranking) && !(ranking =="BEST" || ranking =="WORST")){
                        
                                return("ERROR: Invalid Ranking")
                        
                }
                
                ## Filter out hospitals without data on specific condition,  select the hospital based on ranking input to the function
                
                if (outcome=="HEART ATTACK" ){
                        
                    
                        outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) & outcomes$State==state)
                        outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomes$State, FUN = function(x) rank(x , ties.method = "max", na.last=TRUE ) )
                        
                        
                       
                                
                                
                        
                        
                } else if (outcome=="HEART FAILURE" ) {
                        
                        outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) & outcomes$State==state)
                        outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcomes$State, FUN = function(x) rank(x , ties.method = "max", na.last=TRUE ) )
                       
                        
                } else { ## This can only be Pneumonia
                        
                        
                        outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) & outcomes$State==state)
                        outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcomes$State, FUN = function(x) rank(x , ties.method = "max", na.last=TRUE ) )
                      
                }

                        ## Check ranking input for BEST & WORSE cases
                
                        if (is.numeric(ranking)){
                                
                                result <- subset.data.frame(outcomes,outcomes$rank==ranking)
                                
                        } else if (ranking=="BEST"){
                                        
                                        result <- subset.data.frame(outcomes,outcomes$rank==1)
                                        
                                        
                                } else{ ## it means it's WORST
                                        
                                        result <- subset.data.frame(outcomes,max(outcomes$rank)==outcomes$rank)   
                                        
                                }
                
                
                ## Return hospital name in that state with lowest 30-day death rate. If there are more than one hospitals with minimum rate, choose the first in alphabetical order.
                
                if(length(result)>1){
                        
                        
                       return(head(sort(result$Hospital.Name,decreasing=TRUE),1))
                        
                } else{
                        
                        return(result) 
                        
                }
                
                        }    
                
                
                
        
        
        
        
        
        


