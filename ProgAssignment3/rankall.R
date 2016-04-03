rankall <- function (outcome = character(), ranking = "BEST"){
        
        ## Read outcome data and filter our unnecessary columns. Also change the outcome data to numeric
        
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcomes <- subset.data.frame(outcomes,select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        
        outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        
        
        ## standardize to uppercase Conditions
        
        
        outcome <- casefold(outcome,upper=TRUE)
        
        
        # get states list
        
        states <- unique(outcomes$State)
        
        
        ## handle lower case "best" or "worst" inputs and turn them to uppercase
        if (is.character(ranking)){
                
                ranking <- casefold(ranking,upper=TRUE)
        }
        
        
        #create a rank column
       ## outcomes$rank <- numeric(1)
        
        
        ## Check that anking and outcome are valid
        
        
        if (!(outcome %in% c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA"))){
                
                return("ERROR: Invalid Outcome")
                
        } 
        
        if (is.character(ranking) && !(ranking =="BEST" || ranking =="WORST")){
                
                return("ERROR: Invalid Ranking")
                
        }
        
        ## Filter out hospitals without data on specific condition,  select the hospital based on ranking input to the function
        
        if (outcome=="HEART ATTACK" ){
                
                
               ## outcomes<-subset.data.frame(outcomes,outcomes$State=="AK")
                sort.data.frame(outcomes, decreasing = FALSE,na.last = TRUE, by=c("State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
                
               ## outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomes$State, FUN = function(x) rank(x, na.last = TRUE , ties.method = "min" ) )
                outcomes$rank <-rank(outcomes,c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "State"), na.last = TRUE , ties.method = "min" ) 
                
  
     return(result)
                
                
                
        } else if (outcome=="HEART FAILURE" ) {
                
                ##outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) 
                
                outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcomes$State, FUN = function(x) rank(x, na.last = TRUE, ties.method = "max" ) )
                
                
        } else { ## This can only be Pneumonia
                
                
                outcomes<-subset.data.frame(outcomes,!is.na(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                
                outcomes$rank <- ave(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcomes$State, FUN = function(x) rank(x, na.last = TRUE, ties.method = "max" ) )
                
        }
        
        ## Check ranking input for BEST & WORSE cases
        
     
        if (is.numeric(ranking)){
                
                
             
                        
                       
                        
                        result <- subset.data.frame(outcomes,as.numeric(outcomes$rank)==as.numeric(ranking))
                        
                      
                        
                
                
                
                
                
        } else if (ranking=="BEST"){
                
                result <- subset.data.frame(outcomes,outcomes$rank==1)
                
                
        } else{ ## it means it's WORST
                
                result <- subset.data.frame(outcomes,max(outcomes$rank)==outcomes$rank && outcomes$State==outcomes$State)   
                
        }
        
        
        ## Return hospital name in that state with lowest 30-day death rate. If there are more than one hospitals with minimum rate, choose the first in alphabetical order.
        
        sort(result$State)
        
        if(length(result[,1])>1){
                
                
                return(sort(result[1,2:1]))
                
        } else{
                
                return(result[,2:1])
                
        }
        
}    











