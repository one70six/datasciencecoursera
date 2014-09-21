#================================================================
# Func:         rankall.R   
# Name:         Programming Assignment 3: Hospital Quality 
# Desc:         This funtion ranks hospitals in all states
# Auth:         Ryan Samo
# Date:         9-21-2014
#================================================================

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        care_measures <- read.csv("outcome-of-care-measures.csv", header = TRUE, na.strings = "Not Available")

        ## Check that outcome and num are valid        
        if(!outcome %in% c("heart attack","heart failure","pneumonia"))
                stop("invalid outcome")
        
        if(!is.numeric(num) & !num %in% c("best","worst"))
                stop("invalid num")
        
        ## Get a subset of the data by outcome
        if(outcome == "heart attack"){
                care_measures <- subset(care_measures,select = c(7, 2, 11))            
        }else if(outcome == "heart failure"){
                care_measures <- subset(care_measures, select = c(7, 2, 17))            
        }else if(outcome == "pneumonia"){
                care_measures <- subset(care_measures, select = c(7, 2, 23))
        }
                 
        #Declare empty vectors for use below
        hospital = character()
        state = character()
        
        ##For each state, find the hospital of the given rank
        states <- as.character(unique(care_measures[,"State"]))
        
        for(i in 1:length(states)){
                                
                ##Get a subset of the data by state
                state_care_measures <- subset(care_measures, State == states[i])            
                
                #Sort the data set by the death rate then hospital name
                state_care_measures <- state_care_measures[order(state_care_measures[,3],state_care_measures[,2]),]
                                                
                #Retrieve the row from the data set based on num parameter
                if(num == "best"){
                        
                        #Grab the top row hospital name as it should be the smallest(best)
                        state_care_measures <- state_care_measures[1,]
                        
                }else if(num == "worst"){
                                                
                        #Grab the bottom row hospital name as it should be the smallest(worst)
                        state_care_measures <- state_care_measures[nrow(state_care_measures),]
                        
                }else if(num %in% 1:nrow(state_care_measures)){
                        
                        #Grab the specified rows hospital name
                        state_care_measures <- state_care_measures[num,]
                        
                }else{
                        #Build an NA row for the state
                        state_care_measures <- data.frame(State = states[i], Hospital.Name = NA, NA)
                }
                
                #Append the state record to the vectors
                hospital[i] <- as.character(state_care_measures[,"Hospital.Name"])
                state[i] <- as.character(state_care_measures[,"State"])           
        }
        
        ## Build a data frame with the hospital names and the (abbreviated) state name
        df <- data.frame(hospital, state)
        
        #Sort the data frame by state
        df <- df[order(df[,"state"]),]

        #Return the data frame
        df
}