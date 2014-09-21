#================================================================
# Func:         rankhospital.R   
# Name:         Programming Assignment 3: Hospital Quality 
# Desc:         This funtion ranks hospitals by outcome in a state
# Auth:         Ryan Samo
# Date:         9-21-2014
#================================================================

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        care_measures <- read.csv("outcome-of-care-measures.csv", header = TRUE, na.strings = "Not Available")
        
        ## Check that state, outcome, and num are valid
        if(!state %in% care_measures[,"State"])
                stop("invalid state")
        
        if(!outcome %in% c("heart attack","heart failure","pneumonia"))
                stop("invalid outcome")
        
        if(!is.numeric(num) & !num %in% c("best","worst"))
                stop("invalid num")  
        
        ## Get a subset of the data by state and outcome
        if(outcome == "heart attack"){
                care_measures <- subset(care_measures, State == state,select = c(2, 11))            
        }else if(outcome == "heart failure"){
                care_measures <- subset(care_measures, State == state,select = c(2, 17))            
        }else if(outcome == "pneumonia"){
                care_measures <- subset(care_measures, State == state,select = c(2, 23))
        }
        
        #Remove NAs from the result set
        care_measures <- care_measures[complete.cases(care_measures),]
        
        #Sort the data set by the death rate then hospital name
        care_measures <- care_measures[order(care_measures[,2],care_measures[,1]),]
        
        #Retrieve the row from the data set based on num parameter
        if(num == "best"){
                
                #Grab the top row hospital name as it should be the smallest(best)
                care_measures <- as.character(care_measures[1,1])
                
        }else if(num == "worst"){
                
                #Grab the bottom row hospital name as it should be the smallest(worst)
                care_measures <- as.character(care_measures[nrow(care_measures),1])
                
        }else if(num %in% 1:nrow(care_measures)){
                
                #Grab the specified rows hospital name
                care_measures <- as.character(care_measures[num,1])
                
        }else{
                return(NA)
        }
        
        #Return the result
        care_measures
}