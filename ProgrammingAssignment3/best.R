#================================================================
# Func:         best.R   
# Name:         Programming Assignment 3: Hospital Quality 
# Desc:         This file finds the best hospital in a state
# Auth:         Ryan Samo
# Date:         9-20-2014
#================================================================

best <- function(state, outcome) {
        
        ## Read outcome data
        care_measures <- read.csv("outcome-of-care-measures.csv", header = TRUE, na.strings = "Not Available")
        
        ## Check that state and outcome are valid
        if(!state %in% care_measures[,"State"])
                stop("invalid state")
        
        if(!outcome %in% c("heart attack","heart failure","pneumonia"))
                stop("invalid outcome")
        
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
        
        #Grab the top row hospital name as it should be the smallest
        care_measures <- as.character(care_measures[1,1])
        
        #Return the result
        care_measures
}