## --------------------------------
##
## Script name: makeCompleteFiles.R
##
## Purpose of script: Make stitched CSVs -- one for trackerholder (the real hand), 
## and one for cursorObject (the object)
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-12-16
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: For each participant, make a "complete" file for hand, and one for object
##
## --------------------------------

## Load packages
library(data.table)
library(tidyverse)

## make into a function
# setup
path <- "data/raw"

expVersions <- c("reachToTarget_distance_20_EI", "reachToTarget_distance_20_IE")

dir.create(paste("data", "complete", sep = "/"))

for(expVersion in expVersions){
    
  dir.create(paste("data", "complete", expVersion, sep = "/"))
  
  for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
    
    dir.create(paste("data", "complete", expVersion, ppt, sep = '/'))
    
    for (session in list.files(path = paste(path, expVersion, ppt, sep = '/'))){
      dir.create(paste("data", "complete", expVersion, ppt, session, sep = '/'))
      
      for(trackerTag in c("cursorobjecttracker", "trackerholderobject")){
        
        # make a vector of filenames to load (these are entire paths)       
        filesToLoad <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                  pattern = glob2rx(paste("*",trackerTag,"*", sep = "")), 
                                  full.names = TRUE)
        
        datalist <- list()
        i <- 1
        
        # fill up datalist
        for (eachFilePath in filesToLoad){
          eachFile <- fread(eachFilePath, stringsAsFactors = FALSE)
          
          # save this one df to datalist
          datalist[[i]] <- eachFile
          
          i <- i+1
        }
        
        # save this
        complete_df <- do.call(rbind, datalist)
        
        fileName <- paste(trackerTag, "_complete.csv", sep = "")
        
        # print(head(complete_df))
        fwrite(complete_df, file = paste("data", "complete", expVersion, ppt, session, fileName, sep = '/'))
      }
      
      #copy over the trial results file
      trialResultPath <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                    pattern = glob2rx("*_results*"), 
                                    full.names = TRUE) 
      trialResult <- fread(trialResultPath, stringsAsFactors = FALSE)
      
      fwrite(trialResult, file = paste("data", "complete", expVersion, ppt, session, "trial_results.csv", sep = '/'))
    }
  }
}
