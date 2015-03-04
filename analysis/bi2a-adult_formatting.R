# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# --- INDIA -----------------------------------

# set working directory for india
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-india_01/")

# mike's json for-loop
files <- dir("production-results/")

# hackily exclude problematic files for india (if including any fingerprint data):
# files2 <- files[-9]
# files3 <- files2[-24]

# hackily exclude two problematic files (no idea why)
files <- files[-96] #3OLQQLKKNTYHBUCSBD20JHUSU0PJEU.json
files <- files[-126] #3X73LLYYQ2NPEUUFGC2YXR8MVZCHN0.json

d.raw <- data.frame()
for (f in files) {
  # gather files
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))

  # store relevant variables in dataframe 
  id <- data.frame(
    # subject-level data: identity
    condition = jd$answers$data$allData$condition,
    worker_id = jd$WorkerId,
    
    # subject-level data: demographics
    education = jd$answers$data$allData$education,
    english_native = jd$answers$data$allData$englishNative,
    gender = jd$answers$data$allData$gender,
    age = jd$answers$data$allData$age,
    religion = jd$answers$data$allData$religion,
    comments = jd$answers$data$allData$comments,
                   
    # trial-level data:                    
    trialNum = jd$answers$data$allData$trialData$trialNum,
    swatch = jd$answers$data$allData$trialData$swatch,
    response = jd$answers$data$allData$trialData$response,
    responseCoded = jd$answers$data$allData$trialData$responseCoded,
    rt = jd$answers$data$allData$trialData$rt)
  
  # bind into same dataframe
  d.raw <- bind_rows(d.raw, id)
}

glimpse(d.raw)

# clean up variables
d_tidy = d.raw %>%
  mutate(condition = factor(condition),
         worker_id = factor(worker_id),
         education = factor(education),
         english_native = factor(english_native),
         gender = factor(gender),
         age = as.numeric(age),
         religion = factor(religion),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# fix miscoding of responseCoded
d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3
  
glimpse(d_tidy)

# add country variable for us
d_india = d_tidy %>%
  mutate(country = factor("india"))
glimpse(d_india)

# --- US -----------------------------------

# set working directory for us
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-us_01/")

# mike's json for-loop
files <- dir("production-results/")

# hackily exclude problematic files for us (if including any fingerprint data)
# files2 <- files[-47]
# files3 <- files2[-45]
# files4 <- files3[-40]
# files5 <- files4[-35]
# files6 <- files5[-24]
# files7 <- files6[-21]
# files8 <- files7[-19]
# files9 <- files8[-8]
# files10 <- files9[-2]

# still hackily exclude one file (no idea why!!)
files <- files[-4] #3180JW2OT5LHFBJZ0C9XFNOFH5NJ5M.json

d.raw <- data.frame()
for (f in files) {
  # gather files
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    # subject-level data: identity
    condition = jd$answers$data$allData$condition,
    worker_id = jd$WorkerId,
    
    # subject-level data: demographics
    education = jd$answers$data$allData$education,
    english_native = jd$answers$data$allData$englishNative,
    gender = jd$answers$data$allData$gender,
    age = jd$answers$data$allData$age,
    religion = jd$answers$data$allData$religion,
    comments = jd$answers$data$allData$comments,
    
    # trial-level data:                    
    trialNum = jd$answers$data$allData$trialData$trialNum,
    swatch = jd$answers$data$allData$trialData$swatch,
    response = jd$answers$data$allData$trialData$response,
    responseCoded = jd$answers$data$allData$trialData$responseCoded,
    rt = jd$answers$data$allData$trialData$rt)
  
  # bind into same dataframe
  d.raw <- bind_rows(d.raw, id)
}

glimpse(d.raw)

# clean up variables
d_tidy = d.raw %>%
  mutate(condition = factor(condition),
         worker_id = factor(worker_id),
         education = factor(education),
         english_native = factor(english_native),
         gender = factor(gender),
         age = as.numeric(age),
         religion = factor(religion),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# fix miscoding of responseCoded
d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3

glimpse(d_tidy)

# add country variable for us
d_us = d_tidy %>%
  mutate(country = "us")

# --- COMBINE -----------------------------------

d_tidy = full_join(d_india, d_us) %>%
  mutate(worker_id = factor(worker_id),
         religion = factor(religion),
         country = factor(country))
glimpse(d_tidy)

# write to csv
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_01.csv")
