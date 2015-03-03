# # libraries
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(lme4)
# library(jsonlite)

# clear environment
rm(list=ls())

# set working directory
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-india_01/")

# mike's json for-loop
files <- dir("production-results/")
d.raw <- data.frame()

for (f in files) {
  print(f);
  
  # gather files
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))

  # store relevant variables in dataframe 
  id <- data.frame(
    # subject-level data: identity
#     country = jd$answers$data$allData$fingerprintData$geo$country_name,
    condition = jd$answers$data$allData$condition,
    worker_id = jd$WorkerId,
    ip_address = jd$answers$data$allData$fingerprintData$ip,
    
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

View(d_tidy)
