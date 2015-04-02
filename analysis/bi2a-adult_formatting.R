# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# --- RUN 1 -------------------------------------------------------------------

# -------- INDIA: RUN 1 -------------------------------------------------------

# set working directory for india
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-india_01/")

# mike's json for-loop
files <- dir("production-results/")
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
    education = ifelse(is.null(jd$answer$data$allData$education) == TRUE, "NA",
                    jd$answer$data$allData$education),
    english_native = ifelse(is.null(jd$answer$data$allData$english_native) == TRUE, "NA",
                       jd$answer$data$allData$english_native),
    ethnicity = ifelse(is.list(jd$answer$data$allData$ethnicity) == TRUE, "NA",
                       jd$answer$data$allData$ethnicity),
    gender = ifelse(is.null(jd$answer$data$allData$gender) == TRUE, "NA",
                       jd$answer$data$allData$gender),
    age = ifelse(is.null(jd$answer$data$allData$age) == TRUE, "NA",
                    jd$answer$data$allData$age),
    religion = ifelse(is.list(jd$answer$data$allData$religion) == TRUE, "NA",
                      jd$answer$data$allData$religion),
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
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         religion = factor(religion),
         age = as.numeric(age),
         education = factor(education),
         english_native = factor(english_native),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# fix miscoding of responseCoded
d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3
  
glimpse(d_tidy)

# add country variable for us
d_india_01 = d_tidy %>%
  mutate(country = factor("india"))
glimpse(d_india_01)

# -------- US: RUN 1 ----------------------------------------------------------

# set working directory for us
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-us_01/")

# mike's json for-loop
files <- dir("production-results/")
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
    education = ifelse(is.null(jd$answer$data$allData$education) == TRUE, "NA",
                       jd$answer$data$allData$education),
    english_native = ifelse(is.null(jd$answer$data$allData$english_native) == TRUE, "NA",
                            jd$answer$data$allData$english_native),
    ethnicity = ifelse(is.list(jd$answer$data$allData$ethnicity) == TRUE, "NA",
                       jd$answer$data$allData$ethnicity),
    gender = ifelse(is.null(jd$answer$data$allData$gender) == TRUE, "NA",
                    jd$answer$data$allData$gender),
    age = ifelse(is.null(jd$answer$data$allData$age) == TRUE, "NA",
                 jd$answer$data$allData$age),
    religion = ifelse(is.list(jd$answer$data$allData$religion) == TRUE, "NA",
                      jd$answer$data$allData$religion),
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
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         religion = factor(religion),
         age = as.numeric(age),
         education = factor(education),
         english_native = factor(english_native),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# fix miscoding of responseCoded
d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3

glimpse(d_tidy)

# add country variable for us
d_us_01 = d_tidy %>%
  mutate(country = "us")

# -------- COMBINE: RUN 1 -----------------------------------------------------

d_tidy_01 = full_join(d_india_01, d_us_01) %>%
  mutate(worker_id = factor(worker_id),
         education = factor(education, 
                            levels = c("hs_diploma", 
                                       "college_some",
                                       "college_assocDegree", 
                                       "college_bachDegree",
                                       "grad_some",
                                       "grad_degree")),
         english_native = factor(english_native,
                                 levels = c("no",
                                            "no_somewhatwell",
                                            "no_moderatelywell",
                                            "no_fluent",
                                            "yes_multiple",
                                            "yes_only")),
         religion = factor(religion),
         response = factor(response,
                           levels = c("definitely no",
                                      "probably no",
                                      "maybe no",
                                      "equally yes/no",
                                      "maybe yes",
                                      "probably yes",
                                      "definitely yes")),
         country = factor(country))
glimpse(d_tidy_01)

# write to csv
write.csv(d_tidy_01, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_01.csv")

# --- RUN 2 -------------------------------------------------------------------

# -------- INDIA: RUN 2A ------------------------------------------------------

# set working directory for india
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-india_02A/")

# mike's json for-loop
files <- dir("production-results/")
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
    education = ifelse(is.null(jd$answer$data$allData$education) == TRUE, "NA",
                       jd$answer$data$allData$education),
    english_native = ifelse(is.null(jd$answer$data$allData$english_native) == TRUE, "NA",
                            jd$answer$data$allData$english_native),
    ethnicity = ifelse(is.list(jd$answer$data$allData$ethnicity) == TRUE, "NA",
                       jd$answer$data$allData$ethnicity),
    gender = ifelse(is.null(jd$answer$data$allData$gender) == TRUE, "NA",
                    jd$answer$data$allData$gender),
    age = ifelse(is.null(jd$answer$data$allData$age) == TRUE, "NA",
                 jd$answer$data$allData$age),
    religion = ifelse(is.list(jd$answer$data$allData$religion) == TRUE, "NA",
                      jd$answer$data$allData$religion),
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

# fix miscoding of responseCoded - NOT NEEDED FOR RUN 2
# d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3

# add country variable for us
d_india_02A = d_tidy %>%
  mutate(country = factor("india"))

# -------- INDIA: RUN 2B ------------------------------------------------------

# set working directory for us
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-india_02B/")

# mike's json for-loop
files <- dir("production-results/")
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
    education = ifelse(is.null(jd$answer$data$allData$education) == TRUE, "NA",
                       jd$answer$data$allData$education),
    english_native = ifelse(is.null(jd$answer$data$allData$english_native) == TRUE, "NA",
                            jd$answer$data$allData$english_native),
    ethnicity = ifelse(is.list(jd$answer$data$allData$ethnicity) == TRUE, "NA",
                       jd$answer$data$allData$ethnicity),
    gender = ifelse(is.null(jd$answer$data$allData$gender) == TRUE, "NA",
                    jd$answer$data$allData$gender),
    age = ifelse(is.null(jd$answer$data$allData$age) == TRUE, "NA",
                 jd$answer$data$allData$age),
    religion = ifelse(is.list(jd$answer$data$allData$religion) == TRUE, "NA",
                      jd$answer$data$allData$religion),
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
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         religion = factor(religion),
         age = as.numeric(age),
         education = factor(education),
         english_native = factor(english_native),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# fix miscoding of responseCoded - NOT NEEDED FOR RUN 2
# d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3

# add country variable for us
d_india_02B = d_tidy %>%
  mutate(country = "india")

# -------- US: RUN 2 ----------------------------------------------------------

# set working directory for us
setwd("/Users//kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/turk/run-us_02/")

# mike's json for-loop
files <- dir("production-results/")
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
    education = ifelse(is.null(jd$answer$data$allData$education) == TRUE, "NA",
                       jd$answer$data$allData$education),
    english_native = ifelse(is.null(jd$answer$data$allData$english_native) == TRUE, "NA",
                            jd$answer$data$allData$english_native),
    ethnicity = ifelse(is.list(jd$answer$data$allData$ethnicity) == TRUE, "NA",
                       jd$answer$data$allData$ethnicity),
    gender = ifelse(is.null(jd$answer$data$allData$gender) == TRUE, "NA",
                    jd$answer$data$allData$gender),
    age = ifelse(is.null(jd$answer$data$allData$age) == TRUE, "NA",
                 jd$answer$data$allData$age),
    religion = ifelse(is.list(jd$answer$data$allData$religion) == TRUE, "NA",
                      jd$answer$data$allData$religion),
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
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         religion = factor(religion),
         age = as.numeric(age),
         education = factor(education),
         english_native = factor(english_native),
         trialNum = as.integer(trialNum),
         response = factor(response))

glimpse(d_tidy)

# # fix miscoding of responseCoded - NOT NEEDED FOR RUN 2
# d_tidy$responseCoded[d_tidy$response == "definitely no"] = -3

glimpse(d_tidy)

# add country variable for us
d_us_02 = d_tidy %>%
  mutate(country = "us")

# -------- COMBINE: RUN 2 -----------------------------------------------------

d_tidy_02 = full_join(d_india_02A, d_india_02B) %>%
  full_join(d_us_02) %>%
  mutate(worker_id = factor(worker_id),
         education = factor(education, 
                            levels = c("hs_diploma", 
                                       "college_some",
                                       "college_assocDegree", 
                                       "college_bachDegree",
                                       "grad_some",
                                       "grad_degree")),
         english_native = factor(english_native,
                                 levels = c("no",
                                            "no_somewhatwell",
                                            "no_moderatelywell",
                                            "no_fluent",
                                            "yes_multiple",
                                            "yes_only")),
         religion = factor(religion),
         response = factor(response,
                           levels = c("definitely no",
                                      "probably no",
                                      "maybe no",
                                      "equally yes/no",
                                      "maybe yes",
                                      "probably yes",
                                      "definitely yes")),
         country = factor(country))
glimpse(d_tidy_02)

# write to csv
write.csv(d_tidy_02, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_02.csv")