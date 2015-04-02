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
    ethnicity = jd$answer$data$allData$ethnicity,
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
d_tidy_01 = d.raw %>%
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

glimpse(d_tidy_01)

# fix miscoding of responseCoded
d_tidy_01$responseCoded[d_tidy_01$response == "definitely no"] = -3
  
glimpse(d_tidy_01)

# add country variable for us
d_india_01 = d_tidy_01 %>%
  mutate(country = factor("india"))
glimpse(d_india_01)

# -------- US: RUN 1 ----------------------------------------------------------

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

# still hackily exclude two files (no idea why!!)
files <- files[-4] #3180JW2OT5LHFBJZ0C9XFNOFH5NJ5M.json
files <- files[-44] #3BWI6RSP7HIORZBQA4D3GRPJ3417EL.json
files <- files[-69] #3HMVI3QICK18MIDFLP8OMKQMIU5Y1T.json

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
    ethnicity = jd$answer$data$allData$ethnicity,
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
d_tidy_01 = d.raw %>%
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

glimpse(d_tidy_01)

# fix miscoding of responseCoded
d_tidy_01$responseCoded[d_tidy_01$response == "definitely no"] = -3

glimpse(d_tidy_01)

# add country variable for us
d_us_01 = d_tidy_01 %>%
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
    education = jd$answers$data$allData$education,
    english_native = jd$answers$data$allData$englishNative,
    ethnicity = jd$answer$data$allData$ethnicity,
    gender = jd$answers$data$allData$gender,
    age = jd$answers$data$allData$age,
    religion = jd$answers$data$allData$religion,
    
    # subject-level data: free response
    comp_check = jd$answers$data$allData$comprehensionCheck,
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
d_tidy_02A = d.raw %>%
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

glimpse(d_tidy_02A)

# fix miscoding of responseCoded - NOT NEEDED FOR RUN 2
# d_tidy_02A$responseCoded[d_tidy_02A$response == "definitely no"] = -3

# add country variable for us
d_india_02A = d_tidy_02A %>%
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
    education = jd$answers$data$allData$education,
    english_native = jd$answers$data$allData$englishNative,
    ethnicity = jd$answer$data$allData$ethnicity,
    gender = jd$answers$data$allData$gender,
    age = jd$answers$data$allData$age,
    religion = jd$answers$data$allData$religion,
    
    # subject-level data: free response
    comp_check = jd$answers$data$allData$comprehensionCheck,
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
d_tidy_02B = d.raw %>%
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

glimpse(d_tidy_02B)

# fix miscoding of responseCoded - NOT NEEDED FOR RUN 2
# d_tidy_02B$responseCoded[d_tidy_02B$response == "definitely no"] = -3

# add country variable for us
d_india_02B = d_tidy_02B %>%
  mutate(country = "india")

# -------- COMBINE: RUN 2 -----------------------------------------------------

d_tidy_02 = full_join(d_india_02A, d_india_02B) %>%
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
write.csv(d_tidy_02, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-us-and-india_02.csv")

