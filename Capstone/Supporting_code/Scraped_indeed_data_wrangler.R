#### THIS SCRIPT MERGES SCRAPED DATA FROM INDEED AND CLEANS IT WHERE NECCESSARY

library(tidyverse)
library(lubridate)
library(tibble)
library(stringr)

### MERGE SCRAPED DATA

# create starting dataframe
df_jobs <- data.frame(
  Company = character(),
  job_title = character(),
  salary = character(),
  skills = character(),
  days_online = numeric(),
  job_desc = character(),
  job_link = character(),
  listing_date= Date()
)

# loop over files and union onto base dataframe
for (f in list.files("./Data_raw/D002")){
  df_temp <- read.csv(paste("./Data_raw/D002", f,sep = "/"),header = TRUE)
  # Two files do not contain the collumn listing_date this needs to be amended
  if(!"listing_date" %in% colnames(df_temp)){
    # take the date of the file from the filename
    f_date <- f %>%
      substr(1,10) %>%
      as.Date()
    
    # use this date to calculating the placement date per job listing
    df_temp$listing_date <- f_date - as.numeric(df_temp$days_online)
  }
  
  # union the temp dataframe onto the main dataframe
  df_jobs <- rbind(df_jobs, df_temp)
}

# remove duplicate columns based on job listing url
df_jobs_unduped <- df_jobs %>%
  distinct(job_link, .keep_all = TRUE)

# only keep rows where job_link starts with https
df_jobs_removed_wrong_rows <- df_jobs_unduped[(grepl("https", df_jobs_unduped$job_link)),]

### CLEAN MERGED DATAFRAME

## clean salary
tbl_jobs_cleaned_salary <- tibble(df_jobs_removed_wrong_rows)

#remove digits in salary string
tbl_jobs_cleaned_salary$salary <- tbl_jobs_cleaned_salary$salary %>%
  str_replace_all("[^[:digit:]]", "")

class(tbl_jobs_cleaned_salary)

## clean skills
# if machine and learning  or deep learning appear as skill make this into one skill 
tbl_job_skills_corrected <- tibble(tbl_jobs_cleaned_salary)
tbl_job_skills_corrected$skills <- tolower(tbl_job_skills_corrected$skills)

for (i in 1:nrow(tbl_job_skills_corrected)){
  
  skills <- tbl_job_skills_corrected$skills[i]
  
  # check if any combination of machine and deep with at least learning appears
  check_ml_dl <- (((grepl("machine", skills)|| grepl("deep", skills)) && grepl("learning", skills)))
  
  if (check_ml_dl){
    
    # check if machine and/or deep appear in the skill
    check_m_d <- (grepl("machine", skills) && grepl("deep", skills))
    check_m <- grepl("machine", skills)
    check_d <- grepl("deep", skills)
    
    if (check_m_d){
      
      # do stuff when machine learning and deep learning appears
      new_skill1 <- 'machine learning'
      new_skill2 <- 'deep learning'
      
      # add deep learnign and machine learning skill
      tbl_job_skills_corrected$skills[i] <- paste(tbl_job_skills_corrected$skills[i], new_skill1, sep=", ")
      tbl_job_skills_corrected$skills[i] <- paste(tbl_job_skills_corrected$skills[i], new_skill2, sep=", ")

      # remove first appearances of words
      tbl_job_skills_corrected$skills[i] <- sub("machine","", tbl_job_skills_corrected$skills[i])
      tbl_job_skills_corrected$skills[i] <- sub("deep","", tbl_job_skills_corrected$skills[i])
      tbl_job_skills_corrected$skills[i] <- sub("learning","", tbl_job_skills_corrected$skills[i])
      
    }
    
    else if (check_m){
      #do stuff when machine learning appears
      new_skill <- 'machine learning'
      
      # add machine learning skill
      tbl_job_skills_corrected$skills[i] <- paste(tbl_job_skills_corrected$skills[i], new_skill, sep=", ")
      
      # remove first appearances of words
      tbl_job_skills_corrected$skills[i] <- sub("machine","", tbl_job_skills_corrected$skills[i])
      tbl_job_skills_corrected$skills[i] <- sub("learning","", tbl_job_skills_corrected$skills[i])
    }
    
    else if (check_d){
      #do stuff when  deep learning appears
      new_skill <- 'deep learning'
      
      # add deep learning skill
      tbl_job_skills_corrected$skills[i] <- paste(tbl_job_skills_corrected$skills[i], new_skill, sep=", ")
      
      # remove first appearances of words
      tbl_job_skills_corrected$skills[i] <- sub("deep","", tbl_job_skills_corrected$skills[i])
      tbl_job_skills_corrected$skills[i] <- sub("learning","", tbl_job_skills_corrected$skills[i])
    }
    
    # remove duplicate comma's
    tbl_job_skills_corrected$skills[i] <- sub(", ,",",", tbl_job_skills_corrected$skills[i])
    tbl_job_skills_corrected$skills[i] <- sub(", ,",",", tbl_job_skills_corrected$skills[i])
    
    # remove leading comma if the comma appears as the first character
    if (str_locate(tbl_job_skills_corrected$skills[i], ",")[1] == 1){
      tbl_job_skills_corrected$skills[i] <- sub(", ","", tbl_job_skills_corrected$skills[i])
      
    }

  }
}


### ENRICH DATA

## add jobtype (data analist, data engineer, data scientist) based on highest levenstein distance/ration

## add skill count collumn
#create new tibble
tbl_job_skill_counts <- tibble(tbl_job_skills_corrected)

# if skills are empty then 0 else count(comma's) + 1
tbl_job_skill_counts$skill_count <- ifelse(
  tbl_job_skill_counts$skills =="",
  0,
  str_count(tbl_job_skill_counts$skills, ",") +1
  ) 
