#### THIS SCRIPT MERGES SCRAPED DATA FROM INDEED AND CLEANS IT WHERE NECCESSARY

library(tidyverse)
library(lubridate)
library(tibble)
library(stringr)
library(textTinyR)


merge_scraped_data <- function(location="./Data_raw/D002", max_file_date=""){
  #' merge .csv files from specified location up to a specified date keeping unique rows
  #' if no date is specified then current date will be the latest file allowed
  #' @param location the relative directory path to the data files
  #' @param max_file_date the max date of the filenames to be merged, format "yyyy-mm-dd"
  #' @returns dataframe based on union of input files, without duplicates
  
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
  
  max_date <- ifelse(max_file_date=="", today(), max_file_date)
  
  # loop over files and union onto base dataframe
  for (f in list.files(location)){
    file_date <- f %>%
      substr(1,10) %>%
      as.Date()
    
    # use the date of the file and user input to determine if df's should be appended
    if (file_date <= max_date){
      df_temp <- read.csv(
        paste(
          location,
          f,
          sep = "/"
          ),
        header = TRUE
        )
      
      # Two files do not contain the collumn listing_date this needs to be amended
      if(!"listing_date" %in% colnames(df_temp)){
        
        # use file date calculate the placement date per job listing
        df_temp$listing_date <- file_date - as.numeric(df_temp$days_online)
      }
      
      # union the temp dataframe onto the main dataframe
      df_jobs <- rbind(df_jobs, df_temp)
      }
    }
  
  # remove duplicate columns based on job listing url
  df_jobs_unduped <- df_jobs %>%
    distinct(job_link, .keep_all = TRUE)
  
  # only keep rows where job_link starts with https
  df_jobs_removed_wrong_rows <- df_jobs_unduped[
    (grepl("https", df_jobs_unduped$job_link)),
    ]
  
  df_merged_data <- df_jobs_removed_wrong_rows
  
  return(df_merged_data)
}

clean_scraped_date <- function(df_merged_data){
  #' cleans columns: salary and skills. of input dataframe
  #' salary: non-numeric symbols will be removed and salary will be converted to monthly salary based on businessrules
  #' skills: skills machine learning and deep learning will be recreated based on the appearance in selected vector
  #' @param df_merged_data dataframe containing scraped indeed data
  #' @returns dataframe with cleaned salary and skill column
  
  ### CLEAN MERGED DATAFRAME ###
  ### CLEAN salary COLUMN ###
  tbl_jobs_cleaned_salary <- tibble(df_merged_data)
  
  # remove non-digits in salary string
  tbl_jobs_cleaned_salary$salary <- tbl_jobs_cleaned_salary$salary %>%
    str_replace_all("[^[:digit:]]", "")
 
  # Transformeer de kolom naar numeric
  tbl_jobs_cleaned_salary <- tbl_jobs_cleaned_salary %>%
    transform(salary = as.numeric(salary))
  
  # check salary type (hour, month, year, other)
  for (i in 1:nrow(tbl_jobs_cleaned_salary)){
    if(!is.na(tbl_jobs_cleaned_salary[i, 4]) &
       tbl_jobs_cleaned_salary[i, 4] > 10000){ # if year salary (>10000) then /12
      tbl_jobs_cleaned_salary[i, 4] <- (tbl_jobs_cleaned_salary[i, 4] / 12)
    }
    else if(!is.na(tbl_jobs_cleaned_salary[i, 4]) &
              tbl_jobs_cleaned_salary[i, 4] < 100){ # if hour salary (<100) then *162 (est. hours of work per month)
      tbl_jobs_cleaned_salary[i, 4] <- (tbl_jobs_cleaned_salary[i, 4] * 168)
    }
    else if(!is.na(tbl_jobs_cleaned_salary[i, 4]) &
              tbl_jobs_cleaned_salary[i, 4] >= 100 &
              tbl_jobs_cleaned_salary[i, 4] < 1000){ # if other salary (100 - 1000) then NA
      tbl_jobs_cleaned_salary[i, 4] <- NA
    }
  }
  

  ### CLEAN skills COLUMN ###
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
  
  tbl_cleaned_data <- tbl_job_skills_corrected
  return(tbl_cleaned_data)
}

enrich_scraped_date <- function(tbl_cleaned_data){
  #'Enriches a cleaned tible based on needed columns
  #'Columns skill_count, job_type, werkervaring will be added
  #'@param tbl_cleaned_data a cleaned dataframe with scraped indeed data
  #'@returns a dataframe with new columns added
  
  
  ### CREATE skill_count COLUMN ###
  #create new tibble
  tbl_job_skill_counts <- tibble(tbl_cleaned_data)
  
  # if skills are empty then 0 else count(comma's) +1 for number of skills
  tbl_job_skill_counts$skill_count <- ifelse(
    tbl_job_skill_counts$skills =="",
    0,
    str_count(tbl_job_skill_counts$skills, ",") +1
    )
  
  tbl_enriched_data <- tbl_job_skill_counts
  
  
  ### CREATE job_type COLUMN ###
  # create placeholder columns to compute job_name distance to job_type
  tbl_enriched_data$dist_data_analist <- NA
  tbl_enriched_data$dist_data_engineer <- NA
  tbl_enriched_data$dist_data_scientist <- NA
  
  
  # # put all original job titles to lowercase
  tbl_enriched_data$job_title <- tolower(tbl_enriched_data$job_title)
  
  
  # loop over data and determine jobtype based on levensthein distance
  for (i in 1:nrow(tbl_enriched_data)){
    
    #check for data analist
    job <- tbl_enriched_data$job_title[i]
    
    
    # Check data analyst similarity
    if (grepl('data analist', job) || grepl('data analyst', job)){
      tbl_enriched_data$dist_data_analist[i] <- 0
    } else{
      tbl_enriched_data$dist_data_analist[i] <- levenshtein_distance(
        job,
        'analist'
      )
    }
    
    
    # Check data engineer similarity
    if (grepl('data engineer', job)){
      tbl_enriched_data$dist_data_engineer[i] <- 0
    } else{
      tbl_enriched_data$dist_data_engineer[i] <- levenshtein_distance(
        job,
        'engineer'
      )
    }
    
    
    # Check data engineer similarity
    if (grepl('data scientist', job)){
      tbl_enriched_data$dist_data_scientist[i] <- 0
    } else{
      tbl_enriched_data$dist_data_scientist[i] <- levenshtein_distance(
        job,
        'scientist'
      )
    }
    
  }
  
  # column with NA placeholders to determine job_type
  tbl_enriched_data$job_type <- NA
  
  # check which column has the lowest distance and fill job_type accordingly
  tbl_enriched_data$job_type <- ifelse(
    (tbl_enriched_data$dist_data_analist < tbl_enriched_data$dist_data_engineer &
       tbl_enriched_data$dist_data_analist < tbl_enriched_data$dist_data_scientist),
    "Data analist",
    ifelse(
      (tbl_enriched_data$dist_data_engineer < tbl_enriched_data$dist_data_analist &
         tbl_enriched_data$dist_data_engineer < tbl_enriched_data$dist_data_scientist),
      "Data engineer",
      "Data scientist"
    )
  )
  
  #### CREATE werkervaring COLUMN ###
  # Check if desciption contains "werkervaring"
  tbl_enriched_data$contains_werkervaring <- grepl(
    "werkervaring",
    tbl_enriched_data$job_desc,
    ignore.case = T
  )
  
  # Create empty column to hold werkervaring description
  tbl_enriched_data$raw_werkervaring <- NA
  
  # grab large text size surounding the instance of werkervaring
  # sometimes we see text like: "minimaal 5 jaar aan vergelijkbare werkervaring"
  for (i in 1:nrow(tbl_enriched_data)){
    if(tbl_enriched_data$contains_werkervaring[i]){
      tbl_enriched_data$raw_werkervaring[i] <- substring(
        tbl_enriched_data$job_desc[i],
        first= (str_locate(tolower(tbl_enriched_data$job_desc[i]), "werkervaring")[1] - 20 ),
        last= (str_locate(tolower(tbl_enriched_data$job_desc[i]), "werkervaring")[2] + 20)
      )
    }
  }
  
  # extract het jaar uit de raw werkervaring kolom, pak hier vervolgens het laatste jaartal van
  tbl_enriched_data$cleaned_werkervaring_jaren <- tbl_enriched_data$raw_werkervaring %>%
    str_replace_all("[^[1-9]]", "") %>%
    substr(nchar(.), nchar(.))
  
  # add YYYYWW format to column weeknum
  tbl_enriched_data$weeknum <- format(tbl_enriched_data$listing_date, "%Y%W")
  
  # fill blank rows with NA
  tbl_enriched_data[tbl_enriched_data == ""] <- NA
  
  # remove fully empty rows
  tbl_enriched_data<- filter(tbl_enriched_data, rowSums(is.na(tbl_enriched_data)) != ncol(tbl_enriched_data)) 
  
  
  return(tbl_enriched_data)

}

unnest_skills <- function(tbl_enriched_data){
  #'Create an exploded dataframe where every skill within a vector of skills gets a new row
  #'@param tbl_enriched_data an enriched dataframe with scraped data from indeed
  #'@return exploded dataframe of skills
  
  #create an unnested skills table
  tbl_indeed_skills <- tbl_enriched_data %>% 
    mutate(skills = strsplit(as.character(skills), ",")) %>%
    unnest(skills)
  
  tbl_indeed_skills$skills <- str_trim(tbl_indeed_skills$skills)
  
  # remove empty rows from final tables
  tbl_indeed_skills <- filter(
    tbl_indeed_skills,
    rowSums(is.na(tbl_indeed_skills)) != ncol(tbl_indeed_skills)
  ) 
  
  return(tbl_indeed_skills)
}

filter_dates <- function(tbl_enriched_data, start_date="2022-02-05"){
  #'Create a dataframe with filtered dates for better time analysis and remove empty rows
  #'@param tbl_enriched_data enriched dataframe of scraped indeed data
  #'@param start_date the first date that should appear in the output dataframe
  #'@return dataframe with earlier dates removed
  
  # filter final tables based on relevant dates
  tbl_indeed_dates_filtered <- tbl_enriched_data[tbl_enriched_data$listing_date >= start_date, ]
  
  # remove empty rows from final tables
  tbl_indeed_dates_filtered <- filter(
    tbl_indeed_dates_filtered,
    rowSums(is.na(tbl_indeed_dates_filtered)) != ncol(tbl_indeed_dates_filtered)
  ) 
  
  return(tbl_indeed_dates_filtered)
}


