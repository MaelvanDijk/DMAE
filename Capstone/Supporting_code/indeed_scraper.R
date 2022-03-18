library(tidyverse)
library(rvest)
library(XML)
library(stringr)
library(lubridate)

job_base_url <- 'https://nl.indeed.com'
job_base_search_url <- 'https://nl.indeed.com/jobs?q=data&start='
# url_job_search <- read_html('https://nl.indeed.com/jobs?q=data')

wordlist <- read.table(
  './Data_cleaned/D001/trimmed_skill_names.txt',
  header = TRUE
) %>% .$x

jobs_df <- data.frame(
  Company = character(),
  job_title = character(),
  salary = character(),
  skills = character(),
  days_online = numeric(),
  job_desc = character(),
  job_link = character()
)

# gather ~330 job listings
for (i in seq.default(0, 290, 10)){
  
  job_list_url <- paste(job_base_search_url, as.character(i), sep= "")
  job_list_read <- read_html(job_list_url) # lees html in
  
  job_urls <- job_list_read %>%
    html_nodes(xpath= '//*[@id="mosaic-zone-jobcards"]') %>% # ga naar het specifieke xpath
    html_nodes('a') %>% # ga naar a
    html_attr("href")# pak de href
  
  # loop over job_urls
  for (j in 1:length(job_urls)){
    
    tryCatch(
      # Try below code
      expr = {
        # when url is longer than 200 chars it is a job url
        # nchar(job_urls[[j]]) > 200
        if (grepl('/pagead/', job_urls[[j]]) | grepl('/clk?', job_urls[[j]]) | grepl('/company/', job_urls[[j]])  ){
          full_job_url <- paste(job_base_url, job_urls[[j]], sep="")
          
          # read html
          job_html_read <- read_html(full_job_url)
          
          # extract company name
          company_name <- job_html_read %>%
            html_nodes(css= 'head > meta:nth-child(15)') %>%
            html_attr('content')
          
          # extract title
          job_title <- job_html_read %>%
            html_node('h1') %>%
            html_text()
          
          time_since_placed_text <- job_html_read %>%
            html_nodes(css= '.jobsearch-JobMetadataFooter > div:nth-child(2)') %>%
            html_text()
          
          # check if string does not contain integer using regex
          if (!grepl("\\d", time_since_placed_text) & !grepl("Vandaag", time_since_placed_text)){
            time_since_placed_text <- job_html_read %>%
              html_nodes(css= '.jobsearch-JobMetadataFooter > div:nth-child(1)') %>%
              html_text()
          }
          
          if (grepl("\\d", time_since_placed_text)){
            time_since_placed <- str_extract_all(time_since_placed_text, "\\(?[0-9,.]+\\)?")[[1]]
          } else if (grepl("Vandaag", time_since_placed_text)) {
            time_since_placed <- 0
          } else {
            time_since_placed <- NA
          }
          
          
          # extract description
          job_description <- job_html_read %>%
            html_nodes(xpath= '//*[@id="jobDescriptionText"]') %>%
            html_text()
          
          # remove non alpha numeric characters
          job_description_cleaned <- gsub('[^[:alnum:] ]', ' ', job_description)
          
          # generate wordvector
          wordvec <- unname(unlist(sapply(job_description_cleaned, function(z) str_split(tolower(z), " "))))
          
          # extract skills from word vector
          skill_table <- wordvec[wordvec %in% tolower(wordlist)] %>%
            unique()
          
          # probeer salari op te halen via specifieke salary indicator
          salary_indicator <- job_html_read %>%
            html_nodes(css= '#salaryInfoAndJobType') %>%
            html_text()
          
          # Als de € niet voorkomt in het bestand probeer dan de salary distribution
          if (length(salary_indicator) < 1){
            salary_indicator <- job_html_read %>%
              html_nodes(css= '.cmp-SalaryDistributionDisplayWidget-subinfo') %>%
              html_text()
          }
          
          if (length(salary_indicator) < 1){
            salary <- NA
          } else if (grepl('€',salary_indicator)) {
            # anders zoek de eerste salaris indicatie
            salary_start_num <- unlist(gregexpr('€', salary_indicator))[1]
            salary <- substring(salary_indicator, salary_start_num, salary_start_num + 7) 
          } else {
            salary <- NA
          }
          
          new_row <- c(
            company_name,
            job_title,
            salary,
            toString(unique(skill_table)),
            as.numeric(time_since_placed),
            job_description_cleaned,
            full_job_url)
          
          jobs_df[nrow(jobs_df) + 1,] = new_row
        }
      },
      warning = function(w){
        message("a warning occured with url. ", full_job_url)
        message("the data trying to be stored in the the dataframe is:")
        message(new_row)
        message("And below is the warning message from R:")
        message(w)
        
      },
      error=function(error_message) {
        message("an error occured with url.", full_job_url)
        message("And below is the error message from R:")
        message(error_message)
      },
      finally = {
        Sys.sleep(4)
      }
    )
  }
}

jobs_df$listing_date <- today() - as.numeric(jobs_df$days_online)

file_n<-sprintf("./Data_raw/D002/% s scraped indeed data.csv", today())
write.csv(jobs_df, file_n)


