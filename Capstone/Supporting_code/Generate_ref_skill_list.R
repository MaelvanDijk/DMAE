library(readr)
library(tidyverse)

# Read data containing skills
indeed_raw <- read_csv("./Data_raw/D001/indeed_job_dataset.csv", show_col_types = FALSE)


# unnest the skills in  every row
indeed <- indeed_raw %>% 
  mutate(Skill = strsplit(as.character(Skill), ",")) %>%
  unnest(Skill)

# Remove { and } and ' from the remaining skills 
indeed$Skill <- gsub('[[]', '', indeed$Skill)
indeed$Skill <- gsub('[]]', '', indeed$Skill)
indeed$Skill <- gsub("[']", '', indeed$Skill)

# remove whitespaces and remove duplicates
skills <- table(indeed$Skill)
skill_names <- names(skills)
trimmed_skill_names<- trimws(skill_names, "left") %>% unique()

# save the remaining skills to a txt file for later use. 
# write.table(trimmed_skill_names,"./Data_cleaned/D001/trimmed_skill_names.txt",sep="\t",row.names=FALSE, append = FALSE)
