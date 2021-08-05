# GLOBAL.R ----

# Import packages and data ----
library(tidyverse)
library(panelr)
library(readxl)

exit_survey <- read_xlsx("pokemon_data/Survey Data - PhD Completers Survey.xlsx", guess_max = 2000)
redcap_data <- read_xlsx("pokemon_data/Survey Data - REDCap Data.xlsx", guess_max = 11000)


# Filter out weird/non-aligning data
redcap_data <- filter(redcap_data, record_id > 106000)
exit_survey <- filter(exit_survey, record_id > 106000)

# Dedupe surveys ----

# Create function to be used to remove partial dupes from redcap lists, modeled after this process. Second parameter deals with the special case of the year 5 data, where the 
# column name that determines the validity of a row actually differs from the others (grad school experience timestamp vs position information timestamp)
dedupe_partial <- function(x, type = 'position', index = 1) {
  # Create new df with the ids that have duplicates
  dupe_ids <- x %>%
    group_by(x[[index]]) %>%
    filter(n() > 1) %>%
    count()
  
  # Store the full rows of these duplicate ids in a new df and add a new column to count blanks
  dupe_rows <- x %>%
    filter(record_id %in% dupe_ids[[1]])
  dupe_rows$blanks <- rowSums(is.na(dupe_rows))
  
  # Sort these rows by id, then by blanks, then by date, and save first of each id in a new df to keep. In other words, when there are duplicate entries for the same id, 
  # we keep the one with the fewest blanks, with ties in that being broken by keeping the newer entry
  if (type == 'experience') {
    dupe_rows_to_keep <- dupe_rows %>%
      arrange(., record_id, dupe_rows$blanks, desc(dupe_rows$grad_school_experience_timestamp)) %>%
      distinct(., record_id, .keep_all = TRUE)
  } else if (type == 'position') {
    dupe_rows_to_keep <- dupe_rows %>%
      arrange(., record_id, dupe_rows$blanks, desc(dupe_rows$position_information_timestamp)) %>%
      distinct(., record_id, .keep_all = TRUE)
  } else {
    dupe_rows_to_keep <- dupe_rows %>%
      arrange(., record_id, dupe_rows$blanks, desc(`Capture Date`)) %>%
      distinct(., record_id, .keep_all = TRUE)
  }
  
  # Create a new df with those duplicate rows that ARE NOT the ones we just identified to keep
  dupe_rows_to_remove <- setdiff(dupe_rows, dupe_rows_to_keep)
  
  # Modify our original df to no longer include the duped rows we indicated to remove
  x <- x %>%
    setdiff(., dupe_rows_to_remove[,-(ncol(x) + 1)])
}

#Call dedupe function on exit survey; need 'exit' parameter to specify which column identifies date of submission, and 2 to locate the record_id position.
exit_survey <- exit_survey %>%
  dedupe_partial(., 'exit', 2)

## Exit survey ----

# New Names imported from this file 
name_legend<-read_csv("pokemon_data/exit_survey_col_names.csv")

#Removed Roman Numerals from PhD Completers' modules and made all modules title case
name_legend$module <- str_replace_all(name_legend$module, c("XIII. OVERALL SATISFACTION" = "OVERALL SATISFACTION",
                                                            "XIV. EMPLOYMENT STATUS OR EXPECTATIONS" = "EMPLOYMENT STATUS OR EXPECTATIONS",
                                                            "XV. DEMOGRAPHIC INFORMATION" = "DEMOGRAPHIC INFORMATION")) %>% 
  str_to_title()




# Grab the new column names from our name legend file and assign them to be the names of exit survey
newnames_es <- name_legend %>%
  select(new_name)%>%
  slice(c(3:187)) %>%
  drop_na()%>%
  pull()

names(exit_survey) <- newnames_es


## Redcap ----

# Separate redcap data by year to facilitate merging and dedupe each
redcap_baseline <- redcap_data %>%
  filter(., redcap_event_name == 'baseline_arm_1') %>%
  subset(., select = c(1:32)) %>%
  distinct()
redcap_pregraduation <- redcap_data %>%
  filter(., redcap_event_name == 'pregraduation_arm_1') %>%
  subset(., select = c(1,3,33:102)) %>%
  filter(., !is.na(redcap_repeat_instrument)) %>%
  dedupe_partial()
redcap_postgraduation <- redcap_data %>%
  filter(., redcap_event_name == 'postgraduation_arm_1') %>%
  subset(., select = c(1,3,33:102)) %>%
  filter(., !is.na(redcap_repeat_instrument)) %>%
  dedupe_partial()
redcap_year_1 <- redcap_data %>%
  filter(., redcap_event_name == 'year_1_arm_1') %>%
  subset(., select = c(1,3,33:102)) %>%
  filter(., !is.na(redcap_repeat_instrument)) %>%
  dedupe_partial()
redcap_year_3 <- redcap_data %>%
  filter(., redcap_event_name == 'year_3_arm_1') %>%
  subset(., select = c(1,3,33:102)) %>%
  filter(., !is.na(redcap_repeat_instrument)) %>%
  dedupe_partial()
redcap_year_5_pos <- redcap_data %>%
  filter(., redcap_event_name == 'year_5_arm_1') %>%
  subset(., select = c(1,3,33:102)) %>%
  filter(., !is.na(redcap_repeat_instrument)) %>%
  dedupe_partial()
redcap_year_5_que <- redcap_data %>%
  filter(., redcap_event_name == 'year_5_arm_1') %>%
  subset(., select = c(1,3,103:210)) %>%
  filter(., !is.na(grad_school_experience_timestamp)) %>%
  dedupe_partial(., type = 'experience')

# Bind groups of deduped rows ----

# Since pre-/post-graduation surveys accomplish the same purpose 
# (according to Cisco they are both intended to ideally be the "same" temporal point, it just maximized response rate to send it out twice), I will treat them as one dataset and combine and dedupe accordingly.
redcap_year_0 <- redcap_pregraduation %>%
  bind_rows(., redcap_postgraduation) %>%
  dedupe_partial(.)

# Also recombining the year 5 position questions and the survey questions ('pos' and 'que' respectively). 
redcap_year_5 <- redcap_year_5_pos %>%
  full_join(., y = redcap_year_5_que, by = c('record_id'))


# Start combining exit survey with redcap data according to record id's
master_df <- exit_survey %>%
  inner_join(., y = redcap_baseline, by = c("record_id"))

# Combine the other 4 time points in 2 pairs of 2 to best facilitate suffixing
redcap_year_0 <- redcap_year_0 %>%
  full_join(., y = redcap_year_1, by = c("record_id"), suffix = c("_yr0", "_yr1"))

redcap_year_3 <- redcap_year_3 %>%
  full_join(., y = redcap_year_5, by = c("record_id"), suffix = c("_yr3", "_yr5"))

# Put her all together baby! 
master_df <- master_df %>%
  full_join(., y = redcap_year_0, by = c("record_id")) %>%
  full_join(., y = redcap_year_3, by = c("record_id"))

master_df <- master_df %>%
  long_panel(., prefix = "_yr", periods = c(0,1,3,5), label_location = "end")

newnames_ml <- name_legend %>%
  select(new_name)%>%
  pull(.)

names(master_df) <- newnames_ml

write_csv(master_df, 'output/data/master_df.csv')
write_csv(name_legend, 'output/data/name_legend.csv')

### End of Initial Doctoral ###