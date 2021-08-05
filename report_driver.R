
# Import Packages -----
library(rmarkdown)
library(tidyverse)
library(knitr)
library(snakecase)
library(table1)
library(kableExtra)

# Import Data; create global objects and functions ----

# This function will be used in report.Rmd to preserve the ordering of questions and still wrap them properly
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# Import Data (this csv is created in regression_exploration)
happy_df_numeric <- read_csv('output/data/summarised_happy_questions_numeric.csv') %>%
  mutate(pokemon = factor(program), .after = program) %>%
  mutate(program = NULL)

# Grab list of all programs; this will be used to map() over at very bottom (and auto-generate a report for each program!)
allprograms <- happy_df_numeric %>%
  select(pokemon) %>%
  distinct() %>%
  pull() %>%
  as.character()



# Choose list of questions to be included in boxplots on reports; these are currently arbitrarily chosen by Evan but could eventually be result of regression 
# or chosen by someone more relevant. Use the short names here; longer names are *automatically grabbed* later on
myquestions <- c("a_interact_h",
                 "do_effective",
                 "do_assess",
                 "gs_fin",
                 "pq_employ",
                 "pq_overall",
                 "c_intelpos",
                 "c_socpos",
                 "c_mcareer",
                 "o_req",
                 "qe_course",
                 "m_help_careeracad",
                 "m_help_careernonacad",
                 "m_help_employ",
                 "pp_oncampus",
                 "pp_offcampus",
                 "pp_pubs",
                 "f_sat",
                 "os_acad_exper",
                 "os_stud_exper",
                 "os_overall_exper",
                 "os_same_uni",
                 "os_recommend") %>%
  sort() # <- put in alphabetical order to match up with long names on the next lines

# Grab vector of these longquestion names from name_legend; this eliminates need for name_legend inside report.Rmd
myquestionslongform <- name_legend %>%
  filter(new_name %in% myquestions) %>%
  select(new_name, longer_name) %>%
  arrange(new_name) %>%
  select(longer_name) %>%
  pull()

# Creates mapping from "old" (myquestions) to "new" (myquestionslongform). This "old"/"new" is in the context of for the boxplots (IE. we want the full, longname to be "new"ly
# added on the boxplots)
names(myquestionslongform) <- myquestions

# Create new "global" df to be used in all summary statistics in reports
summary_df <- master_df %>%
  filter(wave == 0) %>%
  select(pokemon, b_grad_term_binned, b_age_at_enroll_binned, dem_gender, dem_racial_group_cleaned, dem_citizen_cleaned) %>%
  mutate(across(c(2:4), ~ factor(.x))) %>%
  mutate(dem_racial_group_cleaned = factor(dem_racial_group_cleaned, levels = c("2+ groups",
                                                                                "African-American/African-Caribbean/Black",
                                                                                "American Indian or Alaska Native",
                                                                                "Asian",
                                                                                "Native Hawaiian or other Pacific Islander",
                                                                                "White (Hispanic)",
                                                                                "White (non-Hispanic)",
                                                                                "Other"
                                                                                ))) %>%
  mutate(dem_citizen_cleaned = factor(dem_citizen_cleaned, levels = c("Since birth",
                                                                                "Naturalized",
                                                                                "Permanent Visa/ Green Card",
                                                                                "With a Temporary U.S. Visa"
                                                                                )))




# Render_report function ----

# Function that takes in a program name and generates a report for it
invisible(map(allprograms, function(mypokemon) { 
  
  # Generate filename for reports
  myfilename <- paste0("output/reports/",to_snake_case(mypokemon),"_summary_boxplots",".pdf")
  
  # Determine program's generation and store this as mygeneration
  mygeneration <- master_df %>%
    .[.$pokemon == mypokemon,] %>%
    select(b_generation) %>%
    distinct() %>%
    pull()
  
  # This hard-coding step is needed because of two programs, for whom graduates indicated 2 different Divisions (II & III;  III & IV respectively.)
  # Oh, the skill of survey respondents! *chef's kiss*
  # We want the latter of each, respectively, here:
  if (mypokemon == "Slakoth") { mygeneration = "Generation III" }
  if (mypokemon == "Turtwig") { mygeneration = "Generation IV" }
  
  #Determine vector of programs to compare to (including this program itself); these will be the ones represented in the boxplots in the report
  comparison_group <- master_df %>%
    select(b_generation, pokemon) %>%
    filter(b_generation == mygeneration) %>%
    select(pokemon) %>%
    distinct() %>%
    droplevels() %>%
    pull() %>%
    as.character()

  
  # Create mini version of happy_df_numeric to use inside report.Rmd, with full-name questions.
  mydata <- happy_df_numeric %>%
    filter(pokemon %in% comparison_group) %>% # <- grab just the relevant programs
    select(pokemon, all_of(myquestions)) %>% # <- grab just the program column and each of the question columns named above
    pivot_longer(!pokemon, names_to = "question", values_to = "score") %>%  # <- pivot this longer to be plotted easier
    mutate(question = recode(question, !!!myquestionslongform)) %>% # <- "index match" the short questions into their long form
    mutate(highlight = case_when(
      pokemon == mypokemon ~ TRUE,
      !is.na(pokemon) ~ FALSE
    )) %>% # <- create new column that will be mapped to alpha to on the plot (ie shade in the specific program's plots)
    group_by(question) %>%
    mutate(z_score = scale(score)) %>% # <- calculate z score, to order plots (ie. along a particular program's z score)
    ungroup(.) %>%
    arrange(desc(highlight), desc(z_score)) # <- put questions into their final order, to be saved as the levels below
  
  mydata <- mydata %>%
    mutate(extreme = case_when(
      !highlight ~ "Other-program values",
      z_score <= -1 ~ "Below Average",
      z_score >= 1 ~ "Above Average",
      !is.na(z_score) ~ "Near Average"
    )) # <- create new column that will be used to shade extreme values
  
  mydata$extreme <- factor(mydata$extreme, levels = c( "Other-program values" ,
                                                       "Below Average",
                                                       "Near Average",
                                                       "Above Average"))
  
  # Reorder the levels of "question" and reverse to be properly plotted in order in report.Rmd
  mydata$question <- fct_inorder(mydata$question) %>%
    fct_rev()
  
  # Rearrange mydata into this proper order to plot by descending myprogram z scores
  mydata<- mydata %>%
    arrange(question)
  
  # Filter out for just one program's data
  mysummary_df <- summary_df %>%
    filter(pokemon == mypokemon) %>% 
    mutate(across(c(2:6), ~ droplevels(.x))) # <-  Drop unused levels of summary_df so they won't appear as 0s in table

  
  

  # Execute the report for this particular program!
  rmarkdown::render(input = "report.Rmd", 
                    params = list(
                      pokemon = mypokemon
                      ),
                    output_format = "pdf_document",
                    output_file = myfilename
  )
    
})) 


