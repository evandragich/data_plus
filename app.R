# GLOBAL.R -----

### Beginning of Doctoral Stats ###

# This Doctoral Stats section takes in the master_long csv written in initial_preprocessing.R, and converts it to master_df, the final, long dataframe which
# will be used as the ground-level df in app.R. Also, we create several useful objects and add the character vectors to name_legend, which determine which
# divisions a program belongs to.

# Import packages and data; coerce factors ----

# Import Packages
library(tidyverse)
library(panelr)
library(snakecase)
library(reactable)

# Import data; coerce (nearly) all columns to factors with appropriate levels. (Note: Levels appear reversed due to later ggplot issues;
# when we coord_flip() in app.R, the levels would become reversed, so we reverse them now to cancel this out later)
master_df <- read_csv('output/data/master_df.csv', guess_max = 2436) %>%
  mutate(across(c(12, 15:16, 19:20, 53, 81:82, 101, 105, 108, 111, 113:119, 129, 133, 134, 138, 150:154, 178, 180:181), ~factor(.x, levels = c("Yes","No")))) %>%
  
  mutate(across(c(14, 63:71, 83:91, 104, 106, 112), ~factor(.x, levels = c("Very helpful","Somewhat helpful","Not very helpful","Not at all helpful","N/A-No advice received")))) %>%
  
  mutate(across(c(17), ~factor(.x, levels = c("Yes, and I attended", "Yes, but I did not attend", "No", "I don't remember")))) %>%
  
  mutate(across(c(18), ~factor(.x, levels = c("Very effective", "Fairly effective", "Neither effective nor ineffective", "Somewhat ineffective", "Very ineffective")))) %>%
  
  mutate(across(c(21:35, 55:58), ~factor(.x, levels = c("Excellent", "Very Good","Good","Fair","Poor")))) %>%
  
  mutate(across(c(36:40, 42:46, 59:61), ~factor(.x, levels = c("Strongly agree", "Agree", "Ambivalent", "Disagree", "Strongly Disagree")))) %>%
  
  mutate(across(c(41), ~factor(.x, levels = c("Strongly Disagree", "Disagree", "Ambivalent", "Agree", "Strongly agree")))) %>%
  
  mutate(across(c(45:51), ~factor(.x, levels = c("Not an obstacle","Minor obstacle","Major obstacle")))) %>%
  
  mutate(across(c(72:80, 92:100), ~factor(.x, levels = c("Very timely", "Somewhat timely", "Not very timely", "Not at all timely","N/A-No advice received")))) %>%
  
  mutate(across(c(102, 109, 135, 182), ~factor(.x, levels = c("5 or more","4","3","2","1")))) %>%
  
  mutate(across(c(127:128, 131:132), ~factor(.x, levels = c("10 or more","9","8","7","6","5","4","3","2","1")))) %>%
  
  mutate(across(c(139:140), ~factor(.x, levels = c("Not applicable","Increased","Decreased","Did not affect")))) %>%
  
  mutate(across(c(141), ~factor(.x, levels = c("$0","$1-$9,999","$10,000-$19,999","$20,000-$29,999","$30,000-$39,999","$40,000-$49,999","$50,000-$59,999","$60,000-$69,999","$70,000-$79,999","$80,000 or more")))) %>%
  
  mutate(across(c(142:149), ~factor(.x, levels = c("Excellent", "Very good","Good","Fair","Poor")))) %>%
  
  mutate(across(c(155:159), ~factor(.x, levels = c("Very dissatisfied","Somewhat dissatisfied","Somewhat satisfied","Very satisfied")))) %>%
  
  mutate(across(c(160:163), ~factor(.x, levels = c("Definitely", "Probably", "Maybe","Probably not", "Definitely not"))))


# Important: If you add new columns to exit_survey_col_names.csv, OR the name legend csv that is created in initial_preprocessing.R, you will *need* to add a new item
# in the new_row creation that occurs in the split_apart_m function below.
name_legend <- read_csv('output/data/name_legend.csv')

# Coerce program and grad term columns to be easily binned; these don't need specified factors though. Levels will get constantly overwritten
# in app.R, each time the user chooses a new sort mode.
master_df$pokemon <- as.factor(master_df$pokemon)
master_df$b_grad_term <- as.factor(master_df$b_grad_term)

# Create module vectors ----

# Create vectors of each survey's module choices to be used in app.R

es_modules <- c("Selection & Admission" = "a_", 
                "Departmental Orientation & Communication" = "do_",
                "General Support" = "gs_", 
                "Training Program & Program Quality" = "pq_", 
                "Program Climate" = "c_",
                "Obstacles" = "o_",
                "Certificate Programs" = "cp_", 
                "Qualifying Exam & Dissertation" = "qe_", 
                "Mentoring & Advising: Dissertation Advisor" = "m_",
                "Mentoring & Advising: Faculty Mentor" = "me_",
                "Career & Professional Development" = "d_",
                "Presentations & Publications" = "pp_",
                "Financial Support" = "f_",
                "Overall Satisfaction" = "os_",
                "Employment Status or Expectations" = "e_"
)

ao_modules <- c("Career and Professional Development" = "ca_",
                "Job Satisfaction" = "js_",
                "Program Quality" = "pr_",
                "Program Climate" = "cl_",
                "Obstacles" = "ob_",
                "Reflections" = "r_",
                "Alumni Event Interest" = "ev_",
                "Alumni Activity Interest" = "al_"
)

# Cleaning/binning binned variables ----


# Making a "key" of program->school 
normal_programs <- c("Pidgey","Sentret","Zigzagoon","Rattata","Linoone","Starly","Bidoof","Spearow","Hoothoot", "Jigglypuff", "Ambipom","Meowth","Taillow","Farfetch'd","Doduo","Swellow","Buneary","Glameow","Lickitung","Chansey","Happiny","Slakoth")
water_programs <- c("Totodile","Croconaw","Feraligatr","Chinchou","Lanturn","Marill","Azumarill","Politoed","Wooper","Quagsire","Slowking")
poison_programs <- c("Drapion")
electric_programs <- c("Blitzle","Zebstrika","Emolga","Tynamo")
grass_programs <- c("Treecko","Bellossom","Grovyle","Turtwig","Sceptile")
fighting_programs <- c("Lucario")
psychic_programs <- c("Gallade")

# Creating a new "school" column whose values are mapped from the above program->school key
master_df <- master_df %>% 
  mutate(., type = case_when(
    pokemon %in% normal_programs ~ "Normal",
    pokemon %in% water_programs ~ "Water",
    pokemon %in% poison_programs ~ "Poison", 
    pokemon %in% electric_programs ~ "Electric",
    pokemon %in% grass_programs ~ "Grass",
    pokemon %in% fighting_programs ~ "Fighting",
    pokemon %in% psychic_programs ~ "Psychic"))%>% 
  relocate(`type`, .after = `pokemon`)

# Making a key of grad term -> grad term bins                
spr_12_to_sum_2_15 <- c("2012 Fall Term","2012 Spring Term","2012 Summer Term 2","2013 Fall Term","2013 Spring Term",
                        "2013 Summer Term 2","2014 Fall Term","2014 Spring Term","2014 Summer Term 2","2015 Spring Term",
                        "2015 Summer Term 2")
fal_15_to_sum_2_20 <- c("2015 Fall Term","2016 Fall Term","2016 Spring Term","2016 Summer Term 2","2017 Fall Term",
                        "2017 Spring Term","2017 Summer Term 2","2018 Fall Term","2018 Spring Term","2018 Summer Term 2",
                        "2019 Fall Term","2019 Spring Term","2019 Summer Term 2","2020 Fall Term","2020 Spring Term","2020 Summer Term 2")

# Map grad_term to binned_grad_term following key from above
master_df <- master_df %>% 
  mutate(., b_grad_term_binned = case_when(
    b_grad_term %in% spr_12_to_sum_2_15 ~ "Spring '12 to Summer '15",
    b_grad_term %in% fal_15_to_sum_2_20 ~ "Fall '15 to Summer '20")) %>%
  relocate(`b_grad_term_binned`, .after = `b_grad_term`)

# Calculate new age at enrollment column, bin this to be used as facetting variable on plots
master_df <- master_df %>%
  mutate(enroll_year = as.numeric(enroll_year)) %>%
  mutate(dem_year_birth = as.numeric(dem_year_birth)) %>%
  mutate(., b_age_at_enroll = enroll_year - dem_year_birth) %>%
  relocate(b_age_at_enroll, .after = dem_year_birth) %>%
  mutate(., b_age_at_enroll_binned = cut(b_age_at_enroll,
                                         breaks=c(-Inf, 22, 30, 40,50, Inf),
                                         labels = c("18-22","23-29","30-39","40-49","50+"))) %>%
  relocate(b_age_at_enroll_binned, .after = b_age_at_enroll)

# Calculate cleaned, facetable version of race variable to be used as facetting variable on plots
racial_groups <- c("African-American/African-Caribbean/Black","American Indian or Alaska Native","Asian",
                   "Native Hawaiian or other Pacific Islander","White (non-Hispanic)","Other",NA)

case_racial_group <- function(dem_racial_group, dem_hisp_latino) {
  case_when(
    !(dem_racial_group %in% racial_groups) ~ "2+ groups",
    (dem_racial_group == "White (non-Hispanic)" & dem_hisp_latino == "Yes") ~ "White (Hispanic)",
    TRUE  ~ dem_racial_group
  )
}

master_df <- master_df %>%
  mutate(., dem_racial_group_cleaned = case_when(
    !(dem_racial_group %in% racial_groups) ~ "2+ groups",
    (dem_racial_group == "White (non-Hispanic)" & dem_hisp_latino == "Yes") ~ "White (Hispanic)",
    TRUE  ~ dem_racial_group)) %>%
  relocate(dem_racial_group_cleaned, .after = dem_racial_group)

master_df <- master_df %>%
  mutate(dem_citizen_cleaned = case_when(
    !is.na(dem_us_citizen) ~ dem_us_citizen,
    dem_non_us_citizen == "With a Permanent U.S. Resident Visa (“Green Card”)" ~ "Permanent Visa/ Green Card",
    !is.na(dem_non_us_citizen) ~ dem_non_us_citizen)) %>%
  relocate(dem_citizen_cleaned, .after = dem_non_us_citizen)

# Cleaning multiple-select entries ----

# Load necessary information ----
#(to be used in splitter function)
interact_choices <- c("email communications","campus visit","telephone","video-conference")
interact_names <- c("Prior to application, admission, or enrollment, did you communicate via email?",
                    "Prior to application, admission, or enrollment, did you visit campus?",
                    "Prior to application, admission, or enrollment, did you communicate via phone call?",
                    "Prior to application, admission, or enrollment, did you communicate via video-conference?")
interact_col <- "a_interact_t"

ta_choices <- c("Faculty/instructor","Department training","Graduate School","Other")
ta_names <- c("Did you receive TA Training from a faculty/instructor?",
              "Did you receive TA Training from a department training",
              "Did you receive TA Training from The Graduate School",
              "Did you receive TA Training from another source?")
ta_col <- "d_ta_training_location"

colteach_choices <- c("Faculty/instructor","Department training","Graduate School","Other")
colteach_names <- c("Did you receive College Teaching Training from a faculty/instructor?",
                    "Did you receive College Teaching Training from a department training",
                    "Did you receive College Teaching Training from The Graduate School",
                    "Did you receive College Teaching Training from another source?")
colteach_col <- "d_colteach_training_location"

ta_solo_choices <- c("Duke Summer School","Duke regular term course","Other institution(s)")
ta_solo_names <- c("Did you teach your own courses as a Duke regular term course?",
                   "Did you teach your own courses as a Duke summer term course?",
                   "Did you teach your own courses at other instutition(s)?")
ta_solo_col <- "d_ta_solo_location"

fundsource_choices <- c("A research grant","Your department or program","The Graduate School","Other institutional funds","Other")
fundsource_names <- c("Did you receive funding for off-campus presentations from a research grant?",
                      "Did you receive funding for off-campus presentations from your department or program?",
                      "Did you receive funding for off-campus presentations from The Graduate School?",
                      "Did you receive funding for off-campus presentations from other institutional funds?",
                      "Did you receive funding for off-campus presentations from another source?")
fundsource_col <- "pp_fundsource"

fin_supp_choices <- c("University or department","Teaching Assistantship",
                      "Research Assistantship","nationally competitive","locally competitive",
                      "Work study","Other Source")
fin_supp_names <- c("Did you receive financial support from a University or department fellowship or scholarship?",
                    "Did you receive financial support from a TA Stipend?",
                    "Did you receive financial support from a RA Stipend?",
                    "Did you receive financial support from a non-university, nationally-competitive scholarship?",
                    "Did you receive financial support from a non-university, locally-competitive scholarship?",
                    "Did you receive financial support from work study?",
                    "Did you receive financial support from another source?")
fin_supp_col <- "f_sources"

# Function to return the index of a column in name_legend; need this due to global assignment of name_legend in split_apart_m
index <- function(col) {
  which(name_legend[,2] == col)
}

#Function to create new binary columns for each option of a multiple-select question
split_apart_m <- function(df, nl, col, index, choices, names, k, module, survey) {
  # Termination condition for recursion; sends df back through the recursion and modifies the global version of name_legend
  if (k >= length(choices)) {
    name_legend <<- nl[-c(index),]
    return(df)
  }
  
  # Update temp variables for this iteration of recursion
  k <- k + 1
  tempname = paste0(col,"_",to_snake_case(choices[k]))
  
  # Create new column for a particular option in col
  df <- df %>%
    mutate(!!sym(tempname) := case_when(
      is.na(!!sym(col)) ~ NA_character_,
      str_detect(!!sym(col),choices[k]) ~ "Yes",
      TRUE ~ "No")) %>%
    relocate(!!sym(tempname), .before = !!sym(col)) %>%
    mutate(across(c(!!sym(tempname)), ~factor(.x, levels = c("Yes","No")))) 
  
  # Create a new name legend entry for this; add it to the local version of name legend, in the appropriate spot.
  # IMPORTANT: Since this method relies on new_row having the correct number of arguments as columns in name_legend; if you add a column 
  # to name_legend (ie. another similar display_streamlined, etc), add another argument here (such as NA_character_) so rbind() can function
  # properly. Don't worry about the value itself; you can change it manually (as done below for display_streamlined).
  new_row <- list("newly_created", tempname,names[k],module, survey, TRUE, TRUE, NA_character_, NA_character_)
  nl <- rbind(nl[1:(index + k - 1),],new_row,nl[-(1:(index + k - 1)),])
  
  # Enter the recursion loop with the newly updated local master df, with the new calculated column
  df <- df %>%
    split_apart_m(., nl, col, index, choices, names, k, module, survey)
  
}

# Execute this splitter function on the multiple-select columns ----

master_df <- split_apart_m(master_df, name_legend, interact_col, index(interact_col), interact_choices, interact_names, 0, "Selection & Admission", "Exit Survey") %>%
  select(., -c(!!sym(interact_col))) %>%
  split_apart_m(., name_legend, ta_col, index(ta_col) , ta_choices, ta_names, 0, "Career & Professional Development", "Exit Survey") %>% # need to + 1 to index call to account for having added a new row last time!
  select(., -c(!!sym(ta_col))) %>%
  split_apart_m(., name_legend, colteach_col, index(colteach_col) , colteach_choices, colteach_names, 0, "Career & Professional Development", "Exit Survey") %>%
  select(., -c(!!sym(colteach_col))) %>%
  split_apart_m(., name_legend, ta_solo_col, index(ta_solo_col) , ta_solo_choices, ta_solo_names, 0, "Career & Professional Development", "Exit Survey") %>%
  select(., -c(!!sym(ta_solo_col))) %>%
  split_apart_m(., name_legend, fundsource_col, index(fundsource_col) , fundsource_choices, fundsource_names, 0, "Presentations & Publications", "Exit Survey") %>%
  select(., -c(!!sym(fundsource_col))) %>%
  split_apart_m(., name_legend, fin_supp_col, index(fin_supp_col) , fin_supp_choices, fin_supp_names, 0, "Financial Support", "Exit Survey") %>%
  select(., -c(!!sym(fin_supp_col)))

# Remove some of the new calculated columns from being displayed in the streamlined view. (Others carry FALSE from the name_legend csv;
# that is not possible to do for these since we just calculated them)

name_legend <- name_legend %>%
  transform(., display_streamlined = ifelse(new_name %in% c("a_interact_t_email_communications",
                                                            "a_interact_t_campus_visit",
                                                            "a_interact_t_telephone",
                                                            "a_interact_t_video_conference",
                                                            "a_interact_h",
                                                            "d_ta_training_location_faculty_instructor",
                                                            "d_ta_training_location_department_training",
                                                            "d_ta_training_location_graduate_school",
                                                            "d_ta_training_location_other",
                                                            "d_colteach_training_location_faculty_instructor",
                                                            "d_colteach_training_location_department_training",
                                                            "d_colteach_training_location_graduate_school",
                                                            "d_colteach_training_location_other",
                                                            "d_ta_solo_location_duke_summer_school",
                                                            "d_ta_solo_location_duke_regular_term_course",
                                                            "d_ta_solo_location_other_institution_s",
                                                            "pp_fundsource_the_graduate_school",
                                                            "pp_fundsource_other_institutional_funds",
                                                            "pp_fundsource_other",
                                                            "f_sources_teaching_assistantship",
                                                            "f_sources_research_assistantship",
                                                            "f_sources_nationally_competitive",
                                                            "f_sources_locally_competitive",
                                                            "f_sources_work_study",
                                                            "f_sources_other_source")
                                            ,FALSE, display_streamlined))

name_legend <- name_legend %>%
  transform(., question_type = ifelse(new_name %in% c("a_interact_t_email_communications",
                                                            "a_interact_t_campus_visit",
                                                            "a_interact_t_telephone",
                                                            "a_interact_t_video_conference",
                                                            "a_interact_h",
                                                            "d_ta_training_location_faculty_instructor",
                                                            "d_ta_training_location_department_training",
                                                            "d_ta_training_location_graduate_school",
                                                            "d_ta_training_location_other",
                                                            "d_colteach_training_location_faculty_instructor",
                                                            "d_colteach_training_location_department_training",
                                                            "d_colteach_training_location_graduate_school",
                                                            "d_colteach_training_location_other",
                                                            "d_ta_solo_location_duke_summer_school",
                                                            "d_ta_solo_location_duke_regular_term_course",
                                                            "d_ta_solo_location_other_institution_s",
                                                            "pp_fundsource_a_research_grant",
                                                            "pp_fundsource_your_department_or_program",
                                                            "pp_fundsource_the_graduate_school",
                                                            "pp_fundsource_other_institutional_funds",
                                                            "pp_fundsource_other",
                                                            "f_sources_teaching_assistantship",
                                                            "f_sources_research_assistantship",
                                                            "f_sources_nationally_competitive",
                                                            "f_sources_locally_competitive",
                                                            "f_sources_work_study",
                                                            "f_sources_other_source")
                                            ,"yes_no", question_type))


# Making the reactable ----

# Removed unnecessary rows whose contents aren't present in the Shiny dashboard
key_table <- name_legend %>% 
  slice(., c(3:420)) %>% 
  filter(., (happy_question))

# Make the reactable object to be used in app.R
key <- reactable(
  key_table[5:3],
  groupBy = c("data_source", "module"),
  columns = list(
    longer_name = colDef(name = "Question"),
    module = colDef(name = "Module"),
    data_source = colDef(name = "Data Source")),
  filterable = TRUE, minRows = 10,
  searchable = TRUE,
)

# APP.R -----
# Load packages; no need to load data as environment is already created in global.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(scales)
#library(snakecase)
#library(reactable)

# UI ----

# UI should be pretty self explanatory; it's comprised of a dashboardHeader, Sidebar, and Body, each with various widgets contained in boxes and fluidRow layout.
# Look up shinydashboard documentation to explore anything further

ui <- dashboardPage(
     # dashboardHeader(title = "Duke Grad School Alumni Career Outcomes", titleWidth = 450),
     
     dashboardHeader(title = "Graduate School Data",
                     tags$li(a(href='http://duke.edu',
                               tags$img(src='logo_duke.png',
                                        height = "30px")),
                             class = "dropdown"),
                     tags$li(a(href='https://gradschool.duke.edu/about/program-statistics',
                               tags$img(src='logo_tgs.png',
                                        height = "30px")),
                             class = "dropdown"),
                     tags$li(a(href='https://bigdata.duke.edu/projects/mapping-trajectories-duke-doctoral-students',
                               tags$img(src='logo_data.png',
                                        height = "30px")),
                             class = "dropdown")),
     
    ## Sidebar content 
    dashboardSidebar(disable = TRUE),
    
 
    
 ## Body content 
    dashboardBody(

                  tabsetPanel(type = "tabs",
                              ## Exit Survey ----
                              tabPanel("Exit Survey",fluidRow(
                                ### Pre-Plot Selection ----
                                                        column(width = 2,
                                                                 box(title = "Save:", width = NULL,
                                                                     downloadButton('downloadData1', 'Current data (.csv)'),
                                                                     downloadButton('downloadPlot1', 'Current plot (.png)')
                                                                 ),
                                                                 box(title = "Question Selection", width = NULL,
                                                                     prettyRadioButtons("streamline1","Streamline Settings",
                                                                                        choices = c("On (\"Show me what matters\")" = TRUE, 
                                                                                                    "Off (\"Show me everything\")" = FALSE),
                                                                                        selected = TRUE
                                                                     ),
                                                                     selectInput("module1","Select a question module:",
                                                                                 choices = es_modules,
                                                                                 selected = "os_"
                                                                     ),
                                                                     selectInput("question1","Select a question:", 
                                                                                 choices = NULL)
                                                                 )
                                                          ),
                                                          column(width = 2,
                                                                 box(title = "Filtering Selection", width = NULL,
                                                                     prettyCheckboxGroup("type1","Select type(s) to view:",
                                                                                         choices = c("All Types", 
                                                                                                     "Electric",
                                                                                                     "Fighting",
                                                                                                     "Grass",
                                                                                                     "Normal",
                                                                                                     "Poison",
                                                                                                     "Psychic",
                                                                                                     "Water"),
                                                                                         selected = "All Types"
                                                                     ),
                                                                     prettyCheckboxGroup("generation1","Select generation(s) to view:",
                                                                                         choices = c("All Generations" = "ALL", 
                                                                                                     "Generation I" = "Generation I",
                                                                                                     "Generation II" = "Generation II",
                                                                                                     "Generation III" = "Generation III",
                                                                                                     "Generation IV" = "Generation IV",
                                                                                                     "Generation V" = "Generation V"),
                                                                                         selected = "Generation III"
                                                                     ),
                                                                     prettyRadioButtons("one1","Select one Pokemon:",
                                                                                        choices = c("Off", 
                                                                                                    "On"),
                                                                                        selected = "Off"
                                                                     ),
                                                                     conditionalPanel(condition = "input.one1 == 'On'", 
                                                                                      selectInput("pokemon1", "Select a Pokemon:",
                                                                                                  choices = NULL
                                                                                      )
                                                                     )
                                                                     
                                                                 )
                                                          ),
                                ### Plot Display ----
                                                          column(width = 8,
                                                          box(title = "Plot", width = NULL,
                                                              textOutput("plot1warning"),
                                                              plotOutput("plot1", height = 500)
                                                          ),
                                                          
                                       )
                                                                 
                              ),
                              fluidRow(
                                ### Post-Plot Selection ----
                                column(width = 4),
                                column(width = 3,
                                       box(title = "Comparisons", width = NULL,
                                           prettyCheckboxGroup("facet1","Select up to 2 attributes to compare by:",
                                                               choices = c("Graduation Term" = "b_grad_term_binned",
                                                                           "Age at Enrollment" = "b_age_at_enroll_binned",
                                                                           "Race/Ethnicity" = "dem_racial_group_cleaned",
                                                                           "Type" = "type",
                                                                           "Generation" = "b_generation",
                                                                           "Gender" = "dem_gender",
                                                                           "Citizenship Status" = "dem_citizen_cleaned"
                                                               )
                                           )
                                       )
                                ),
                                column(width = 3,
                                box(title = "View Selection", width = NULL,
                                    prettyRadioButtons("merge1","Choose plot view:",
                                                       choices = c("Individual Pokemon", 
                                                                   "Grouped Summary"),
                                                       selected = "Individual Pokemon"
                                    ),
                                    prettyRadioButtons("remove1","Hide NA (blank) responses?",
                                                       choices = c("Yes", 
                                                                   "No"),
                                                       selected = "No"
                                    ),
                                    prettyRadioButtons("label1","Toggle labels:",
                                                       choices = c("Percent" = "percent",
                                                                   "Number" = "number"),
                                                       selected = "percent")
                                )
                                ),
                                column(width = 2,
                                       box(title = "Sort Selection", width = NULL,
                                           prettyRadioButtons("sort1","Sort Pokemon by:",
                                                              choices = c("Alphabetical Order", 
                                                                          "Descending Mean Ratings",
                                                                          "Descending Size"),
                                                              selected = "Descending Mean Ratings"
                                           ),
                                           
                                       ))
                              )
                              ),
                              ## Alumni Outcomes ----
                              tabPanel("Alumni Outcomes", fluidRow(
                                ### Pre-Plot Selection ----
                                column(width = 2,
                                       box(title = "Save:", width = NULL,
                                           downloadButton('downloadData2', 'Current data (.csv)'),
                                           downloadButton('downloadPlot2', 'Current plot (.png)')
                                       ),
                                       box(title = "Question Selection", width = NULL,
                                           selectInput("module2","Select a question module:",
                                                       choices = ao_modules
                                           ),
                                           selectInput("question2","Select a question:", 
                                                       choices = NULL
                                           ),
                                       )
                                ),
                                column(width = 2,
                                       box(title = "Filtering Selection", width = NULL,
                                           prettyCheckboxGroup("type2","Select type(s) to view:",
                                                               choices = c("All Types", 
                                                                           "Electric",
                                                                           "Fighting",
                                                                           "Grass",
                                                                           "Normal",
                                                                           "Poison",
                                                                           "Psychic",
                                                                           "Water"),
                                                               selected = "All Types"
                                           ),
                                           prettyCheckboxGroup("generation2","Select generation(s) to view:",
                                                               choices = c("All Generations" = "ALL", 
                                                                           "Generation I" = "Generation I",
                                                                           "Generation II" = "Generation II",
                                                                           "Generation III" = "Generation III",
                                                                           "Generation IV" = "Generation IV",
                                                                           "Generation V" = "Generation V"),
                                                               selected = "Generation III"
                                           ),
                                           prettyRadioButtons("one2","Select one Pokemon:",
                                                              choices = c("Off", 
                                                                          "On"),
                                                              selected = "Off"
                                           ),
                                           conditionalPanel(condition = "input.one2 == 'On'", 
                                                            selectInput("pokemon2", "Select a Pokemon:",
                                                                        choices = NULL
                                                            )
                                           )
                                           
                                       )
                                ),
                                ### Plot Display ----
                                column(width = 8,
                                       box(title = "Plot", width = NULL,
                                           textOutput("plot2warning"),
                                           plotOutput("plot2", height = 500)
                                       ),
                                       
                                )
                                
                              ),
                              fluidRow(
                                ### Post-Plot Selection ----
                                column(width = 4),
                                column(width = 3,
                                       box(title = "Comparisons", width = NULL,
                                           prettyCheckboxGroup("facet2","Select up to 2 attributes to compare by:",
                                                               choices = c("Graduation Term" = "b_grad_term_binned",
                                                                           "Age at Enrollment" = "b_age_at_enroll_binned",
                                                                           "Race/Ethnicity" = "dem_racial_group_cleaned",
                                                                           "Type" = "type",
                                                                           "Generation" = "b_generation",
                                                                           "Gender" = "dem_gender",
                                                                           "Citizenship Status" = "dem_citizen_cleaned"
                                                               )
                                           )
                                       )
                                ),
                                column(width = 3,
                                       box(title = "View Selection", width = NULL,
                                           prettyRadioButtons("merge2","Choose plot view:",
                                                              choices = c("Individual Pokemon", 
                                                                          "Grouped Summary"),
                                                              selected = "Individual Pokemon"
                                           ),
                                           prettyRadioButtons("remove2","Hide NA (blank) responses?",
                                                              choices = c("Yes", 
                                                                          "No"),
                                                              selected = "Yes"
                                           ),
                                           prettyRadioButtons("label2","Toggle labels:",
                                                              choices = c("Percent" = "percent",
                                                                          "Number" = "number"),
                                                              selected = "percent")
                                       )
                                ),
                                column(width = 2,
                                       box(title = "Sort Selection", width = NULL,
                                           prettyRadioButtons("sort2","Sort Pokemon by:",
                                                              choices = c("Alphabetical Order", 
                                                                          "Descending Mean Ratings",
                                                                          "Descending Size"),
                                                              selected = "Descending Mean Ratings"
                                           ),
                                           
                                       ))
                              )
                              ),
                              ## User Guide Table ----
                              tabPanel("User Guide to Modules and Questions",
                                       fluidRow(
                                         column(width = 2,
                                                box(title = "Link to user guide:", width = NULL,
                                                    #uiOutput("guidelink")
                                                    textOutput("text")
                                                    )
                                           
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12,
                                                box(title = "Drop-down Module Guide", width = NULL,
                                                  reactableOutput("table")
                                                  )
                                         )
                                       )
                              )
                                       
                  )
                  
                    
                
                       
                )
    )








# Server ----
server <- function(input, output, session) {

## UPDATE  MENUS ----
  
  
  
    # Update question 1 options based on answer to module 1
    observe({
        
        temp <- toString(input$module1)
        temp <- paste0('^', temp)
        
        q_choices <- name_legend %>%
          filter(., (happy_question)) %>%
          `if`((input$streamline1), .[.$display_streamlined,], .) %>%
            select(new_name, longer_name) %>%
            filter(., str_detect(new_name, temp)) %>%
            pull(longer_name)
        
        updateSelectInput(session,'question1',
                          choices=q_choices)
    }) 
  
  # Update "select one pokemon" option for plot 1 based on answer to division1/school1
  observe({
    
    choices_df <- master_df %>%
      select(pokemon, type, b_generation) %>%
      distinct(.)
    
    if (!("All Types" %in% input$type1)) {
      choices_df <- choices_df %>%
        filter(type %in% input$type1)
    }
    if (!("ALL" %in% input$generation1)) {
      choices_df <- choices_df %>%
        filter(b_generation %in% input$generation1)
    }
      
    p_choices <- choices_df$pokemon %>%
      droplevels() %>%
      levels()
    
    updateSelectInput(session,'pokemon1',
                      choices=p_choices)
  }) 
  
  # Update question 2 menu options based on answer to module 2
  observe({
    
    temp <- toString(input$module2)
    temp <- paste0('^', temp)
    
    q_choices <- name_legend %>%
      filter(., (happy_question)) %>%
      `if`((input$streamline1), .[.$display_streamlined,], .) %>%
      select(new_name, longer_name) %>%
      filter(., str_detect(new_name, temp)) %>%
      pull(longer_name)
    
    updateSelectInput(session,'question2',
                      choices=q_choices)
  }) 
  
  # Update "select one program" option for plot 2 based on answer to division2/school2
  observe({
    
    choices_df <- master_df %>%
      select(pokemon, type, b_generation) %>%
      distinct(.)
    
    if (!("All Types" %in% input$type2)) {
      choices_df <- choices_df %>%
        filter(type %in% input$type2)
    }
    if (!("ALL" %in% input$generation2)) {
      choices_df <- choices_df %>%
        filter(b_generation %in% input$generation2)
    }
    
    p_choices <- choices_df$pokemon %>%
      droplevels() %>%
      levels()
    
    updateSelectInput(session,'pokemon2',
                      choices=p_choices)
  }) 
  
## GLOBAL FUNCTIONS ----
  
    # Define function that will facilitate optional faceting based on facet inputs from above
    fac_by <- function(var_fac1, var_fac2) {
      facet_grid(rows=if(missing(var_fac1)) NULL else enquos(var_fac1),
                 cols=if(missing(var_fac2)) NULL else enquos(var_fac2), 
                 scales = "free",
                 labeller = label_wrap_gen(width = 16, multi_line = TRUE))
    } 
    
    # Define function to sort with removing means; only used as input into the next sorting reactive() :
    mean_func <- function(x) {
      mean(x, na.rm = TRUE)
    }

## PLOT ONE REACTIVES ----
    
    ### Plot One Temp Variables ----
    
  # Reassign module prefix for first plot based on user input.
  temp1 <- reactive(
    paste0('^', toString(input$module1))
  )
  
  # Reassign short form, column form question for first plot based on user input.   
  tempcol1 <- reactive(
    name_legend %>%
      filter(., longer_name == input$question1) %>%
      filter(., str_detect(new_name, temp1())) %>%
      pull(new_name) %>%
      sym(.)
  )
  # Reassign longform question name for first plot based on user input.    
  tempq1 <- reactive(
    sym(input$question1)
  )
  # Reassign question 1 type based on user input.
  questiontype1 <- reactive(
    name_legend %>%
      filter(., longer_name == input$question1) %>%
      select(question_type) %>%
      distinct() %>%
      pull(.) %>%
      toString()
  )
  
  
  # Reassign first faceting variable for first plot based on user input.    
  var_A_1 <- reactive(
    if (length(input$facet1) > 0) {
      if (! is.na(input$facet1[1])) {
        sym(input$facet1[1])
      } else {
        NULL
      }
    } else {
      NULL
    }
  )
  # Reassign second faceting variable for first plot based on user input.
  var_B_1 <- reactive(
    if (length(input$facet1) > 0) {
      if (! is.na(input$facet1[2])) {
        sym(input$facet1[2])
      } else {
        NULL
      }
    } else {
      NULL
    }
  )
  
  ### Plot One Pipe Chain ----
  
  # Reassign "NA-removed" dataframe for first plot based on user input to NA removal toggle
  removed_df1 <- reactive(
    if (input$remove1 == "Yes") {
      master_df %>%
        drop_na(!!tempcol1())
    } else {
      master_df
    }
    
  )
  
  # Reassign filtered dataframe for first plot based on updated user inputs to survey/school/division/program/gender filtration
  filtered_df1 <- reactive(
    removed_df1() %>%
      # Now that we know we are not dealing with any longitudinal questions from REDCap questions, we can keep only wave 0 data from everyone.
      .[.$wave == "0",] %>%
      `if`(!("All Types" %in% input$type1), .[.$type %in% input$type1,], .) %>%
      `if`(!("ALL" %in% input$generation1), .[.$b_generation %in% input$generation1,], .) %>%
      `if`(input$one1 == "On", .[.$pokemon == input$pokemon1,], .) %>%
      # Special case when facetting by gender and/or age and/or racial group--remove NA for these demographics by default, 
      # as there are many (and that 3rd panel on the facetted graph output becomes really gross...)
      `if`("gender" %in% input$facet1, .[!(is.na(.$dem_gender)),], .) %>% 
      `if`("b_age_at_enroll_binned" %in% input$facet1, .[!(is.na(.$b_age_at_enroll_binned)),], .) %>%
      `if`("dem_racial_group_cleaned" %in% input$facet1, .[!(is.na(.$dem_racial_group_cleaned)),], .)
    
  )
  
  # Reassign sorted dataframe for first plot based on updated user inputs to sort selection
  sorted_df1 <- reactive(
    filtered_df1() %>%
      # Intermediate filter step to remove bins with <5 people
      select(pokemon, !!tempcol1(), !!var_A_1(), !!var_B_1()) %>%
      group_by(pokemon, !!var_A_1(), !!var_B_1()) %>%
      mutate(sorter = mean_func(as.numeric(!!tempcol1()))) %>%
      ungroup(.) %>%
      group_by(pokemon, !!var_A_1(), !!var_B_1()) %>%
      filter(., n() >=5) %>%
      ungroup(.) %>%
      # Now that we've done that, go back to the broader df to finalize the data for plotting
      group_by(pokemon, !!var_A_1(), !!var_B_1(), sorter, !!tempcol1()) %>%
      summarise(n=n()) %>%
      ungroup(., sorter) %>%
      mutate(perc = round(((n / sum(n))*100), digits=0)) %>%
      mutate(perclabel = paste0(perc, "%")) %>%
      mutate(total = sum(n)) %>%
      mutate(total = paste(pokemon, "\n n =", total, sep = " ")) %>%
      mutate(total = as.factor(total)) %>%
      ungroup(.) %>%
      `if`((input$sort1 == "Descending Mean Ratings"), 
           mutate(., total = fct_reorder(total, sorter, mean)), .) %>%
      `if`((input$sort1 == "Descending Size"), 
           mutate(., total = fct_reorder(total, n, sum, .desc = TRUE)), .) %>%
      group_by(pokemon, !!var_A_1(), !!var_B_1())
  )
  
  # Reassign the "grouped" version of the dataframe for first plot based on updated user inputs to sort selection
  sorted_df1merged <- reactive(
    filtered_df1() %>%
      # Intermediate filter step to remove bins with <5 people
      group_by(!!var_A_1(), !!var_B_1()) %>%
      filter(., n() >=5) %>%
      ungroup(.) %>%
      # Now that we've done that, go back to the broader df to finalize the data for plotting
      select(!!tempcol1(), !!var_A_1(), !!var_B_1()) %>%
      group_by(!!var_A_1(), !!var_B_1(), !!tempcol1()) %>%
      summarise(n=n()) %>%
      mutate(perc = round(((n / sum(n))*100), digits=0)) %>%
      mutate(perclabel = paste0(perc, "%")) %>%
      mutate(total = sum(n)) %>%
      mutate(total = paste("n =", total, sep = " ")) %>%
      mutate(dum = "All selected pokemons")
  )
  
  # Reassign labeling variable based on user input
  labels1 <- reactive(
    if (input$label1 == "percent") {
      sym("perclabel")
    } else {
      sym("n")
    }
  )
  
  # WEIRD THING: For whatever reason, it can't recognize the obstacel module questions as being likert_3. That's why
  # their case (likert_3) is also the catch-all case. I literally have no idea why this works, but it does
  scale1geom <- reactive(
      if (questiontype1() == "yes_no") {
        rev(brewer.pal(3, "RdYlBu")[c(1,3)]) %>%
          scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "likert_3") {
      rev(brewer.pal(11, "RdYlBu")[c(4,6,8)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "split_scale_4") {
      rev(brewer.pal(11, "RdYlBu")[c(4,6,8,9)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "likert_4") {
      rev(brewer.pal(11, "RdYlBu")[c(4:5,7:8)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "likert_5") {
      rev(brewer.pal(11, "RdYlBu")[c(3:4,6,8:9)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "scale_3") {
      rev(brewer.pal(9, "Blues")[c(3,4,7)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "scale_5") {
      rev(brewer.pal(9, "Blues")[c(3,4,6,7,8)]) %>%
        scale_fill_manual(values = ., name = str_wrap(tempq1()), na.value = "gray73")
    } else if (questiontype1() == "qualitative") {
      scale_fill_brewer(palette = "Set3", name = str_wrap(tempq1()), na.value = "gray73")
    } else {
        scale_fill_brewer(palette = "RdYlBu", name = str_wrap(tempq1()), direction = -1, na.value = "gray73")
    }
  )
  
  # Reassign first plots based on all previous updated user inputs
  g1 <- reactive(
    ggplot(sorted_df1(), aes(x = fct_rev(total),y = perc, fill=!!tempcol1(),label = !!labels1())) +
      geom_col(position = "stack") +  
      geom_text(position = position_stack(vjust = .5))  + 
      scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0, 0)) + 
      scale_x_discrete(expand = c(0, 0)) +
      coord_flip() +
      scale1geom() +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(x="Pokemon",y="Rating (Percentage)") +
      theme(legend.position = "top") +
      fac_by(!!var_A_1(), !!var_B_1())
    
  )
  
  g1merged <- reactive(
    ggplot(sorted_df1merged(), aes(x = total,y = perc, fill=!!tempcol1(),label = !!labels1())) +
      geom_col(position = "stack") +  
      geom_text(position = position_stack(vjust = .5))  + 
      scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0)) +
      coord_flip() +
      scale_fill_brewer(palette = 'RdYlBu', name = str_wrap(tempq1()), direction = -1, na.value = "gray73") +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(x="Summary of Selected Pokemon",y="Rating (Percentage)") +
      theme(legend.position = "top") +
      fac_by(!!var_A_1(), !!var_B_1())
    
  )
  
  ### Plot One Output/Saving ----
  
  # Update which plot is currently active based on user's merge choice, for printing and for saving
  plot1 <- reactive(
    if (input$merge1 != "Individual Pokemon") {
      g1merged()
    } else {
      g1()
    }
  )
  
  badplot1 <- reactive(
    if (input$merge1 != "Individual Pokemon") {
      if (nrow(sorted_df1merged()) == 0) {
        TRUE
      } else if (nrow(sorted_df1merged()) != 0) {
        FALSE
      }
    } else {
      if (nrow(sorted_df1()) == 0) {
        TRUE
      } else if (nrow(sorted_df1()) != 0) {
        FALSE
      }
    }
  )
  
  
  output$plot1 <- renderPlot({
    if (badplot1()) {
      print(NULL)
    } else {
      print(plot1())
    }
  })
  
  output$plot1warning <- renderText({
    if (badplot1()) {
      "Error: No categories with 5+ respondents remain. To see a plot, try any of the following: \n
              (1) Turn off the comparison feature,\n
              (2) Choose a grouped summary view,\n
              (3) Select more generations, types, or Pokemon, or\n
              (4) Pick another question."
    } else {
      NULL
    }
  })
  
  # Experiment with downloading first data/plot
  output$downloadData1 <- downloadHandler(
    filename = function() { paste0(to_snake_case(toString(tempq1())), '.csv') },
    content = function(file) {
      write.csv(sorted_df1(), file)
    }
  )
  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste0(to_snake_case(toString(tempq1())), '.png') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = 12, res = 300, units = "in")
      ggsave(file, plot = plot1(), device = device)
    }
  )
    
  


## PLOT TWO REACTIVES ----
  
  ### Plot Two Temp Variables ----

    # Reassign module prefix for second plot based on user input.
    temp2 <- reactive(
      paste0('^', toString(input$module2))
    )

    # Reassign short form, column form question for second plot based on user input.   
    tempcol2 <- reactive(
          name_legend %>%
          filter(., longer_name == input$question2) %>%
          filter(., str_detect(new_name, temp2())) %>%
          pull(new_name) %>%
          sym(.)
        )
    # Reassign longform question name for second plot based on user input.    
    tempq2 <- reactive(
      sym(input$question2)
    )

    # Reassign first faceting variable for second plot based on user input.    
    var_A_2 <- reactive(
      if (length(input$facet2) > 0) {
        if (! is.na(input$facet2[1])) {
          sym(input$facet2[1])
        } else {
          NULL
        }
      } else {
        NULL
      }
    )
    # Reassign second faceting variable for second plot based on user input.
    var_B_2 <- reactive(
      if (length(input$facet2) > 0) {
        if (! is.na(input$facet2[2])) {
          sym(input$facet2[2])
        } else {
          NULL
        }
      } else {
        NULL
      }
    )
    
    ### Plot Two Pipe Chain ----
    
    # Reassign "NA-removed" dataframe for second plot based on user input to NA removal toggle
    removed_df2 <- reactive(
      if (input$remove2 == "Yes") {
        master_df %>%
          drop_na(!!tempcol2())
      } else {
        master_df
      }
      
    )
    
    # Reassign filtered dataframe for second plot based on updated user inputs to school/division/program/gender filtration
    filtered_df2 <- reactive(
      removed_df2() %>%
        # Now that we know we are not dealing with any longitudinal questions from REDCap questions, we can keep only wave 0 data from everyone.
        .[.$wave == 0,] %>%
        `if`(!("All Types" %in% input$type2), .[.$type %in% input$type2,], .) %>%
        `if`(!("ALL" %in% input$generation2), .[.$b_generation %in% input$generation2,], .) %>%
        `if`(input$one2 == "On", .[.$pokemon == input$pokemon2,], .) %>%
        # Special case when facetting by gender and/or age and/or racial group--remove NA for these demographics by default, 
        # as there are many (and that 3rd panel on the facetted graph output becomes really gross...)
        `if`("gender" %in% input$facet2, .[!(is.na(.$dem_gender)),], .) %>%
        `if`("b_age_at_enroll_binned" %in% input$facet2, .[!(is.na(.$b_age_at_enroll_binned)),], .) %>%
        `if`("dem_racial_group_cleaned" %in% input$facet2, .[!(is.na(.$dem_racial_group_cleaned)),], .)

    )
  
    
    # Reassign sorted dataframe for first plot based on updated user inputs to sort selection
    sorted_df2 <- reactive(
      filtered_df2() %>%
        # Intermediate filter step to remove bins with <5 people
        select(pokemon, !!tempcol2(), !!var_A_2(), !!var_B_2()) %>%
        group_by(pokemon, !!var_A_2(), !!var_B_2()) %>%
        mutate(sorter = mean_func(as.numeric(!!tempcol2()))) %>%
        ungroup(.) %>%
        group_by(pokemon, !!var_A_2(), !!var_B_2()) %>%
        filter(., n() >=5) %>%
        ungroup(.) %>%
        group_by(pokemon, !!var_A_2(), !!var_B_2(), sorter, !!tempcol2()) %>%
        summarise(n=n()) %>%
        ungroup(., sorter) %>%
        # sorter = mean(as.numeric(a_interact_yn))) %>%
        mutate(perc = round(((n / sum(n))*100), digits=0)) %>%
        mutate(perclabel = paste0(perc, "%")) %>%
        mutate(total = sum(n)) %>%
        mutate(total = paste(pokemon, "\n n =", total, sep = " ")) %>%
        mutate(total = as.factor(total)) %>%
        ungroup(.) %>%
        `if`((input$sort2 == "Descending Mean Ratings"), 
             mutate(., total = fct_reorder(total, sorter, mean)), .) %>%
        `if`((input$sort2 == "Descending Size"), 
            mutate(., total = fct_reorder(total, n, sum, .desc = TRUE)), .) %>%
        group_by(pokemon, !!var_A_2(), !!var_B_2())
        
    )
    
    # Reassign labeling variable based on user input
    labels2 <- reactive(
      if (input$label2 == "percent") {
        sym("perclabel")
      } else {
        sym("n")
      }
    )
    
    # Reassign the "grouped" version of the dataframe for first plot based on updated user inputs to sort selection
    sorted_df2merged <- reactive(
      filtered_df2() %>%
        # Intermediate filter step to remove bins with <5 people
        group_by(!!var_A_2(), !!var_B_2()) %>%
        filter(., n() >=5) %>%
        ungroup(.) %>%
        # Now that we've done that, go back to the broader df to finalize the data for plotting
        select(!!tempcol2(), !!var_A_2(), !!var_B_2()) %>%
        group_by(!!var_A_2(), !!var_B_2(), !!tempcol2()) %>%
        summarise(n=n()) %>%
        mutate(perc = round(((n / sum(n))*100), digits=0)) %>%
        mutate(perclabel = paste0(perc, "%")) %>%
        mutate(total = sum(n)) %>%
        mutate(total = paste("n =", total, sep = " ")) %>%
        mutate(dum = "All selected pokemons")
    )
    
    # Reassign second plots based on all previous updated user inputs
    g2 <- reactive(
      ggplot(sorted_df2(), 
             aes(x = fct_rev(total),
                 y = perc,
                 fill=!!tempcol2(),
                 label = !!labels2(),
                 ))+
        geom_col(position = "stack") +  
        geom_text(position = position_stack(vjust = .5))  + 
        scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0, 0)) + 
        scale_x_discrete(expand = c(0, 0)) +
        coord_flip() +
        scale_fill_brewer(palette = 'RdYlBu', name = str_wrap(tempq2()), direction = -1, na.value = "gray73") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(x="Pokemon",y="Rating (Percentage)") +
        theme(legend.position = "top") +
        fac_by(!!var_A_2(), !!var_B_2())
      
    )
    
    g2merged <- reactive(
      ggplot(sorted_df2merged(),
             aes(x = total,
                 y = perc,
                 fill=!!tempcol2(),
                 label = !!labels2(),
                 )) +
        geom_col(position = "stack") +  
        geom_text(position = position_stack(vjust = .5))  + 
        scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0, 0)) +
        scale_x_discrete(expand = c(0, 0)) +
        coord_flip() +
        scale_fill_brewer(palette = 'RdYlBu', name = str_wrap(tempq2()), direction = -1, na.value = "gray73") +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(x="Summary of Selected Pokemon",y="Rating (Percentage)") +
        theme(legend.position = "top") +
        fac_by(!!var_A_2(), !!var_B_2())
    )
    
    ### Plot Two Output/Saving ----
    
    # Update which plot is currently active based on user's merge choice, for printing and for saving
    plot2 <- reactive(
      if (input$merge2 != "Individual Pokemon") {
        g2merged()
      } else {
        g2()
      }
    )
    
    badplot2 <- reactive(
      if (input$merge2 != "Individual Pokemon") {
        if (nrow(sorted_df2merged()) == 0) {
          TRUE
        } else if (nrow(sorted_df2merged()) != 0) {
          FALSE
        }
    } else {
        if (nrow(sorted_df2()) == 0) {
        TRUE
      } else if (nrow(sorted_df2()) != 0) {
      FALSE
      }
    }
    )
    
    
    output$plot2 <- renderPlot({
      if (badplot2()) {
        print(NULL)
      } else {
      print(plot2())
      }
    })
    
    output$plot2warning <- renderText({
      if (badplot2()) {
        "Error: No categories with 5+ respondents remain. To see a plot, try any of the following: \n
              (1) Turn off the comparison feature,\n
              (2) Choose a grouped summary view,\n
              (3) Select more generations, types, or Pokemon, or\n
              (4) Pick another question."
      } else {
        NULL
      }
    })
    
    # Experiment with downloading second data/plot
    output$downloadData2 <- downloadHandler(
      filename = function() { paste0(to_snake_case(toString(tempq2())), '.csv') },
      content = function(file) {
        write.csv(sorted_df2(), file)
      }
    )
    
      
    output$downloadPlot2 <- downloadHandler(
      filename = function() { paste0(to_snake_case(toString(tempq2())), '.png') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = 10, height = 12, res = 300, units = "in")
        ggsave(file, plot = plot2(), device = device)
      }
    )
  
  # ### REACTABLE ----
  # url <- a("User Guide", href="this is where the link would go once i anonymize the user guide")
  # output$guidelink <- renderUI({
  #   tagList(url)
  # })
    
    output$text <- renderText({ "User Guide TBA (To be anonymized!)"})
      
  output$table <-
    renderReactable(key)

    
}

shinyApp(ui, server)
