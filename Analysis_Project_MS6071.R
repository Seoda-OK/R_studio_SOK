# MS6071 - R for Statistical Data Science
# EndoLIST Project

# Load packages -----------------------------------------------------------
library(tidyverse)

# Import baseline data ----------------------------------------------------
baseline <- read_csv("data/Patient_onboarding_info.csv")
View(baseline)

dim(baseline) #gives dimensions ierows & columns
names(baseline) #names of columns


# Import longitudinal data ------------------------------------------------
#find all CSV files that start with "User"
user.files <- list.files(
  path = "data",
  pattern = "^User.*\\.csv$", #^=start of filename, .*followed by anything, \\. full stop, $end of filename
  full.names = TRUE
)

#check that all participant files have been found
length(user.files)
user.files

# PHYSICAL WELLBEING ------------------------------------------------
#check physical wellbeing column names
wellbeing.columns <- map(
  user.files,
  function(file) {
    data <- read_csv(file, show_col_types = FALSE)
    data |>
      select(contains("General physical condition")) |>
      names()
  }
) |>
  unlist()

#the names of the columns that contain general physical condition
table(wellbeing.columns)

# function to extract general physical condition variables
extract_user_data <- function(file) {
  data <- read_csv(file, show_col_types = FALSE)
  
  # Identify which wellbeing scale the participant uses
  wellbeing_column <- names(data) |>
    str_subset("General physical condition")
  
  max_wellbeing_score <- case_when(
    str_detect(wellbeing_column, "10-amazing") ~ 10,
    str_detect(wellbeing_column, "4-amazing") ~ 4
  )
  
  # Extract and standardise wellbeing
  data |>
    select(
      `User id`,
      `Day in study`,
      `Cycle number`,
      `Cycle day`,
      contains("General physical condition")
    ) |>
    rename(
      physical_wellbeing = contains("General physical condition")
    ) |>
    mutate(
      max_wellbeing_score = max_wellbeing_score,
      physical_wellbeing = if_else(
        physical_wellbeing < 1 | physical_wellbeing > max_wellbeing_score,
        NA_real_,
        as.numeric(physical_wellbeing)
      ), #one participant put 0 on a 1-10 scale
      wellbeing_standardised =
        (physical_wellbeing - 1) / (max_wellbeing_score - 1) * 10
    )
}


longitudinal <- user.files |>
  map(extract_user_data) |> 
  list_rbind() #combine into datafame by stacking on top of each other

View(longitudinal)
dim(longitudinal)

missing.data <- longitudinal |>
  summarise(
    missing_user = sum(is.na(`User id`)),
    missing_day = sum(is.na(`Day in study`)),
    missing_cycle = sum(is.na(`Cycle number`)),
    missing_cycle_day = sum(is.na(`Cycle day`)),
    missing_wellbeing = sum(is.na(physical_wellbeing))
  )
View(missing.data)

#if wellbeing < median, let it be equal to 1, but if not, let it be equal to 0
wellbeing_data <- longitudinal |>
  filter(!is.na(physical_wellbeing))|> #remove rows that are missing physical wellbeing
  group_by(`User id`) |>
  mutate(
    median_wellbeing = median(physical_wellbeing),
    wellbeing_score = if_else(
      physical_wellbeing < median_wellbeing, 1,0 
  )
    ) |>
  ungroup()

wellbeing_data |>
  count(
    below_median = physical_wellbeing < median_wellbeing,
    wellbeing_score
  )

# # A tibble: 2 × 3
# below_median wellbeing_score     n
# <lgl>                  <dbl> <int>
# FALSE                      0  3195
# TRUE                       1  1393

wellbeing_data |>
  count(wellbeing_score) |>
  mutate(
    percentage = n / sum(n) * 100
  )
 
# # A tibble: 2 × 3
# wellbeing_score     n     percentage
# <dbl> <int>      <dbl>
#  0                3195       69.6
# 1                 1393       30.4

#check wellbeing scores
wellbeing_data |>
  group_by(max_wellbeing_score) |>
  summarise(
    participants = n_distinct(`User id`),
    min_score = min(wellbeing_standardised),
    max_score = max(wellbeing_standardised),
    .groups = "drop"
  ) |>
  print(width = Inf)
# # A tibble: 2 × 4
# max_wellbeing_score participants min_score max_score
# <dbl>        <int>     <dbl>     <dbl>
#  4           11      0           10
#  10           23     -1.11        10

# AFTER UPDATING CODE
# # A tibble: 2 × 4
# max_wellbeing_score participants min_score max_score
# <dbl>        <int>     <dbl>     <dbl>
#   4           11         0        10
#  10           23         0        10

View(wellbeing_data)


# PAIN SEVERITY ---------------------------------------------------
symptom_map <- read_csv(
  "data/Symptom_MedDRA_map.csv",
  show_col_types = FALSE
)
names(symptom_map)

#ignore case ie not case sensitive
#to find most common pain types
pain_types <- symptom_map |>
  filter(str_detect(`PT name`, regex("pain", ignore_case = TRUE))) |>
  group_by(`PT name`) |>
  summarise(
    participants = n_distinct(`User id`), .groups = "drop"
  ) |>
  arrange(desc(participants))
View(pain_types)

# Map participant-specific symptoms into four pain categories.
pain_map <- symptom_map |>
 mutate(
    pain_group = case_when(
      `PT name` %in% c(
        "Abdominal pain",
        "Abdominal pain upper",
        "Abdominal pain lower"
      ) ~ "abdominal_pain",
      
      `PT name` == "Back pain" ~ "back_pain",
      `PT name` == "Pelvic pain" ~ "pelvic_pain",
      `PT name` == "Pain" ~ "general_pain",
      TRUE ~ NA_character_ #For everything that hasn't matched one of the previous conditions → put a missing value (NA).
    )
  )  |>
  filter(!is.na(pain_group))|>
  distinct( ##keep only 1 row per user per pain_group
    `User id`,
    `Symptom full name`,
    pain_group,
    .keep_all = TRUE #keep all other columns in df
  )
View(pain_map) #table of all pain recordings in these 4 groups from all participants over all days

pain_map |>
  group_by(pain_group) |>
  summarise(
    participants = n_distinct(`User id`)
  ) #how many users in each pain group
# A tibble: 4 × 2
# pain_group     participants
# <chr>                 <int>
#   1 abdominal_pain           31
# 2 back_pain                29
# 3 general_pain             19
# 4 pelvic_pain              27

overlapping_symptoms <- pain_map |>
  count(`User id`, `Symptom full name`) |>
  filter(n > 1)
View(overlapping_symptoms) #check if 1 symptom full name is in more than 1 pain_group

pain_map |>
  semi_join( #semi_join() finds the rows in pain_map that match the participant and symptom combinations in overlapping_symptoms.
    overlapping_symptoms,
    by = c("User id", "Symptom full name")
  ) |>
  select(`User id`, `Symptom full name`, `PT name`, pain_group) |>
  arrange(`User id`, pain_group) |>
  print(n = Inf) #investigate which symptoms are in more than 1 pain_group


#make list of symptoms to extract
pain_symptoms <- pain_map |>
  distinct(
    `User id`,
    `Symptom full name`
  ) #ensures that a symptom such as Lower back pain and pelvic pain is extracted only once, even though it belongs to two MedDRA categories.

#Check that the symptom names match the column names in user files
check_pain_columns <- function(file) {
  data <- read_csv(file, show_col_types = FALSE)
  user <- data$`User id`[1] #select the User id column, select the 1st value in the column
  symptoms <- pain_symptoms |>
    filter(`User id` == user) |>
    pull(`Symptom full name`)
  
  tibble(
    `User id` = user,
    symptom = symptoms,
    found = symptoms %in% names(data) #checks whether each symptom is an actual column in their CSV.
  )
}

column_check <- user.files |>
  map(check_pain_columns) |>
  list_rbind()

column_check |>
  count(found) #found is TRUE when the column exists.

# # A tibble: 1 × 2
# found     n
# <lgl> <int>
# TRUE    169

# Display any symptoms that could not be matched
column_check |>
  filter(!found)
# A tibble: 0 × 3


#extract pain scores
extract_pain_data <- function(file) {
  
  data <- read_csv(file, show_col_types = FALSE)
  
  user <- data$`User id`[1]
  
  symptoms <- pain_symptoms |>
    filter(`User id` == user) |>
    pull(`Symptom full name`)
  
  if (length(symptoms) == 0) {
    return(NULL) #If this participant has no symptoms belonging to our selected pain groups, skip that file. error without it for user 81
  }
  
  data |>
    select(
      `User id`,
      `Day in study`,
      all_of(symptoms)
    ) |>
    pivot_longer(
      cols = all_of(symptoms),
      names_to = "Symptom full name",
      values_to = "pain_score"
    )
}

pain_data <- user.files |>
  map(extract_pain_data) |>
  list_rbind()

pain_data |>
  summarise(
    observations = n(),
    participants = n_distinct(`User id`),
    symptoms = n_distinct(`Symptom full name`)
  )


# # A tibble: 1 × 3
# observations participants symptoms
# <int>        <int>    <int>
# 25077           33       56

#only 33 partcicpants not 34 because user 80 didnt record any of the 4 pain_groups analysed

#add pain groups & standardise scores 0-10
pain_data <- pain_data |>
  left_join(
    pain_map,
    by = c("User id", "Symptom full name"),
    relationship = "many-to-many" #some symptoms belong to two pain_groups
  ) |> 
  mutate(
    max_score = case_when(
      str_detect(`Symptom full name`, "4-worst pain") ~ 4,
      str_detect(`Symptom full name`, "10-worst pain") ~ 10,
      TRUE ~ NA_real_
    ),
    pain_standardised = pain_score / max_score * 10
  )

View(pain_data)

pain_data |>
  group_by(pain_group) |>
  summarise(
    observations = n(),
    missing_scores = sum(is.na(pain_score)),
    participants = n_distinct(`User id`),
    .groups = "drop"
  )

# # A tibble: 4 × 4
# pain_group     observations missing_scores participants
# <chr>                 <int>          <int>        <int>
#   1 abdominal_pain        10769           1693           31
# 2 back_pain              5043            856           29
# 3 general_pain           3437            638           19
# 4 pelvic_pain            6720           1361           27

#check pain data df
pain_data |>
  filter(is.na(max_score)) |>
  distinct(`Symptom full name`)
# A tibble: 0 × 1

pain_data |>
  filter(!is.na(pain_score), pain_score < 0 | pain_score > max_score)
# A tibble: 0 × 18

pain_data |>
  summarise(
    min_score = min(pain_standardised, na.rm = TRUE),
    max_score = max(pain_standardised, na.rm = TRUE),
    missing = sum(is.na(pain_standardised))
  )
# A tibble: 1 × 3
# min_score max_score missing
# <dbl>     <dbl>   <int>
#   0        10    4548

#calculate 1 daily score per pain group
daily_pain <- pain_data |>
  group_by(`User id`, `Day in study`, pain_group) |>
  summarise(
    daily_score = if (all(is.na(pain_standardised))) {
      NA_real_
    } else {
      mean(pain_standardised, na.rm = TRUE)
    },
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = pain_group,
    values_from = daily_score
  )
View(daily_pain)

#Check for duplicate participant-days
daily_pain |>
  count(`User id`, `Day in study`) |>
  filter(n > 1) 
# A tibble: 0 × 3



# MENSTRUAL CYCLE PHASE ---------------------------------------------------
period.columns <- map(
  user.files,
  function(file) {
    data <- read_csv(file, show_col_types = FALSE)
    data |>
      select(contains("period")) |>
      names()
  }
) |>
  unlist()
table(period.columns)


extract_bleeding_data <- function(file) {
  data <- read_csv(file, show_col_types = FALSE)
  
  data |>
    select(
      `User id`,
      `Day in study`,
      `First day of period`,
      `Period (0-no, 1-yes)`,
      `Cycle number`,
      `Cycle day`
    )
}

bleeding_data <- user.files |>
  map(extract_bleeding_data) |>
  list_rbind() |>
  rename(
    first_day_period = `First day of period`,
    bleeding = `Period (0-no, 1-yes)`
  )
View(bleeding_data)

# Method 1: Original cycle-length method
# Calculate cycle length using consecutive period starts.
period_starts <- bleeding_data |>
  filter(`first_day_period` == 1) |>
  select(
    `User id`,
    `Day in study`,
    `Cycle number`
  ) |>
  arrange(`User id`, `Day in study`) |>
  group_by(`User id`) |>
  mutate(
    next_period_start_day = lead(`Day in study`),
    cycle_length = next_period_start_day- `Day in study`
  ) |>
  ungroup()


# Add cycle length to bleeding data using Cycle number
original_method <- bleeding_data |>
  left_join(
    period_starts |>
      filter(!is.na(`Cycle number`)) |>
      select(
        `User id`,
        `Cycle number`,
        cycle_length
      ),
    by = c("User id", "Cycle number")
  ) |>
  mutate(
    original_phase = case_when(
      bleeding == 1 | first_day_period == 1 ~ "MP",
      
      !is.na(cycle_length) &
        `Cycle day` > cycle_length - 5 ~ "PMP",
      
      !is.na(cycle_length) &
        bleeding == 0 ~ "IP",
      
      TRUE ~ NA_character_
    )
  )

View(original_method)

# Method 2: Next observed period method 
# Calculate days until the next recorded period start.
# This method does not depend on Cycle number.
period_groups <- bleeding_data |>
  group_by(`User id`) |>
  arrange(`Day in study`, .by_group = TRUE) |>
  mutate(
    next_period_start_day = if_else(
      first_day_period == 1,
      as.numeric(`Day in study`),
      NA_real_
    )
  ) |>
  fill(next_period_start_day, .direction = "up") |>
  mutate(
    days_to_period = next_period_start_day - `Day in study`,
    phase = case_when(
      bleeding == 1 | first_day_period == 1 ~ "MP",
      days_to_period >= 1 & days_to_period <= 5 ~ "PMP",
      bleeding == 0 & !is.na(days_to_period) ~ "IP",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup()
View(period_groups)

# Compare the two methods
# Check whether they assign the same menstrual phase.
phase_comparison <- original_method |>
  select(
    `User id`,
    `Day in study`,
    `Cycle day`,
    cycle_length,
    original_phase
  ) |>
  left_join(
    period_groups |>
      select(
        `User id`,
        `Day in study`,
        days_to_period,
        phase
      ),
    by = c("User id", "Day in study")
  ) |>
  rename(revised_phase = phase)


# Count the classifications from each method
phase_comparison |>
  count(original_phase, revised_phase)
# A tibble: 6 × 3
# original_phase revised_phase     n
# <chr>          <chr>         <int>
# 1 IP             IP             2674
# 2 MP             MP              996
# 3 PMP            PMP             801
# 4 NA             IP               35
# 5 NA             PMP              19
# 6 NA             NA              587

# Calculate the agreement ie both methods assigned a phase
phase_comparison |>
  filter(
    !is.na(original_phase),
    !is.na(revised_phase)
  ) |>
  summarise(
    days_compared = n(),
    agreement = mean(original_phase == revised_phase) * 100
  )

# Results:
# 4471 days compared, 100% agreement


# Compare the number of unclassified days
phase_comparison |>
  summarise(
    missing_original = sum(is.na(original_phase)),
    missing_revised = sum(is.na(revised_phase))
  )

# Results:
# Original method: 641 unclassified days
# Revised method: 587 unclassified days


# Investigate the 54 additional days classified by Method 2
additional_days <- phase_comparison |>
  filter(
    is.na(original_phase),
    !is.na(revised_phase)
  )

additional_days |>
  count(`User id`, revised_phase)
# A tibble: 7 × 3
# `User id` revised_phase     n
# <dbl> <chr>         <int>
#44 IP               15
#44 PMP               5
# 51 IP                3
#  51 PMP               5
# 80 IP               17
# 80 PMP               5
#  82 PMP               4

additional_days |>
  summarise(
    missing_cycle_length = sum(is.na(cycle_length)),
    missing_cycle_day = sum(is.na(`Cycle day`))
  )

# All 54 additional days had missing cycle_length.
# None had missing Cycle day.


# Check for duplicate participant-days
period_groups |>
  count(`User id`, `Day in study`) |>
  filter(n > 1)
# A tibble: 0 × 3

# Final menstrual-phase summary 
period_groups |>
  count(phase)
# A tibble: 4 × 2
# phase     n
# <chr> <int>
#   1 IP     2709
# 2 MP      996
# 3 PMP     820
# 4 NA      587

# FINAL ANALYSIS DATASET ------------------------------------
# Merge wellbeing, pain and bleeding
# Keep just the columns needed for summaries and models.
analysis_data <- wellbeing_data |>
  select(
    `User id`, `Day in study`, `Cycle number`, `Cycle day`,
    physical_wellbeing, wellbeing_standardised,
    median_wellbeing, wellbeing_score
  ) |>
  left_join(
    daily_pain |>
      select(
        `User id`, `Day in study`,
        abdominal_pain, back_pain, pelvic_pain, general_pain
      ),
    by = c("User id", "Day in study"),
    relationship = "many-to-one"
  ) |>
  left_join(
    period_groups |>
      select(`User id`, `Day in study`, phase),
    by = c("User id", "Day in study"),
    relationship = "many-to-one"
  ) |>
  mutate(phase = factor(phase, levels = c("IP", "PMP", "MP")))

# Check the final dataset.
dim(analysis_data)       
names(analysis_data)
View(analysis_data)

analysis_data |>
  count(`User id`, `Day in study`) |>
  filter(n > 1)
# A tibble: 0 × 3

# DATA ANALYSIS -------------------------------------------------
phase_summary <- analysis_data |>
  filter(!is.na(phase)) |>
  group_by(phase) |>
  summarise(
    days = n(),
    below_usual = sum(wellbeing_score),
    percentage = mean(wellbeing_score) * 100,
    .groups = "drop"
  )

phase_summary
# # A tibble: 3 × 4
# phase  days below_usual percentage
# <fct> <int>       <dbl>      <dbl>
#   1 IP     2699         635       23.5
# 2 PMP     728         241       33.1
# 3 MP      974         484       49.7

pain_summary <- analysis_data |>
  filter(!is.na(phase)) |>
  pivot_longer(
    cols = c(
      abdominal_pain,
      back_pain,
      pelvic_pain,
      general_pain
    ),
    names_to = "pain_type",
    values_to = "pain_score"
  ) |>
  group_by(phase, pain_type) |>
  summarise(
    days = sum(!is.na(pain_score)),
    mean_pain = mean(pain_score, na.rm = TRUE),
    .groups = "drop"
  )

pain_summary
# # A tibble: 12 × 4
# phase pain_type       days mean_pain
# <fct> <chr>          <int>     <dbl>
#   1 IP    abdominal_pain  2495      1.09
# 2 IP    back_pain       2196      2.08
# 3 IP    general_pain    1365      1.50
# 4 IP    pelvic_pain     2047      1.95
# 5 PMP   abdominal_pain   672      1.47
# 6 PMP   back_pain        596      2.33
# 7 PMP   general_pain     389      1.72
# 8 PMP   pelvic_pain      545      2.26
# 9 MP    abdominal_pain   893      2.40
# 10 MP    back_pain        817      3.23
# 11 MP    general_pain     528      2.54
# 12 MP    pelvic_pain      730      2.84

pain_wellbeing <- analysis_data |>
  pivot_longer(
    cols = c(
      abdominal_pain,
      back_pain,
      pelvic_pain,
      general_pain
    ),
    names_to = "pain_type",
    values_to = "pain_score"
  ) |>
  group_by(wellbeing_score, pain_type) |>
  summarise(
    days = sum(!is.na(pain_score)),
    mean_pain = mean(pain_score, na.rm = TRUE),
    .groups = "drop"
  )

pain_wellbeing

# # A tibble: 8 × 4
# wellbeing_score pain_type       days mean_pain
# <dbl> <chr>          <int>     <dbl>
#  0 abdominal_pain  2901      1.15
#  0 back_pain       2638      2.02
#  0 general_pain    1742      1.50
# 0 pelvic_pain     2483      1.82
#  1 abdominal_pain  1317      2.11
#  1 back_pain       1134      3.33
#  1 general_pain     667      2.43
# 1 pelvic_pain      985      3.24


####1. correlation:----------------------------------------------------
#Which pain types are associated with poorer wellbeing?
correlation_matrix <- analysis_data |>
  select(
    pelvic_pain,
    abdominal_pain,
    back_pain,
    general_pain,
    wellbeing_standardised
  ) |>
  cor(use = "pairwise.complete.obs")

round(correlation_matrix, 2)

#                         pelvic_pain abdominal_pain back_pain general_pain wellbeing_standardised
# pelvic_pain                   1.00           0.42      0.61         0.53                  -0.40
# abdominal_pain                0.42           1.00      0.39         0.35                  -0.39
# back_pain                     0.61           0.39      1.00         0.38                  -0.30
# general_pain                  0.53           0.35      0.38         1.00                  -0.28
# wellbeing_standardised       -0.40          -0.39     -0.30        -0.28                   1.00


# Convert correlation matrix into a dataframe
cor_df <- as.data.frame(as.table(correlation_matrix))

# Create heatmap
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 4) +
  scale_fill_gradient2(
    low = "#C44E52",
    mid = "white",
    high = "#4C78A8",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  coord_equal() +
  labs(
    title = "Correlations between pain 
    and physical wellbeing",
    x = NULL,
    y = NULL,
    fill = "Pearson r"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )

#####2. linear regression ----------------------------------------------------
#model for phase vs pain
model_data <- analysis_data |>
  filter(
    !is.na(wellbeing_standardised),
    !is.na(phase),
    !is.na(pelvic_pain),
    !is.na(abdominal_pain),
    !is.na(back_pain),
    !is.na(general_pain)
  )
nrow(model_data)  # Check how many days are available for model comparison.
#[1] 1864

#a How is wellbeing associated with menstrual-cycle phase?
model_phase <- lm(wellbeing_standardised ~ phase,data = model_data)

#b. How much variation in wellbeing is explained by pain, and does menstrual phase add information?
# Pain and physical wellbeing
model_pain <- lm( wellbeing_standardised ~ pelvic_pain +abdominal_pain + back_pain + general_pain, data = model_data)

# c Add menstrual phase
model_pain_phase <- lm(wellbeing_standardised ~ phase + pelvic_pain + abdominal_pain + back_pain + general_pain, data = model_data)

summary(model_phase)
summary(model_pain)
summary(model_pain_phase)

#Does including menstrual phase improve fit?
anova(model_phase, model_pain, model_pain_phase)

model_comparison <- tibble(
  model = c("Phase only","Pain only", "Pain + phase"),
  adjusted_R2 = c(
    summary(model_phase)$adj.r.squared,
    summary(model_pain)$adj.r.squared,
    summary(model_pain_phase)$adj.r.squared
  ),
  RMSE = c(
    sqrt(mean(residuals(model_phase)^2)),
    sqrt(mean(residuals(model_pain)^2)),
    sqrt(mean(residuals(model_pain_phase)^2))
  )
)

model_comparison
# A tibble: 3 × 3
# model        adjusted_R2  RMSE
# <chr>              <dbl> <dbl>
#   1 Phase only        0.0727  1.92
# 2 Pain only         0.149   1.84
# 3 Pain + phase      0.170   1.82

#check for colinearity
library(car)
#not needed for model_phase
vif(model_pain)
vif(model_pain_phase)

#3 exploratory lag analysis ----------------------------------------------------

# Create previous-day pelvic pain variable
lag_data <- analysis_data |>
  arrange(`User id`, `Day in study`) |>
  group_by(`User id`) |>
  mutate(
    pelvic_pain_lag1 = if_else(
      `Day in study` - lag(`Day in study`) == 1,
      lag(pelvic_pain),
      NA_real_
    ),
    wellbeing_lag1 = if_else(
      `Day in study` - lag(`Day in study`) == 1,
      lag(wellbeing_standardised),
      NA_real_
    )
  ) |>
  ungroup()

# Keep complete observations for the comparison
lag_model_data <- lag_data |>
  filter(
    !is.na(wellbeing_standardised),
    !is.na(wellbeing_lag1),
    !is.na(pelvic_pain_lag1),
    !is.na(pelvic_pain),
    !is.na(phase)
  )
nrow(lag_model_data)
#3122

# Model without yesterday's pain
model_today <- lm(
  wellbeing_standardised ~ pelvic_pain + wellbeing_lag1 + phase,
  data = lag_model_data
)

summary(model_today)

# Model including yesterday's pain
model_yesterday <- lm(
  wellbeing_standardised ~ pelvic_pain +
    pelvic_pain_lag1 +  wellbeing_lag1+ phase,
  data = lag_model_data
)

summary(model_yesterday)
# Does yesterday's pain improve the model?
anova(model_today, model_yesterday)

#did not significantly improve model fit

# Model without previous-day wellbeing
model_no_lag_wellbeing <- lm(
  wellbeing_standardised ~ pelvic_pain +
    pelvic_pain_lag1 + phase,
  data = lag_model_data
)

# Compare with full model
anova(model_no_lag_wellbeing, model_yesterday)

# Compare adjusted R²
summary(model_no_lag_wellbeing)$adj.r.squared
summary(model_yesterday)$adj.r.squared



# GRAPHS ----------------------------------------------------------

phase_colours <- c(
  "IP" = "#4C78A8",
  "PMP" = "#E69F00",
  "MP" = "#C44E52"
)

phase_labels <- c(
  "PMP" = "Premenstrual",
  "MP" = "Menstrual",
  "IP" = "Intermenstrual"
)

theme_project <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, colour = "grey35"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

#fig1 = group physical wellbeing over cycle phases
fig1 <- ggplot(
  phase_summary,
  aes(x = phase, y = percentage, fill = phase)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    vjust = -0.5,
    size = 4.5
  ) +
  scale_fill_manual(values = phase_colours) +
  scale_x_discrete(labels = phase_labels) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Percentage of days below each participant's median physical wellbeing",
    #subtitle = "",
    x = NULL,
    y = "Days below usual wellbeing (%)",
    caption = "n = 4,401 classified days"
  ) +
  theme_project +
  theme(legend.position = "none")

fig1

#fig2 = group pain severity over cycle phases
fig2 <- ggplot(
  pain_summary,
  aes(
    x = phase,
    y = mean_pain,
    colour = pain_type,
  )
) +
  geom_point(size = 3 ) +
  scale_x_discrete(labels = phase_labels) +
  scale_colour_brewer(
    palette = "Dark2",
    labels = c(
      abdominal_pain = "Abdominal pain",
      back_pain = "Back pain",
      general_pain = "General pain",
      pelvic_pain = "Pelvic pain"
    )
  ) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(
    title = "Mean standardised pain scores by menstrual phase",
   # subtitle = "",
    x = NULL,
    y = "Mean pain score (0–10)",
    colour = "Pain type"
  ) +
  theme_project

fig2


##fig3 = relationship between wellbeing & pain
# Convert the four pain types into one column
fig3_data <- analysis_data |>
  select(
    wellbeing_standardised,
    pelvic_pain,
    abdominal_pain,
    back_pain,
    general_pain
  ) |>
  pivot_longer(
    cols = c(
      pelvic_pain,
      abdominal_pain,
      back_pain,
      general_pain
    ),
    names_to = "pain_type",
    values_to = "pain_score"
  ) |>
  mutate(
    pain_type = dplyr::recode(
      pain_type,
      pelvic_pain = "Pelvic",
      abdominal_pain = "Abdominal",
      back_pain = "Back",
      general_pain = "General"
    )
  ) |>
  filter(
    !is.na(wellbeing_standardised),
    !is.na(pain_score)
  )


fig3 <- ggplot(
  fig3_data,
  aes(x = pain_score,
      y = wellbeing_standardised,
      colour = pain_type)
) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Pain severity is negatively associated with physical wellbeing",
    x = "Pain severity (0–10)",
    y = "Physical wellbeing (0–10)",
    colour = "Pain type"
  ) +
  theme_project

fig3

#fig 4 = linear regression model visualisation
fig4_data <- model_comparison |>
  mutate(
    explained_percent = adjusted_R2 * 100,
    model = factor(model, levels = model)
  )

fig4 <- ggplot(
  fig4_data,
  aes(x = model, y = explained_percent)
) +
  geom_col(fill = "#4C78A8", width = 0.6) +
  geom_text(
    aes(label = paste0(round(explained_percent, 1), "%")),
    vjust = -0.5,
    size = 4.5
  ) +
  scale_y_continuous(
    limits = c(0, 22),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Pain and menstrual phase jointly explain wellbeing variation",
    subtitle = "Adjusted R² from three linear regression models",
    x = NULL,
    y = "Explained variation in wellbeing",
    caption = "All models fitted to the same 1,864 complete participant-days"
  ) +
  theme_project

fig4

# fig_lag = Compare models with and without yesterday's pain & wellbeing
lag_comparison <- tibble(
  comparison = c(
    "Previous-day wellbeing",
    "Previous-day wellbeing",
    "Previous-day pelvic pain",
    "Previous-day pelvic pain"
  ),
  model = c(
    "Without", "With",
    "Without", "With"
  ),
  adjusted_R2 = c(
    summary(model_no_lag_wellbeing)$adj.r.squared,
    summary(model_yesterday)$adj.r.squared,
    summary(model_today)$adj.r.squared,
    summary(model_yesterday)$adj.r.squared
  )
) |>
  mutate(
    explained_percent = adjusted_R2 * 100,
    model = factor(model, levels = c("Without", "With"))
  )

fig_lag <- ggplot(
  lag_comparison,
  aes(x = model, y = explained_percent, fill = model)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(explained_percent, 1), "%")),
    vjust = -0.5,
    size = 4.5
  ) +
  facet_wrap(~ comparison, nrow = 1) +
  scale_fill_manual(
    values = c("Without" = "#A6B8C8",
               "With" = "#4C78A8")
  ) +
  scale_y_continuous(
    limits = c(0, 52),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Contribution of previous-day wellbeing and pelvic pain",
    subtitle = "Adjusted R² from linear regression models",
    x = NULL,
    y = "Variation in wellbeing explained",
    caption = "n = 3,122 participant-days; both comparisons p < 0.001"
  ) +
  theme_project +
  theme(
    legend.position = "none"
  )

fig_lag

#fig 5 & 6 ->check if wellbeing is normally distributed
fig_5 <- ggplot(analysis_data, aes(x = wellbeing_standardised)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "#4C78A8",
    colour = "white"
  ) +
  labs(
    title = "Distribution of physical wellbeing",
    subtitle = "Standardised Wellbeing Scores",
    x = "Physical wellbeing (0–10)",
    y = "Number of observations"
  ) +
  theme_project

fig_6 <- ggplot(analysis_data, aes(sample = wellbeing_standardised)) +
  stat_qq() +
  stat_qq_line(colour = "#C44E52") +
  labs(title = "Q-Q plot of physical wellbeing") +
  theme_project
fig_6
#check if regression is normally distributed 
plot(model_pain_phase, which = 2)

#fig_cycle = individual wellbeing over cycle phases
fig_cycle_data <- analysis_data |>
  filter(!is.na(phase)) |>
  group_by(`User id`, phase) |>
  summarise(
    mean_wellbeing = mean(wellbeing_standardised, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.nan(mean_wellbeing))

fig_cycle <- ggplot(
  fig_cycle_data,
  aes(
    x = phase,
    y = mean_wellbeing,
    colour = factor(`User id`),
    group = `User id`
  )
) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_discrete(labels = phase_labels)+
  labs(
    title = "Average wellbeing across menstrual phases",
    subtitle = "Each colour represents a participant",
    x = "Menstrual phase",
    y = "Mean physical wellbeing (0–10)",
    colour = "Participant"
  ) +
  theme_project +
  theme(legend.position = "none")

fig_cycle

#fig_pain_data = individual pain severity over cycle phases
fig_pain_data <- analysis_data |>
  filter(!is.na(phase)) |>
  group_by(`User id`, phase) |>
  summarise(
    pelvic_pain = mean(pelvic_pain, na.rm = TRUE),
    abdominal_pain = mean(abdominal_pain, na.rm = TRUE),
    back_pain = mean(back_pain, na.rm = TRUE),
    general_pain = mean(general_pain, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(pelvic_pain, abdominal_pain, back_pain, general_pain),
    names_to = "pain_type",
    values_to = "mean_pain"
  ) |>
  filter(!is.nan(mean_pain)) |>
  mutate(
    pain_type = dplyr::recode(
      pain_type,
      pelvic_pain = "Pelvic pain",
      abdominal_pain = "Abdominal pain",
      back_pain = "Back pain",
      general_pain = "General pain"
    )
  )

fig_pain <- ggplot(
  fig_pain_data,
  aes(
    x = phase,
    y = mean_pain,
    colour = factor(`User id`),
    group = `User id`
  )
) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_discrete(labels = phase_labels)+
  facet_wrap(~pain_type, ncol = 2) +
  labs(
    title = "Average pain across menstrual phases",
    subtitle = "Each colour represents a participant",
    x = "Menstrual phase",
    y = "Mean pain severity (0–10)",
    colour = "Participant"
  ) +
  theme_project +
  theme(legend.position = "none")

fig_pain


