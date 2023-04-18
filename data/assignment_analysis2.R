

## -----------------------------------------------------------------------------

## load up the packages we will need

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # All purpose wrangling for dataframes
               openxlsx) # writing excel documents

## Import data (Task 1)

# Import excel data

## This is the FMS results of the team

dat_fms <-  read.xlsx (xlsxFile = "XXX")

## Accelerometry findings of the bad leg from a drop land task

bad_leg_accl <-  read.xlsx (xlsxFile = "XXX")

## Accelerometry findings of the good leg from a drop land task

good_leg_accl<-  read.xlsx (xlsxFile = "XXX")

## Make the group FMS wide to long (Task 2) ------------------------------------

# Make data wide to long

dat_long <- dat_fms %>% # original data
  pivot_longer(cols = -id,
               names_to = "XXX",
               values_to = "XXX")

# Split task column

dat_long <- dat_long %>%
  mutate (
    side = case_when(
      str_detect(task, "R_") ~ "right",
      str_detect(task, "L_") ~ "left",
      TRUE ~ "central"
    )) %>%
  mutate (task = str_remove_all(task, "R_|L_"))

## Count number of athletes per task subscore (Task 3) -------------------------

# Count

fms_count <- dat_long %>%
  group_by(XXX, XXX) %>%
  summarise (score = XXX (score)) %>%
  group_by (task, score) %>%
  summarise (count = XXX())

## Plot the group FMS count data (Task 4) --------------------------------------

# Make factor

fms_count <- fms_count %>%
  mutate (score = factor (score, levels = c("0", "1", "2", "3")))

############# ********** Code 1 (start)********** ##############################

## Group barplot

ggplot(fms_count) + 
  geom_col(aes(x = XXX, y = XXX, fill = XXX), position = "dodge")+ 
  scale_fill_discrete(drop=FALSE)

############# ********** Code 1 (end)********** ################################

## Summary statistics of the total FMS score for the team (Task 5) -------------

############# ********** Code 2 (start)********** ##############################

# Total

fms_total <- dat_long %>%
  group_by(id, task) %>%
  summarise (score = XXX (score)) %>%
  group_by (id) %>%
  summarise (Total = XXX (score))

# summary

fms_summary <- fms_total %>%
  summarise (Mean = XXX (Total),
             Sd = XXX (Total))

############# ********** Code 2 (end)********** ################################

## Filter out athlete a data (Task 6) ------------------------------------------

athlete <- dat_fms %>%
  filter (XXX == "XXX")

## Make the athlete FMS wide to long (Task 7) ----------------------------------

athlete_long <- athlete %>% # original data
  pivot_longer(cols = -id,
               names_to = "XXX",
               values_to = "XXX")

# Split task column

athlete_c_long <- athlete_c_long %>%
  mutate (
    side = case_when(
      str_detect(task, "R_") ~ "right",
      str_detect(task, "L_") ~ "left",
      TRUE ~ "central"
    )) %>%
  mutate (task = str_remove_all(task, "R_|L_"))

## Plot the individual athlete's FMS data (Task 8) -----------------------------

############# ********** Code 3 (start)********** ##############################

ggplot(athlete_long) + 
  geom_col(aes(x = XXX, y = XXX, fill = XXX), position = "dodge")

############# ********** Code 3(end)********** ################################

## Analyze drop jump data (Task 9) ---------------------------------------------

############# ********** Code 4 (start)********** ##############################

# Maximal bad leg impact value
max_bad_ampl <- max(bad_imu %>% select (XXX))

# Maximal good leg impact value
max_good_ampl <- max(good_imu %>% select (XXX))

# Symmetry index 

symmmetry_index <- 
  ((2* (max_bad_ampl - max_good_ampl))/(max_bad_ampl + max_good_ampl)) * 100


############# ********** Code 4 (end)********** ################################

