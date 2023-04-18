# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # All purpose wrangling for dataframes
               openxlsx) # moving average for vo2

# Import excel data

dat_fms <-  read.xlsx (xlsxFile = "data/groupFMS.xlsx")

bad_leg_accl <-  read.xlsx (xlsxFile = "data/rightleg_imu.xlsx")

good_leg_accl<-  read.xlsx (xlsxFile = "data/leftleg_imu.xlsx")

# Make data wide to long

dat_long <- dat_fms %>% # original data
  pivot_longer(cols = -id,
               names_to = "task",
               values_to = "score")

# Split task column

dat_long <- dat_long %>%
  mutate (
    side = case_when(
      str_detect(task, "R_") ~ "right",
      str_detect(task, "L_") ~ "left",
      TRUE ~ "central"
    )) %>%
  mutate (task = str_remove_all(task, "R_|L_"))

# Count

fms_count <- dat_long %>%
  group_by(id, task) %>%
  summarise (score = min (score)) %>%
  group_by (task, score) %>%
  summarise (count = n())

# Total

fms_total <- dat_long %>%
  group_by(id, task) %>%
  summarise (score = min (score)) %>%
  group_by (id) %>%
  summarise (Total = sum (score))

# summary

fms_summary <- fms_total %>%
  summarise (Mean = mean (Total),
             Sd = sd (Total))

# Select athlete

athlete_c <- dat_fms %>%
  filter (id == "athlete_c")

# Make athlete c data long

athlete_c_long <- athlete_c %>% # original data
  pivot_longer(cols = -id,
               names_to = "task",
               values_to = "score")

# Split task column

athlete_c_long <- athlete_c_long %>%
  mutate (
    side = case_when(
      str_detect(task, "R_") ~ "right",
      str_detect(task, "L_") ~ "left",
      TRUE ~ "central"
    )) %>%
  mutate (task = str_remove_all(task, "R_|L_"))


## Athlete c Barplot

ggplot(athlete_c_long) + 
  geom_col(aes(x = task, y = score, fill = side), position = "dodge")

# Make factor

fms_count <- fms_count %>%
  mutate (score = factor (score, levels = c("0", "1", "2", "3")))

## Group barplot

ggplot(fms_count) + 
  geom_col(aes(x = task, y = count, fill = score), position = "dodge")+ 
  scale_fill_discrete(drop=FALSE)
