# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # All purpose wrangling for dataframes
               openxlsx,# writing excel documents
               lubridate,# date-time 
               tibbletime) # moving average for vo2

# Import excel data

dat <-  read.xlsx (xlsxFile = "data/Athlete_1_treadmill.xlsx",
                    sheet = "raw")

dat_stage <-  read.xlsx (xlsxFile = "data/Athlete_1_treadmill.xlsx",
                   sheet = "stage")

dat_fms <-  read.xlsx (xlsxFile = "data/Athlete_1_FMS.xlsx",
                   sheet = "Sheet1")

dat_fms_grp <-  read.xlsx (xlsxFile = "data/simFMS.xlsx",
                   sheet = "FMS")

# Tidy data

## Rename columns
names (dat) <- c("time", "bf", "vo2_norm", "rer", "vo2", "vco2", "ve", "hr")

## Slice off first row

dat <- dat%>% 
  slice (-1) 

## Make numeric and time

dat <- dat %>%
  mutate (bf = as.numeric(bf),
          vo2_norm = as.numeric(vo2_norm),
          rer = as.numeric(rer),
          vo2 = as.numeric(vo2),
          vco2 = as.numeric(vco2),
          ve = as.numeric(ve),
          hr = as.numeric(hr)) %>%
  mutate (time = time %>% 
            str_squish() %>% # function strips any whitespaces
            ms() %>% # convert to minutes and seconds
            as.period(unit = "sec") %>% # converts entirely to seconds
            as.numeric ()) # strips the S symbol to make it a number

## Create variable stage

dat <- dat%>%
  mutate (stage = cut_interval(time, length = 210, labels = FALSE))

## Create variable of row numbers

dat <- dat %>%
  group_by(stage) %>%
  mutate (row_id = row_number())

## Remove last 30s data per stage

dat <- dat %>%
  group_by(stage) %>% # for each group
  filter (row_id < 37) 

## keep last 30s per stage

dat <- dat %>%
  group_by(stage) %>% # for each group
  slice_tail (n = 6)

## Summarize each column
dat <- dat %>%
  group_by(stage) %>% # for each group
  summarise_at (vars(bf:hr), mean)

## Export clean vo2 data

write.xlsx(x = dat,
           file = "data/Athlete_1_treadmill_clean.xlsx")