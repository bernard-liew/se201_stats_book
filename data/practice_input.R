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