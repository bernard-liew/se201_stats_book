# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # All purpose wrangling for dataframes
               openxlsx) # moving average for vo2

# Import excel data

dat_fms <-  read.xlsx (xlsxFile = "data/groupFMS.xlsx")

bad_leg_accl <-  read.xlsx (xlsxFile = "data/rightleg_imu.xlsx")

good_leg_accl<-  read.xlsx (xlsxFile = "data/leftleg_imu.xlsx")