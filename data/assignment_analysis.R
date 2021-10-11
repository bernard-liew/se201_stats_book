
## ---------------------------
##
##
## Author: Put your name
##
## Date Created: Put the date
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## ---------------------------

## load up the packages we will need

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # All purpose wrangling for dataframes
               lubridate, # Time
               tibbletime,
               openxlsx) # writing excel documents

## Custom function to get interection between two lines
## To get Lactate and Anaerobic Threshold values
## Ignore the complexity, highlight between Start and End, Run --------------
## Start ----------------
curve_intersect <- function (curve1, curve2, empirical = TRUE, domain = NULL) 
{
  if (!empirical & missing(domain)) {
    stop("'domain' must be provided with non-empirical curves")
  }
  if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
    stop("'domain' must be a two-value numeric vector, like c(0, 10)")
  }
  if (empirical) {
    curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
    curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
    point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), 
                       c(min(curve1$x), max(curve1$x)))$root
    point_y <- curve2_f(point_x)
  }
  else {
    point_x <- uniroot(function(x) curve1(x) - curve2(x), 
                       domain)$root
    point_y <- curve2(point_x)
  }
  return(list(x = point_x, y = point_y))
}
## End ----------------

## Import data (Task 1)

### Import the sheet with the Vo2 data
dat_vo2 <-  read.xlsx (xlsxFile = "data/XXX.xlsx",
                       sheet = "XXX")

### Import the sheet with the lactate data
dat_stage <-  read.xlsx (xlsxFile = "data/XXX.xlsx",
                         sheet = "XXX")

### Import group FMS data
dat_fms_grp <-  read.xlsx (xlsxFile = "data/XXX.xlsx",
                           sheet = "XXX")

### Import individual FMS data
dat_fms_indv <-  read.xlsx (xlsxFile = "data/XXX.xlsx",
                            sheet = "XXX")

## Analyze FMS data ------------------------------------------------------------

################################ Group FMS #####################################

### Number of athletes scoring a level in FMS (Task 2)

dat_fms_grp <- dat_fms_grp %>% # original data
  pivot_longer(cols = -id,
               names_to = "task",
               values_to = "score") %>%
  group_by(XXX, XXX) %>%
  summarize (count = n()) %>%
  mutate (count = factor (count),
          score = factor (score, levels = c("0", "1", "2", "3")))

### Plot group FMS (Task 3)

plot_fms_grp <- ggplot (XXX) +
  geom_col(aes(x = XXX, y = XXX, fill = XXX), position = "dodge") +
  scale_fill_discrete(drop=FALSE) +
  scale_x_discrete(drop=FALSE)

ggsave(filename = "grp_fms.png", 
       plot = plot_fms_grp , # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)

############################# Individual FMS ###################################

### Plot group FMS (Task 4)

plot_fms_indv <- ggplot(XXX) +
  geom_col(aes(x = XXX, y = XXX, fill = XXX), position = "dodge")

ggsave(filename = "ind_fms.png", 
       plot = plot_fms_indv, # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)


## Analyze VO2 data ------------------------------------------------------------

### Rename column names of Vo2 data (Task 5)

new_names <- c()

colnames (dat_vo2)  <- new_names

### Remove first row of Vo2 data

dat_vo2 <- dat_vo2 %>% 
  slice (-c(1))

### Convert column type of Vo2 data (Task 6)

dat_vo2 <-  dat_vo2 %>%
  mutate (bf = XXX (bf),
          vo2_norm = XXX(vo2_norm),
          rer = XXX(rer),
          vo2 = XXX(vo2),
          vco2 = XXX(vco2),
          ve = XXX(ve),
          hr = XXX(hr)) %>%
  # Convert time to seconds
  mutate (time = time %>% 
            str_squish() %>% 
            ms() %>% 
            as.period(unit = "sec") %>% 
            as.numeric ()) 

### Create a "stage" variable in Vo2 data

dat_vo2 <- dat_vo2 %>%
  mutate (stage = cut_interval(time, length = 210, labels = FALSE))

### Calculate average 30s data per stage of Vo2 data (Task 7)

dat_vo2_summ <- dat_vo2 %>%
  group_by(XXX) %>% # for each group
  mutate (row_id = row_number()) %>%
  filter (row_id < XXX) %>% # throw away all data between 3 to 3:30 min
  slice_tail (n = XXX)%>% # keep last 30 sec per stage
  summarise (bf = mean (bf),
             vo2_norm = mean (vo2_norm),
             rer = mean (rer),
             vo2 = mean (vo2),
             vco2 = mean (vco2),
             ve = mean (ve),
             hr = mean (hr))


### Combine Vo2 staged data with lactate data (Task 8)

dat_vo2_comb <- dat_vo2_summ %>%
  inner_join(dat_stage, by = "XXX")

### Plot

#### Lactate (Task 9)

f <- ggplot (dat_vo2_comb) +
  geom_line (aes (x = XXX, y = XXX), colour = "XXX", size = 1.5) + 
  labs (x = "Speed (km/h)",
        y = "Lactate (mmol)") +
  theme_bw() + 
  labs (title = "XXX") + 
  theme(axis.text.x = element_text(size = XXX),
        axis.text.y = element_text(size = XXX),  
        axis.title.x = element_text(size = XXX),
        axis.title.y = element_text(size = XXX))

ggsave(filename = "lactate.png", 
       plot = f, # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)

#### Heart rate (Task 10)

f <- ggplot (dat_vo2_comb) +
  geom_line (aes (x = XXX, y = XXX), colour = "XXX", size = 1.5) + 
  labs (x = "Speed (km/h)",
        y = "Heart Rate (bpm)") +
  theme_bw() + 
  labs (title = "XXX") + 
  theme(axis.text.x = element_text(size = XXX),
        axis.text.y = element_text(size = XXX),  
        axis.title.x = element_text(size = XXX),
        axis.title.y = element_text(size = XXX))

ggsave(filename = "heartrate.png", 
       plot = f, # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)

#### Export table (Task 11)

write.xlsx(x = XXX,
           sheetName = "vo2",
           file = "data/vo2_table.xlsx")

## Bonus Codes to help you -------------------------------------------------
### Click on everything below and run


### Get VO2 max , VO2 max relative

rolling_mean6 <- rollify(mean, window = 6)

raw_roll <- dat_vo2 %>%
  arrange (desc (time)) %>%
  mutate_at (vars(bf:hr), rolling_mean6) %>%
  na.omit() %>%
  summarise_at(vars(bf:hr), max, na.rm = TRUE) %>%
  mutate (vo2 = vo2/1000)

cat ("The relative peak O2 uptake is:", raw_roll$vo2_norm, "(ml/kg/min)")
cat ("The absolute peak O2 uptake is:", raw_roll$vo2, "(L/min)")

### Get max aerobic speed

#### Get maximal stage completed number
max_stage <- max (dat_vo2$stage)

#### Find number of rows in final stage
dat_vo2_last <- dat_vo2 %>%
  filter (stage == max_stage)

#### Calculate based on rows the proportion of stage completed
last_stage_prop <- nrow (dat_vo2_last)/42

#### Get the speed increment, which should be even across all stage
increment <- mean (diff(dat_vo2_comb$speed, lag = 1))

#### Get the maximal aerobic speed
ans <-  last_stage_prop * increment
max_aerobic_speed <- ans + max (dat_vo2_comb$speed)

cat ("The maximal aerobic speed:", max_aerobic_speed,  "(Km/h)")


### Get Lactate and Anaerobic Thresold

m <- loess (lactate ~ speed, data = dat_vo2_comb)
min.speed <- ceiling (min (dat_vo2_comb$speed))
max.speed <- floor (max (dat_vo2_comb$speed))
n_points <- 100

new_lac <- data.frame (speed = seq (min.speed, max.speed, length.out = n_points))
new_lac$y <- predict (m, newdata = new_lac)
colnames(new_lac)[1] <- "x"

#### Threshold values

lactate_thres <- data.frame (x = seq (min.speed, max.speed, length.out = n_points),
                             y = 2)

anaerobic_thres <- data.frame (x = seq (min.speed, max.speed, length.out = n_points),
                               y = 4)
#### Speed at thresold

speed_at_lac_thres <- curve_intersect(new_lac, lactate_thres)$x
speed_at_ane_thres <- curve_intersect(new_lac, anaerobic_thres)$x

### Get HR at thresholds

m <- loess (hr ~ speed, data = dat_vo2_comb)

new_hr <- data.frame (speed = seq (min.speed, max.speed, length.out = n_points))
new_hr$y <- predict (m, newdata = new_hr)
colnames(new_hr)[1] <- "x"


hr_at_lac_thres <- new_hr[which.min(abs(new_hr$x -speed_at_lac_thres)), "y"] %>%
  round (0)
hr_at_ane_thres <- new_hr[which.min(abs(new_hr$x -speed_at_ane_thres)), "y"] %>%
  round 

pi <- data.frame(Variable = c("Lactate Threshold", "Anaerobic Threshold"),
                 Speed = c(speed_at_lac_thres, speed_at_ane_thres ),
                 HR = c(hr_at_lac_thres, hr_at_ane_thres))

cat ("The speed (km/h) and heart rate (b/min) at lactate threshold are:", pi[1,2], "and", pi[1,3])
cat ("The speed (km/h) and heart rate (b/min) at anaerobic threshold are:", pi[2,2], "and", pi[2,3])

df_plot <- data.frame(speed = new_lac$x,
                      lactate = new_lac$y,
                      hr = new_hr$y,
                      speed_lac = speed_at_lac_thres,
                      speed_ane = speed_at_ane_thres) %>%
  pivot_longer(cols = lactate:hr,
               names_to = "var",
               values_to = "val")

ggplot (df_plot) +
  geom_line (aes (x = speed, y = val)) + 
  geom_vline(xintercept = speed_at_lac_thres, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = speed_at_ane_thres, color = "red", linetype = "dashed") + 
  facet_wrap(~var, ncol = 2, scales = "free") +
  labs (x = "Speed",
        y = "Values") +
  theme_bw() + 
  labs (title = "Plot of Treadmill test") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))