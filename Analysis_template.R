## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Bernard Liew
##
## Date Created: 2020-09-02
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

## Import data

dat_vo2 <-  read.xlsx (xlsxFile = "data/Athlete 1 (Male) Treadmill Test Raw Data.xlsx",
                       sheet = "raw")

dat_stage <-  read.xlsx (xlsxFile = "data/Athlete 1 (Male) Treadmill Test Raw Data.xlsx",
                       sheet = "stage")

## Analyze VO2 data ----------------------------------------------------------------------

### Rename column names

new_names <- c("time", "bf", "vo2_norm", "rer", "vo2", "vco2", "ve", "hr")

colnames (dat_vo2)  <- new_names

### Remove first row 

dat_vo2 <- dat_vo2 %>% 
  slice (-(1))

### Convert column type

dat_vo2 <-  dat_vo2 %>%
  mutate (bf = as.numeric(bf),
          vo2_norm = as.numeric(vo2_norm),
          rer = as.numeric(rer),
          vo2 = as.numeric(vo2),
          vco2 = as.numeric(vco2),
          ve = as.numeric(ve),
          hr = as.numeric(hr)) %>%
  # Convert time to seconds
  mutate (time = time %>% 
            str_squish() %>% 
            ms() %>% 
            as.period(unit = "sec") %>% 
            as.numeric ()) 

### Create a "stage" variable

dat_vo2 <- dat_vo2 %>%
  mutate (stage = cut_interval(time, length = 210, labels = FALSE))

### Calculate average 30s data per stage

dat_vo2_summ <- dat_vo2 %>%
  group_by(stage) %>% # for each group
  slice(31:36) %>% # keep time 2:30 - 3min per stage
  summarise_at (vars(bf:hr), mean)

### Combine stage and average data

dat_vo2_comb <- dat_vo2_summ %>%
  inner_join(dat_stage, by = "stage")

### Plot

#### Lactate

f <- ggplot (dat_vo2_comb) +
  geom_line (aes (x = speed, y = lactate), colour = "red", size = 1.5) + 
  labs (x = "Speed (km/h)",
        y = "Lactate (mmol)") +
  theme_bw() + 
  labs (title = "Plot of Treadmill test") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggsave(filename = "myplot.png", 
       plot = f, # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)

#### Heart rate

f <- ggplot (dat_vo2_comb) +
  geom_line (aes (x = speed, y = lactate), colour = "red", size = 1.5) + 
  labs (x = "Speed (km/h)",
        y = "Lactate (mmol)") +
  theme_bw() + 
  labs (title = "Plot of Treadmill test") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggsave(filename = "myplot.png", 
       plot = f, # the name of the image object you created above.
       width = 8, 
       height = 8, 
       unit = "cm", 
       dpi = 300)

## Bonus Codes to help you

### Get VO2 max , VO2 max relative

rolling_mean6 <- rollify(mean, window = 6)

raw_roll <- dat_vo2 %>%
  arrange (desc (time)) %>%
  summarise_at (vars(bf:hr), rolling_mean6) %>%
  summarise_at(vars(bf:hr), max, na.rm = TRUE)

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

pi

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
