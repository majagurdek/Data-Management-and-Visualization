library(tidyverse)
setwd("~/Desktop/DV project")
library(readr)
library(dplyr)
library(janitor)
pak::pak("hadley/genzplyr")
install.packages("genzplyr")
install.packages("pak")
pak::pak("hadley/genzplyr")
library(genzplyr)
rides <- read_csv("Rides_Data.csv")
drivers <- read_csv("Drivers_Data.csv")
glimpse2(rides)
glimpse(rides)
skim(rides)
library(skimr)
skimr(rides)
skim(rides)
View(drivers)
View(rides)
View(drivers)
View(rides)
skim(drivers)
View(rides)
rides <- rides %>% lowkey(
ride_id        = Ride_ID,
driver_id      = Driver_ID,
city           = City,
date           = Date,
promo_code     = Promo_Code,
distance_km    = Distance_km,
duration_min   = Duration_Min,
fare           = Fare,
rating         = Rating
)
rides <- rides %>% lowkey(
ride_id        = Ride_ID,
driver_id      = Driver_ID,
city           = City,
date           = Date,
promo_code     = Promo_Code,
distance_km    = Distance_km,
duration_min   = Duration_min,
fare           = Fare,
rating         = Rating
)
View(drivers)
drivers <- drivers %>% lowkey(
driver_id        = Driver_ID,
name             = Name,
city             = City,
active_status    = Active_Status,
age              = Age,
experience_years = Experience_Years,
average_rating   = Average_Rating
)
rides %>% no_cap(across(where(is.numeric)))
rides %>%
no_cap(
avg_distance = mean(distance_km),
avg_duration = mean(duration_min),
avg_fare     = mean(fare),
avg_rating   = mean(rating),
min_fare     = min(fare),
max_fare     = max(fare),
)
rides %>%
no_cap(
avg_distance = mean(distance_km),
avg_duration = mean(duration_min),
avg_fare     = mean(fare),
avg_rating   = mean(rating),
min_fare     = min(fare),
max_fare     = max(fare),
min_rating   = min(rating),
max_rating   = max(rating),
)
rides %>%
squad_up(city) %>%
no_cap(
+         avg_distance = mean(distance_km),
rides %>%
squad_up(driver_id) %>%
no_cap(
avg_distance = mean(distance_km),
avg_duration = mean(duration_min),
avg_fare     = mean(fare),
avg_rating   = mean(rating),
min_fare     = min(fare),
max_fare     = max(fare),
min_rating   = min(rating),
max_rating   = max(rating),
) %>%
slay(desc(rides_count))
rides %>%
squad_up(driver_id) %>%
no_cap(
avg_distance = mean(distance_km),
avg_duration = mean(duration_min),
avg_fare     = mean(fare),
avg_rating   = mean(rating),
min_fare     = min(fare),
max_fare     = max(fare),
min_rating   = min(rating),
max_rating   = max(rating),
) %>%
slay(desc(rides_count))
rides %>%
squad_up(driver_id) %>%
no_cap(
rides_count  = n(),
avg_distance = mean(distance_km),
avg_duration = mean(duration_min),
avg_fare     = mean(fare),
avg_rating   = mean(rating),
min_fare     = min(fare),
max_fare     = max(fare),
min_rating   = min(rating),
max_rating   = max(rating),
) %>%
slay(desc(rides_count))
rides %>% its_giving(city)
View(drivers)
drivers %>%
+     no_cap(
+         avg_age = mean(age),
drivers %>%
no_cap(
avg_age = mean(age),
avg_experience = mean(experience_years),
avg_rating   = mean(rating),
min_rating   = min(rating),
max_rating   = max(rating),
)
drivers %>%
no_cap(
avg_age = mean(age),
avg_experience = mean(experience_years),
min_experience = min(experience_years),
max_experience = max(experience_years),
)
drivers %>% its_giving(city)
drivers %>% its_giving(active)
drivers %>% its_giving(city)
drivers %>% its_giving(active_status)
rides_clean <- rides %>%
yeet(
distance_km >= 0,
duration_min >= 0,
fare >= 0
)
View(rides_clean)
View(rides)
View(rides_clean)
View(rides)
rides <- rides %>%
glow_up(fare_per_km = fare / distance)
rides <- rides %>%
glow_up(fare_per_km = fare / distance_km)
rides <- rides %>%
glow_up(ride_day = lubridate::wday(date, label = TRUE))
head(rides$date)
rides <- rides %>%
glow_up(date = as.Date(date, format = "%m/%d/%Y"))
rides <- rides %>%
glow_up(
fare_per_km = fare / distance_km,
ride_day = lubridate::wday(date, label = TRUE)
)
rides <- rides %>%
glow_up(fare_per_km = round(fare_per_km, 2))
drivers <- drivers %>%
glow_up(
is_active = active_status == "Active"
)
any(duplicated(drivers$driver_id))
any(duplicated(rides$ride_id))
rides%>% summarize_numeric()
rides %>% summarize_categorical()
drivers[drivers$age - drivers$experience_years < 16, ]
drivers %>%
filter(age - experience_years < 16) %>%
select(driver_id, age, experience_years, average_rating, active_status)
drivers <- drivers %>%
filter(age - experience_years >= 16)
rides %>%
yeet((distance_km / (duration_min / 60)) > 150)
rides %>%
no_cap(
min_speed = min(distance_km / (duration_min / 60)),
max_speed = max(distance_km / (duration_min / 60)),
avg_speed = mean(distance_km / (duration_min / 60))
)
speed_kmh = distance_km / (duration_min / 60)
rides_checked %>%
ggplot(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40)
rides_checked %>%
ggplot2(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40)
install.packages("ggplot2")
rides_checked %>%
ggplot(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40)
rides_checked %>%
ggplot2(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40)
library(ggplot2)
rides %>%
ggplot(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40)
rides %>%
ggplot(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40) +
labs(
x = "Speed (km/h)",
y = "Number of rides",
title = "Distribution of Ride Speeds"
)
rides %>%
ggplot(aes(distance_km / (duration_min / 60))) +
geom_histogram(bins = 40) +
labs(x = "Speed (km/h)") +
scale_x_continuous(breaks = seq(0, 150, by = 10))
rides <- rides %>%
glow_up(
speed_kmh = distance_km / (duration_min / 60)
)
rides <- rides %>%
glow_up(
speed_kmh = round(distance_km / (duration_min / 60), 2)
)
rides %>%
ggplot(aes(speed_kmh)) +
geom_histogram(bins = 40) +
labs(x = "Speed (km/h)") +
scale_x_continuous(breaks = seq(0, 150, by = 10))
rides %>%
ggplot(aes(speed_kmh)) +
geom_histogram(bins = 40) +
labs(
x = "Speed (km/h)",
y = "Number of rides",
title = "Distribution of Ride Speeds"
)
rides_full <- rides %>%
link_up(drivers, by = "driver_id")
rides_full <- left_join(rides, drivers, by = "driver_id")
View(rides_full)
rm(rides_cleed)
rm(rides_clean)
View(rides_full)
rides_full <- rides_full %>%
lowkey(
city_ride   = city.x,
city_driver = city.y
)
rides_full %>%
yeet(city_ride != city_driver)
rides_full %>%
yeet(is.na(name))
rides_full %>%
yeet(is.na(name)) %>%
send_it(100)
View(drivers)
View(drivers)
rides_full %>%
yeet(is.na(name)) %>%
print(n=100)
rides_full %>%
yeet(is.na(name)) %>%
periodt(driver_id) %>%
its_giving()
rides_full %>%
yeet(is.na(name)) %>%     
periodt(driver_id)          
rides_full %>%
its_giving(is.na(.))
rides <- rides %>%
glow_up(
Promo_Code = ifelse(is.na(Promo_Code), "NO_PROMO", Promo_Code)
)
rides <- rides %>%
glow_up(
Promo_Code = ifelse(is.na(Promo_Code), "NO_PROMO", Promo_Code)
)
rides <- rides %>%
glow_up(
promo_code = ifelse(is.na(promo_code), "NO_PROMO", promo_code)
)
rides_full <- rides_full %>%
glow_up(
promo_code = ifelse(is.na(promo_code), "NO_PROMO", promo_code)
)
rides_full %>% yeet(distance_km <= 0 | duration_min <= 0)
rides_full %>%
yeet(speed_kmh < 0 | speed_kmh > 150)
rides_full %>% yeet(rating < 1 | rating > 5)
rides_full %>% periodt(ride_id) %>% its_giving()
dump(list = ls(), file = "project_code.R")
savehistory("project_history.Rhistory")
savehistory("project_history.R")
