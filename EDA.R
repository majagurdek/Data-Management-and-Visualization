library(tidyverse)
library(genzplyr)

setwd("C:/Users/Privat/Desktop/DV Project")
rides      <- read_csv("rides.csv")
drivers    <- read_csv("drivers.csv")
rides_full <- read_csv("rides_full.csv")

# ===============================================================
# RIDE-LEVEL EDA
# ===============================================================

# 1. Distribution of numeric variables
rides %>%
  pivot_longer(cols = c(distance_km, duration_min, fare, fare_per_km, speed_kmh, rating),
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "gray40") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Distribution of Key Ride-Level Variables",
    x = "Value",
    y = "Count"
  )

# 2. City distribution
rides %>%
  count(city, sort = TRUE) %>%
  ggplot(aes(x = reorder(city, n), y = n)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  labs(
    title = "Ride Distribution Across Cities",
    x = "City",
    y = "Number of Rides"
  )

# 3. Day-of-week distribution
rides %>%
  count(ride_day) %>%
  ggplot(aes(x = ride_day, y = n)) +
  geom_col(fill = "gray40") +
  labs(
    title = "Ride Volume by Day of Week",
    x = "Day of Week",
    y = "Number of Rides"
  )

# 4. Distance vs fare, colored by city
rides %>%
  ggplot(aes(x = distance_km, y = fare, colour = city)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Distance and Fare Across Cities",
    x = "Distance (km)",
    y = "Fare (USD)",
    colour = "City"
  )

# 5. Duration vs distance
rides %>%
  ggplot(aes(x = distance_km, y = duration_min)) +
  geom_point(alpha = 0.5, colour = "gray40") +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  labs(
    title = "Relationship Between Distance and Ride Duration",
    x = "Distance (km)",
    y = "Duration (minutes)"
  )

# 6. Fare per km by city
rides %>%
  ggplot(aes(x = city, y = fare_per_km)) +
  geom_boxplot(fill = "white") +
  coord_flip() +
  labs(
    title = "Fare per Kilometre by City",
    x = "City",
    y = "Fare per km (USD)"
  )


# ===============================================================
# DRIVER-LEVEL EDA
# ===============================================================

# 7. Distributions of driver attributes
drivers %>%
  pivot_longer(cols = c(age, experience_years, average_rating),
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "gray40") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Distribution of Driver Characteristics",
    x = "Value",
    y = "Count"
  )

# 8. Ratings by active/inactive status
drivers %>%
  ggplot(aes(x = active_status, y = average_rating)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Driver Average Ratings by Active/Inactive Status",
    x = "Active Status",
    y = "Average Rating"
  )

# 9. Experience by city
drivers %>%
  ggplot(aes(x = city, y = experience_years)) +
  geom_boxplot(fill = "white") +
  coord_flip() +
  labs(
    title = "Driver Experience by City",
    x = "City",
    y = "Years of Experience"
  )

# ===============================================================
# STEP 4 — EDA ON JOINED DATASET (rides_full)
# ===============================================================

# rides_full contains both ride-level and driver-level variables.
# This allows you to analyze relationships such as pricing vs experience,
# ratings vs driver activity, city mismatches, promo effects, etc.


# ---------------------------------------------------------------
# 4.1 SUMMARY METRICS BY CITY (city_ride)
# ---------------------------------------------------------------

# Create a summary table of key performance metrics by city
city_summary <- rides_full %>%
  group_by(city_ride) %>%
  summarise(
    rides_count  = n(),
    total_fare   = sum(fare),
    avg_fare     = mean(fare),
    avg_rating   = mean(rating),
    avg_distance = mean(distance_km),
    avg_speed    = mean(speed_kmh)
  )

city_summary  # View the summary table

# Visualize ride count by city
city_summary %>%
  ggplot(aes(x = reorder(city_ride, rides_count), y = rides_count)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  labs(
    title = "Ride Volume by City",
    x = "City",
    y = "Number of Rides"
  )



# ---------------------------------------------------------------
# 4.2 DRIVER EXPERIENCE VS RIDE RATING
# ---------------------------------------------------------------

# Scatterplot: Driver experience vs customer ride rating
rides_full %>%
  ggplot(aes(x = experience_years, y = rating)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  labs(
    title = "Driver Experience vs Ride Rating",
    x = "Years of Experience",
    y = "Ride Rating"
  )



# ---------------------------------------------------------------
# 4.3 EXPERIENCE VS FARE PER KM (PRICE EFFICIENCY)
# ---------------------------------------------------------------

# Scatterplot: Driver experience vs fare per km
rides_full %>%
  ggplot(aes(x = experience_years, y = fare_per_km)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "darkgreen") +
  labs(
    title = "Driver Experience vs Fare Efficiency (Fare per Km)",
    x = "Years of Experience",
    y = "Fare per Km (USD)"
  )



# ---------------------------------------------------------------
# 4.4 ACTIVE VS INACTIVE DRIVER PERFORMANCE
# ---------------------------------------------------------------

# Summary table comparing active vs inactive drivers
rides_full %>%
  group_by(is_active) %>%
  summarise(
    rides_count  = n(),
    avg_fare     = mean(fare),
    avg_rating   = mean(rating),
    avg_distance = mean(distance_km)
  )

# Boxplot: Ride-level ratings for active vs inactive drivers
rides_full %>%
  ggplot(aes(x = is_active, y = rating)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Ride Ratings by Driver Active Status",
    x = "Driver Active Status",
    y = "Ride Rating"
  )



# ---------------------------------------------------------------
# 4.5 PROMO VS NON-PROMO RIDES
# ---------------------------------------------------------------

# Summary table comparing promo vs non-promo rides
rides_full %>%
  mutate(has_promo = promo_code != "NO_PROMO") %>%
  group_by(has_promo) %>%
  summarise(
    rides_count  = n(),
    avg_fare     = mean(fare),
    avg_distance = mean(distance_km),
    avg_rating   = mean(rating)
  )

# Boxplot: fare distribution by promo usage
rides_full %>%
  mutate(has_promo = promo_code != "NO_PROMO") %>%
  ggplot(aes(x = has_promo, y = fare)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Fare Distribution for Promo vs Non-Promo Rides",
    x = "Promo Applied?",
    y = "Fare (USD)"
  )



# ---------------------------------------------------------------
# 4.6 CITY_RIDE VS CITY_DRIVER MISMATCH ANALYSIS
# ---------------------------------------------------------------

# Count how many rides happen outside the driver's primary city
rides_full %>%
  mutate(mismatch = city_ride != city_driver) %>%
  count(mismatch)

# Visualize mismatch counts
rides_full %>%
  mutate(mismatch = city_ride != city_driver) %>%
  ggplot(aes(x = mismatch)) +
  geom_bar(fill = "gray40") +
  labs(
    title = "Driver City Mismatch: Ride City vs Driver's Primary City",
    x = "Mismatch (TRUE = Driver operated in another city)",
    y = "Number of Rides"
  )

# ===============================================================
# STEP 5 — EXPERIENCE BANDS & SEGMENT ANALYSIS
# ===============================================================
rides_full <- rides_full %>% 
  filter(!is.na(experience_years))

# Create experience bands
rides_full <- rides_full %>%
  mutate(
    experience_band = case_when(
      experience_years <= 5 ~ "New (0–5)",
      experience_years <= 10 ~ "Junior (6–10)",
      experience_years <= 15 ~ "Mid (11–15)",
      experience_years <= 20 ~ "Senior (16–20)",
      TRUE ~ "Other"
    )
  )

# Convert to ordered factor for proper plotting
rides_full$experience_band <- factor(
  rides_full$experience_band,
  levels = c("New (0–5)", "Junior (6–10)", "Mid (11–15)", "Senior (16–20)")
)



# ---------------------------------------------------------------
# 5.1 — Summary Stats by Experience Band
# ---------------------------------------------------------------

experience_summary <- rides_full %>%
  group_by(experience_band) %>%
  summarise(
    rides_count = n(),
    avg_rating  = mean(rating),
    avg_fare    = mean(fare),
    avg_fare_per_km = mean(fare_per_km),
    avg_distance = mean(distance_km),
    avg_speed = mean(speed_kmh)
  )

experience_summary  # View table



# ---------------------------------------------------------------
# 5.2 — Ratings by Experience Band (Boxplot)
# ---------------------------------------------------------------

rides_full %>%
  ggplot(aes(x = experience_band, y = rating)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Ride Ratings by Driver Experience Band",
    x = "Driver Experience Level",
    y = "Ride Rating"
  )



# ---------------------------------------------------------------
# 5.3 — Fare_per_km by Experience Band (Boxplot)
# ---------------------------------------------------------------

rides_full %>%
  ggplot(aes(x = experience_band, y = fare_per_km)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Fare Efficiency (Fare per Km) by Driver Experience Band",
    x = "Driver Experience Level",
    y = "Fare per Km (USD)"
  )



# ---------------------------------------------------------------
# 5.4 — Distance by Experience Band (Optional)
# ---------------------------------------------------------------

rides_full %>%
  ggplot(aes(x = experience_band, y = distance_km)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Ride Distance by Driver Experience Band",
    x = "Driver Experience Level",
    y = "Distance (km)"
  )



# ---------------------------------------------------------------
# 5.5 — Speed by Experience Band (Optional)
# ---------------------------------------------------------------

rides_full %>%
  ggplot(aes(x = experience_band, y = speed_kmh)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Speed (km/h) by Driver Experience Band",
    x = "Driver Experience Level",
    y = "Speed (km/h)"
  )






# City dimension: union of ride cities and driver home cities
DimCity <- rides_full %>%
  transmute(city_name = city_ride) %>%
  bind_rows(
    rides_full %>% transmute(city_name = city_driver)
  ) %>%
  distinct(city_name) %>%
  arrange(city_name) %>%
  mutate(city_id = row_number()) %>%
  select(city_id, city_name)

DimDriver <- drivers %>%
  left_join(DimCity, by = c("city" = "city_name")) %>%
  rename(driver_city_id = city_id) %>%
  select(
    driver_id,
    name,
    age,
    experience_years,
    average_rating,
    active_status,
    is_active,
    driver_city_id
  )


DimDate <- rides_full %>%
  distinct(date) %>%
  mutate(
    date = as.Date(date),
    date_id   = as.integer(date),            # surrogate key
    year      = year(date),
    month     = month(date, label = TRUE),
    day       = day(date),
    weekday   = wday(date, label = TRUE),
    is_weekend = weekday %in% c("Sat", "Sun")
  ) %>%
  select(date_id, date, year, month, day, weekday, is_weekend)

DimPromo <- rides_full %>%
  distinct(promo_code) %>%
  arrange(promo_code) %>%
  mutate(promo_id = row_number()) %>%
  select(promo_id, promo_code)


FactRides <- rides_full %>%
  mutate(date = as.Date(date)) %>%
  # link to DimDate
  left_join(DimDate, by = "date") %>%
  # link to DimCity for ride city
  left_join(DimCity %>% rename(city_ride = city_name,
                               ride_city_id = city_id),
            by = "city_ride") %>%
  # link to DimPromo
  left_join(DimPromo, by = "promo_code") %>%
  # keep only surrogate keys + measures
  select(
    ride_id,
    driver_id,        # FK -> DimDriver
    date_id,          # FK -> DimDate
    ride_city_id,     # FK -> DimCity
    promo_id,         # FK -> DimPromo
    distance_km,
    duration_min,
    fare,
    fare_per_km,
    speed_kmh,
    rating
  )


write_csv(FactRides, "FactRides.csv")
write_csv(DimDriver, "DimDriver.csv")
write_csv(DimDate,   "DimDate.csv")
write_csv(DimCity,   "DimCity.csv")
write_csv(DimPromo,  "DimPromo.csv")
