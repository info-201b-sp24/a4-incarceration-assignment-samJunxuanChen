library(dplyr)
library(ggplot2)
library(readr)
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")
prison_jail_per_100000 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")
prison_jail_1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true")
WA_prison_jail_1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv")

# Create a function to calculate jail rates per 100,000 people
calculate_rate <- function(data, total_col, pop_col) {
  data %>%
    mutate(rate = (!!sym(total_col) / !!sym(pop_col)) * 100000)
}

# Average jail rates by race in the latest year (2018)
latest_year <- 2018

jail <- jail %>%
  filter(year == latest_year) %>%
  mutate(
    black_rate = (black_jail_pop / black_pop_15to64) * 100000,
    white_rate = (white_jail_pop / white_pop_15to64) * 100000,
    aapi_rate = (aapi_jail_pop / aapi_pop_15to64) * 100000,
    latinx_rate = (latinx_jail_pop / latinx_pop_15to64) * 100000,
    other_rate = ((aapi_jail_pop + latinx_jail_pop) / (aapi_pop_15to64+latinx_pop_15to64)) * 100000 / 2
  )

jail_rates <- jail %>%
  summarise(
    average_black_rate = mean(black_rate, na.rm = TRUE),
    average_white_rate = mean(white_rate, na.rm = TRUE),
    average_other_rate = mean(other_rate, na.rm = TRUE)
  )

# Average jail rates by gender and age in the latest year (2018)
gender_age_rates <- jail %>%
  mutate(
    male_adult_rate = ((male_adult_jail_pop + male_juvenile_jail_pop) / male_pop_15to64) * 100000,
    female_adult_rate = ((female_adult_jail_pop + female_juvenile_jail_pop) / female_pop_15to64) * 100000
  ) %>%
  summarise(
    average_male_adult_rate = mean(male_adult_rate, na.rm = TRUE),
    average_female_adult_rate = mean(female_adult_rate, na.rm = TRUE)
  )

# Average jail rates by race in Washington State from 1990 to 2018
average_WA_race <- WA_prison_jail_1990 %>%
  summarise(
    average_native_WA_rate = mean(native_jail_pop_rate, na.rm = TRUE)
  )

# Variables for the summary paragraph
average_black_rate <- jail_rates$average_black_rate
average_white_rate <- jail_rates$average_white_rate
average_other_rate <- jail_rates$average_other_rate
average_male_adult_rate <- gender_age_rates$average_male_adult_rate
average_female_adult_rate <- gender_age_rates$average_female_adult_rate
average_native_WA_rate <- average_WA_race$average_native_WA_rate
