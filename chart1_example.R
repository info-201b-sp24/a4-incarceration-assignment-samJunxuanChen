library(ggplot2)
library(dplyr)
library(readr)

prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# National Jail Data
national_jail_data <- jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(
    national_black_jail_rate = sum(black_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100000
  )

# National Prison Data
national_prison_data <- prison %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(
    national_black_prison_rate = sum(black_prison_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100000
  )

# Washington State Jail Data
wa_jail_data <- jail %>%
  filter(state == "WA" & year >= 1990) %>%
  group_by(year) %>%
  summarise(
    wa_black_jail_rate = sum(black_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100000
  )

# Washington State Prison Data
wa_prison_data <- prison %>%
  filter(state == "WA" & year >= 1990) %>%
  group_by(year) %>%
  summarise(
    wa_black_prison_rate = sum(black_prison_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100000
  )

# Merge all datasets
merged_data <- national_jail_data %>%
  left_join(national_prison_data, by = "year") %>%
  left_join(wa_jail_data, by = "year") %>%
  left_join(wa_prison_data, by = "year")

# Plotting
Black_Jail_Prison_Rates <- ggplot(merged_data, aes(x = year)) +
  geom_line(aes(y = national_black_jail_rate, color = "National Black Jail Rate"), na.rm = TRUE) +
  geom_line(aes(y = national_black_prison_rate, color = "National Black Prison Rate"), na.rm = TRUE) +
  geom_line(aes(y = wa_black_jail_rate, color = "WA Black Jail Rate"), na.rm = TRUE) +
  geom_line(aes(y = wa_black_prison_rate, color = "WA Black Prison Rate"), na.rm = TRUE) +
  labs(
    title = "Trends of Black Jail and Prison Population Rates",
    x = "Year",
    y = "Rate per 100,000 People",
    color = "Legend"
  ) +
  scale_color_manual(values = c(
    "National Black Jail Rate" = "blue",
    "National Black Prison Rate" = "red",
    "WA Black Jail Rate" = "green",
    "WA Black Prison Rate" = "purple"
  )) +
  theme_minimal()
