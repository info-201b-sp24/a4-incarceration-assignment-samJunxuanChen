library(ggplot2)
library(dplyr)

jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

# Summary of jail population
jail_summary <- jail %>%
  group_by(year, state, county_name) %>%
  summarize(total_jail_pop = sum(female_adult_jail_pop, male_adult_jail_pop, 
                                 female_juvenile_jail_pop, male_juvenile_jail_pop, na.rm = TRUE))
# Summary of prison population
prison_summary <- prison %>%
  group_by(year, state, county_name) %>%
  summarize(total_prison_pop = sum(female_prison_pop, male_prison_pop, na.rm = TRUE))

# Merge the jail and prison data together
combined_data <- merge(jail_summary, prison_summary, by = c("year", "state", "county_name"))

# Remove all NA values
combined_data_clean <- combined_data %>%
  filter(!is.na(total_jail_pop) & !is.na(total_prison_pop))

# Plotting chart
Jail_Prison_Relationship <- ggplot(combined_data_clean, aes(x = total_jail_pop, y = total_prison_pop)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Relationship between Total Jail and Prison Populations",
    x = "Total Jail Population",
    y = "Total Prison Population"
  ) +
  theme_minimal()
