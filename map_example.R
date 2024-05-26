library(ggplot2)
library(dplyr)
library(usmap)
jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# Summarize total jail population group by state
jail_summary <- jail %>%
  group_by(state) %>%
  summarize(total_jail_pop = sum(female_adult_jail_pop, male_adult_jail_pop, 
                                 female_juvenile_jail_pop, male_juvenile_jail_pop, na.rm = TRUE))

# Map
Jail_Population_Map <- plot_usmap(data = jail_summary, values = "total_jail_pop", lines = "black") +
  scale_fill_continuous(name = "Total Jail Population", low = "white", high = "darkred", na.value = "grey50") +
  labs(
    title = "Geographical Distribution of Total Jail Population",
    subtitle = "Total jail population by state",
    caption = "Source: Vera Institute of Justice"
  ) +
  theme(legend.position = "right")