# Compare BE Set and Drift + Pilot Sonar to see how each site is catching

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

theme_set(theme_classic())


# Read in
beset <- read_csv("data/beset.csv")
bedrift <- read_csv("data/bedrift.csv")
pilotsonar <- read_csv("data/pilotsonar.csv")

# Munge
beset$source <- "LYTFSET"
bedrift$source <- "LYTFDRIFT"
pilotsonar$source <- "PILOMINUS3D"

# Offset pilotsonar by 3 days to account for travel time
pilotsonar$day <- pilotsonar$day - 3

# Fix names
beset %>%
  rename(count = cpue) -> beset

bedrift %>%
  rename(count = cpue) -> bedrift

# Combine
all_sites <- rbind(beset, bedrift, pilotsonar)

# Scale
all_sites %>%
  group_by(source) %>%
  mutate(count = count / sum(count)) -> all_sites

# Crop to day 45
# Based on my visual inspection
all_sites %>%
  filter(day <= 45) -> all_sites

# Plot
comparison <- ggplot(all_sites, aes(day, count, linetype = source)) +
  geom_line() +
  labs(x = "Day of June",
       y = "% of Total")
comparison
ggsave("analyses/figures/comparison.png", comparison, width = 10, height = 2)

# Compare cumulative
all_sites %>%
  group_by(source) %>%
  mutate(cumulative = cumsum(count)) %>%
  mutate(pct_cumulative = cumulative / max(cumulative)) -> all_sites

comparison_cumulative <- ggplot(all_sites, aes(day, pct_cumulative, linetype = source)) +
  geom_line()

ggsave("analyses/figures/comparison_cumulative.png", comparison_cumulative, width = 6, height = 4)
