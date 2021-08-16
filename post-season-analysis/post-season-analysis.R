#' Post-season analysis

library(dplyr)
library(readr)

# Read in all three data sources
beset <- read_csv("data/beset.csv")
bedrift <- read_csv("data/bedrift.csv")
pilotsonar <- read_csv("data/pilotsonar.csv")

# Munge
beset$source <- "LYTFSET"
bedrift$source <- "LYTFDRIFT"
pilotsonar$source <- "PILOT"

# Fix names
beset %>%
  rename(count = cpue) -> beset

bedrift %>%
  rename(count = cpue) -> bedrift

# Combine
all_sites <- rbind(beset, bedrift, pilotsonar)

# Calculate medians
all_sites %>%
  group_by(source) %>%
  mutate(cumulative = cumsum(count)) %>%
  mutate(pct_cumulative = cumulative / max(cumulative)) -> all_sites

all_sites %>%
  group_by(source) %>%
  filter(pct_cumulative >= 0.50) %>%
  filter(row_number() == 1)
# # A tibble: 3 x 5
# # Groups:   source [3]
# day   count source    cumulative pct_cumulative
# <dbl>   <dbl>         <chr>           <dbl>             <dbl>
# 1       22    0.42    LYTFSET         15.2              0.509
# 2       18    3.16    LYTFDRIFT       33.6              0.518
# 3       29    5558    PILOT           66829             0.535
