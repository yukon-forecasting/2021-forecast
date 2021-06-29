# read in in-season data to date
# for some combinations of mu and s, calculate the RSS

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

calculate_fit_rss <- function(mu, s) {
  logi_fun <- function(x, mu, s) { 1 / (1 + exp(-((x - mu)/s))) }

  inseason <- read_csv("data/bedrift.csv") %>%
    mutate(ccpue = cumsum(cpue))

  xrange <- -10:50
  logistic <- data.frame(day = xrange,
                         cpue_modeleded = logi_fun(xrange, mu, s))

  today <- tail(inseason, n = 1)$day
  ccpue <- tail(inseason, n = 1)$ccpue
  final_ccpue <- ccpue / (logistic[logistic$day == today, "cpue_modeleded"])

  estimated <- inseason
  estimated$pccpue_estimated <- estimated$ccpue / final_ccpue

  # Trim to fit
  estimated %>%
    left_join(logistic, by = "day") %>%
    mutate(r = cpue_modeleded - pccpue_estimated) %>%
    summarize(rss = sum((r/max(cpue_modeleded))^2))
}

crossing(mu = 10:30, s = 3:7) %>%
  purrr::pmap(~ bind_cols(tibble(mu = .x, s = .y), calculate_fit_rss(.x, .y))) %>%
  bind_rows() %>%
  arrange(rss) %>%
  mutate(rank = seq_len(nrow(.)),
         label = paste(mu, s, sep = "/")) %>%
  ggplot(aes(x = rank, y = rss, label = label)) +
  geom_col() +
  geom_text(angle = 90, hjust = -3)

ggsave("refit/figures/scores.pdf", width = 20, height = 4)
