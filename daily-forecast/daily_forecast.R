#' daily_forecast.R
#'
#' Code for generating the daily forecast.

library(ggplot2)
library(readr)
library(dplyr)

# Load data
inseason <- read_csv("data/pilotsonar.csv") %>%
  mutate(day = day - 3,
         ccpue = cumsum(count))
logistic_curve <- read_csv("pre-season-forecast/output/logistic_curve.csv")
predictions <- read_csv("pre-season-forecast/output/predictions.csv")

# Calculate estimated pccpue
today <- tail(inseason, n = 1)$day
ccpue <- tail(inseason, n = 1)$ccpue

final_ccpue <- (ccpue / (logistic_curve[logistic_curve$day == today,"pccpue"] / 100))[[1]]

estimated <- inseason %>%
  mutate(pccpue = ccpue / final_ccpue)

# Export estimated pccpues for Chart1
write.table(subset(estimated, day >= 1)$pccpue * 100, row.names = FALSE, col.names = FALSE, file = "daily-forecast/output/estimated.csv")

# Write cumulative CPUEs for Chart 2
write.table(inseason[inseason$day >= 1,"ccpue"], row.names = FALSE, col.names = FALSE, file = "daily-forecast/output/cumulative_cpue.csv")

# Subset the columns for later use
estimated <- estimated %>%
  select(day, pccpue)

# Plot
logistic_curve %>%
  mutate(date = as.Date(date))

predictions$percent <- c(15, 25, 50)
predictions$label <- paste0(c(15, 25, 50), "%")
predictions$date <- as.Date(predictions$prediction, format = "%j", origin = as.Date("2016-05-31"))
predictions$date <- predictions$date + 5

estimated$date <- as.Date(estimated$day, format = "%j", origin = as.Date("2016-05-31"))
estimated$pccpue <- estimated$pccpue * 100

logistic_curve$curve <- "Modeled"
estimated$curve <- "Estimated"
combined <- rbind(logistic_curve,
                  estimated)
combined$curve <- ordered(combined$curve, level = c("Modeled", "Estimated"))

ggplot() +
  geom_bar(data = predictions, aes(date, percent), stat = "identity", fill = NA, color = "black") +
  geom_text(data = predictions, aes(date, percent, label = label), vjust = -1, size = 3) +
  geom_line(data = combined, aes(date, pccpue, color = curve)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Date", y = "Cumulative % CPUE") +
  theme_bw() +
  theme(legend.position = c(.10,.85),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NULL, linetype = "solid", color = "black"))

ggsave("daily-forecast/figures/daily_forecast.png", width = 6, height = 3)

# Chart 2: Finaly CPUE time series
final_cpue <- data.frame(day = inseason$day,
                         estimate = inseason$ccpue / (logistic_curve[logistic_curve$day %in% inseason$day,"pccpue"][[1]] / 100))
final_cpue$date <- as.Date(final_cpue$day, format = "%j", origin = as.Date("2016-05-31"))

ggplot(final_cpue, aes(date, estimate)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Estimated Total CPUE") +
  theme_bw()

ggsave("daily-forecast/figures/final_cpue.png", width = 6, height = 3)
