library(readr)
library(tibble)

mu <- 21
s <- 4

logi_fun <- function(x, mu, s) { 1 / (1 + exp(-((x - mu)/s))) }

xrange <- -10:50
cpue <- data.frame(day = xrange,
                   date = as.Date(xrange, format = "%j", origin = as.Date("2016-05-31")),
                   pccpue = 100 * logi_fun(xrange, mu, s))

# Calculate new 15/25/50 dates
day_15pct <- cpue[cpue$pccpue >= 15,"day"][1]
day_25pct <- cpue[cpue$pccpue >= 25,"day"][1]
day_50pct <- cpue[cpue$pccpue >= 50,"day"][1]

new_predictions <- tibble(percentile = c("fifdj", "qdj", "mdj"), 
                          prediction = c(day_15pct, day_25pct, day_50pct))
write_csv(new_predictions, "refit/refit_predictions.csv")

# Write out
write_csv(cpue, "refit/refit_logistic_curve.csv")
