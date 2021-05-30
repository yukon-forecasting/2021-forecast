# model_select.R
#
# Run hindcasting model selection to find the best model to use in forecasting

# Data
library(readr)
yukon <- read_csv("../data/yukon.csv")

# Canddiate models
models <- c("mdj ~ amatc",
            "mdj ~ msstc",
            "mdj ~ pice",
            "mdj ~ amatc + msstc",
            "mdj ~ amatc + pice",
            "mdj ~ msstc + pice",
            "mdj ~ amatc + msstc + pice")

# Set up selection
hindcast_years <- 2000:2019
round_method <- floor # Floor predictions

result <- data.frame()

for (model_formula in models) {
  model_result <- data.frame()

  for(year in hindcast_years) {
    training_data <- yukon[yukon$year < year,]
    training_model <- lm(formula(model_formula), training_data)

    new_data <- yukon[yukon$year == year,]
    prediction <- predict(training_model, newdata = new_data, se.fit=TRUE)
    prediction_fit <- round_method(prediction$fit[[1]])
    prediction_interval <- prediction_fit + c(-2, 2) * qnorm(0.975) *
      prediction$se.fit[[1]]

    actual <- new_data$mdj
    in_interval <- actual >= round_method(prediction_interval[1]) &&
      actual <= round_method(prediction_interval[2])

    model_result <- rbind(model_result,
                          data.frame(
                            "formula"=model_formula,
                            "year"=year,
                            "predicted"=(prediction_fit),
                            "observed"=actual,
                            "diff"=actual - (prediction_fit),
                            "predict_se"=prediction$se.fit[[1]],
                            "in_interval"=in_interval,
                            "int_lower"=prediction_interval[1],
                            "int_upper"=prediction_interval[2],
                            "int_width"=prediction_interval[2] -
                              prediction_interval[1]))
  }

  model_table <- data.frame(
    model = model_formula,
    "MAPE"=round(mean(abs(model_result$predicted - model_result$observed)), 2),
    "SDMAPE"=round(sd(abs(model_result$predicted - model_result$observed)), 2),
    "width"=round(mean(model_result$int_width), 2),
    "p.in"=round(sum(model_result$in_interval) / length(model_result$in_interval), 2),
    "absmax"=max(abs(model_result$predicted - model_result$observed)),
    "meanbias" = round(mean(model_result$predicted - model_result$observed), 2)
  )

  result <- rbind(result,
                  model_table)
}

result

write.csv(result, file = "pre-season-forecast/output/model_select.csv")
