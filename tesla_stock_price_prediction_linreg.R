TSLA
library(tidyverse)
library(tidymodels)
set.seed(123)

tesla_stock <- TSLA |>
  arrange(by = desc(Date))

tesla_stock

tesla_split <- initial_split(tesla_stock, prop = 0.70, strata = Open)
tesla_train <- training(tesla_split)
tesla_test <- testing(tesla_split)

## creating recipe
tesla_recipe <- recipe(Open ~ Date, data = tesla_train) 

## creating model spec.
tesla_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

## creating fitting
tesla_fit <- workflow() |>
  add_recipe(tesla_recipe) |>
  add_model(tesla_spec) |>
  fit(tesla_train) 

tesla_fit 

tesla_training_metrics <- tesla_fit |>
  predict(tesla_train) |>
  bind_cols(tesla_train) |>
  metrics(truth = Open, estimate = .pred) |>
  filter(.metric == 'rmse') |>
  select(.estimate) |>
  pull()

tesla_training_metrics

tesla_metrics <- tesla_fit |>
  predict(tesla_test) |>
  bind_cols(tesla_test) |>
  metrics(truth = Open, estimate = .pred) |>
  filter(.metric == "rmse") |>
  select(.estimate) |>
  pull()

# computing RMSPE
tesla_metrics

# example prediction
tesla_date <- tibble(Date = as.Date("2023-12-25"))

tesla_prediction <- tesla_fit |>
  predict(tesla_date) |>
  bind_cols(tesla_date)

tesla_prediction
