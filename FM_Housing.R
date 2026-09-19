library(tidymodels)
library(dplyr)
library(tidyverse)
library(readr)
library(ranger)
library(janitor)



historical_data <- read_csv("Data/FM_Housing_2018_2022_clean.csv")
View(historical_data)

active_listings<- read_csv("Data/FM_Housing_ActiveListings_clean (1).csv")
View(active_listings)

historical_data <- historical_data %>% clean_names()


set.seed(42)

house_split <- initial_split(historical_data, prop = 0.8)

house_trainset <- training(house_split)
house_testset <- testing(house_split)

preproc_house <- recipe(sold_price ~ ., data = house_trainset) %>%
  step_rm(sold_date, sold_price_per_sq_ft, photo_url, listing_agent, days_on_market) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_string2factor(all_nominal_predictors())

house_spec <- rand_forest(mode = "regression", trees = 100) %>%
  set_engine("ranger", importance = "impurity")

house_wf <- workflow() %>%
  add_recipe(preproc_house)%>%
  add_model(house_spec)

house_fit <- fit(house_wf, data = house_trainset)

predictor_names <- house_trainset %>% select(-sold_price) %>% names()

get_default <- function(x) {
  if (is.numeric(x)) {
    median(x, na.rm = TRUE)
  } else {
    x_tab <- table(x)
    names(x_tab)[which.max(x_tab)]
  }
}

reference_row <- house_trainset %>%
  select(all_of(predictor_names)) %>%
  summarise(across(everything(), get_default))

factor_choices <- list(
  city        = sort(unique(house_trainset$city)),
  style       = sort(unique(house_trainset$style)),
  garage_type = sort(unique(house_trainset$garage_type))
)

print(house_fit)

house_preds <- predict(house_fit, house_testset) %>%
  bind_cols(house_testset %>% select(sold_price))

house_metrics <- house_preds %>%
  metrics(truth = sold_price, estimate = .pred)

print(house_metrics)

rf_engine <- house_fit %>% extract_fit_engine()

var_importance <- rf_engine$variable.importance %>%
  sort(decreasing = TRUE) %>%
  {tibble(Variable = names(.), Importance = .)}

print(var_importance)

head(var_importance)

dir.create("model_artifacts", showWarnings = FALSE)

saveRDS(house_fit,       "model_artifacts/house_fit.rds")
saveRDS(house_testset,   "model_artifacts/house_testset.rds")
saveRDS(house_preds,     "model_artifacts/house_preds.rds")
saveRDS(house_metrics,   "model_artifacts/house_metrics.rds")
saveRDS(var_importance,  "model_artifacts/var_importance.rds")
saveRDS(reference_row,  "model_artifacts/reference_row.rds")
saveRDS(factor_choices, "model_artifacts/factor_choices.rds")

