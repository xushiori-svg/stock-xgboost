library(tidyverse)
library(tidymodels)
library(slider)
library(xgboost)

set.seed(123)

prices <- read_csv("data/prices.csv", show_col_types = FALSE) %>%
  filter(date >= as.Date("2022-01-01"))

spy <- read_csv("data/spy.csv", show_col_types = FALSE) %>%
  filter(date >= as.Date("2022-01-01")) %>%
  select(date, spy_price = adjusted) %>%
  mutate(spy_ret = spy_price / dplyr::lag(spy_price) - 1)

df <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(return = adjusted / dplyr::lag(adjusted) - 1) %>%
  ungroup() %>%
  left_join(spy %>% select(date, spy_ret), by = "date") %>%
  group_by(symbol) %>%
  mutate(
    mom_1m = dplyr::lag(adjusted, 21) / dplyr::lag(adjusted, 1) - 1,
    mom_3m = dplyr::lag(adjusted, 63) / dplyr::lag(adjusted, 1) - 1,
    mom_6m = dplyr::lag(adjusted, 126) / dplyr::lag(adjusted, 1) - 1,
    vol_1m = slide_dbl(dplyr::lag(return), sd, .before = 21, .complete = TRUE),
    ret_5d = dplyr::lag(adjusted, 5) / dplyr::lag(adjusted, 1) - 1,
    excess_ret = return - spy_ret
  ) %>%
  ungroup() %>%
  drop_na()

df <- df %>%
  group_by(symbol) %>%
  mutate(target = dplyr::lead(adjusted, 20) / dplyr::lead(adjusted, 1) - 1) %>%
  ungroup() %>%
  drop_na()

train <- df %>% filter(date < as.Date("2024-01-01"))
test <- df %>% filter(date >= as.Date("2024-01-01"))

rec <- recipe(target ~ ., data = train) %>%
  update_role(date, symbol, new_role = "id")

xgb_spec <- boost_tree(
  trees = 300,
  tree_depth = 5,
  learn_rate = 0.05,
  loss_reduction = 0,
  min_n = 10
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_fit <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec) %>%
  fit(data = train)

preds <- predict(final_fit, test) %>%
  bind_cols(test) %>%
  select(symbol, date, pred = .pred)

write_csv(preds, "data/original_preds.csv")
cat("Original model: trained on", nrow(train), "rows, predicted", nrow(preds), "rows\n")
