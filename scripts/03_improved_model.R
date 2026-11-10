library(tidyverse)
library(lubridate)
library(tidymodels)
library(slider)
library(xgboost)

set.seed(123)

prices <- read_csv("data/prices.csv", show_col_types = FALSE)
spy <- read_csv("data/spy.csv", show_col_types = FALSE)

month_ends <- spy %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(date = max(date), .groups = "drop") %>%
  arrange(date) %>%
  mutate(next_date = dplyr::lead(date)) %>%
  select(date, next_date)

daily <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    ret = adjusted / dplyr::lag(adjusted) - 1,
    mom_1m = adjusted / dplyr::lag(adjusted, 21) - 1,
    mom_3m = adjusted / dplyr::lag(adjusted, 63) - 1,
    mom_6m = adjusted / dplyr::lag(adjusted, 126) - 1,
    mom_12_1 = dplyr::lag(adjusted, 21) / dplyr::lag(adjusted, 252) - 1,
    rev_5d = adjusted / dplyr::lag(adjusted, 5) - 1,
    vol_1m = slide_dbl(ret, sd, .before = 20, .complete = TRUE),
    vol_6m = slide_dbl(ret, sd, .before = 125, .complete = TRUE),
    high_52w = adjusted / slide_dbl(adjusted, max, .before = 251, .complete = TRUE),
    volume_trend = slide_dbl(volume, mean, .before = 20, .complete = TRUE) /
      slide_dbl(volume, mean, .before = 251, .complete = TRUE)
  ) %>%
  ungroup()

features <- c("mom_1m", "mom_3m", "mom_6m", "mom_12_1", "rev_5d",
              "vol_1m", "vol_6m", "high_52w", "volume_trend")
rank_features <- paste0(features, "_r")

monthly <- daily %>%
  inner_join(month_ends, by = "date") %>%
  left_join(
    prices %>% select(symbol, next_date = date, next_adjusted = adjusted),
    by = c("symbol", "next_date")
  ) %>%
  mutate(fwd_ret = next_adjusted / adjusted - 1) %>%
  filter(date >= as.Date("2015-01-01")) %>%
  drop_na(all_of(features)) %>%
  group_by(date) %>%
  mutate(
    across(all_of(features), percent_rank, .names = "{.col}_r"),
    target = percent_rank(fwd_ret)
  ) %>%
  ungroup()

write_csv(
  monthly %>% select(symbol, date, all_of(features), fwd_ret),
  "data/monthly_features.csv"
)

fit_xgb <- function(train_data, depth, rate) {
  spec <- boost_tree(
    trees = 500,
    tree_depth = depth,
    learn_rate = rate,
    min_n = 50,
    sample_size = 0.8
  ) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  workflow() %>%
    add_formula(target ~ .) %>%
    add_model(spec) %>%
    fit(data = train_data %>% select(target, all_of(rank_features)))
}

add_pred <- function(model, new_data) {
  new_data %>%
    mutate(pred = predict(model, new_data %>% select(all_of(rank_features)))$.pred)
}

mean_ic <- function(d) {
  d %>%
    group_by(date) %>%
    summarise(ic = cor(pred, fwd_ret, method = "spearman"), .groups = "drop") %>%
    pull(ic) %>%
    mean()
}

train_val <- monthly %>% filter(date < as.Date("2022-12-01"), !is.na(target))
valid <- monthly %>% filter(date >= as.Date("2023-01-01"), date < as.Date("2023-12-01"))

grid <- expand_grid(depth = c(2, 4), rate = c(0.01, 0.05))
grid$valid_ic <- map2_dbl(grid$depth, grid$rate, function(d, r) {
  model <- fit_xgb(train_val, d, r)
  mean_ic(add_pred(model, valid))
})
print(grid)

best <- grid %>% slice_max(valid_ic, n = 1, with_ties = FALSE)
cat("Chosen: depth", best$depth, "learn rate", best$rate, "\n")

preds <- map_dfr(2024:2026, function(y) {
  train_y <- monthly %>%
    filter(date < as.Date(paste0(y - 1, "-12-01")), !is.na(target))
  test_y <- monthly %>%
    filter(year(date) == y, !is.na(fwd_ret))
  model <- fit_xgb(train_y, best$depth, best$rate)
  cat("Test year", y, ": trained on", nrow(train_y), "stock-months\n")
  add_pred(model, test_y) %>% select(symbol, date, pred, fwd_ret)
})

dir.create("results", showWarnings = FALSE)
write_csv(grid, "results/validation_grid.csv")
write_csv(preds, "data/improved_preds.csv")
