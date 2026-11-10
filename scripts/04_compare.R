library(tidyverse)
library(lubridate)

spy <- read_csv("data/spy.csv", show_col_types = FALSE)
monthly <- read_csv("data/monthly_features.csv", show_col_types = FALSE)
original <- read_csv("data/original_preds.csv", show_col_types = FALSE)
improved <- read_csv("data/improved_preds.csv", show_col_types = FALSE)

test_dates <- sort(unique(improved$date))
cost_rate <- 0.001

penn_colors <- c(
  "Improved XGBoost" = "#990000",
  "Original XGBoost" = "#011F5B",
  "Momentum top 10" = "#82AFD3",
  "Equal weight, all stocks" = "#7F7F7F",
  "SPY" = "#000000"
)

rets <- monthly %>%
  filter(date %in% test_dates, !is.na(fwd_ret)) %>%
  select(symbol, date, fwd_ret)

spy_m <- spy %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(spy_ret = dplyr::lead(adjusted) / adjusted - 1) %>%
  select(date, spy_ret)

top10_equal <- function(scores) {
  scores %>%
    filter(!is.na(score)) %>%
    group_by(date) %>%
    slice_max(score, n = 10, with_ties = FALSE) %>%
    mutate(weight = 1 / n()) %>%
    ungroup() %>%
    select(symbol, date, weight)
}

w_original <- original %>%
  filter(date %in% test_dates) %>%
  group_by(date) %>%
  slice_max(pred, n = 10, with_ties = FALSE) %>%
  mutate(
    pos = pmax(pred, 0),
    weight = if (sum(pos) == 0) 1 / n() else pos / sum(pos)
  ) %>%
  ungroup() %>%
  select(symbol, date, weight)

w_improved <- top10_equal(improved %>% rename(score = pred))

w_momentum <- top10_equal(
  monthly %>% filter(date %in% test_dates) %>% transmute(symbol, date, score = mom_12_1)
)

w_equal <- rets %>%
  group_by(date) %>%
  mutate(weight = 1 / n()) %>%
  ungroup() %>%
  select(symbol, date, weight)

strategy_returns <- function(w, name) {
  gross <- w %>%
    left_join(rets, by = c("symbol", "date")) %>%
    group_by(date) %>%
    summarise(ret = sum(weight * fwd_ret, na.rm = TRUE), .groups = "drop")

  turnover <- w %>%
    complete(symbol, date, fill = list(weight = 0)) %>%
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    mutate(change = abs(weight - dplyr::lag(weight, default = 0))) %>%
    group_by(date) %>%
    summarise(turnover = sum(change), .groups = "drop")

  gross %>%
    left_join(turnover, by = "date") %>%
    mutate(strategy = name)
}

all_returns <- bind_rows(
  strategy_returns(w_original, "Original XGBoost"),
  strategy_returns(w_improved, "Improved XGBoost"),
  strategy_returns(w_momentum, "Momentum top 10"),
  strategy_returns(w_equal, "Equal weight, all stocks"),
  spy_m %>%
    filter(date %in% test_dates) %>%
    transmute(date, ret = spy_ret, turnover = if_else(date == min(date), 1, 0), strategy = "SPY")
) %>%
  mutate(net_ret = ret - turnover * cost_rate)

ew <- all_returns %>%
  filter(strategy == "Equal weight, all stocks") %>%
  select(date, ew_ret = ret)

metrics <- all_returns %>%
  left_join(spy_m, by = "date") %>%
  left_join(ew, by = "date") %>%
  group_by(strategy) %>%
  summarise(
    months = n(),
    ann_return = prod(1 + ret)^(12 / n()) - 1,
    ann_return_after_cost = prod(1 + net_ret)^(12 / n()) - 1,
    ann_volatility = sd(ret) * sqrt(12),
    sharpe = mean(ret) / sd(ret) * sqrt(12),
    max_drawdown = min(cumprod(1 + ret) / cummax(cumprod(1 + ret)) - 1),
    beta = coef(lm(ret ~ spy_ret))[2],
    alpha_ann = coef(lm(ret ~ spy_ret))[1] * 12,
    beat_equal_weight = mean(ret > ew_ret),
    avg_turnover = mean(turnover),
    .groups = "drop"
  ) %>%
  arrange(desc(ann_return))

ic_monthly <- bind_rows(
  original %>%
    filter(date %in% test_dates) %>%
    inner_join(rets, by = c("symbol", "date")) %>%
    mutate(model = "Original XGBoost"),
  improved %>%
    mutate(model = "Improved XGBoost"),
  monthly %>%
    filter(date %in% test_dates) %>%
    transmute(symbol, date, pred = mom_12_1, fwd_ret, model = "Momentum (12-1)")
) %>%
  drop_na(pred, fwd_ret) %>%
  group_by(model, date) %>%
  summarise(ic = cor(pred, fwd_ret, method = "spearman"), .groups = "drop")

ic_summary <- ic_monthly %>%
  group_by(model) %>%
  summarise(
    months = n(),
    mean_ic = mean(ic),
    t_stat = mean(ic) / sd(ic) * sqrt(n()),
    share_positive = mean(ic > 0),
    .groups = "drop"
  )

dir.create("results", showWarnings = FALSE)
write_csv(all_returns, "results/monthly_returns.csv")
write_csv(metrics, "results/performance.csv")
write_csv(ic_monthly, "results/ic_monthly.csv")
write_csv(ic_summary, "results/ic_summary.csv")

print(metrics %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
print(ic_summary %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

to_month <- function(d) floor_date(d, "month") %m+% months(1)
start_month <- to_month(min(test_dates)) %m-% months(1)

growth <- all_returns %>%
  arrange(date) %>%
  group_by(strategy) %>%
  mutate(month = to_month(date), growth = cumprod(1 + ret)) %>%
  ungroup() %>%
  select(strategy, month, growth)

plot_data <- bind_rows(
  growth %>% distinct(strategy) %>% mutate(month = start_month, growth = 1),
  growth
)

p1 <- ggplot() +
  geom_line(data = plot_data, aes(month, growth, color = strategy), linewidth = 1) +
  scale_color_manual(values = penn_colors) +
  labs(
    title = "Growth of $1, February 2024 to October 2026",
    subtitle = "Top 10 stocks each month, held for one month, before trading costs",
    x = NULL,
    y = "Value of $1",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("results/fig1_growth.png", p1, width = 8, height = 5, dpi = 200)

p2 <- ggplot(ic_monthly, aes(date, ic)) +
  geom_col(fill = "#011F5B") +
  geom_hline(
    data = ic_summary,
    aes(yintercept = mean_ic),
    color = "#990000",
    linetype = "dashed"
  ) +
  facet_wrap(~ model, ncol = 1) +
  labs(
    title = "Monthly rank correlation between predictions and next-month returns",
    subtitle = "Dashed line is the average over the test period",
    x = NULL,
    y = "Rank correlation (IC)"
  ) +
  theme_minimal()

ggsave("results/fig2_ic.png", p2, width = 8, height = 6, dpi = 200)
