library(tidyverse)
library(tidyquant)

tickers <- c(
  "AAPL","MSFT","NVDA","GOOG","META","AMZN","AVGO","ORCL","ADBE","CRM",
  "CSCO","AMD","INTC","QCOM","TXN","IBM","NOW","INTU","AMAT","MU",
  "JPM","BAC","WFC","GS","MS","BLK","C","SCHW","SPGI","AXP",
  "USB","PNC","TFC","COF",
  "UNH","JNJ","LLY","PFE","MRK","ABBV","TMO","DHR","ABT","BMY",
  "AMGN","GILD","CVS","ISRG","MDT",
  "TSLA","HD","MCD","NKE","SBUX","LOW","TJX","BKNG","CMG","ORLY",
  "WMT","COST","PG","KO","PEP","PM","MO","CL","EL",
  "CAT","GE","HON","UPS","BA","RTX","DE","LMT","MMM","ETN",
  "XOM","CVX","COP","SLB","EOG",
  "NFLX","DIS","CMCSA","TMUS","VZ",
  "AMT","PLD","NEE","DUK","SO","LIN","APD"
)

start_date <- "2014-01-01"
end_date <- "2026-11-01"

prices <- tq_get(tickers, from = start_date, to = end_date) %>%
  filter(!is.na(adjusted))

spy <- tq_get("SPY", from = start_date, to = end_date) %>%
  select(date, open, high, low, close, volume, adjusted)

dir.create("data", showWarnings = FALSE)
write_csv(prices, "data/prices.csv")
write_csv(spy, "data/spy.csv")

cat("Stocks downloaded:", n_distinct(prices$symbol), "of", length(tickers), "\n")
cat("Dates:", as.character(min(prices$date)), "to", as.character(max(prices$date)), "\n")
