# install.packages("crypto2")

rm(list = ls())

library(crypto2)
library(tidyverse)
library(ggplot2)
library(ggExtra)

#### Choose cryptocurrency -----------------------------------------------------

crypto_listings(which = "latest", 
                quote = TRUE, 
                limit = 1000, 
                finalWait = FALSE) |>
    filter(substr(toupper(symbol), 1, 1) %in% c("F", "P", "M"),
           market_cap > 5e7, market_cap < 1.5e9,
           volume24h > 5e6, 
           date_added < "2022-01-01", 
           active = TRUE) |>                      
    arrange(desc(market_cap)) |>
    select(name, symbol, slug, market_cap, volume24h, date_added) |>
    print(n = 60)

# filecoin, mina, mx-token, prom

#### download the data ---------------------------------------------------------

seq.Date(as.Date("2021-06-01"), as.Date("2026-09-07")) |> length()

hist_raw_df <- crypto_list(only_active = TRUE) |> 
    filter(slug %in% c("filecoin", "mina", "mx-token", "prom")) |> 
    crypto_history(start_date = "2021-06-01", end_date = "2026-09-07", 
                   interval = "1d", finalWait = FALSE)

hist_raw_df |> 
    group_by(slug) |> 
    summarize(date_min = min(timestamp), 
              date_max = max(timestamp), 
              n_obs = n()) |> 
    ungroup()

hist_raw_df |> 
    group_by(slug, timestamp) |> 
    filter(n() > 1) |> 
    summarize(diff = max(market_cap) - min(market_cap)) |> 
    ungroup() |> 
    View()

which(!complete.cases(hist_raw_df))


#### data prep -----------------------------------------------------------------

hist_df <- hist_raw_df |> 
    mutate(across(where(is.POSIXct), as.Date)) |> 
    distinct(timestamp, slug, .keep_all = TRUE) |> 
    group_by(timestamp) |> 
    mutate(w = market_cap / sum(market_cap)) |> 
    ungroup() |> 
    group_by(slug) |> 
    mutate(w_lag_1 = lag(w, n = 1, default = mean(w), order_by = timestamp), 
           close_lag_1 = lag(close, default = 0, n = 1, order_by = timestamp)) |> 
    # mutate(r_i = log(close) - log(close_lag_1)) |> 
    mutate(r_i = close / close_lag_1 - 1) |> 
    ungroup() 

r_portfolio_df <- hist_df |> 
    group_by(timestamp) |> 
    summarize(r_p = sum(w_lag_1 * r_i)) |> 
    ungroup() |> 
    right_join(distinct(select(hist_df, timestamp)))

#### Exploratory plots ---------------------------------------------------------

# weights
ggplot(pivot_longer(hist_df, cols = c("w", "market_cap", "close"), names_to = "metric")) +
    geom_line(aes(x = timestamp, y = value, color = slug)) +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    theme_bw()

# histograms
ggplot(hist_df, aes(x = r_i, fill = slug)) +
    geom_histogram(color = "black", position = "stack") + 
    geom_boxplot(aes(y = -1)) +
    facet_wrap(~slug, ncol = 2) +
    theme_bw()

# statistics
summary(r_portfolio_df$r_p)

# portfolio density
ggplot(r_portfolio_df, aes(x = r_p)) +
    geom_density(fill = "lightblue") + 
    geom_boxplot(aes(y = -1), fill = "lightblue") +
    theme_bw()

ggplot(r_portfolio_df, aes(x = timestamp, y = (r_p)^2)) +
    geom_line() +
    theme_bw()
