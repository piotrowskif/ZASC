#### data prep -----------------------------------------------------------------

hist_raw_df <- read_csv2(here("data/4_cryptocurrencies_data.csv"))

crypto_df <- hist_raw_df |> 
    mutate(across(where(is.POSIXct), as.Date)) |> 
    distinct(timestamp, slug, .keep_all = TRUE) |> 
    mutate(w = market_cap / sum(market_cap), 
           .by = timestamp) |> 
    group_by(slug) |> 
    mutate(w_lag_1 = dplyr::lag(w, n = 1, order_by = timestamp), 
           close_lag_1 = dplyr::lag(close, n = 1, order_by = timestamp)) |> 
    # mutate(r_i = log(close) - log(close_lag_1)) |> 
    mutate(r_i = close / close_lag_1 - 1) |> 
    ungroup() |> 
    drop_na()

r_p_df <- crypto_df |> 
    summarize(r_p = sum(w_lag_1 * r_i), 
              .by = timestamp) |> 
    right_join(distinct(select(crypto_df, timestamp))) |> 
    arrange(timestamp)


#### Data split ----------------------------------------------------------------

split_date <- as.Date("2025-12-31")

r_p_df <- r_p_df |>
    mutate(sample = if_else(timestamp <= split_date, "train", "test")) |> 
    mutate(r_p2 = r_p^2)

r_train <- r_p_df |>
    filter(sample == "train") |>
    pull(r_p)

r_test <- r_p_df |>
    filter(sample == "test") |>
    pull(r_p)

length(r_train) / nrow(r_p_df)
length(r_test) / nrow(r_p_df)

#### XTS objects ---------------------------------------------------------------

r_p_xts <- xts(r_p_df$r_p, order.by = r_p_df$timestamp)
