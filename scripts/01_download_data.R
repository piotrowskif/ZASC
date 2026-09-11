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

seq.Date(as.Date("2021-06-01"), as.Date("2026-09-08")) |> length()

hist_raw_df <- crypto_list(only_active = TRUE) |> 
    filter(slug %in% c("filecoin", "mina", "mx-token", "prom")) |> 
    crypto_history(start_date = "2021-06-01", end_date = "2026-09-08", 
                   interval = "1d", finalWait = FALSE)

write_csv2(hist_raw_df, here("data/4_cryptocurrencies_data.csv"))
