#### Forecast computations -----------------------------------------------------

window_size <- 1000

# Rolling window for sGARCH
roll_sgarch <- ugarchroll(
    spec = spec_sgarch,
    data = r_p_xts,
    n.ahead = 1,
    # forecast.length = length(r_test),
    n.start = length(r_train),
    refit.every = 1, 
    refit.window = "moving", 
    window.size = window_size,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01, 0.025, 0.05),
    solver = "hybrid",
    keep.coef = FALSE
)

# Rolling window for eGARCH
roll_egarch <- ugarchroll(
    spec = spec_egarch,
    data = r_p_xts,
    n.ahead = 1,
    # forecast.length = length(r_test),
    n.start = length(r_train),
    refit.every = 1, 
    refit.window = "moving", 
    window.size = window_size,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01, 0.025, 0.05),
    solver = "hybrid",
    keep.coef = FALSE
)


# Rolling window for GJR-GARCH
roll_gjr <- ugarchroll(
    spec = spec_gjr,
    data = r_p_xts,
    n.ahead = 1,
    # forecast.length = length(r_test),
    n.start = length(r_train),
    refit.every = 1, 
    refit.window = "moving", 
    window.size = window_size,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01, 0.025, 0.05),
    solver = "hybrid",
    keep.coef = FALSE
)

saveRDS(roll_sgarch, here("data/roll_sgarch.rds"))
saveRDS(roll_egarch, here("data/roll_egarch.rds"))
saveRDS(roll_gjr, here("data/roll_gjr.rds"))



#### One table -----------------------------------------------------------------

extract_var <- function(roll_object, model_name) {
    
    df <- as.data.frame(
        roll_object,
        which = "VaR"
    ) |>
        rownames_to_column("timestamp")
    
    tibble(
        timestamp = as.Date(df$timestamp),
        actual = df$realized,
        VaR_95 = df[["alpha(5%)"]],
        VaR_99 = df[["alpha(1%)"]],
        model = model_name
    )
}

var_df <- bind_rows(
    extract_var(roll_sgarch, "sGARCH"),
    extract_var(roll_egarch, "eGARCH"),
    extract_var(roll_gjr, "GJR-GARCH")
)

write_csv2(var_df, here("data/Var_one_table.csv"))
