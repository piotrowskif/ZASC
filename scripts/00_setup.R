#### Libraries -----------------------------------------------------------------

rm(list = ls())

library(crypto2)
library(tidyverse)
library(ggplot2)
library(here)
library(tseries) # adf.test, kpss.test
library(FinTS)
library(rugarch)
library(gt)
library(xts)

#### Functions -----------------------------------------------------------------

# Helper function to extract robust estimates and add significance stars
get_model_summary <- function(model_fit) {
    # Extract the matrix containing robust standard errors and p-values
    coef_mat <- model_fit@fit$robust.matcoef
    
    tibble(
        parameter = rownames(coef_mat),
        estimate = coef_mat[, " Estimate"],
        p_val = coef_mat[, "Pr(>|t|)"]
    ) |> 
        mutate(
            # Assign stars based on standard p-value thresholds
            stars = case_when(
                p_val < 0.01 ~ "***",
                p_val < 0.05 ~ "**",
                p_val < 0.10 ~ "*",
                TRUE ~ ""
            ),
            # Format strictly to 4 decimals and append stars
            formatted_est = paste0(sprintf("%.4f", estimate), stars)
        ) |> 
        select(parameter, formatted_est)
}
