# ==============================================================================
# main.R
# The primary engine that processes data and pre-calculates all 12 analyses
# ==============================================================================

# 1. Load the helper scripts
source("R/startup.R")
source("R/cleaners.R")
source("R/helpers.R")

# 2. Load and clean the raw data
raw_data <- readRDS("all_outright.rds")

fx_processed <- raw_data |> 
  lustig_returns(.side = "side", .market = "mkt") |> 
  compute_signals() |> 
  assign_market_groups()

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 1 & 2: Currency Counts and Descriptive Statistics
# ------------------------------------------------------------------------------
currency_counts <- fx_processed |>
  dplyr::filter(date >= as.Date("1990-05-01")) |>
  dplyr::group_by(date) |>
  dplyr::summarise(num_currencies = dplyr::n(), .groups = "drop")

# Extract the absolute first raw spot observation date
raw_starts <- raw_data |>
  dplyr::group_by(from) |>
  dplyr::summarise(raw_start_date = min(date), .groups = "drop")

# Build the summary table and join the raw start dates
currency_summary_stats <- fx_processed |>
  dplyr::group_by(market_group, from) |>
  dplyr::summarise(
    end_date   = max(date),
    obs_count  = dplyr::n(),
    mean_ret   = (exp(mean(rl, na.rm = TRUE) * 12) - 1) * 100, 
    volatility = sd(expm1(rl), na.rm = TRUE) * sqrt(12) * 100,
    .groups    = "drop"
  ) |>
  dplyr::left_join(raw_starts, by = "from") |>
  dplyr::select(market_group, from, start_date = raw_start_date, end_date, obs_count, mean_ret, volatility) |>
  dplyr::arrange(market_group, from) # This handles the alphabetical sorting
# ------------------------------------------------------------------------------
# PRE-ANALYSIS 3, 4 & 5: Portfolio Sorting (Global, DM, EM)
# ------------------------------------------------------------------------------
fx_filtered <- fx_processed |> dplyr::filter(date >= as.Date("1996-06-01"))

fx_long_signals <- fx_filtered |> 
  tidyr::pivot_longer(cols = c(carry, mom1, mom3, mom6, mom12), names_to = "signal", values_to = "var") |> 
  tidyr::drop_na(var) |> 
  dplyr::group_by(date, signal) |>       
  dplyr::filter(dplyr::n() >= 20) |>     
  dplyr::ungroup() |>                    
  dplyr::arrange(date, signal)

# Global Portfolios (Using the full sample starting in 1990)
portfolios_global <- fx_processed |> 
  tidyr::pivot_longer(cols = c(carry, mom1, mom3, mom6, mom12), names_to = "signal", values_to = "var") |> 
  tidyr::drop_na(var) |> 
  dplyr::group_by(date, signal) |> 
  dplyr::filter(dplyr::n() >= 20) |> # The N >= 20 rule
  dplyr::ungroup() |> 
  dplyr::arrange(date, signal) |>
  multiple_portfolio_sorts(.variable = var, .n_portfolios = 5) |> 
  multiple_hml(.n_portfolios = 5) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(market_segment = "Global")

# Developed Market (DM) Portfolios
portfolios_dm <- fx_long_signals |> 
  dplyr::filter(market_group == "DM") |> 
  multiple_portfolio_sorts(.variable = var, .n_portfolios = 5) |> 
  multiple_hml(.n_portfolios = 5) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(market_segment = "DM")

# Emerging Market (EM) Portfolios
portfolios_em <- fx_long_signals |> 
  dplyr::filter(market_group == "EM") |> 
  multiple_portfolio_sorts(.variable = var, .n_portfolios = 5) |> 
  multiple_hml(.n_portfolios = 5) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(market_segment = "EM")

final_portfolios <- dplyr::bind_rows(portfolios_global, portfolios_dm, portfolios_em)

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 6: Time Alignment & Performance Metrics
# ------------------------------------------------------------------------------
em_start_date <- final_portfolios |>
  dplyr::filter(market_segment == "EM", portfolio == "hml", !is.na(ret_l)) |>
  dplyr::pull(date) |> min()

aligned_portfolios <- final_portfolios |>
  dplyr::filter(date >= em_start_date, market_segment %in% c("DM", "EM"), portfolio == "hml")

agg_performance <- final_portfolios |> 
  dplyr::filter(market_segment == "Global", portfolio == "hml") |> 
  perf_stats(.ret = ret_l, strategy)

aligned_performance <- aligned_portfolios |>
  perf_stats(.ret = ret_l, market_segment, strategy)

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 7 & 8: T-Tests
# ------------------------------------------------------------------------------
t_test_results <- aligned_portfolios |>
  dplyr::group_by(market_segment, strategy) |>
  dplyr::summarise(
    t_stat = t.test(ret_l)$statistic,
    p_value = t.test(ret_l)$p.value,
    .groups = "drop"
  )

dm_em_diff_ttest <- aligned_portfolios |>
  dplyr::select(date, market_segment, strategy, ret_l) |>
  tidyr::pivot_wider(names_from = market_segment, values_from = ret_l) |>
  dplyr::mutate(Difference = DM - EM) |>
  dplyr::group_by(strategy) |>
  dplyr::summarise(
    mean_diff = mean(Difference, na.rm = TRUE) * 12 * 100,
    t_stat = t.test(Difference)$statistic,
    p_value = t.test(Difference)$p.value,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 9: Fama-MacBeth Regressions
# ------------------------------------------------------------------------------
fmb_step1 <- fx_filtered |>
  dplyr::filter(!is.na(rl) & !is.na(carry) & !is.na(mom1) & market_group %in% c("DM", "EM")) |>
  dplyr::group_by(date, market_group) |>
  dplyr::filter(dplyr::n() >= 10) |>
  dplyr::summarise(
    carry_model = list(lm(rl ~ carry)),
    mom1_model  = list(lm(rl ~ mom1)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    carry_gamma_t = purrr::map_dbl(carry_model, ~ coef(.x)["carry"]),
    mom1_gamma_t  = purrr::map_dbl(mom1_model, ~ coef(.x)["mom1"])
  )

fmb_step2_results <- fmb_step1 |>
  dplyr::group_by(market_group) |>
  dplyr::summarise(
    carry_fmb_gamma = mean(carry_gamma_t, na.rm = TRUE),
    carry_t_stat    = t.test(carry_gamma_t)$statistic,
    carry_p_value   = t.test(carry_gamma_t)$p.value,
    mom1_fmb_gamma  = mean(mom1_gamma_t, na.rm = TRUE),
    mom1_t_stat     = t.test(mom1_gamma_t)$statistic,
    mom1_p_value    = t.test(mom1_gamma_t)$p.value,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 10 & 11: Maximum Drawdowns & Underwater Data
# ------------------------------------------------------------------------------
drawdown_data <- aligned_portfolios |>
  dplyr::group_by(market_segment, strategy) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    simple_ret = expm1(ret_l),
    simple_ret = tidyr::replace_na(simple_ret, 0),
    equity_curve = cumprod(1 + simple_ret),
    rolling_max = cummax(equity_curve),
    drawdown = (equity_curve / rolling_max) - 1
  ) |>
  dplyr::ungroup()

max_dd_table <- drawdown_data |>
  dplyr::group_by(market_segment, strategy) |>
  dplyr::summarise(max_drawdown = min(drawdown) * 100, .groups = "drop") |>
  dplyr::arrange(market_segment, max_drawdown)

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 12: Correlation Matrix (Full Cross-Market)
# ------------------------------------------------------------------------------
correlation_results <- aligned_portfolios |>
  dplyr::select(date, market_segment, strategy, ret_l) |>
  # This creates 10 columns like "DM_cs_carry", "EM_cs_mom1", etc.
  tidyr::pivot_wider(
    names_from = c(market_segment, strategy), 
    names_sep = "_", 
    values_from = ret_l
  ) |>
  dplyr::select(-date) |> # Remove date before calculating correlation
  cor(use = "pairwise.complete.obs") |>
  base::as.data.frame() |>
  tibble::rownames_to_column(var = "Strategy")

# ------------------------------------------------------------------------------
# SAVE ALL DATA TO A SINGLE LIST FOR THE R MARKDOWN
# ------------------------------------------------------------------------------
thesis_data <- list(
  currency_counts = currency_counts,
  currency_summary_stats = currency_summary_stats,
  portfolios_global = final_portfolios |> dplyr::filter(market_segment == "Global"),
  aligned_portfolios = aligned_portfolios,
  agg_performance = agg_performance,
  aligned_performance = aligned_performance,
  t_test_results = t_test_results,
  dm_em_diff_ttest = dm_em_diff_ttest,
  fmb_step2_results = fmb_step2_results,
  drawdown_data = drawdown_data,
  max_dd_table = max_dd_table,
  correlation_results = correlation_results,
  em_start_date = em_start_date
)

saveRDS(thesis_data, "thesis_data.rds")
print("All 12 analyses processed flawlessly! Data saved as thesis_data.rds")