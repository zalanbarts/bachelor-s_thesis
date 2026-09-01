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
# PRE-ANALYSIS: Sample Definition
# ------------------------------------------------------------------------------

# Global sample: June 1990 to May 2025
# Monthly returns are dated by their realization month.
fx_global <- fx_processed |>
  dplyr::filter(date >= as.Date("1990-06-30"))

# Number of currencies available through time
currency_counts <- fx_global |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    num_currencies = dplyr::n(),
    .groups = "drop"
  )

# Global signals:
# Require at least 20 available currencies for each signal in each month
fx_global_signals <- fx_global |> 
  tidyr::pivot_longer(
    cols = c(carry, mom1, mom3, mom6, mom12),
    names_to = "signal",
    values_to = "var"
  ) |> 
  tidyr::drop_na(var) |> 
  dplyr::group_by(date, signal) |>       
  dplyr::filter(dplyr::n() >= 20) |>     
  dplyr::ungroup() |> 
  dplyr::arrange(date, signal)

# Segmented DM/EM sample
# Retain the existing June 1996 starting point for now.
fx_filtered <- fx_processed |>
  dplyr::filter(date >= as.Date("1996-06-01"))

# Long-format signals for the segmented analysis.
# The N >= 10 condition is applied separately within DM and EM below.
fx_long_signals <- fx_filtered |> 
  tidyr::pivot_longer(
    cols = c(carry, mom1, mom3, mom6, mom12),
    names_to = "signal",
    values_to = "var"
  ) |> 
  tidyr::drop_na(var) |> 
  dplyr::arrange(date, signal)

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 1 & 2: Currency Counts and Descriptive Statistics
# ------------------------------------------------------------------------------

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

fx_filtered |>
  tidyr::pivot_longer(
    cols = c(carry, mom1, mom3, mom6, mom12),
    names_to = "signal",
    values_to = "var"
  ) |>
  tidyr::drop_na(var) |>
  dplyr::count(date, signal, market_group) |>
  dplyr::group_by(signal, market_group) |>
  dplyr::summarise(
    min_n = min(n),
    median_n = median(n),
    months_n10 = sum(n >= 10),
    months_n15 = sum(n >= 15),
    months_n20 = sum(n >= 20),
    .groups = "drop"
  )|>
  
  print(n = Inf, width = Inf)

# Segmented DM/EM sample
fx_filtered <- fx_processed |>
  dplyr::filter(date >= as.Date("1996-06-01"))
fx_long_signals <- fx_filtered |> 
  tidyr::pivot_longer(
    cols = c(carry, mom1, mom3, mom6, mom12),
    names_to = "signal",
    values_to = "var"
  ) |> 
  tidyr::drop_na(var) |> 
  dplyr::arrange(date, signal)

# Global Portfolios
# Full global sample from June 1990.
# A minimum of 20 currencies is required for each signal-month.
portfolios_global <- fx_processed |> 
  dplyr::filter(date >= as.Date("1990-06-30")) |>
  tidyr::pivot_longer(
    cols = c(carry, mom1, mom3, mom6, mom12),
    names_to = "signal",
    values_to = "var"
  ) |> 
  tidyr::drop_na(var) |> 
  dplyr::group_by(date, signal) |> 
  dplyr::filter(dplyr::n() >= 20) |>
  dplyr::ungroup() |> 
  dplyr::arrange(date, signal) |>
  multiple_portfolio_sorts(
    .variable = var,
    .n_portfolios = 5
  ) |> 
  multiple_hml(
    .n_portfolios = 5
  ) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(
    market_segment = "Global"
  )

# Developed Market (DM) Portfolios
# A minimum of 10 DM currencies is required for each signal-month.
portfolios_dm <- fx_long_signals |> 
  dplyr::filter(market_group == "DM") |>
  dplyr::group_by(date, signal) |>
  dplyr::filter(dplyr::n() >= 10) |>
  dplyr::ungroup() |>
  dplyr::arrange(date, signal) |>
  multiple_portfolio_sorts(
    .variable = var,
    .n_portfolios = 5
  ) |> 
  multiple_hml(
    .n_portfolios = 5
  ) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(
    market_segment = "DM"
  )

# Emerging Market (EM) Portfolios
# A minimum of 10 EM currencies is required for each signal-month.
portfolios_em <- fx_long_signals |> 
  dplyr::filter(market_group == "EM") |>
  dplyr::group_by(date, signal) |>
  dplyr::filter(dplyr::n() >= 10) |>
  dplyr::ungroup() |>
  dplyr::arrange(date, signal) |>
  multiple_portfolio_sorts(
    .variable = var,
    .n_portfolios = 5
  ) |> 
  multiple_hml(
    .n_portfolios = 5
  ) |> 
  rename_edge_portfolios() |> 
  dplyr::mutate(
    market_segment = "EM"
  )

# Combine Global, DM, and EM portfolios
final_portfolios <- dplyr::bind_rows(
  portfolios_global,
  portfolios_dm,
  portfolios_em
)

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
  dplyr::select(date,market_segment,strategy,ret_l) |>
  tidyr::pivot_wider(names_from = market_segment,values_from = ret_l ) |>
  dplyr::filter(!is.na(DM),!is.na(EM)) |>
  dplyr::mutate(Difference = DM - EM) |>
  dplyr::group_by(strategy) |>
  dplyr::summarise(
    
    # Annualised geometric return for DM
    dm_ann_ret = (exp(mean(DM, na.rm = TRUE) * 12) - 1) * 100,
    
    # Annualised geometric return for EM
    em_ann_ret = (exp(mean(EM, na.rm = TRUE) * 12) - 1) * 100,
    
    # Difference in annualised returns, in percentage points
    mean_diff = dm_ann_ret - em_ann_ret,
    
    # Paired test based on monthly log-return differences
    t_stat = stats::t.test(Difference)$statistic,
    p_value = stats::t.test(Difference)$p.value,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# PRE-ANALYSIS 9: Fama-MacBeth Regressions
# ------------------------------------------------------------------------------

# First-stage monthly cross-sectional regressions: Carry
fmb_carry_step1 <- fx_filtered |>
  dplyr::filter(
    market_group %in% c("DM", "EM"),
    !is.na(rl),
    !is.na(carry)
  ) |>
  dplyr::group_by(date, market_group) |>
  dplyr::filter(dplyr::n() >= 10) |>
  dplyr::summarise(
    model = list(stats::lm(rl ~ carry)),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    lambda_t = purrr::map_dbl(
      model,
      ~ stats::coef(.x)[["carry"]]
    )
  )

# First-stage monthly cross-sectional regressions: 1-month Momentum
fmb_mom1_step1 <- fx_filtered |>
  dplyr::filter(
    market_group %in% c("DM", "EM"),
    !is.na(rl),
    !is.na(mom1)
  ) |>
  dplyr::group_by(date, market_group) |>
  dplyr::filter(dplyr::n() >= 10) |>
  dplyr::summarise(
    model = list(stats::lm(rl ~ mom1)),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    lambda_t = purrr::map_dbl(
      model,
      ~ stats::coef(.x)[["mom1"]]
    )
  )

# Second-stage Fama-MacBeth estimates with Newey-West HAC inference
fmb_carry_results <- fmb_carry_step1 |>
  dplyr::group_by(market_group) |>
  dplyr::group_modify(~ fmb_nw_stats(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(strategy = "CS-Carry")

fmb_mom1_results <- fmb_mom1_step1 |>
  dplyr::group_by(market_group) |>
  dplyr::group_modify(~ fmb_nw_stats(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(strategy = "MOM-1M")

# Combined Fama-MacBeth results
fmb_results <- dplyr::bind_rows(
  fmb_carry_results,
  fmb_mom1_results
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
  fmb_results = fmb_results,
  drawdown_data = drawdown_data,
  max_dd_table = max_dd_table,
  correlation_results = correlation_results,
  em_start_date = em_start_date
)

saveRDS(thesis_data, "thesis_data.rds")
print("All 12 analyses processed flawlessly! Data saved as thesis_data.rds")