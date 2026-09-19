# ==============================================================================
# helpers.R
# Core mathematical and portfolio sorting functions
# ==============================================================================

# Reshapes price data and computes monthly long and short log returns net of transaction costs.
lustig_returns <- function(.data, .side = "side", .market = "mkt"){
  .data |> 
    tidyr::pivot_wider(names_from  = c({{ .market }}, {{ .side }}),
        names_sep   = ".",
        values_from = px) |> 
    dplyr::arrange(date, from) |>
    dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) |> 
    dplyr::group_by(date, from) |> 
    dplyr::slice_tail(n = 1) |> 
    dplyr::ungroup() |> 
    purrr::modify_if(
      .p = is.numeric,
      .f = base::log) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
        return_date = dplyr::lead(date),
        rl = fwd.bid - dplyr::lead(spot.ask),
        rs = -fwd.ask + dplyr::lead(spot.bid)
      ) |> 
    dplyr::ungroup() |> 
    tidyr::drop_na(rl, rs, return_date) |>
    dplyr::mutate(date = return_date) |>
    dplyr::select(-return_date)
  }

compute_signals <- function(.data){
  # Constructs the carry signal and momentum signals over 1, 3, 6, and 12 months.
  .data |> 
    dplyr::mutate(carry = fwd.bid - spot.ask) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      mom1  = dplyr::lag(rl),
      mom3  = slider::slide_dbl(.x = rl, .f = base::sum, .before = 2, .complete = TRUE) |> dplyr::lag(),
      mom6  = slider::slide_dbl(.x = rl, .f = base::sum, .before = 5, .complete = TRUE) |> dplyr::lag(),
      mom12 = slider::slide_dbl(.x = rl, .f = base::sum, .before = 11, .complete = TRUE) |> dplyr::lag()
    ) |> 
    dplyr::ungroup()
}

cs_logret <- function(.x){
  # Aggregates individual currency log returns into an equal-weighted cross-sectional log return.
  # THIS FIXES THE AGGREGATION MATH: log(mean(exp(x)))
  log(mean(exp(.x), na.rm = TRUE))
}

assign_portfolio <- function(.data, .variable, .n_portfolios) {
  # Determines quantile breakpoints for the selected sorting variable.
  breakpoints <- .data |>
    dplyr::pull({{ .variable }}) |>
    quantile(probs = seq(0, 1, length.out = .n_portfolios + 1), na.rm = TRUE, names = FALSE)

  # Assigns each currency to a portfolio according to these breakpoints. 
  .data |>
    dplyr::mutate(portfolio = findInterval(
      dplyr::pick(dplyr::everything()) |> dplyr::pull({{ .variable }}),
      breakpoints, all.inside = TRUE
    )) |>
    dplyr::pull(portfolio)
}

multiple_portfolio_sorts <- function(.data, .variable, .n_portfolios = 5){
  # Sorts currencies into portfolios by date and signal and calculates equal-weighted portfolio returns.
  .data |> 
    tidyr::drop_na({{ .variable }}) |> 
    dplyr::group_by(date, signal) |> 
    dplyr::mutate(
      portfolio = assign_portfolio(dplyr::pick(dplyr::everything()), {{ .variable }}, .n_portfolios),
      portfolio = base::as.factor(base::paste0("p", portfolio))
    ) |>
    dplyr::group_by(portfolio, date, signal) |>
    dplyr::summarize(
      ret_l = cs_logret(rl),
      ret_s = cs_logret(rs),
      .groups = "drop"
    ) |> 
    dplyr::arrange(date, signal, portfolio) |> 
    dplyr::mutate(strategy = base::paste0("cs_", signal)) |> 
    dplyr::select(-signal)
}

multiple_hml <- function(.data, .n_portfolios = 5) {
  # Constructs the HML strategy by combining long positions in the highest-ranked
  # portfolio with short positions in the lowest-ranked portfolio.
  
    high_id <- base::paste0("p", .n_portfolios)
  
  longs <- .data |> 
    dplyr::filter(portfolio == high_id) |> 
    dplyr::select(-portfolio) |> 
    dplyr::rename(ret_l_p5 = ret_l, ret_s_p5 = ret_s)
  
  .data |>
    dplyr::filter(portfolio == "p1") |> 
    dplyr::select(-portfolio) |> 
    dplyr::rename(ret_l_p1 = ret_l, ret_s_p1 = ret_s) |>
    dplyr::inner_join(longs, dplyr::join_by(date, strategy)) |>
    dplyr::mutate(
      ret_l = base::log1p(base::expm1(ret_l_p5) + base::expm1(ret_s_p1)),
      ret_s = base::log1p(base::expm1(ret_l_p1) + base::expm1(ret_s_p5)),
      portfolio = base::as.factor("hml")
    ) |>
    dplyr::select(date, strategy, portfolio, ret_l, ret_s) |> 
    dplyr::bind_rows(.data) |> 
    dplyr::arrange(date, strategy, portfolio)
}

rename_edge_portfolios <- function(.data){
  # Renames the lowest and highest portfolios as "short" and "long".
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::case_when(
        portfolio == "p1" ~ "short",
        portfolio == "p5" ~ "long",
        .default          = as.character(portfolio)
      )
    )
}

# Calculates annualised returns, volatility, and the Sharpe ratio from monthly log returns.
perf_stats <- function(.data, .ret = ret_l, ...){
  .data |> 
    dplyr::group_by(...) |> 
    dplyr::summarise(
      ann_ret = (exp(mean({{ .ret }}, na.rm = TRUE) * 12) - 1) * 100,
      ann_vol = sd(exp({{ .ret }}) - 1, na.rm = TRUE) * sqrt(12) * 100,
      sharpe = sqrt(12) *
        mean(exp({{ .ret }}) - 1, na.rm = TRUE) /
        sd(exp({{ .ret }}) - 1, na.rm = TRUE),
      .groups = "drop"
    )
}

# Computes the second-stage Fama-MacBeth statistics using
# Newey-West HAC standard errors for the time series of monthly slopes.
fmb_nw_stats <- function(df) {
  
  df <- df |>
    dplyr::arrange(date) |>
    dplyr::filter(!is.na(lambda_t))
  
  model <- stats::lm(lambda_t ~ 1, data = df)
  
  vcov_nw <- sandwich::NeweyWest(
    model,
    lag = 6,
    prewhite = FALSE
  )
  
  se <- sqrt(vcov_nw[1, 1])
  lambda <- unname(stats::coef(model)[1])
  t_stat <- lambda / se
  
  tibble::tibble(
    lambda = lambda,
    std_error = se,
    t_stat = t_stat,
    p_value = 2 * stats::pnorm(-abs(t_stat)),
    n_months = nrow(df),
    avg_n = mean(df$n_obs, na.rm = TRUE)
  )
}