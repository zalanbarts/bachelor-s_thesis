# ==============================================================================
# helpers.R
# Core mathematical and portfolio sorting functions
# ==============================================================================

lustig_returns <- function(.data, .side = "side", .market = "mkt"){
  # Computes the long and short log returns (rl and rs) net of transaction costs.
  .data |> 
    tidyr::pivot_wider(
      names_from  = c({{ .market }}, {{ .side }}),
      names_sep   = ".",
      values_from = px
    ) |> 
    dplyr::arrange(date, from) |> 
    dplyr::mutate(
      date = lubridate::ceiling_date(date, unit = "month") - 1
    ) |> 
    dplyr::group_by(date, from) |> 
    dplyr::slice_tail(n = 1) |> 
    dplyr::ungroup() |> 
    purrr::modify_if(.p = is.numeric, .f = base::log) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      rl = fwd.bid - dplyr::lead(spot.ask),
      rs = -fwd.ask + dplyr::lead(spot.bid)
    ) |> 
    dplyr::ungroup() |> 
    tidyr::drop_na(rl, rs)
}

compute_signals <- function(.data){
  # Computes Carry and Momentum (1, 3, 6, 12 months) signals.
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
  # Computes the cross-sectional return for a vector, returning it as a log return.
  # THIS FIXES THE AGGREGATION MATH: log(mean(exp(x)))
  log(mean(exp(.x), na.rm = TRUE))
}

assign_portfolio <- function(.data, .variable, .n_portfolios) {
  # Computes breakpoints and sorts currencies into portfolios.
  breakpoints <- .data |>
    dplyr::pull({{ .variable }}) |>
    quantile(probs = seq(0, 1, length.out = .n_portfolios + 1), na.rm = TRUE, names = FALSE)
  
  .data |>
    dplyr::mutate(portfolio = findInterval(
      dplyr::pick(dplyr::everything()) |> dplyr::pull({{ .variable }}),
      breakpoints, all.inside = TRUE
    )) |>
    dplyr::pull(portfolio)
}

multiple_portfolio_sorts <- function(.data, .variable, .n_portfolios = 5){
  # Sorts into portfolios and computes equal-weighted returns per portfolio.
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
  # Computes the High-Minus-Low (HML) portfolio from sorted portfolios.
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
      ret_l = ret_l_p5 + ret_s_p1,
      ret_s = ret_l_p1 + ret_s_p5,
      portfolio = base::as.factor("hml")
    ) |>
    dplyr::select(date, strategy, portfolio, ret_l, ret_s) |> 
    dplyr::bind_rows(.data) |> 
    dplyr::arrange(date, strategy, portfolio)
}

rename_edge_portfolios <- function(.data){
  # Renames the extreme portfolios for cleaner presentation.
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::case_when(
        portfolio == "p1" ~ "short",
        portfolio == "p5" ~ "long",
        .default          = as.character(portfolio)
      )
    )
}

perf_stats <- function(.data, .ret = ret_l, ...){
  # Computes annualized simple returns (%), volatility (%), and Sharpe ratio correctly from log returns.
  .data |> 
    dplyr::group_by(...) |> 
    dplyr::summarise(
      ann_ret = (base::exp(base::mean({{ .ret }}, na.rm = TRUE) * 12) - 1) * 100,
      ann_vol = stats::sd(base::exp({{ .ret }}) - 1, na.rm = TRUE) * base::sqrt(12) * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(sharpe = ann_ret / ann_vol) |> 
    purrr::modify_if(base::is.numeric, ~ base::round(.x, 2)) |> 
    dplyr::arrange(dplyr::desc(sharpe))
}