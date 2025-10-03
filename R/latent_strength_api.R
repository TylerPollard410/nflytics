#' Stan file helper for latent strength models
#'
#' @param gq Whether to return the standalone generated quantities program.
#' @return Absolute path to the Stan file shipped with the package.
#' @export
latent_strength_stan_file <- function(gq = FALSE) {
  file_name <- if (isTRUE(gq)) {
    "latent_strength_ssm_gq.stan"
  } else {
    "latent_strength_ssm.stan"
  }

  path <- system.file("stan", file_name, package = "nflytics")
  if (!nzchar(path)) {
    stop(
      sprintf("Stan file '%s' is not available inside the package.", file_name),
      call. = FALSE
    )
  }
  path
}

#' Create Stan data for the latent strength model
#'
#' @param data Data frame containing (at minimum) columns for season, week,
#'   home and away teams, and home/away scores.
#' @param season_col Column name holding season identifiers.
#' @param week_col Column name holding within-season week numbers.
#' @param home_team_col Column name holding home-team abbreviations.
#' @param away_team_col Column name holding away-team abbreviations.
#' @param home_score_col Column name holding home scores.
#' @param away_score_col Column name holding away scores.
#' @param result_col Optional column containing signed game results (home minus
#'   away). When omitted the difference of the score columns is used.
#' @param total_col Optional column containing game totals. When omitted the sum
#'   of the score columns is used.
#' @param hfa_col Optional column indicating whether the listed home team gets
#'   home-field advantage. Accepts either logical indicators or the strings
#'   "Home"/"Away"/"Neutral". Defaults to treating every game as home-field.
#' @param teams Optional ordered vector of team identifiers. When supplied, the
#'   output will respect this ordering. When omitted the teams are inferred from
#'   the data and sorted alphabetically.
#' @param verbose Whether to print a short progress summary.
#'
#' @return A list ready to pass to Stan, with attributes storing the lookup
#'   tables used to encode teams, weeks, and seasons.
#' @export
prepare_latent_strength_data <- function(
  data,
  season_col = "season",
  week_col = "week",
  home_team_col = "home_team",
  away_team_col = "away_team",
  home_score_col = "home_score",
  away_score_col = "away_score",
  result_col = NULL,
  total_col = NULL,
  hfa_col = NULL,
  teams = NULL,
  verbose = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required <- c(
    season_col,
    week_col,
    home_team_col,
    away_team_col,
    home_score_col,
    away_score_col,
    result_col
  )
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols)) {
    stop(
      sprintf(
        "Missing required column(s) for Stan data: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df <- data[,
    unique(c(required, result_col, total_col, hfa_col)),
    drop = FALSE
  ]

  required_filter <- c(home_score_col, away_score_col)
  complete_required <- stats::complete.cases(df[,
    required_filter,
    drop = FALSE
  ])
  removed <- sum(!complete_required)
  if (removed > 0) {
    cli::cli_warn(c(
      "Dropping rows with missing values in required columns before building Stan data.",
      "i" = sprintf("Rows removed: %s", removed)
    ))
    df <- df[complete_required, , drop = FALSE]
  }

  if (!nrow(df)) {
    stop(
      "After removing rows with missing required values no data remains for Stan.",
      call. = FALSE
    )
  }

  df <- dplyr::mutate(
    df,
    season = as.integer(df[[season_col]]),
    week = as.integer(df[[week_col]]),
    home_team = as.character(df[[home_team_col]]),
    away_team = as.character(df[[away_team_col]]),
    home_score = as.integer(df[[home_score_col]]),
    away_score = as.integer(df[[away_score_col]])
  )

  if (!is.null(result_col) && result_col %in% names(df)) {
    df$result <- as.integer(df[[result_col]])
  } else {
    df$result <- df$home_score - df$away_score
  }

  if (!is.null(total_col) && total_col %in% names(df)) {
    df$total <- as.integer(df[[total_col]])
  } else {
    df$total <- df$home_score + df$away_score
  }

  if (!is.null(hfa_col) && hfa_col %in% colnames(data)) {
    hfa_raw <- data[[hfa_col]]
    if (is.logical(hfa_raw)) {
      df$hfa <- as.integer(hfa_raw)
    } else if (is.numeric(hfa_raw)) {
      df$hfa <- as.integer(hfa_raw)
    } else {
      df$hfa <- ifelse(
        tolower(as.character(hfa_raw)) %in% c("home", "h", "1"),
        1L,
        0L
      )
    }
  } else {
    df$hfa <- 1L
  }

  if (is.null(teams)) {
    teams <- sort(unique(c(df$home_team, df$away_team)))
  }
  team_lookup <- tibble::tibble(team = teams, team_idx = seq_along(teams))

  df <- df |>
    dplyr::arrange(season, week) |>
    dplyr::mutate(
      home_idx = team_lookup$team_idx[match(home_team, team_lookup$team)],
      away_idx = team_lookup$team_idx[match(away_team, team_lookup$team)]
    )

  if (anyNA(df$home_idx) || anyNA(df$away_idx)) {
    stop(
      "Every team must appear in `teams`; consider supplying the `teams` argument.",
      call. = FALSE
    )
  }

  season_lookup <- df |>
    dplyr::distinct(season) |>
    dplyr::arrange(season) |>
    dplyr::mutate(season_idx = dplyr::row_number())

  df <- df |>
    dplyr::left_join(season_lookup, by = "season")

  week_lookup <- df |>
    dplyr::distinct(season_idx, season, week) |>
    dplyr::arrange(season_idx, week) |>
    dplyr::mutate(week_idx = dplyr::row_number())

  df <- df |>
    dplyr::left_join(week_lookup, by = c("season", "season_idx", "week"))

  week_summary <- df |>
    dplyr::group_by(season_idx) |>
    dplyr::summarise(
      first_week = min(week_idx),
      last_week = max(week_idx),
      .groups = "drop"
    )

  df <- df |>
    dplyr::left_join(week_summary, by = "season_idx") |>
    dplyr::mutate(
      fw_season_idx = as.integer(week_idx == first_week),
      lw_season_idx = as.integer(week_idx == last_week)
    )

  stan_data <- list(
    N_games = nrow(df),
    N_teams = nrow(team_lookup),
    N_seasons = max(df$season_idx),
    N_weeks = max(df$week_idx),
    home_team = as.integer(df$home_idx),
    away_team = as.integer(df$away_idx),
    season_idx = as.integer(df$season_idx),
    week_idx = as.integer(df$week_idx),
    fw_season_idx = as.integer(df$fw_season_idx),
    lw_season_idx = as.integer(df$lw_season_idx),
    hfa = as.integer(df$hfa),
    home_score = as.integer(df$home_score),
    away_score = as.integer(df$away_score),
    total = as.integer(df$total),
    result = as.integer(df$result)
  )

  attr(stan_data, "teams") <- team_lookup
  attr(stan_data, "seasons") <- season_lookup
  attr(stan_data, "weeks") <- week_lookup
  attr(stan_data, "source_n") <- nrow(df)

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "Preparing Stan data",
      "i" = sprintf("Games: %s", stan_data$N_games),
      "i" = sprintf("Teams: %s", stan_data$N_teams),
      "i" = sprintf("Seasons: %s", stan_data$N_seasons),
      "i" = sprintf("Weeks: %s", stan_data$N_weeks)
    ))
  }

  class(stan_data) <- c("nflytics_stan_data", class(stan_data))
  stan_data
}

#' Fit the latent strength model using Stan
#'
#' @param data Either a data frame that can be passed to
#'   [prepare_latent_strength_data()] or a pre-built Stan data list.
#' @param engine Sampling backend to use. Choose between `"rstan"` and
#'   `"cmdstanr"`.
#' @param iter_warmup Number of warmup iterations.
#' @param iter_sampling Number of post-warmup iterations.
#' @param chains Number of Markov chains.
#' @param parallel_chains Number of chains to run in parallel (CmdStanR only).
#' @param seed Optional seed passed to the sampler.
#' @param refresh Print frequency for sampler progress.
#' @param adapt_delta, max_treedepth HMC tuning parameters.
#' @param init Initialisation strategy passed to Stan.
#' @param ... Additional arguments forwarded to the underlying sampler.
#'
#' @return An object of class `nflytics_fit` containing the fitted draws and
#'   data used for sampling.
#' @export
fit_latent_strength <- function(
  data,
  engine = c("rstan", "cmdstanr"),
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = min(
    chains,
    max(1, parallel::detectCores(logical = FALSE) - 1)
  ),
  seed = NULL,
  refresh = 50,
  adapt_delta = 0.9,
  max_treedepth = 12,
  init = 0,
  ...
) {
  stan_data <- if (is.data.frame(data)) {
    prepare_latent_strength_data(data, verbose = FALSE)
  } else {
    data
  }

  if (!is.list(stan_data) || is.null(stan_data$N_games)) {
    stop(
      "`data` must be either a data frame or a Stan data list created by prepare_latent_strength_data().",
      call. = FALSE
    )
  }

  engine <- match.arg(engine)
  seed <- if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else seed

  if (identical(engine, "rstan")) {
    control <- list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
    fit <- rstan::sampling(
      stanmodels$latent_strength_ssm,
      data = stan_data,
      chains = chains,
      iter = iter_warmup + iter_sampling,
      warmup = iter_warmup,
      seed = seed,
      init = init,
      refresh = refresh,
      control = control,
      ...
    )
    draws <- posterior::as_draws_array(fit)
  } else {
    mod <- cmdstanr::cmdstan_model(latent_strength_stan_file())
    fit <- mod$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      seed = seed,
      refresh = refresh,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      init = init,
      ...
    )
    draws <- posterior::as_draws_array(fit$draws())
  }

  out <- list(
    engine = engine,
    fit = fit,
    draws = draws,
    data = stan_data,
    metadata = list(
      seed = seed,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      chains = chains,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      stan_file = latent_strength_stan_file(),
      gq_file = latent_strength_stan_file(TRUE)
    ),
    lookup = list(
      teams = attr(stan_data, "teams"),
      seasons = attr(stan_data, "seasons"),
      weeks = attr(stan_data, "weeks")
    )
  )
  class(out) <- c("nflytics_fit", class(fit))
  out
}

#' Run generated quantities for the latent strength model
#'
#' @description
#' Executes the standalone generated-quantities program
#' `latent_strength_ssm_gq.stan` using an existing `nflytics_fit` so that
#' out-of-sample forecasts can be produced without refitting the core model.
#'
#' @param object An `nflytics_fit` created by [fit_latent_strength()]. Only
#'   fits obtained with `engine = "cmdstanr"` are supported.
#' @param schedule_idx Data frame describing the game schedule with the index
#'   columns expected by [prepare_schedule_indices()], including (at minimum)
#'   `season_idx`, `week_idx`, `fw_season_idx`, `lw_season_idx`, `home_idx`,
#'   `away_idx`, and `hfa`. Additional descriptive columns such as
#'   `season`, `week`, `home_team`, and `away_team` are preserved and attached
#'   to the result for downstream summaries.
#' @param targets Vector of global `week_idx` values identifying the games to
#'   forecast, or a data frame with columns `season` and `week` that can be
#'   matched against `schedule_idx`. When `NULL`, supply `horizon` to request
#'   the next `horizon` weeks automatically.
#' @param horizon Optional integer specifying how many weeks beyond the fitted
#'   window to score. Ignored when `targets` is supplied.
#' @param gq_model Optional pre-compiled CmdStan model returned by
#'   [cmdstanr::cmdstan_model()]. When omitted the helper compiles (or reuses)
#'   `latent_strength_ssm_gq.stan`.
#' @param seed Optional RNG seed forwarded to
#'   [cmdstanr::generate_quantities()]. Defaults to the seed stored on the fit
#'   object when available.
#' @param parallel_chains Optional number of parallel chains used by
#'   `generate_quantities()`. By default the function reuses the number of fit
#'   chains (bounded by the detected core count).
#' @param sig_figs Optional significant-figure setting passed to
#'   `generate_quantities()`.
#' @param refresh Frequency for CmdStan progress output. Defaults to `0` (no
#'   progress messages).
#' @param ... Additional arguments forwarded to
#'   [cmdstanr::generate_quantities()].
#'
#' @return A `CmdStanGQ` object. The result is tagged with the schedule rows
#'   used for scoring (`attr(, "oos_schedule")`), the resolved `week_idx`
#'   targets (`attr(, "targets")`), and the full data list supplied to Stan
#'   (`attr(, "gq_data")`).
#' @export
gq_latent_strength <- function(
  object,
  schedule_idx,
  targets = NULL,
  horizon = NULL,
  gq_model = NULL,
  seed = NULL,
  parallel_chains = NULL,
  sig_figs = NULL,
  refresh = 0,
  ...
) {
  if (!inherits(object, "nflytics_fit")) {
    stop("Object is not an nflytics_fit result.", call. = FALSE)
  }
  if (!identical(object$engine, "cmdstanr")) {
    stop(
      "latent_strength_generate_quantities() currently requires engine = 'cmdstanr'.",
      call. = FALSE
    )
  }
  if (missing(schedule_idx) || !is.data.frame(schedule_idx)) {
    stop(
      "`schedule_idx` must be a data frame produced by prepare_schedule_indices().",
      call. = FALSE
    )
  }

  required_cols <- c(
    "season_idx",
    "week_idx",
    "fw_season_idx",
    "lw_season_idx",
    "home_idx",
    "away_idx",
    "hfa"
  )
  missing_cols <- setdiff(required_cols, colnames(schedule_idx))
  if (length(missing_cols)) {
    stop(
      sprintf(
        "`schedule_idx` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(targets) && !is.null(horizon)) {
    stop("Supply either `targets` or `horizon`, not both.", call. = FALSE)
  }

  if (is.null(targets)) {
    if (is.null(horizon)) {
      stop("Provide either `targets` or `horizon`.", call. = FALSE)
    }
    targets <- next_week_targets(object$data, horizon = horizon)
  } else if (is.data.frame(targets)) {
    if (!all(c("season", "week") %in% names(targets))) {
      stop(
        "When `targets` is a data frame it must contain `season` and `week` columns.",
        call. = FALSE
      )
    }
    lookup <- unique(schedule_idx[, c("season", "week", "week_idx")])
    merged <- merge(
      unique(targets[, c("season", "week"), drop = FALSE]),
      lookup,
      by = c("season", "week"),
      all.x = TRUE,
      sort = FALSE
    )
    if (anyNA(merged$week_idx)) {
      missing_rows <- merged[
        is.na(merged$week_idx),
        c("season", "week"),
        drop = FALSE
      ]
      stop(
        sprintf(
          "Unable to find week_idx values for %s.",
          paste(
            paste(missing_rows$season, missing_rows$week, sep = " week "),
            collapse = "; "
          )
        ),
        call. = FALSE
      )
    }
    targets <- merged$week_idx
  }

  targets <- sort(unique(as.integer(targets)))
  if (!length(targets)) {
    stop("No target weeks supplied for generated quantities.", call. = FALSE)
  }

  oos_subset <- schedule_idx[schedule_idx$week_idx %in% targets, , drop = FALSE]
  oos_subset <- oos_subset[order(oos_subset$week_idx), , drop = FALSE]
  if (!nrow(oos_subset)) {
    stop(
      "No schedule rows matched the requested targets. Check `schedule_idx` and `targets`.",
      call. = FALSE
    )
  }

  gq_data <- prepare_gq_data(object$data, schedule_idx, targets)

  if (is.null(gq_model)) {
    gq_model <- cmdstanr::cmdstan_model(latent_strength_stan_file(TRUE))
  } else if (!inherits(gq_model, "CmdStanModel")) {
    stop("`gq_model` must be a CmdStanModel object.", call. = FALSE)
  }

  if (is.null(seed) && !is.null(object$metadata$seed)) {
    seed <- object$metadata$seed
  }
  if (!is.null(seed)) {
    seed <- as.integer(seed)
  }

  if (is.null(parallel_chains)) {
    chains <- object$metadata$chains
    if (is.null(chains) && inherits(object$fit, "CmdStanMCMC")) {
      chains <- object$fit$num_chains()
    }
    if (is.null(chains)) {
      chains <- 4L
    }
    cores <- try(parallel::detectCores(), silent = TRUE)
    if (inherits(cores, "try-error") || is.na(cores)) {
      cores <- chains
    }
    parallel_chains <- max(1L, min(as.integer(chains), as.integer(cores)))
  } else {
    parallel_chains <- as.integer(parallel_chains)
  }

  args <- list(
    fitted_params = object$fit,
    data = gq_data,
    parallel_chains = parallel_chains,
    refresh = refresh
  )
  if (!is.null(seed)) {
    args$seed <- seed
  }
  if (!is.null(sig_figs)) {
    args$sig_figs <- as.integer(sig_figs)
  }
  extra <- list(...)
  if (length(extra)) {
    args <- c(args, extra)
  }

  gq_obj <- do.call(gq_model$generate_quantities, args)
  attr(gq_obj, "targets") <- targets
  attr(gq_obj, "oos_schedule") <- oos_subset
  attr(gq_obj, "gq_data") <- gq_data
  gq_obj
}

#' Print method for nflytics fits
#' @export
print.nflytics_fit <- function(x, ...) {
  cat("nflytics latent strength fit\n")
  cat(sprintf("  Engine: %s\n", x$engine))
  cat(sprintf("  Chains: %s\n", x$metadata$chains))
  cat(sprintf(
    "  Iterations: warmup %s / sampling %s\n",
    x$metadata$iter_warmup,
    x$metadata$iter_sampling
  ))
  cat(sprintf("  Games: %s\n", x$data$N_games))
  if (identical(x$engine, "rstan")) {
    cat("\nStan summary (first 5 parameters):\n")
    summ <- rstan::summary(x$fit)$summary
    print(utils::head(summ, 5))
  } else if (inherits(x$fit, "CmdStanMCMC")) {
    cat("\nCmdStan summary (first 5 parameters):\n")
    x$fit$print(...)
  }
  invisible(x)
}

#' Extract the underlying stanfit object
#'
#' @param object An `nflytics_fit` object.
#' @return A `stanfit` object when the model was fit with rstan.
#' @export
as_stanfit <- function(object, ...) {
  UseMethod("as_stanfit")
}

#' @export
as_stanfit.nflytics_fit <- function(object, ...) {
  if (!inherits(object, "nflytics_fit")) {
    stop("Object is not an nflytics_fit result.", call. = FALSE)
  }
  if (!identical(object$engine, "rstan")) {
    stop(
      "The underlying stanfit is only available when fitting with engine = 'rstan'.",
      call. = FALSE
    )
  }
  object$fit
}

#' Convert fitted draws to posterior formats
#'
#' @param object An `nflytics_fit` object.
#' @param format Output format recognised by the posterior package.
#' @export
nflytics_draws <- function(
  object,
  format = c("array", "df", "matrix", "list", "rvar")
) {
  format <- match.arg(format)
  if (!inherits(object, "nflytics_fit")) {
    stop("Object is not an nflytics_fit result.", call. = FALSE)
  }
  switch(
    format,
    array = object$draws,
    df = posterior::as_draws_df(object$draws),
    matrix = posterior::as_draws_matrix(object$draws),
    list = posterior::as_draws_list(object$draws),
    rvar = posterior::as_draws_rvars(object$draws)
  )
}

#' Posterior trajectory summary for latent team strength
#'
#' @param object An `nflytics_fit` object.
#' @param teams Optional character vector limiting the output to specific team
#'   abbreviations.
#' @param seasons Optional integer vector restricting the output to specific
#'   seasons.
#' @param weeks Optional integer vector restricting the output to specific week
#'   numbers (within the retained seasons).
#' @param draws_format Format for the returned draws. Supported options are
#'   `"df"`, `"matrix"`, `"array"`, and `"list"`. Defaults to `"df"`.
#'
#' @return A filtered draws object of the requested format. The result is tagged
#'   with `trajectory_lookup`, a tibble mapping parameter names to
#'   `team`/`season`/`week`, and `trajectory_filters`, the original filters that
#'   were applied.
#' @export
posterior_trajectory <- function(object, ...) {
  UseMethod("posterior_trajectory")
}

#' @export
posterior_trajectory.nflytics_fit <- function(
  object,
  teams = NULL,
  seasons = NULL,
  weeks = NULL,
  draws_format = c("df", "matrix", "array", "list"),
  ...
) {
  if (!inherits(object, "nflytics_fit")) {
    stop("Object is not an nflytics_fit result.", call. = FALSE)
  }

  lookup_teams <- object$lookup$teams
  lookup_weeks <- object$lookup$weeks

  team_lookup <- lookup_teams
  if (!is.null(teams)) {
    team_lookup <- team_lookup |> dplyr::filter(team %in% teams)
  }
  if (!nrow(team_lookup)) {
    stop("No teams remain after filtering; check `teams` input.", call. = FALSE)
  }
  team_lookup <- team_lookup |> dplyr::mutate(team_pos = dplyr::row_number())

  week_lookup <- lookup_weeks
  if (!is.null(seasons)) {
    week_lookup <- week_lookup |> dplyr::filter(season %in% seasons)
  }
  if (!is.null(weeks)) {
    week_lookup <- week_lookup |> dplyr::filter(week %in% weeks)
  }
  if (!nrow(week_lookup)) {
    stop(
      "No weeks remain after filtering; check `seasons`/`weeks` inputs.",
      call. = FALSE
    )
  }
  week_lookup <- week_lookup |>
    dplyr::arrange(season_idx, week_idx) |>
    dplyr::mutate(week_pos = dplyr::row_number())

  week_grid <- week_lookup |> dplyr::select(week_idx, week_pos, season, week)
  team_grid <- team_lookup |> dplyr::select(team_idx, team_pos, team)

  var_grid <- tidyr::crossing(week_grid, team_grid) |>
    dplyr::mutate(
      variable_orig = sprintf("team_strength[%s,%s]", week_idx, team_idx),
      variable = sprintf("team_strength[%s,%s]", week_pos, team_pos)
    )

  draws_array <- posterior::subset_draws(
    object$draws,
    variable = unique(var_grid$variable_orig)
  )

  variables_orig <- posterior::variables(draws_array)
  mapping <- var_grid |>
    dplyr::filter(variable_orig %in% variables_orig)
  mapping <- mapping[
    match(variables_orig, mapping$variable_orig),
    ,
    drop = FALSE
  ]

  rename_args <- as.list(stats::setNames(
    mapping$variable_orig,
    mapping$variable
  ))
  draws_array <- do.call(
    posterior::rename_variables,
    c(list(draws_array), rename_args)
  )

  draw_fmt <- match.arg(draws_format)
  out <- switch(
    draw_fmt,
    df = posterior::as_draws_df(draws_array),
    matrix = posterior::as_draws_matrix(draws_array),
    array = posterior::as_draws_array(draws_array),
    list = posterior::as_draws_list(draws_array)
  )

  lookup_tbl <- mapping |>
    dplyr::mutate(variable = as.character(variable)) |>
    dplyr::select(
      variable,
      variable_orig,
      team,
      team_idx,
      team_pos,
      season,
      week,
      week_idx,
      week_pos
    )

  attr(out, "trajectory_lookup") <- lookup_tbl
  attr(out, "trajectory_filters") <- list(
    teams = teams,
    seasons = seasons,
    weeks = weeks
  )

  out
}
