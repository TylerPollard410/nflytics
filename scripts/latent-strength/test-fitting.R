# Packages ----
library(magrittr)
library(tictoc)
library(plotly)
library(smplot2)
library(patchwork)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(Metrics)
library(broom.mixed)
library(tidybayes)

library(nflverse)
library(tidyverse)

library(nflendzonePipeline)
library(nflendzone)
#library(nflytics)

devtools::load_all()

# 1. Gather data ----
teams_data <- load_teams(current = TRUE)
teams <- teams_data$team_abbr
all_seasons <- 2002:get_current_season()
games <- nflendzone::load_game_data(seasons = all_seasons)
games_fit <- bind_rows(
  games |> filter(season < 2006),
  games |> filter(season == 2006, week < 10)
) |>
  distinct()

# 2. Prepare data for Stan model ----
stan_data <- prepare_latent_strength_data(games_fit)
str(stan_data, max.level = 1)
attr(stan_data, "teams")
attr(stan_data, "seasons")

# 3. Sampling the Latent Strength Model ----
fit <- fit_latent_strength(
  stan_data,
  engine = "cmdstanr", # rstan
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 500,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 10,
  refresh = 100,
  seed = 52,
  sig_figs = 10
)
fit

schedule_idx <- prepare_schedule_indices(
  seasons = unique(attr(fit$lookup$seasons, "data")$season),
  teams = fit$lookup$teams$team
)
gq <- gq_latent_strength(
  fit,
  schedule_idx,
  horizon = 1
)

# 4. Accessing Posterior Draws ----
posterior <- nflytics_draws(fit, format = "df")
head(posterior)

# 5. Posterior Predictive Checks ----
# Density overlay for two scale parameters
bayesplot::mcmc_dens_overlay(
  posterior,
  pars = c("sigma_obs", "sigma_team_strength_init")
)

# Histogram for league-level persistence
bayesplot::mcmc_hist(
  posterior,
  pars = "phi_league_hfa"
)

# 6. Posterior Team Strength Trajectories ----
filtered_idx <- filter_seasons(
  fit$data,
  after_season = 2006
) |>
  distinct()

trajectory_draws <- posterior_trajectory(
  fit,
  seasons = filtered_idx$season,
  weeks = filtered_idx$week,
  draws_format = "df"
)
trajectory_draws
head(colnames(trajectory_draws))
attributes(trajectory_draws)

trajectory_rvars <- as_draws_rvars(trajectory_draws)
trajectory_rvars

trajectory_sum <- trajectory_draws |>
  spread_draws(team_strength[week_pos, team_pos]) |>
  summarise_draws(
    default_summary_measures()[1:4],
    quantile2 = ~ quantile2(.x, probs = c(0.05, 0.95)),
    default_convergence_measures()
  ) |>
  left_join(
    attr(trajectory_draws, "trajectory_lookup") |>
      select(-variable, -variable_orig),
    by = c("week_pos", "team_pos")
  ) |>
  left_join(
    teams_data |>
      select(
        team = team_abbr,
        conf = team_conf,
        division = team_division
      ),
    by = "team"
  )
trajectory_sum


trajectory_sum |>
  ggplot(aes(
    x = week,
    y = mean,
    ymin = q5,
    ymax = q95,
    colour = team,
    fill = team
  )) +
  #geom_ribbon(alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_nfl() +
  #facet_wrap(~division, ncol = 2, dir = "v") +
  #facet_grid(rows = vars(division), cols = vars(conf), scales = "free_x") +
  labs(
    title = "Posterior team strength trajectories",
    y = "Posterior mean with 80% interval",
    x = "Week"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
