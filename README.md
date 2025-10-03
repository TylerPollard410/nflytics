# nflytics

**Bayesian NFL team strength modeling with Stan.**

The `nflytics` R package bundles a latent team-strength state space model, along
with helper functions for preparing game data, fitting the model via Stan
(`rstan` or `cmdstanr`), and interrogating the posterior with tidy tooling. It
draws inspiration from packages like `brms`, `rstanarm`, and `footBayes`, but is
specialised for NFL workflow needs.

## Features

- Preconfigured Stan models (`latent_strength_ssm` and generated-quantities
  companion) for regular-season and forecasting workflows.
- R helpers to encode game schedules into Stan data lists, run the sampler, and
  convert draws into `posterior` formats.
- Convenience wrappers for posterior summaries, including an
  `posterior_trajectory()` method analogous to `rstanarm`.
- Quarto vignette demonstrating end-to-end usage, plus optional plotting recipes
  with `ggplot2`, `tidybayes`, and `bayesplot`.

## Installation

Clone the repository and install from source:

```r
# install.packages("devtools")
devtools::install_local("path/to/nflytics")
```

If you plan to use the `cmdstanr` backend, ensure CmdStan and the required tool
chain are installed. For the rstan backend, use the official Stan/R setup docs.

## Getting Started

The package vignette `nflytics-intro.qmd` (rendered under `doc/` when you run
`devtools::build_vignettes()`) walks through:

1. Preparing a tibble of NFL games and converting it with
   `prepare_latent_strength_data()`.
2. Fitting the latent strength model with `fit_latent_strength()` using either
   backend.
3. Summarising posterior draws via `nflytics_draws()` and
   `posterior_trajectory()`.
4. Visualising trajectories and parameter distributions with `ggplot2` and
   `bayesplot`.

Quick teaser:

```r
library(nflytics)

# game_data is a data frame with season/week/team columns and scores
stan_data <- prepare_latent_strength_data(game_data)
fit <- fit_latent_strength(stan_data, engine = "cmdstanr")
trajectory_draws <- posterior_trajectory(fit, draws_format = "df")
trajectory_lookup <- attr(trajectory_draws, "trajectory_lookup")
# Need summaries? leverage the posterior package helpers
trajectory_summary <- posterior::summarise_draws(
  trajectory_draws,
  probs = c(0.1, 0.9)
) |>
  dplyr::left_join(trajectory_lookup, by = "variable")
```

For more examples, see the vignette or the scripts under `dev/`.

## Development Notes

- Stan sources live in `inst/stan/`. If you modify them, regenerate the C++
bindings via the standard `rstantools` workflow (`devtools::document()` covers
it).
- The `dev/workflow.R` script lists the typical `devtools` commands used during
package development (document, load, test, build vignettes, check, install).
- Vignettes require the Quarto CLI; install it from <https://quarto.org/> and
confirm with `quarto check`.

## License

MIT © Tyler Pollard

## Acknowledgements

Built on top of Stan, CmdStanR, RStan, posterior, bayesplot, and the broader
nflverse ecosystem.
