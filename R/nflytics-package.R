#' nflytics: Bayesian NFL team strength tools
#'
#' @description
#' Provides data preparation helpers, Stan models, and posterior summarisation
#' utilities for estimating latent team strengths and forecasts for NFL games.
#'
#' @name nflytics-package
#' @aliases nflytics
#' @useDynLib nflytics, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
#' @keywords internal
"_PACKAGE"
