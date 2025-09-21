# Development workflow reminders

Sourcing `dev/workflow.R` will run the core devtools commands in order:

1. `devtools::document()` → regenerate `NAMESPACE` and Rd files.
2. `devtools::load_all(export_all = FALSE)` → reload the package into the current session.
3. `devtools::test()` → run the test suite (requires testthat).
4. `devtools::build_vignettes()` → rebuild articles (requires Pandoc).
5. `devtools::check(args = c("--no-manual"))` → run R CMD check.
6. `devtools::install(upgrade = "never", dependencies = NA, force = FALSE)` → install the current build.

Edit `dev/workflow.R` if you want to skip or reorder steps.
