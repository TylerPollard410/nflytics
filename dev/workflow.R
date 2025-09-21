# Regenerate NAMESPACE and Rd files
devtools::document()

# Reload package into the current session
devtools::load_all(export_all = FALSE)

# Execute tests (requires testthat)
devtools::test()

# Rebuild vignettes (requires Pandoc)
devtools::build_vignettes()

# Run R CMD check before release
devtools::check(args = c("--no-manual"))

# Install the current package build
devtools::install(upgrade = "never", dependencies = NA, force = FALSE)
