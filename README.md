
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simMultiCov

<!-- badges: start -->

[![R-CMD-check](https://github.com/BmBaczkowski/simMultiCov/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BmBaczkowski/simMultiCov/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/BmBaczkowski/simMultiCov/branch/main/graph/badge.svg)](https://app.codecov.io/gh/BmBaczkowski/simMultiCov?branch=main)
<!-- badges: end -->

**simMultiCov** provides tools for simulating multilevel (clustered)
covariates with flexible correlation structures. It supports continuous,
binary, and ordinal covariates, allowing researchers to generate
realistic clustered data for methodological studies, power analyses, and
simulation-based research.

## Features

- **Multiple covariate types**: Simulate continuous, binary, and ordinal
  covariates
- **Flexible correlation structures**: Specify separate within-cluster
  and between-cluster correlations
- **Latent variable approach**: Binary and ordinal covariates are
  generated using latent normal distributions
- **Unequal cluster sizes**: Support for varying numbers of observations
  per cluster
- **Multiple datasets**: Generate multiple independent datasets in a
  single call
- **Reproducible simulations**: Optional seed parameter for
  reproducibility

## Installation

You can install the development version of simMultiCov from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("BmBaczkowski/simMultiCov")
```

Or using the `remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("BmBaczkowski/simMultiCov")
```

## Quick Start

### Basic Example

``` r
library(simMultiCov)
#> 
#> Attaching package: 'simMultiCov'
#> The following object is masked from 'package:stats':
#> 
#>     simulate

# Define covariates
covs <- make_covariates(
  covariates = list(
    make_continuous("age", mean = 50, total_var = 100, icc = 0.2),
    make_binary("treatment", prob = 0.5, icc = 0.1),
    make_ordinal("satisfaction", probs = c(0.2, 0.3, 0.5), icc = 0.15)
  ),
  correlations = list(
    define_correlation("age", "treatment", corr_within = 0.1, corr_between = 0.05)
  )
)
#> Warning: Correlations of binary / ordinal covariate(s) are in latent space.

# View the specification
print(covs)
#> 
#> ── <covariates> ────────────────────────────────────────────────────────────────
#> 
#> ── Covariates (3) ──
#> 
#> • age: continuous
#> • treatment: binary
#> • satisfaction: ordinal
#> 
#> ── Correlations ──
#> 
#> ℹ Within/between correlation matrices available. Use summary() for details.
```

### Simulate Data

``` r
# Simulate a single dataset with 10 clusters of 20 observations each
df <- simulate(covs, n_clusters = 10, cluster_size = 20, seed = 123)

# View the first few rows
head(df)
#>   cluster      age treatment satisfaction
#> 1       1 51.30789         B            C
#> 2       1 55.34774         A            C
#> 3       1 52.44787         B            C
#> 4       1 44.09044         B            C
#> 5       1 36.17543         A            C
#> 6       1 37.44809         B            C

# Check the data structure
str(df)
#> Classes 'simulation' and 'data.frame':   200 obs. of  4 variables:
#>  $ cluster     : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ age         : num  51.3 55.3 52.4 44.1 36.2 ...
#>  $ treatment   : Factor w/ 2 levels "A","B": 2 1 2 2 1 2 2 2 1 1 ...
#>  $ satisfaction: Ord.factor w/ 3 levels "A"<"B"<"C": 3 3 3 3 3 3 3 3 2 3 ...
#>  - attr(*, "DGP")=List of 3
#>   ..$ mu     : Named num [1:3] 50 0 0
#>   .. ..- attr(*, "names")= chr [1:3] "age" "treatment" "satisfaction"
#>   ..$ Sigma_w: num [1:3, 1:3] 80 0.849 0 0.849 0.9 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:3] "age" "treatment" "satisfaction"
#>   .. .. ..$ : chr [1:3] "age" "treatment" "satisfaction"
#>   ..$ Sigma_b: num [1:3, 1:3] 20 0.0707 0 0.0707 0.1 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:3] "age" "treatment" "satisfaction"
#>   .. .. ..$ : chr [1:3] "age" "treatment" "satisfaction"
```

### Unequal Cluster Sizes

``` r
# Simulate with varying cluster sizes
df_unequal <- simulate(
  covs, 
  n_clusters = 5, 
  cluster_size = c(10, 15, 20, 25, 30),
  seed = 456
)

# View cluster sizes
table(df_unequal$cluster)
#> 
#>  1  2  3  4  5 
#> 10 15 20 25 30
```

### Multiple Datasets

``` r
# Generate 3 independent datasets
datasets <- simulate(
  covs, 
  n_clusters = 5, 
  cluster_size = 10, 
  n_datasets = 3,
  seed = 789
)

# Check the number of datasets
length(datasets)
#> [1] 3

# Each dataset is a data frame
class(datasets[[1]])
#> [1] "simulation" "data.frame"
```

## Detailed Examples

### Continuous Covariates

``` r
# Create a continuous covariate with ICC of 0.3
ability <- make_continuous(
  name = "ability",
  mean = 100,
  total_var = 225,
  icc = 0.3
)

# Build covariate specification
covs <- make_covariates(covariates = list(ability))

# Simulate data
df <- simulate(covs, n_clusters = 8, cluster_size = 15, seed = 111)

# Verify ICC
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
df %>%
  group_by(cluster) %>%
  summarise(cluster_mean = mean(ability)) %>%
  summarise(
    between_var = var(cluster_mean),
    total_var = var(df$ability),
    icc = between_var / total_var
  )
#> # A tibble: 1 × 3
#>   between_var total_var   icc
#>         <dbl>     <dbl> <dbl>
#> 1        40.6      213. 0.191
```

### Binary Covariates

``` r
# Create a binary treatment variable
treatment <- make_binary(
  name = "treatment",
  prob = 0.6,
  icc = 0.15,
  labels = c("Control", "Treatment")
)

# Build specification
covs <- make_covariates(covariates = list(treatment))

# Simulate data
df <- simulate(covs, n_clusters = 6, cluster_size = 20, seed = 222)

# Check treatment distribution
table(df$treatment)
#> 
#>   Control Treatment 
#>        56        64

# Check ICC
df %>%
  group_by(cluster) %>%
  summarise(cluster_prop = mean(treatment == "Treatment")) %>%
  summarise(
    between_var = var(cluster_prop),
    total_prop = mean(df$treatment == "Treatment"),
    icc = between_var / (total_prop * (1 - total_prop))
  )
#> # A tibble: 1 × 3
#>   between_var total_prop   icc
#>         <dbl>      <dbl> <dbl>
#> 1      0.0417      0.533 0.167
```

### Ordinal Covariates

``` r
# Create an ordinal satisfaction variable
satisfaction <- make_ordinal(
  name = "satisfaction",
  probs = c(0.15, 0.25, 0.35, 0.25),
  icc = 0.2,
  labels = c("VeryLow", "Low", "High", "VeryHigh")
)

# Build specification
covs <- make_covariates(covariates = list(satisfaction))

# Simulate data
df <- simulate(covs, n_clusters = 10, cluster_size = 25, seed = 333)

# Check distribution
table(df$satisfaction)
#> 
#>  VeryLow      Low     High VeryHigh 
#>       33       59       93       65
```

### Correlated Covariates

``` r
# Create multiple correlated covariates
covs <- make_covariates(
  covariates = list(
    make_continuous("income", mean = 50000, total_var = 100000000, icc = 0.25),
    make_continuous("education", mean = 16, total_var = 4, icc = 0.2),
    make_binary("employed", prob = 0.7, icc = 0.1)
  ),
  correlations = list(
    define_correlation("income", "education", corr_within = 0.5, corr_between = 0.6),
    define_correlation("income", "employed", corr_within = 0.3, corr_between = 0.2),
    define_correlation("education", "employed", corr_within = 0.4, corr_between = 0.3)
  )
)
#> Warning: Correlations of binary / ordinal covariate(s) are in latent space.

# View the specification
summary(covs)
#> 
#> ── <covariates> ────────────────────────────────────────────────────────────────
#> Full specification
#> 
#> ── Structure ──
#> 
#>       Name            Type  ICC  Mean   Var
#>     income      continuous 0.25 50000 1e+08
#>  education      continuous 0.20    16 4e+00
#>   employed binary (latent) 0.10     0 1e+00
#> 
#> ── Correlations ──
#> 
#> ── Within-Cluster
#>           income education employed
#> income       1.0       0.5      0.3
#> education    0.5       1.0      0.4
#> employed     0.3       0.4      1.0
#> 
#> ── Between-Cluster
#>           income education employed
#> income       1.0       0.6      0.2
#> education    0.6       1.0      0.3
#> employed     0.2       0.3      1.0
#> 
#> ── Variance-Covariance ──
#> 
#> ── Within-Cluster
#>                 income education employed
#> income    75000000.000  7745.967 2464.752
#> education     7745.967     3.200    0.679
#> employed      2464.752     0.679    0.900
#> 
#> 
#> ── Between-Cluster
#>                 income education employed
#> income    25000000.000  2683.282  316.228
#> education     2683.282     0.800    0.085
#> employed       316.228     0.085    0.100

# Simulate data
df <- simulate(covs, n_clusters = 15, cluster_size = 30, seed = 444)

# Check correlations
cor(df[, c("income", "education")])
#>              income education
#> income    1.0000000 0.5233081
#> education 0.5233081 1.0000000
```

## Use Cases

**simMultiCov** is particularly useful for:

- **Methodological research**: Testing the performance of multilevel
  models under various data-generating conditions
- **Power analysis**: Determining required sample sizes for multilevel
  studies
- **Teaching**: Demonstrating multilevel data structures and analysis
  techniques
- **Simulation studies**: Generating realistic clustered data for Monte
  Carlo simulations
- **Sensitivity analysis**: Assessing how violations of assumptions
  affect model performance

## Package Structure

The package provides the following main functions:

- `make_continuous()`: Define continuous covariates
- `make_binary()`: Define binary covariates
- `make_ordinal()`: Define ordinal covariates
- `define_correlation()`: Specify correlations between covariates
- `make_covariates()`: Combine covariate specifications into a complete
  structure
- `simulate()`: Generate simulated data from a covariate specification

## Getting Help

- **Documentation**: Run `?function_name` for detailed help on any
  function
- **Issues**: Report bugs or request features on [GitHub
  Issues](https://github.com/BmBaczkowski/simMultiCov/issues)
- **Questions**: Ask questions on [GitHub
  Discussions](https://github.com/BmBaczkowski/simMultiCov/discussions)

## Citation

If you use simMultiCov in your research, please cite:

``` r
citation("simMultiCov")
```

## License

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
For major changes, please open an issue first to discuss what you would
like to change.

1.  Fork the repository
2.  Create your feature branch
    (`git checkout -b feature/AmazingFeature`)
3.  Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4.  Push to the branch (`git push origin feature/AmazingFeature`)
5.  Open a Pull Request
