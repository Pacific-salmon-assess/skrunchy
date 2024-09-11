
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skrunchy

<!-- badges: start -->
<!-- badges: end -->

**Sk**eena River **Run** Reconstruction for **Ch**inook Salmon

The goal of skrunchy is to recreate and update the run reconstruction
for Skeena River (summer run timing) Chinook upstream of Tyee test
fishery, as documented in [Winther et
al. 2024](https://publications.gc.ca/site/eng/9.901355/publication.html "An assessment of Skeena River Chinook salmon using genetic stock identification 1984 to 2020")

Detailed methods with variables and equations matching (as much as
possible) this package will be added in the file: `methods.PDF`.

## General methods

Here, we take a known abundance of population $K$ and expand it to
estimate an aggregate population size $X_{aggregate}$ using the
proportion of fish from population $K$ in a mixed genetic sample, $P_K$:

$$X_{aggregate} = \frac{K}{P_K}$$

Further, the abundance of other populations $X_i$ can be estimated using
their genetic proportion $P_i$ and the aggregate abundance
$X_{aggregate}$:

$$X_i = X_{aggregate} \cdot P_i$$

After that, the run is “reconstructed”, i.e., from the time the fish are
age 3 ocean fish, all mortalities (fishery, incidental mortality, brood
stock) are added to the estimate of spawners to estimate total recruits
produced by each cohort. This produces estimates of spawners and
recruits by brood year, which can then be use to model productivity.

## Installation

You can install the development version of skrunchy from
[GitHub](https://github.com/) with:

    install.packages("pak")
    pak::pak("Pacific-salmon-assess/skrunchy")

## Examples

Make some fake genetic proportion and catch data, expand to estimates of
annual proportions, and plot:

``` r

d <- make_P_G()
res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
dp <- res$df_merged

ggplot(dp, aes(y = P_tilde, x = y, group = i)) +
  geom_errorbar( aes( ymin = P_tilde - sigma_P_tilde, ymax = P_tilde + sigma_P_tilde ), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2) +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example_P_tilde-1.png" width="100%" />

Now do expansions to get returns to Terrace for each population plus the
aggregate

``` r
k <- read.csv(here("data/kitsumkalum-escapement.csv"))
X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement, 
           sigma_K = k$sd,
           y = k$year)
dt <- X$df_merged

ggplot(dt, aes(y = X, x = y, group = i)) +
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2, scales = "free_y") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example_X-1.png" width="100%" /> Get
escapement for each population, plot with returns to Terrace (note, will
only be different for Skeena aggregate and the three upper populations)

``` r
tau_F_U <- sample(5000:10000, size=length(k$y), replace=TRUE)
E <- get_E(K = k$kitsumkalum_escapement, X = X$X, tau_F_U = tau_F_U,
     known_population = "Kitsumkalum",
     aggregate_population = "Skeena",
     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
de <- E$df

# ggplot(data = de, aes(y = E, x = y, group = i)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~i, ncol=2, scales = "free_y") +
#   geom_hline(aes(yintercept=0)) +
#   theme_classic()
#   

ggplot(dt, aes(y = X, x = y, group = i)) +
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  geom_line(data = de, aes(y = E, x = y, group=i), colour="red") +
  facet_wrap(~i, ncol=2, scales = "free_y") +
  ylab("Return to Terrace (X) in black, escapement (E) in red") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example_E-1.png" width="100%" />

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- #summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- #plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
