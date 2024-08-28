
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skrunchy

<!-- badges: start -->
<!-- badges: end -->

**Sk**eena River **Run** Reconstruction for **Ch**inook Salmon

The goal of skrunchy is to recreate and update the run reconstruction
for Skeena River (summer run timing) Chinook upstream of Tyee test
fishery, as documented in [Winther et
al. 2024](https://publications.gc.ca/site/eng/9.901355/publication.html "An assessment of Skeena River Chinook salmon using genetic stock identification 1984 to 2020")

## Installation

You can install the development version of skrunchy from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("Pacific-salmon-assess/skrunchy")
```

## Example

Make some fake genetic proportion and catch data, expand to estimates of
annual proportions, and plot:

``` r
library(skrunchy)
#> Loading required package: abind
#> Loading required package: rrandvec
#> Loading required package: here
#> here() starts at C:/github/skrunchy
library(ggplot2)
d <- make_P_G()
P_tilde <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
dt <- P_tilde$df_merged
dt$y <- as.integer(as.character(dt$y))

ggplot(dt, aes(y = P_tilde, x = y, group = i)) +
  geom_errorbar( aes( ymin = P_tilde - sigma_P_tilde, ymax = P_tilde + sigma_P_tilde ), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2) +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example-1.png" width="100%" />

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
