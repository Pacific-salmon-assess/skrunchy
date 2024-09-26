
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skrunchy

<!-- badges: start -->
<!-- badges: end -->

**Sk**eena River **Run** Reconstruction for **Ch**inook Salmon

The goal of skrunchy is to recreate and update the run reconstruction
for Skeena River summer run timing Chinook upstream of Tyee test fishery
(aggregate plus six Conservation Units), as documented in [Winther et
al. 2024](https://publications.gc.ca/site/eng/9.901355/publication.html "An assessment of Skeena River Chinook salmon using genetic stock identification 1984 to 2020").

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

After that, the run is “reconstructed”: working backwards from
age-specific wild spawner abundance, all mortalities (brood stock
removals, fishery harvest, incidental mortality) are added back in, to
estimate total recruits produced by brood year. This produces estimates
of spawners and recruits by brood year, which can then be use to model
productivity.

## Installation

You can install the development version of skrunchy from
[GitHub](https://github.com/) with:

    install.packages("pak")
    pak::pak("Pacific-salmon-assess/skrunchy")

## Examples (currently use fake data for several data sources - not accurate)

Make some **fake** genetic proportion and catch data (by week), expand
to estimates of annual proportions, and plot:

``` r

d <- make_P_G()
res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
P_tilde <- res$P_tilde
dp <- res$df_merged

ggplot(dp, aes(y = P_tilde, x = y, group = i)) +
  geom_errorbar( aes( ymin = P_tilde - sigma_P_tilde, ymax = P_tilde + sigma_P_tilde ), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2) +
  ylab(TeX("$\\tilde{P}$")) +
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
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X)) +
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
Tau_U <- sample(5000:10000, size=length(k$y), replace=TRUE)
E <- get_E(K = k$kitsumkalum_escapement, X = X$X, Tau_U = Tau_U,
     known_population = "Kitsumkalum",
     aggregate_population = "Skeena",
     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
de <- E$df


ggplot(dt, aes(y = X, x = y, group = i)) +
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X)) +
  geom_point() +
  geom_line() +
  geom_line(data = de, aes(y = E, x = y, group=i), colour="gray") +
  facet_wrap(~i, ncol=2, scales = "free_y") +
  ylab("Return to Terrace (X) in black, escapement (E) in gray") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example_E-1.png" width="100%" />

Get age proportions by age, population and year (fake data).

``` r
 populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
  n_populations <- length(populations)
  years <- k$year
  n_years <- length(years)
  ages <- c(4,5,6,7)
  p_ages <- c(30,40,40,1) # make up relative frequency of different ages
  n_ages <- length(ages)
  # Make up some age data - number of aged fish of each age
  d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
  n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
  omega <- get_omega(n)
do <- omega$df

ggplot( do, aes(y = omega, x = y, group = i)) +
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\Omega$") )+
  facet_grid( i ~ a ) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_omega-1.png" width="100%" />

Get age-specific escapement by using age proportions.

``` r
E_star <- get_E_star(E = E$E, omega = omega$omega)

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point( colour="gray") + 
  geom_line( colour="gray") + 
  geom_hline(aes(yintercept=0)) + 
  facet_grid( i ~ a , scales = "free_y") + 
  ylab("Escapement (E*)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_E_star-1.png" width="100%" />

Get spawners for each population (accounts for brood stock removals),
plot with returns to Terrace and escapement. Spawners should only be
different from escapement for Skeena aggregate and Kitsumkalum, since
brood removals are only for Kitsumkalum.

``` r
# make up brood removal data. Much larger than actual values, so that it is visible on plots. 
B_star <- E_star$E_star["Kitsumkalum",,] * runif(n = length(E_star$E_star["Kitsumkalum",,] ), 0.1, 0.2 )
B_star[,4] <- 0 # make age 7 brood = 0 so you don't get negative fish in S_star
S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point(colour="gray") + 
  geom_line(colour="gray") + 
  geom_line(data = S_star$df, aes(y = S_star, x = y, group = i), colour="dodgerblue") +
  geom_hline(aes(yintercept=0)) + 
  facet_grid( i ~ a , scales = "free_y") + 
  ylab("Escapement (E*) in gray and spawners (S*) in blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_S_star-1.png" width="100%" />

Get wild spawners for each population (accounts for hatchery origin
spawners), plot with returns to Terrace, escapement, and spawners. Wild
spawners should only be different from spawners for Skeena aggregate and
Kitsumkalum, since hatchery origin spawners only occur for Kitsumkalum.

``` r
# make up hatchery spawners data. Larger than actual values, so that it is visible on plots. 
H_star <- S_star$S_star["Kitsumkalum",,] * runif(n = length(S_star$S_star["Kitsumkalum",,] ), 0.1, 0.5 )
H_star[,4] <- 0 # make age 7 hatchery 0 to avoid negative wild spawner values.
## get Wild spawners
W_star <- get_W_star( S_star = S_star$S_star, H_star = H_star) 

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point(colour="gray") + 
  geom_line(colour="gray") + 
  geom_line(data = S_star$df, aes(y = S_star, x = y, group = i), colour="dodgerblue") +
  geom_line(data = W_star$df, aes(y = W_star, x = y, group = i), colour="firebrick") +
  geom_hline(aes(yintercept=0)) + 
  facet_grid( i ~ a , scales = "free_y") + 
  ylab("Escapement (E*) in gray, spawners (S*) in blue,\nand wild spawners (W*) in red.") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_W_star-1.png" width="100%" />

Get proportion wild spawners for each population and plot. Should only
be \<1 for Skeena aggregate and Kitsumkalum, since hatchery origin
spawners only occur for Kitsumkalum. Note this is not real data, p is
very high for Kitsumkalum across years.

``` r
p <- get_p(W_star = W_star$W_star, E_star = E_star$E_star)

ggplot(p$df, aes(y = p, x = y, group = i)) +
  geom_point() +
  geom_line() +
  facet_grid(i~a) +
  ylab("Proportion wild spawners, p") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
```

<img src="man/figures/README-example_p-1.png" width="100%" />

Estimate terminal total mortalities in the lower Skeena by population,
year, and age.

``` r
Tau_L <- sample(1000:2000, size = length(k$year))
tau_L <- get_tau_L(Tau_L = Tau_L, omega = omega$omega, P_tilde = P_tilde, aggregate_population = "Skeena")
dtauL <- tau_L$df 

ggplot( dtauL, aes(y =tau_L, x = y, group = i)) +
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\tau_L$")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_tau_L-1.png" width="100%" />

Estimate terminal total mortalities in the upper Skeena by population,
year, and age. Should be 0 for Kitsumkalum, Lower Skeena, and
Zymoetz-Fiddler.

``` r
tau_U <- get_tau_U(Tau_U = Tau_U, omega = omega$omega, P_tilde = P_tilde, aggregate_population = "Skeena",
                   upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
    lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler") )

dtauU <- tau_U$df 

ggplot( dtauU, aes(y =tau_U, x = y, group = i)) +
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\tau_U$")) +
  facet_grid( i ~ a) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_tau_U-1.png" width="100%" />

Estimate terminal total mortalities in the marine area by population,
year, and age.

``` r
use_arr <- W_star$W_star["Kitsumkalum",,]
tau_dot_M <- array(runif(length(use_arr), 0.01, 0.2), dim = dim(use_arr), dimnames = dimnames(use_arr))
tau_M <- get_tau_M( W_star = W_star$W_star, tau_dot_M = tau_dot_M)

ggplot( tau_M$df, aes(y =tau_M, x = y, group = i)) +
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\tau_M$")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_tau_M-1.png" width="100%" />

Get total terminal mortality

``` r
tau <- get_tau(tau_U = tau_U$tau_U, tau_L = tau_L$tau_L, tau_M= tau_M$tau_M)

ggplot( tau$df, aes(y =tau, x = y, group = i)) +
  geom_line() +
  geom_point() +
  geom_line( data = tau_L$df, aes(y =tau_L, x = y), colour="goldenrod") +
  geom_line( data = tau_U$df, aes(y =tau_U, x = y), colour="aquamarine") +
  geom_line( data = tau_M$df, aes(y =tau_M, x = y), colour="blue") +
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\tau$")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_tau-1.png" width="100%" />

Get wild total terminal mortality

``` r
tau_W <- get_tau_W(tau= tau$tau, p = p$p)

ggplot( tau$df, aes(y =tau, x = y, group = i)) +
  geom_line() +
  geom_point() +
  geom_line( data = tau_W$df, aes(y =tau_W, x = y), colour="darkorange3") +
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$\\tau_W$ in orange, and $\\tau$ in black")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_tau_W-1.png" width="100%" />

Get wild terminal run

``` r
TermRun <- get_TermRun(tau_W = tau_W$tau_W, W_star = W_star$W_star, B_star = B_star)

ggplot( TermRun$df, aes(y =TermRun, x = y, group = i)) +
  geom_line() +
  geom_point() +
  geom_line( data = tau_W$df, aes(y =tau_W, x = y), colour="darkorange3") +
  geom_line( data = W_star$df, aes(y =W_star, x = y), colour="firebrick") +
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$TermRun$ in black, $\\tau_W$ in orange, and $W_{star}$ in red")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_TermRun-1.png" width="100%" />

Get mature run

``` r
use_arr <- TermRun$TermRun["Kitsumkalum",,]
phi_dot_M <- array(runif(length(use_arr), 0.01, 0.3), dim = dim(use_arr), dimnames = dimnames(use_arr))
MatureRun <- get_MatureRun(TermRun = TermRun$TermRun, phi_dot_M = phi_dot_M)

ggplot( MatureRun$df, aes(y =MatureRun, x = y, group = i)) +
  geom_line(colour="darkgreen") +
  geom_point(colour ="darkgreen") +
  geom_line( data = TermRun$df, aes(y =TermRun, x = y, group = i), colour="black") +
  geom_hline(aes(yintercept=0)) + 
  ylab(TeX("$MatureRun$ in green, $TermRun$ in black")) +
  facet_grid( i ~ a, scales="free_y") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

<img src="man/figures/README-example_MatureRun-1.png" width="100%" />
