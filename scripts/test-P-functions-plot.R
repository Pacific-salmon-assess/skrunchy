library(here)
install.packages("pak")
pak::pak("Pacific-salmon-assess/skrunchy")
library(skrunchy)

d <- make_P_G()
res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)


d <- as.data.frame.table(P_tilde$P_tilde, responseName = "P_tilde")
ds <- as.data.frame.table(P_tilde$sigma_P_tilde, responseName = "sigma_P_tilde")
dt <- merge(d, ds, by = c("i", "y"))


dt <- merge(d, ds, by = c("i", "y"))


ggplot(dt, aes(y = P_tilde, x = y, group = i)) +
  geom_errorbar( aes( ymin = P_tilde - sigma_P_tilde, ymax = P_tilde + sigma_P_tilde ), colour="dodgerblue") +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2)


k <- read.csv(here("data/kitsumkalum-escapement.csv"))
str(k)

T <- get_T(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
           y = k$year)
