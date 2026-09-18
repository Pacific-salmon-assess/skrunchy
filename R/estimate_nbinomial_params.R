#' Uses maximum likelihood estimation to find the most likely parameters to
#' use to populate a negative binomial sampler.
#' This was designed to accomodate the K parameter, but can be generalized
#' to sample from negative binomial probability mass functions.
#' The results of this function can be directly supplied to `rnbinomial()` when
#' performing Monte carlo sampling.
#'
#' @param X Numeric, assumed to be comprised of all observed count data (e.g. K)
#' used for estimating mu, size, and prob parameters for a negative binomial sampler.
#' @importFrom MASS fitdistr


estimate_nbinomial_params <- function(X) {
    if (mean(X) >= var(X)) stop("Mean of the dataset is greater than or equal to its variance; consider a Poisson distribution")
    fit <- MASS::fitdistr(X, "Negative Binomial")
    return(list(
        mu = as.double(fit$estimate["mu"]),
        size = as.double(fit$estimate["size"]),
        prob = as.double(fit$estimate["mu"]) / (
            as.double(fit$estimate["size"]) + as.double(fit$estimate["mu"]))
    ))
}
