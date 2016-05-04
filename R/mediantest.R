#' Median test
#'
#' This function implements a non-parametric test of differences between medians in two samples.
#'
#' @export
#' @importFrom ade4 as.randtest
#'
#' @author T. Jombart
#'
#' @param x a vector of a quantitative variable
#' @param fac a factor defining 2 groups
#' @param n the number of permutations for the Monte Carlo procedure
#' @param ... further arguments passed to as.randtest
#'
medianTest <- function(x, fac, n=999, ...){
    ## check dependencies
    if(!require(ade4)) stop("ade4 is not installed")

    ## function to get the statistic
    f1 <- function(x, fac) {
        medians <- tapply(x, fac, median)
        return(medians[1] - medians[2])
    }

    ## get original statistic
    stat.ori <- f1(x,fac)

    ## get permuted values
    stat.perm <- replicate(n, f1(x, sample(fac)))

    ## return output
    out <- as.randtest(sim=stat.perm, obs=stat.ori, ...)
    return(out)
}
