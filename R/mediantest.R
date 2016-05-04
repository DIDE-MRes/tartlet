
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
