BlandAltman <- function(x, y, alpha = 0.05, group = NA, labx = "average of measurements", 
laby = "difference of measurements", maintit = "", limy = NA, plot = TRUE){

    ## use only pairwise complete observations
    ind <- complete.cases(x, y)
    x <- x[ind]
    y <- y[ind]

    difference <- x - y                                 # vector of differences
    average <- (x + y) / 2                              # vector of means
    difference.mean <- mean(difference)                 # mean difference
    difference.sd <- sd(difference)                     # SD of differences
    al <- qnorm(1 - alpha / 2) * difference.sd
    upper.agreement.limit <- difference.mean + al       # agreement limits
    lower.agreement.limit <- difference.mean - al
    n <- length(difference)                             # number of 'observations'
    
    difference.se <- difference.sd / sqrt(n)            # standard error of the mean
    al.se <- difference.sd * sqrt(3) / sqrt(n)          # standard error of the agreement limit
    tvalue <- qt(1 - alpha / 2, n - 1)                  # t value for 95% CI calculation
    difference.mean.ci <- difference.se * tvalue
    al.ci <- al.se * tvalue
    upper.agreement.limit.ci <- c(upper.agreement.limit - al.ci, upper.agreement.limit + al.ci)
    lower.agreement.limit.ci <- c(lower.agreement.limit - al.ci, lower.agreement.limit + al.ci)
        
    # Return list
    ba <- list(difference.mean = difference.mean, ci.mean = difference.mean + c(-1, 1) * difference.mean.ci, 
    difference.sd = difference.sd, difference.se = difference.se,
    upper.agreement.limit = upper.agreement.limit, lower.agreement.limit = lower.agreement.limit, 
    agreement.limit.se = al.se, ci.upper.loa = upper.agreement.limit.ci, ci.lower.loa = lower.agreement.limit.ci, 
    t.value = tvalue, average = average, difference = difference, xlab=labx, ylab=laby)
    return(ba)
}

