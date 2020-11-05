# Two proportions test.

# PLEASE READ
# 1. No need to worry about the details of the code below.
# 2. Simply highlight everyting and hit 'Run'.
# 3. Then use the following format to enter your values.
#    according to the problem and the hypothesis that
#    you need to test.  Here is an example from class:

# e.g. The Helmet useage data from NC

# two.prop.test( x1=179, n1=1116, x2=161, n2=848, direction = 'lessthan')

two.prop.test <- function( x1, n1, x2, n2, direction)
{

  p1hat <- x1/n1
  p2hat <- x2/n2

  phat_common <- ( x1 + x2 )/(n1 + n2)

  difference <- p1hat - p2hat

  variance <- phat_common*(1 - phat_common)*(1/n1 + 1/n2)

  sd <- sqrt(variance)

  z <- difference/sd

  ifelse( direction == 'notequal',
          ifelse( z < 0, pval <- 2*pnorm(z), pval <- 2*pnorm(-z)),
  ifelse( direction == 'lessthan', pval <- pnorm(z),
          ifelse(direction == 'greaterthan', pval <- 1 - pnorm(z))))

  out <- list('difference(p1hat - p2hat)' = difference,
              'standard_dev' = sd,
              'z_score' = z,
              'p_value' = pval)

  return(out)
}


two.prop.CI <- function( x1, n1, x2, n2, confidence)
{
  
  p1hat <- x1/n1
  p2hat <- x2/n2
  
  
  difference <- p1hat - p2hat
  
  v1 <- p1hat*(1 - p1hat)/n1

  v2 <- p2hat*(1 - p2hat)/n2
  
  sd <- sqrt(v1 + v2)
  
  z = abs(qnorm(p = (1-confidence)/2))
  
  lb <- difference - z*sd
  ub <- difference + z*sd
  
  out <- list('difference(p1hat - p2hat)' = difference,
              'confidence_interval' = c(lb,ub))
  
  return(out)
}
