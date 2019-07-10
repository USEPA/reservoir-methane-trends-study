#### Will Barnett, June 2019
#### Short demo bootstrapping a mean


## Seed
set.seed(5555)


## Fake data
n = 100
x = round(runif(n,0,100))


## Actual mean and confidence interval (assuming CLT)
param_ci = mean(x) + c(-1,1) * 1.96 * sd(x) / sqrt(n)


## Empirical bootstrapped mean
## Take 100 resamples
## Each row is a size n resample of the original data
nsamp = 100
bootsamp = matrix(sample(x = x, size = nsamp*n, replace = TRUE), nrow = nsamp)
boot_mns = apply(bootsamp, 1, mean) # This is astonishingly accurate, by the way
boot_ci = quantile(boot_mns, c(0.025,0.975)) # The distribution of boot_mns is the sampling distribution of the mean of x.


## What happens is we only look at the 'best' estimates of the mean?
## The ANN code takes the top 100 models, out of 16 * 50 = 800 models.
## Grab the top 1/8 of the means and look at the quantiles.
boot_mns_sorted = boot_mns[order(abs(boot_mns - mean(boot_mns)))]
boot_mns_best = boot_mns_sorted[1:ceiling(n/8)]
boot_ci_best = quantile(boot_mns_best, c(0.025,0.975))


## By looking at only the 'top' model results, an empirical bootstrap has intervals that are much too wide.
print(rbind(param_ci,boot_ci,boot_ci_best))

                       