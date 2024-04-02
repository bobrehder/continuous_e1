library (doMC)  # Multicore support 
registerDoMC (8)  # No. of cores to use
library (plyr)
library (e1071)
library("car")
library(fitdistrplus)

noise = sqrt (20**2 - (2*20/3)**2)
target.mn = 50
target.sd = 20
n = 32
breaks=seq (5, 95, by=10)

filter.1d = function (x) {
  x = ifelse (x < 5, 5, x)
  x = ifelse (x > 95, 95, x)
  x
}

mean.err = function (x) {
  mn = mean (x); sd = sd (x); skew = skewness (x)
  x = sort (x)
  qs = qnorm (seq (1/n, 1-1/n, 1/n), mean=50, sd=20)
  fit = glm (x[1:(length (x) -1)] ~ qs)
  residuals = mean (abs (fit$residuals))
  err = abs (mean (x) - 50) + abs (sd (x) - 20) + 5 * residuals + 10 * abs (skew) 
  #err = residuals
  list (mn=mn, sd=sd, skew=skew, residuals=residuals, err=err)
}

best.sample = function (samples) {
  errs = sapply (samples, \(s1) s1$err)
  idx = which (errs == min (errs))[1]
  samples[[idx]]
}

best.cause.distr = function () {
  sample = function (i) {
    cause = filter.1d (rnorm (n, mean=target.mn, sd=target.sd))
    cause.err = mean.err (cause)
    list (cause=cause, cause.err=cause.err, err=cause.err$err)
  }
  samples = llply (1:k, function (i) {
    inner.samples = llply (1:k2, sample, .parallel=F) 
    best.sample (inner.samples)
  }, .parallel=T)
  best.sample (samples)
}

sample = function (i) {
  gen.effect = function () {
    e = filter.1d (2 * (cause - target.mn) / 3 + target.mn + rnorm (n, 0, noise))
  }
  cor.err = function (x, y) {
    cor = cor (x, y)
    list (cor=cor, err=50 * abs (cor - 2/3))
  }
  cause = best.cause$cause
  effect1 = gen.effect ()
  effect2 = gen.effect ()
  effect1.err = mean.err (effect1)
  effect2.err = mean.err (effect2)
  cor1.err = cor.err (cause, effect1)
  cor2.err = cor.err (cause, effect2)
  fit = glm (effect1~cause + effect2)
  #browser ()
  #print (fit$coefficients)
  err = effect1.err$err + effect2.err$err + cor1.err$err + cor2.err$err + 50 * abs (fit$coefficients[3])
  list (
    cause=cause, effect1=effect1, effect2=effect2,
    effect1.err=effect1.err, effect2.err=effect2.err, cor1.err=cor1.err, cor2.err=cor2.err, partial=fit$coefficients,
    err=err
  )
}

k = 10**4; k2 = 10**3

#best.cause = best.cause.distr ()
#print (str (best.cause))
#hist (best$cause, breaks=breaks)
#qqnorm (best$cause)

samples = llply (1:k, function (i) {
  inner.samples = llply (1:k2, sample, .parallel=F) 
  best.sample (inner.samples)
}, .parallel=T)
best = best.sample (samples)
print (str (best))
hist (best$effect1, breaks=breaks)
qqnorm (best$effect1)
hist (best$effect2, breaks=breaks)
qqnorm (best$effect2)
cc.obs = matrix (c (best$cause, best$effect1, best$effect2), ncol=3)
cc.obs = round (cc.obs)
write.csv (cc.obs, file='cc.obs.csv', row.names=F, col.names=F)
