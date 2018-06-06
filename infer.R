library(rstan)
library(ggplot2)
library(dplyr)
library(bayesplot)

setwd("/Users/dom/Dropbox/Tidy/LotkaVolterra")

## We generate the data with generate.hs

## Rate {theta1 = 0.45540774202311274, theta2 = 2.575691072447691e-2,
## theta3 = 0.8137594080059827, theta4 = 2.5017010388809404e-2}

lynx_hare_df <-
  read.csv("generatedLV1.csv",
           comment.char="#")

actual_df <- data.frame( Year = lynx_hare_df$"Year"
                       , Lynx = lynx_hare_df$"Lynx"
                       , Hare = lynx_hare_df$"Hare")

N <- length(actual_df$Year) - 1
ts <- 1:N
y_init <- c(actual_df$Hare[1], actual_df$Lynx[1])
y <- as.matrix(actual_df[2:(N + 1), 2:3])
y <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data <- list(N, ts, y_init, y)

## We use Bob Carpenter's model

model <- stan_model("lotka-volterra.stan")
fit <- sampling(model, data = lynx_hare_data, iter = 400, cores = 4, seed = 123)

print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

## And we get a pretty good fit! 

## Inference for Stan model: lotka-volterra.
## 4 chains, each with iter=400; warmup=200; thin=1; 
## post-warmup draws per chain=200, total post-warmup draws=800.

##             mean se_mean    sd    10%    50%    90% n_eff   Rhat
## theta[1]   0.455       0 0.000  0.455  0.455  0.455   789  1.007
## theta[2]   0.026       0 0.000  0.026  0.026  0.026   489  1.005
## theta[3]   0.814       0 0.000  0.814  0.814  0.814   737  1.006
## theta[4]   0.025       0 0.000  0.025  0.025  0.025   800  1.007
## sigma[1]   0.000       0 0.000  0.000  0.000  0.001     2 12.566
## sigma[2]   0.000       0 0.000  0.000  0.000  0.000     3  4.668
## z_init[1] 30.000       0 0.003 29.998 30.000 30.001   800  1.005
## z_init[2]  4.000       0 0.000  4.000  4.000  4.000   594  0.999

## Samples were drawn using NUTS(diag_e) at Tue Jun  5 17:36:36 2018.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).


## Let's try again with a different data set

## Rate {theta1 = 0.4602754411852609, theta2 = 2.50394328998701e-2,
## theta3 = 0.8559624367887789, theta4 = 2.4001217838770027e-2}

lynx_hare_df <-
  read.csv("generatedLV2.csv",
           comment.char="#")

actual_df <- data.frame( Year = lynx_hare_df$"Year"
                       , Lynx = lynx_hare_df$"Lynx"
                       , Hare = lynx_hare_df$"Hare")

N <- length(actual_df$Year) - 1
ts <- 1:N
y_init <- c(actual_df$Hare[1], actual_df$Lynx[1])
y <- as.matrix(actual_df[2:(N + 1), 2:3])
y <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data <- list(N, ts, y_init, y)

model <- stan_model("lotka-volterra.stan")
fit <- sampling(model, data = lynx_hare_data, iter = 400, cores = 4, seed = 123)

print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

## Again we get a pretty good fit!

## Inference for Stan model: lotka-volterra.
## 4 chains, each with iter=400; warmup=200; thin=1; 
## post-warmup draws per chain=200, total post-warmup draws=800.

##             mean se_mean    sd    10%    50%    90% n_eff   Rhat
## theta[1]   0.460   0.000 0.000  0.460  0.460  0.460   622  1.006
## theta[2]   0.025   0.000 0.000  0.025  0.025  0.025   143  1.016
## theta[3]   0.856   0.000 0.000  0.856  0.856  0.856   623  1.014
## theta[4]   0.024   0.000 0.000  0.024  0.024  0.024   611  1.004
## sigma[1]   0.000   0.000 0.000  0.000  0.000  0.000     3  2.169
## sigma[2]   0.001   0.001 0.001  0.000  0.000  0.002     2 24.406
## z_init[1] 30.000   0.000 0.000 30.000 30.000 30.000   500  0.996
## z_init[2]  4.000   0.000 0.001  4.000  4.000  4.001   143  1.014

## Samples were drawn using NUTS(diag_e) at Tue Jun  5 17:41:55 2018.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).

## Now let's try the hierarchical model

lynx_hare_df <-
  read.csv("generatedLV1.csv",
           comment.char="#")

actual_df <- data.frame( Year = lynx_hare_df$"Year"
                       , Lynx = lynx_hare_df$"Lynx"
                       , Hare = lynx_hare_df$"Hare")

N <- length(actual_df$Year) - 1
ts <- 1:N
y_init <- c(actual_df$Hare[1], actual_df$Lynx[1])
y <- as.matrix(actual_df[2:(N + 1), 2:3])
u <- cbind(y[ , 2], y[ , 1]); # hare, lynx order

lynx_hare_df <-
  read.csv("generatedLV2.csv",
           comment.char="#")

actual_df <- data.frame( Year = lynx_hare_df$"Year"
                       , Lynx = lynx_hare_df$"Lynx"
                       , Hare = lynx_hare_df$"Hare")

N <- length(actual_df$Year) - 1
ts <- 1:N
y_init <- c(actual_df$Hare[1], actual_df$Lynx[1])
y <- as.matrix(actual_df[2:(N + 1), 2:3])
v <- cbind(y[ , 2], y[ , 1]); # hare, lynx order

M <- 2
p <- array(c(u,v), dim = c(20,2,2))
lynx_hare_data_H <- list(N, M, ts, y_init, p)

fitH <- stan(file="lotka-volterra-hierarchical.stan",
             data = lynx_hare_data_H,
             iter = 800,
             cores = 4,
             ## control = list(max_treedepth = 15),
             seed = 123
             )

print(fitH, pars=c("mu", "phi", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

## The results are not totally absurd but they are not great either.

## Inference for Stan model: lotka-volterra-hierarchical.
## 4 chains, each with iter=800; warmup=400; thin=1; 
## post-warmup draws per chain=400, total post-warmup draws=1600.

##             mean se_mean    sd    10%    50%    90% n_eff  Rhat
## mu[1]      0.890   0.125 0.264  0.598  0.824  1.156     4 1.304
## mu[2]      0.057   0.001 0.020  0.030  0.059  0.077   760 1.018
## mu[3]      0.988   0.025 0.243  0.680  0.963  1.273    95 1.037
## mu[4]      0.055   0.009 0.022  0.019  0.064  0.071     6 1.210
## phi[1,1]   0.601   0.112 0.210  0.455  0.455  0.912     4 1.489
## phi[1,2]   0.619   0.127 0.227  0.460  0.460  0.937     3 1.570
## phi[2,1]   0.040   0.013 0.022  0.026  0.026  0.074     3 1.762
## phi[2,2]   0.041   0.015 0.025  0.025  0.025  0.078     3 1.835
## phi[3,1]   0.889   0.206 0.341  0.534  0.814  1.398     3 1.862
## phi[3,2]   0.909   0.174 0.302  0.573  0.856  1.347     3 1.676
## phi[4,1]   0.029   0.008 0.014  0.017  0.025  0.050     3 1.870
## phi[4,2]   0.027   0.007 0.012  0.016  0.024  0.045     3 1.771
## sigma[1]   0.248   0.176 0.252  0.000  0.177  0.548     2 6.334
## sigma[2]   0.353   0.250 0.357  0.000  0.257  0.768     2 6.827
## z_init[1] 26.622   2.497 4.088 20.488 29.999 30.000     3 1.883
## z_init[2]  6.112   1.533 2.364  4.000  4.249  9.411     2 2.373

## Samples were drawn using NUTS(diag_e) at Tue Jun  5 18:31:48 2018.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).

M <- 12
p <- array(0, dim = c(20,2,M))

for (i in 1:M) {
    lynx_hare_df <-
        read.csv(paste0("generatedLV", as.character(i), ".csv"),
                 comment.char="#")

    actual_df <- data.frame( Year = lynx_hare_df$"Year"
                           , Lynx = lynx_hare_df$"Lynx"
                           , Hare = lynx_hare_df$"Hare")

    N <- length(actual_df$Year) - 1
    ts <- 1:N
    y_init <- c(actual_df$Hare[1], actual_df$Lynx[1])
    y <- as.matrix(actual_df[2:(N + 1), 2:3])
    p[,,i] <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
}

lynx_hare_data_H <- list(N, M, ts, y_init, p)

fitH <- stan(file="lotka-volterra-hierarchical.stan",
             data = lynx_hare_data_H,
             iter = 1000,
             cores = 4,
             chains = 8,
             seed = 123
             )

print(fitH, pars=c("mu", "phi", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

posterior2 <- extract(fitH, inc_warmup = TRUE, permuted = FALSE)
png(filename="EightSamples.chains.png")
color_scheme_set("mix-blue-pink")
pp <- mcmc_trace(posterior2,  pars = c("mu[1]","mu[2]", "mu[3]", "mu[4]"), n_warmup = 500,
                facet_args = list(nrow = 2, labeller = label_parsed))
pp + facet_text(size = 15)
dev.off()

posterior <- extract(fitH)
png(filename="EightSamples.posteriors.png")
plot_title <- ggtitle("Posterior distributions for mu[1] and mu[3]",
                      "with medians and 80% intervals")
pp <- mcmc_areas(posterior2,
                 pars = c("mu[1]", "mu[3]"),
                 prob = 0.8) + plot_title
pp + scale_x_continuous(limits = c(0, 3))
dev.off()

png(filename="EightSamples.posteriors2.png")
plot_title <- ggtitle("Posterior distributions for mu[2] and mu[4]",
                      "with medians and 80% intervals")
pp <- mcmc_areas(posterior2,
                 pars = c("mu[2]", "mu[4]"),
                 prob = 0.8) + plot_title
pp + scale_x_continuous(limits = c(0, 0.3))
dev.off()
