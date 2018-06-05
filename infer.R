library(rstan)
library(ggplot2)
library(dplyr)

setwd("/Users/dom/Dropbox/Tidy/LotkaVolterra")

lynx_hare_df <-
  read.csv("hudson-bay-lynx-hare.csv",
           comment.char="#")

lynx_hare_df <-
  read.csv("generatedLV1.csv",
           comment.char="#")

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
fit <- sampling(model, data = lynx_hare_data, cores = 4, seed = 123)

print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

M <- 1
p <- array(c(y), dim = c(20,2,1))
lynx_hare_data_H <- list(N, M, ts, y_init, p)
modelH <- stan_model("lotka-volterra-hierarchical.stan")
fitH <- sampling(modelH, data = lynx_hare_data_H, cores = 4, seed = 123)

fitH <- stan(file="lotka-volterra-hierarchical.stan",
             data = lynx_hare_data_H,
             cores = 4,
             seed = 123
             )

print(fitH, pars=c("mu", "phi", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)


N <- length(lynx_hare_df$Year) - 1
ts <- 1:N
y_init <- c(lynx_hare_df$Hare[1], lynx_hare_df$Lynx[1])
y <- as.matrix(lynx_hare_df[2:(N + 1), 2:3])
y <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data <- list(N, ts, y_init, y)

theta <- c(0.500, 0.025, 0.800, 0.025)
sigma <- c(0.25, 0.25)
z_init <- y_init

lynx_hare_gen_data <- list(N, ts, theta, z_init, sigma)

model <- stan_model("lotka-volterra-gen.stan")

gens <- stan(file="lotka-volterra-gen.stan",
             data = lynx_hare_gen_data,
             algorithm="Fixed_param",
             iter = 1,
             chains = 1,
             seed = 123
             )
u <- extract(gens,permuted=FALSE)
lo <- matrix(as.matrix(u)[3:(2*(N+1))],nrow=20,byrow=FALSE)
hi <- matrix(as.matrix(u)[1:2],nrow=1,byrow=FALSE)
rs <- rbind(hi,lo)

M <- 1
thetas <- t(as.matrix(theta))
z_inits <- t(as.matrix(z_init))
colony_data <- list (N, M, ts, thetas, z_inits, sigma)

model <- stan_model("lotka-volterra-hierarch-gen.stan")

M <- 1
z_inits <- t(as.matrix(z_init))
mu <- c(1.0, 0.05, 1.0, 0.05)
tau <- c(0.1, 0.015, 0.1, 0.015)
colony_data <- list (N, M, ts, z_inits, mu, tau, sigma)

genss <- stan(file="lotka-volterra-hierarch-gen.stan",
             data = colony_data,
             algorithm="Fixed_param",
             iter = 1,
             chains = 1,
             seed = 123
             )
us <- extract(genss,permuted=FALSE)

model <- stan_model("lotka-volterra.stan")

y <- lo
lynx_hare_data <- list(N, ts, y_init, y)
fit <- sampling(model, data = lynx_hare_data, seed = 123)

print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

simwork_df <- read.csv("/Users/dom/simwork.oldish/core/data/SimplerDiff/RunXXX/Outputs/Results/0.csv")

calib_df <- read.csv("/Users/dom/simwork.oldish/core/data/SimplerDiff/CalibratedMdl0/Outputs/Results/0.csv")

obs_df <- read.csv("/Users/dom/example-models/knitr/lotka-volterra/lynx_hare_df.csv")
colnames(obs_df)[1] <- "Time"
obs_df[1] <- obs_df[1] - 1

both_df <- merge(obs_df, calib_df) ## simwork_df)

png(filename="/Users/dom/simwork.oldish/core/data/SimplerDiff/RunXXX/Outputs/Results/1.png")
ggplot(data=both_df, aes(Time)) +
    geom_line(aes(y = predator.mol., colour = "Predator")) +
    geom_line(aes(y = prey.mol., colour = "Prey")) +
    geom_point(aes(y = Lynx, colour = "Predator")) +
    geom_point(aes(y = Hare, colour = "Prey"))
dev.off()
