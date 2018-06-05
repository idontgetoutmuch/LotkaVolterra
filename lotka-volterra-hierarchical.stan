functions {
  real[] dz_dt(real t,       // time
               real[] z,     // system state {prey, predator}
               real[] theta, // parameters
               real[] x_r,   // unused data
               int[] x_i) {
    real u = z[1];
    real v = z[2];

    real alpha = theta[1];
    real beta = theta[2];
    real gamma = theta[3];
    real delta = theta[4];

    real du_dt = (alpha - beta * v) * u;
    real dv_dt = (-gamma + delta * u) * v;
    return { du_dt, dv_dt };
  }
}
data {
  int<lower = 0> N;           // number of measurement times
  int<lower = 0> M;           // number of colonies
  real ts[N];                 // measurement times > 0
  real y_init[2];             // initial measured populations
  real<lower = 0> p[N, 2, M]; // measured populations
}
parameters {
  real<lower = 0> mu[4];      // mean for hares and lynxes
  real<lower = 0> phi[4, M];  // { alpha, beta, gamma, delta }
  real<lower = 0> z_init[2];  // initial population
  real<lower = 0> sigma[2];   // measurement errors
}
transformed parameters {
  real z[N, 2];
  real w[N, 2, M];
  real theta[4];
  for (j in 1:M) {
    for (l in 1:4)
      theta[l] = phi[l,j];
    z = integrate_ode_rk45(dz_dt, z_init, 0, ts, theta,
			   rep_array(0.0, 0), rep_array(0, 0),
			   1e-5, 1e-3, 5e2);
    w[ , , j] = z;
  }
}
model {
  mu[{1, 3}] ~ normal(1.0, 0.5);
  mu[{2, 4}] ~ normal(0.05, 0.05);
  phi[1,] ~ normal(mu[1], 0.5);
  phi[2,] ~ normal(mu[2], 0.05);
  phi[3,] ~ normal(mu[3], 0.5);
  phi[4,] ~ normal(mu[4], 0.05);
  sigma ~ lognormal(-1, 1);
  z_init ~ lognormal(log(10), 1);
  for (k in 1:2) {
    y_init[k] ~ lognormal(log(z_init[k]), sigma[k]);
    for (j in 1:M)
      p[ , k, j] ~ lognormal(log(w[, k, j]), sigma[k]);
  }
}
generated quantities {
  real y_init_rep[2];
  real p_rep[N, 2, M];
  for (k in 1:2) {
    y_init_rep[k] = lognormal_rng(log(z_init[k]), sigma[k]);
    for (n in 1:N) {
      for (j in 1:M)
      	p_rep[n, k, j] = lognormal_rng(log(w[n, k, j]), sigma[k]);
    }
  }
}
