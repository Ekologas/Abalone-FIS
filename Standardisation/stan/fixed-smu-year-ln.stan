////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Title: Fixed SMU/Year (Log Normal)
// Author: Steve Lane
// Date: Sunday, 01 April 2018
// Synopsis: This program fits a model within an SMU and year. It uses yearly
// catch/effort data to remove any month effects. Due to the response being a
// positive catch amount, we use a Gamma regression model.
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

data {
  /* Number of Reefs */
  int<lower=0> n_reefs;
  /* Number of divers */
  int<lower=0> n_divers;
  /* Total number of records */
  int<lower=0> n;
  /* Reef identifier */
  int<lower=1,upper=n_reefs> reef[n];
  /* Diver identifier */
  int<lower=1,upper=n_divers> diver[n];
  /* Total volume per diver/reef over a year */
  real<lower=0> volume[n];
  /* Total effort per diver/reef over a year */
  real<lower=0> effort[n];
}

transformed data {
  /* Transform data */
  real log_response[n];
  for (j in 1:n) {
    log_response[j] = log(volume[j] / effort[j]);
  }
}

parameters {
  /* Intercept */
  real intercept;
  /* Reef modelled effects */
  vector[n_reefs] reef_raw;
  real<lower=0> sigma_reef;
  /* Diver modelled effects */
  vector[n_divers] diver_raw;
  real<lower=0> sigma_diver;
  /* Variance of normal */
  real<lower=0> sigma;
}

transformed parameters {
  /* Reef modelled effects */
  vector[n_reefs] reef_effect;
  /* Diver modelled effects */
  vector[n_divers] diver_effect;
  /* Mean of normal */
  vector[n] beta;
  /* Multiply out modelled effects */
  reef_effect = sigma_reef * reef_raw;
  diver_effect = sigma_diver * diver_raw;
  /* Gamma regression */
  for (j in 1:n) {
    /* Regression */
    beta[j] = intercept + reef_effect[reef[j]] + diver_effect[diver[j]];
  }
}

model {
  /* Intercept */
  intercept ~ normal(0, 5);
  /* Modelled parameters */
  /* Reef */
  reef_raw ~ student_t(3, 0, 1);
  sigma_reef ~ cauchy(0, 2.5);
  /* Diver */
  diver_raw ~ student_t(3, 0, 1);
  sigma_diver ~ cauchy(0, 2.5);
  /* Shape */
  sigma ~ cauchy(0, 2.5);
  /* Model */
  for (j in 1:n) {
    log_response[j] ~ normal(beta[j], sigma);
  }
}

generated quantities {
  /* Produce predicted cpue for fictional reef/diver */
  /* Also spits out predicted for in-sample, on a new reef */
  real new_cpue;
  real diver_cpue[n_divers];
  {
    real new_reef;
    real new_diver;
    real new_mu;
    new_reef = sigma_reef * student_t_rng(3, 0, 1);
    new_diver = sigma_diver * student_t_rng(3, 0, 1);
    new_mu = intercept + new_reef + new_diver;
    new_cpue = exp(normal_rng(new_mu, sigma));
  }
  for (j in 1:n_divers) {
    real new_reef;
    real new_mu;
    new_reef = sigma_reef * student_t_rng(3, 0, 1);
    new_mu = intercept + diver_effect[j] + new_reef;
    diver_cpue[j] = exp(normal_rng(new_mu, sigma));
  }
}
