////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Title: Fixed SMU, Year by Diver
// Author: Steve Lane
// Date: Sunday, 01 April 2018
// Synopsis: This program fits a model within Cliffy Group. It uses yearly
// catch/effort data to remove any month effects, year by reef codes to
// account for seasonal differences, and year by diver codes to account for
// changes in personnel/experience.
// Due to the response being a positive catch amount, we use a Gamma regression
// model.
// The generated quantities include:
//   - predictions for new levels (for standardisation)
//   - predictions for in-sample data (for posterior predictive checks)
//   - log-likelihood calculations (for model comparisons)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

data {
  /* Total number of records */
  int<lower=0> n;
  /* Number of divers */
  int<lower=0> n_divers;
  /* Number of years */
  int<lower=0> n_years;
  /* Diver identifier */
  int<lower=1,upper=n_divers> diver[n];
  /* Year identifier */
  int<lower=1,upper=n_years> year[n];
  /* Total volume per diver/reef over a year */
  real<lower=0> volume[n];
  /* Total effort per diver/reef over a year */
  real<lower=0> effort[n];
  /* Year/s that prediction should be made for standardisation */
  int<lower=1> n_pred_year;
  int<lower=1,upper=n_years> pred_year[n_pred_year];
}

transformed data {
  /* Transform effort to log for offset */
  real log_effort[n];
  for (j in 1:n) {
    log_effort[j] = log(effort[j]);
  }
}

parameters {
  /* Intercept */
  real intercept;
  /* Diver modelled effects */
  vector[n_divers] diver_raw;
  real<lower=0> sigma_diver;
  /* Year modelled effects */
  vector[n_years] year_raw;
  real<lower=0> sigma_year;
  /* Year by diver modelled effects */
  matrix[n_years, n_divers] year_diver_raw;
  vector<lower=0>[n_years] sigma_year_diver;
  /* Shape of Gamma regression */
  real<lower=0> alpha;
}

transformed parameters {
  /* Diver modelled effects */
  vector[n_divers] diver_effect;
  /* Year modelled effects */
  vector[n_years] year_effect;
  /* Year by reef modelled effects */
  matrix[n_years, n_divers] year_diver_effect;
  /* Inverse rate of Gamma regression */
  vector[n] beta;
  /* Multiply out modelled effects */
  diver_effect = sigma_diver * diver_raw;
  year_effect = sigma_year * year_raw;
  for (j in 1:n_years) {
    year_diver_effect[j] = sigma_year_diver[j] * year_diver_raw[j];
  }
  /* Gamma regression */
  for (j in 1:n) {
    real mu;
    /* Regression */
    mu = exp(intercept + log_effort[j] + diver_effect[diver[j]] + year_effect[year[j]] + year_diver_effect[year[j], diver[j]]);
    /* Rate */
    beta[j] = alpha / mu;
  }
}

model {
  /* Intercept */
  intercept ~ normal(0, 5);
  /* Modelled parameters */
  /* Diver */
  diver_raw ~ student_t(3, 0, 1);
  sigma_diver ~ cauchy(0, 2.5);
  /* Year */
  year_raw ~ student_t(3, 0, 1);
  sigma_year ~ cauchy(0, 2.5);
  /* Year by reef and year by diver */
  for (j in 1:n_years) {
    year_diver_raw[j] ~ student_t(3, 0, 1);
  }
  sigma_year_diver ~ cauchy(0, 2.5);
  /* Shape */
  alpha ~ cauchy(0, 2.5);
  /* Model */
  for (j in 1:n) {
    volume[j] ~ gamma(alpha, beta[j]);
  }
}

generated quantities {
  /* Standardised CPUE */
  real new_cpue[n_pred_year];
  /* In-sample posterior predictions */
  real y_ppc[n];
  /* Log-likelihood */
  real loglik[n];
  for (j in 1:n_pred_year) {
    /* Predicted CPUE for chosen years, fictional reef/diver */
    real new_diver;
    real new_diver_year;
    real new_mu;
    real new_beta;
    new_diver = sigma_diver * student_t_rng(3, 0, 1);
    new_diver_year = sigma_year_diver[pred_year[j]] * student_t_rng(3, 0, 1);
    new_mu = exp(intercept + new_diver + year_effect[pred_year[j]] + new_diver_year);
    new_beta = alpha / new_mu;
    new_cpue[j] = gamma_rng(alpha, new_beta);
  }
  for (j in 1:n) {
    y_ppc[j] = gamma_rng(alpha, beta[j]);
    loglik[j] = gamma_lpdf(volume[j] | alpha, beta[j]);
  }
}
