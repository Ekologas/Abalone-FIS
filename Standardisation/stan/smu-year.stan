////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Title: SMU by Year
// Author: Steve Lane
// Date: Sunday, 01 April 2018
// Synopsis: This program fits a model within an SMU. It uses yearly
// catch/effort data to remove any month effects, and includes a year intercept.
// Requires prediction years to be entered for standardisation.
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
  /* Number of Reefs */
  int<lower=0> n_reefs;
  /* Number of divers */
  int<lower=0> n_divers;
  /* Number of years */
  int<lower=0> n_years;
  /* Reef identifier */
  int<lower=1,upper=n_reefs> reef[n];
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
  /* Reef modelled effects */
  vector[n_reefs] reef_raw;
  real<lower=0> sigma_reef;
  /* Diver modelled effects */
  vector[n_divers] diver_raw;
  real<lower=0> sigma_diver;
  /* Year modelled effects */
  vector[n_years] year_raw;
  real<lower=0> sigma_year;
  /* Shape of Gamma regression */
  real<lower=0> alpha;
}

transformed parameters {
  /* Reef modelled effects */
  vector[n_reefs] reef_effect;
  /* Diver modelled effects */
  vector[n_divers] diver_effect;
  /* Year modelled effects */
  vector[n_years] year_effect;
  /* Inverse rate of Gamma regression */
  vector[n] beta;
  /* Multiply out modelled effects */
  reef_effect = sigma_reef * reef_raw;
  diver_effect = sigma_diver * diver_raw;
  year_effect = sigma_year * year_raw;
  /* Gamma regression */
  for (j in 1:n) {
    real mu;
    /* Regression */
    mu = exp(intercept + log_effort[j] + reef_effect[reef[j]] + diver_effect[diver[j]] + year_effect[year[j]]);
    /* Rate */
    beta[j] = alpha / mu;
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
  /* Year */
  year_raw ~ student_t(3, 0, 1);
  sigma_year ~ cauchy(0, 2.5);
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
    real new_reef;
    real new_diver;
    real new_mu;
    real new_beta;
    new_reef = sigma_reef * student_t_rng(3, 0, 1);
    new_diver = sigma_diver * student_t_rng(3, 0, 1);
    new_mu = exp(intercept + new_reef + new_diver + year_effect[pred_year[j]]);
    new_beta = alpha / new_mu;
    new_cpue[j] = gamma_rng(alpha, new_beta);
  }
  for (j in 1:n) {
    y_ppc[j] = gamma_rng(alpha, beta[j]);
    loglik[j] = gamma_lpdf(volume[j] | alpha, beta[j]);
  }
}
