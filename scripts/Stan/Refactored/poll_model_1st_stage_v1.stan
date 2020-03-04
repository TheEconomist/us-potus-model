data {
  // dimensions
  int N;
  int S;
  // index
  int<lower = 0, upper = S + 1> s[N];
  // data
  int n_respondents[N];
  int n_two_parties[N];
}
parameters {
  vector[S + 1] delta_raw;
  real delta_mu;
  real<lower = 0> delta_sigma;
}
transformed parameters {
  vector[S + 1] delta;
  delta = delta_mu + delta_raw * delta_sigma;
}
model {
  delta_raw ~ std_normal();
  delta_mu ~ normal(0, 1);
  delta_sigma ~ normal(0, 0.2);
  for (n in 1:N) n_two_parties[n] ~ binomial_logit_lpmf(n_respondents[n], delta[s[n]]);
}
generated quantities {
  int yrep[N];
  for (n in 1:N) yrep[n] = binomial_rng(n_respondents[n], inv_logit(delta[s[n]]));
}
