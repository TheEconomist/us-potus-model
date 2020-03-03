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
  real delta_sigma;
}
transformed parameters {
  vector[S + 1] delta;
  delta = delta_mu + delta_raw * delta_sigma;
}
model {
  vector[N] pi_delta;
  for (n in 1:N) pi_delta[n] = delta[s[n]];
  delta_raw ~ std_normal();
  n_two_parties ~ binomial_logit_lpmf(n_respondents, pi_delta);
}
generated quantities {
  int yrep[N];
  for (n in 1:N) yrep[n] = binomial_rng(n_respondents[n], logit(delta[n]));
}
