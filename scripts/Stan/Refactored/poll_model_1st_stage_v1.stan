data {
  // dimensions
  int N;
  int S;
  // index
  int<lower = 0, upper = S + 1> s[N];
  // data
  int n_respondents;
  int n_two_parties;
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
  delta_raw ~ std_normal();
  n_two_parties ~ binomial_logit_lpmf(n_respondents, delta);
}
