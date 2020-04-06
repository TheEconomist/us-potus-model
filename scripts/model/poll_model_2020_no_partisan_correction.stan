data{
  int N_national;    // Number of polls
  int N_state;    // Number of polls
  int T;    // Number of days
  int S;    // Number of states (for which at least 1 poll is available) + 1
  int P;    // Number of pollsters
  int<lower = 1, upper = S + 1> state[N_state]; // State index
  int<lower = 1, upper = T> day_state[N_state];   // Day index
  int<lower = 1, upper = T> day_national[N_national];   // Day index
  int<lower = 1, upper = P> poll_state[N_state];  // Pollster index
  int<lower = 1, upper = P> poll_national[N_national];  // Pollster index
  int n_democrat_national[N_national];
  int n_two_share_national[N_national];
  int n_democrat_state[N_state];
  int n_two_share_state[N_state];
  vector<lower = 0, upper = 1.0>[N_national] unadjusted_national;
  vector<lower = 0, upper = 1.0>[N_state] unadjusted_state;
  int<lower = 1, upper = T> current_T;
  matrix[S, S] ss_corr_mu_b_walk;
  matrix[S, S] ss_corr_mu_b_T;
  matrix[S, S] ss_corr_error;
  //*** prior input
  real<lower = 0> prior_sigma_a;
  real<lower = 0> prior_sigma_b;
  vector[S] mu_b_prior; 
  real<lower = 0> prior_sigma_c;
  real<lower = 0> prior_sigma_e_bias;
  real<lower = 0> prior_sigma_mu_e_bias;
  real mu_alpha;    
  real sigma_alpha; 
  real<lower = 0> prior_sigma_measure_noise;
}
transformed data {
  matrix[S, S] cholesky_ss_corr_mu_b_T;
  matrix[S, S] cholesky_ss_corr_mu_b_walk;
  matrix[S, S] cholesky_ss_corr_error;
  cholesky_ss_corr_mu_b_T = cholesky_decompose(ss_corr_mu_b_T);
  cholesky_ss_corr_mu_b_walk = cholesky_decompose(ss_corr_mu_b_walk);
  cholesky_ss_corr_error = cholesky_decompose(ss_corr_error);
}
parameters {
  real raw_alpha;             
  real<lower = 0> raw_sigma_a;
  real raw_mu_a[current_T];
  real<lower = 0> raw_sigma_b;
  vector[S] raw_mu_b_T;
  matrix[S, T] raw_mu_b; 
  real<lower = 0> raw_sigma_c;
  vector[P] raw_mu_c;
  real <lower = 0> raw_sigma_measure_noise_state;
  real <lower = 0> raw_sigma_measure_noise_national;
  vector[N_national] measure_noise_national;
  vector[N_state] measure_noise_state;
  vector[S] raw_polling_error; 
}
transformed parameters {
  //*** parameters
  real sigma_a;
  real alpha;
  vector[current_T] mu_a;
  real sigma_b;
  matrix[S, T] mu_b;
  real sigma_c;
  vector[P] mu_c;
  real sigma_measure_noise_national;
  real sigma_measure_noise_state;
  vector[S] polling_error;  
  //*** containers
  vector[N_state] logit_pi_democrat_state;
  vector[N_national] logit_pi_democrat_national;
  //*** construct parameters
  alpha = mu_alpha + raw_alpha * sigma_alpha;
  polling_error = cholesky_ss_corr_error * raw_polling_error; // cholesky decomposition
  sigma_a = raw_sigma_a * prior_sigma_a;
  mu_a[current_T] = 0;
  for (t in 1:(current_T - 1)) mu_a[current_T - t] = mu_a[current_T - t + 1] + raw_mu_a[current_T - t + 1] * sigma_a; 
  sigma_b = raw_sigma_b * prior_sigma_b;
  mu_b[:,T] = cholesky_ss_corr_mu_b_T * raw_mu_b_T + mu_b_prior;
  for (t in 1:(T - current_T)) mu_b[:, T - t] = cholesky_ss_corr_mu_b_walk * raw_mu_b[:, T - t] + mu_b[:, T - t + 1];
  for (t in 1:(current_T - 1)) mu_b[:, current_T - t] = mu_b[:, current_T - t + 1] + raw_mu_b[:, current_T - t] * sigma_b;
  sigma_c = raw_sigma_c * prior_sigma_c;
  mu_c = raw_mu_c * sigma_c;
  sigma_measure_noise_national = raw_sigma_measure_noise_national * prior_sigma_measure_noise;
  sigma_measure_noise_state = raw_sigma_measure_noise_state * prior_sigma_measure_noise;
  //*** fill pi_democrat
  for (i in 1:N_state){
    logit_pi_democrat_state[i] = 
      mu_a[day_state[i]] + 
      mu_b[state[i], day_state[i]] + 
      mu_c[poll_state[i]] + 
      measure_noise_state[i] * sigma_measure_noise_state + polling_error[state[i]];
  }
  logit_pi_democrat_national = 
    mu_a[day_national] + 
    alpha + 
    mu_c[poll_national] + 
    measure_noise_national * sigma_measure_noise_national;
}
model {
  //*** priors
  raw_alpha ~ std_normal();
  raw_sigma_a ~ std_normal();
  raw_mu_a ~ std_normal();
  raw_mu_b_T ~ std_normal();
  raw_sigma_b ~ std_normal();
  to_vector(raw_mu_b) ~ std_normal();
  raw_sigma_c ~ std_normal();
  raw_mu_c ~ std_normal();
  raw_sigma_measure_noise_state ~ std_normal();
  raw_sigma_measure_noise_national ~ std_normal();
  measure_noise_national ~ std_normal();
  measure_noise_state ~ std_normal();
  raw_polling_error ~ std_normal();
  //*** likelihood
  n_democrat_state ~ binomial_logit(n_two_share_state, logit_pi_democrat_state);
  n_democrat_national ~ binomial_logit(n_two_share_national, logit_pi_democrat_national);
}

generated quantities {
  matrix[T, S] predicted_score;
  for (s in 1:S){
    predicted_score[1:current_T, s] = inv_logit(mu_a[1:current_T] + to_vector(mu_b[s, 1:current_T]));
    predicted_score[(current_T + 1):T, s] = inv_logit(to_vector(mu_b[s, (current_T + 1):T]));
  }
}
