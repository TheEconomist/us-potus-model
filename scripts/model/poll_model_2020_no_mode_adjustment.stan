data{
  int N_national;    // Number of polls
  int N_state;    // Number of polls
  int T;    // Number of days
  int S;    // Number of states (for which at least 1 poll is available) + 1
  int P;    // Number of pollsters
  int Pop;    // Number of poll populations
  int<lower = 1, upper = S + 1> state[N_state]; // State index
  int<lower = 1, upper = T> day_state[N_state];   // Day index
  int<lower = 1, upper = T> day_national[N_national];   // Day index
  int<lower = 1, upper = P> poll_state[N_state];  // Pollster index
  int<lower = 1, upper = P> poll_national[N_national];  // Pollster index
  int<lower = 1, upper = Pop> poll_pop_state[N_state];  // Poll mode index
  int<lower = 1, upper = Pop> poll_pop_national[N_national];  // Poll mode index
  int n_democrat_national[N_national];
  int n_two_share_national[N_national];
  int n_democrat_state[N_state];
  int n_two_share_state[N_state];
  vector<lower = 0, upper = 1.0>[N_national] unadjusted_national;
  vector<lower = 0, upper = 1.0>[N_state] unadjusted_state;
  int<lower = 1, upper = T> current_T;
  cov_matrix[S] ss_cov_mu_b_walk;
  cov_matrix[S] ss_cov_mu_b_T;
  cov_matrix[S] ss_cov_error;
  //*** prior input
  vector[S] mu_b_prior; 
  vector[S] state_weights;
  real sigma_a;
  real sigma_c;
  real sigma_pop;
  real sigma_measure_noise_national;
  real sigma_measure_noise_state;
}
transformed data {
  cholesky_factor_cov[S] cholesky_ss_cov_mu_b_T;
  cholesky_factor_cov[S] cholesky_ss_cov_mu_b_walk;
  cholesky_factor_cov[S] cholesky_ss_cov_error;
  cholesky_ss_cov_mu_b_T = cholesky_decompose(ss_cov_mu_b_T);
  cholesky_ss_cov_mu_b_walk = cholesky_decompose(ss_cov_mu_b_walk);
  cholesky_ss_cov_error = cholesky_decompose(ss_cov_error);
}
parameters {
  real raw_mu_a[T];
  vector[S] raw_mu_b_T;
  matrix[S, T] raw_mu_b; 
  vector[P] raw_mu_c;
  vector[Pop] raw_mu_pop;
  vector[N_national] raw_measure_noise_national;
  vector[N_state] raw_measure_noise_state;
  vector[S] raw_polling_error; 
  real mu_b_T_model_estimation_error;
}
transformed parameters {
  //*** parameters
  vector[T] mu_a;
  matrix[S, T] mu_b;
  vector[P] mu_c;
  vector[Pop] mu_pop;
  vector[S] polling_error = cholesky_ss_cov_error * raw_polling_error;
  vector[T] national_mu_b_average;
  real national_polling_error_average = transpose(polling_error) * state_weights;
  //*** containers
  vector[N_state] logit_pi_democrat_state;
  vector[N_national] logit_pi_democrat_national;
  //*** construct parameters
  mu_a[T] = 0;
  for (i in 1:(T-1)) mu_a[T - i] = raw_mu_a[T - i] * sigma_a + mu_a[T + 1 - i];
  mu_b[:,T] = cholesky_ss_cov_mu_b_T * raw_mu_b_T * mu_b_T_model_estimation_error + mu_b_prior;
  for (i in 1:(T-1)) mu_b[:, T - i] = cholesky_ss_cov_mu_b_walk * raw_mu_b[:, T - i] + mu_b[:, T + 1 - i];
  national_mu_b_average = transpose(mu_b) * state_weights;
  mu_c = raw_mu_c * sigma_c;
  mu_pop = raw_mu_pop * sigma_pop;
  //*** fill pi_democrat
  for (i in 1:N_state){
    logit_pi_democrat_state[i] = 
      mu_a[day_state[i]] + 
      mu_b[state[i], day_state[i]] + 
      mu_c[poll_state[i]] + 
      mu_pop[poll_pop_state[i]] + 
      raw_measure_noise_state[i] * sigma_measure_noise_state + 
      polling_error[state[i]];
  }
  logit_pi_democrat_national = 
    mu_a[day_national] + 
    national_mu_b_average[day_national] + 
    mu_c[poll_national] + 
    mu_pop[poll_pop_national] + 
    raw_measure_noise_national * sigma_measure_noise_national +
    national_polling_error_average;
}

model {
  //*** priors
  raw_mu_a ~ std_normal();
  raw_mu_b_T ~ std_normal();
  mu_b_T_model_estimation_error ~ scaled_inv_chi_square(7, 1);
  to_vector(raw_mu_b) ~ std_normal();
  raw_mu_c ~ std_normal();
  raw_mu_pop ~ std_normal();
  raw_measure_noise_national ~ std_normal();
  raw_measure_noise_state ~ std_normal();
  raw_polling_error ~ std_normal();
  //*** likelihood
  n_democrat_state ~ binomial_logit(n_two_share_state, logit_pi_democrat_state);
  n_democrat_national ~ binomial_logit(n_two_share_national, logit_pi_democrat_national);
}

generated quantities {
  matrix[T, S] predicted_score;
  for (s in 1:S){
    predicted_score[1:T, s] = inv_logit(mu_a[1:T] + to_vector(mu_b[s, 1:T]));
  }
}

