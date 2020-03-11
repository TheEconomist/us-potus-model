data{
  int N;    // Number of polls
  int T;    // Number of days
  int S;    // Number of states (for which at least 1 poll is available) + 1
  int P;    // Number of pollsters
  int<lower = 1, upper = S + 1> state[N]; // State index
  int<lower = 1, upper = T> day[N];   // Day index
  int<lower = 1, upper = P> poll[N];  // Pollster index
  vector[S] state_weights; // subtract 1 for national; based on 2012/16 turnouts
  // data
  int n_democrat[N];
  int n_respondents[N];
  real pred_two_share[N];
  // forward-backward
  int<lower = 1, upper = T> current_T;
  matrix[S, S] ss_corr_mu_b_walk;
  matrix[S, S] ss_corr_mu_b_T;
  matrix[S, S] ss_corr_error;
  //*** prior input
  // mu_a
  real<lower = 0> prior_sigma_a;
  // mu_b
  real<lower = 0> prior_sigma_b;
  vector[S] mu_b_prior; 
  // mu_c
  real<lower = 0> prior_sigma_c;
  // alpha
  real mu_alpha;    
  real sigma_alpha; 
  // measurement noise
  real<lower = 0> prior_sigma_measure_noise;
  // delta
  real prior_delta_sigma;
}
transformed data {
  row_vector[S] rv_state_weights;
  matrix[S, S] cholesky_ss_corr_mu_b_T;
  matrix[S, S] cholesky_ss_corr_mu_b_walk;
  matrix[S, S] cholesky_ss_corr_error;
  real nat_poll_error;
  cholesky_ss_corr_mu_b_T = cholesky_decompose(ss_corr_mu_b_T);
  cholesky_ss_corr_mu_b_walk = cholesky_decompose(ss_corr_mu_b_walk);
  cholesky_ss_corr_error = cholesky_decompose(ss_corr_error);
  nat_poll_error = diagonal(ss_corr_error)[1];
  rv_state_weights = to_row_vector(state_weights);
}
parameters {
  // alpha
  real raw_alpha;             
  // mu_a
  real<lower = 0> raw_sigma_a;
  real raw_mu_a[current_T];
  // mu_b
  real<lower = 0> raw_sigma_b;
  vector[S] raw_mu_b_T;
  matrix[S, T] raw_mu_b; // S state-specific components at time T
  // mu_c
  real<lower = 0> raw_sigma_c;
  vector[P] raw_mu_c;
  // u = measurement noise
  real <lower = 0> raw_sigma_measure_noise_state;
  real <lower = 0> raw_sigma_measure_noise_national;
  vector[N] measure_noise;
  // e polling error
  vector[S + 1] raw_polling_error;  // S state-specific polling errors (raw)
  // delta
  real raw_delta;
}

transformed parameters {
  //*** parameters
  // alpha
  real sigma_a;
  real alpha;
  // mu_a
  vector[current_T] mu_a;
  // mu_b
  real sigma_b;
  matrix[S, T] mu_b;
  // mu_c
  real sigma_c;
  vector[P] mu_c;
  // u
  real sigma_measure_noise_national;
  real sigma_measure_noise_state;
  // e 
  vector[S + 1] polling_error;  
  // delta
  real delta;
  //*** containers
  vector[current_T] sum_average_states; 
  real pi_democrat[N];
  //*** construct parameters
  // alpha
  alpha = mu_alpha + raw_alpha * sigma_alpha;
  // polling error
  polling_error[1:S] = cholesky_ss_corr_error * raw_polling_error[1:S]; // cholesky decomposition
  polling_error[S + 1] = nat_poll_error * raw_polling_error[S + 1];
  // mu_a
  sigma_a = raw_sigma_a * prior_sigma_a;
  mu_a[current_T] = 0;
  for (t in 1:(current_T - 1)) mu_a[current_T - t] = mu_a[current_T - t + 1] + raw_mu_a[current_T - t + 1] * sigma_a; 
  // mu_b
  sigma_b = raw_sigma_b * prior_sigma_b;
    // last (T)
  mu_b[:,T] = cholesky_ss_corr_mu_b_T * raw_mu_b_T + mu_b_prior;
    // forward from current poll (T - 1 to current_T)
  for (t in 1:(T - current_T)) mu_b[:,T - t] = cholesky_ss_corr_mu_b_walk * raw_mu_b[:, T - t] + mu_b[:, T - t + 1];
    // backward from current poll (current_T to 1)
  for (t in 1:(current_T - 1)) mu_b[:, current_T - t] = mu_b[:, current_T - t + 1] + raw_mu_b[:, current_T - t] * sigma_b;
  // mu_c
  sigma_c = raw_sigma_c * prior_sigma_c;
  mu_c = raw_mu_c * sigma_c;
  // u
  sigma_measure_noise_national = raw_sigma_measure_noise_national * prior_sigma_measure_noise;
  sigma_measure_noise_state = raw_sigma_measure_noise_state * prior_sigma_measure_noise;
  // delta
  delta = raw_delta * prior_delta_sigma;
  // averages
  for (t in 1:current_T) sum_average_states[t] = rv_state_weights * inv_logit(mu_b[:, t] + polling_error[1:S]);
  //*** fill pi_democrat
  for (i in 1:N){
    // national-level
    if (state[i] == 52){
      // sum_average_states = weights times state polls trends
      // alpha              = discrepancy adjustment
      // mu_c               = polling house effect
      // measure_noise      = noise of the individual poll
      pi_democrat[i] = mu_a[day[i]] + logit(sum_average_states[day[i]]) + polling_error[52] + alpha + mu_c[poll[i]] + 
        sigma_measure_noise_national * measure_noise[i] + delta * pred_two_share[i];
    } else {
      // state-level
      // mu_a               = national component
      // mu_b               = state component
      // mu_c               = poll component
      // measure_noise (u)  = state noise
      // polling_error (e)  = polling error term (state specific)
      pi_democrat[i] = mu_a[day[i]] + mu_b[state[i], day[i]] + mu_c[poll[i]] + 
        measure_noise[i] * sigma_measure_noise_state + polling_error[state[i]] + delta * pred_two_share[i];
    }
  }
}

model {
  //*** priors
  // alpha
  raw_alpha ~ std_normal();
  // mu_a
  raw_sigma_a ~ std_normal();
  raw_mu_a ~ std_normal_lpdf();
  // mu_b
  raw_mu_b_T ~ std_normal();
  raw_sigma_b ~ std_normal();
  for (s in 1:S) raw_mu_b[s] ~ std_normal();
  // mu_c
  raw_sigma_c ~ std_normal();
  raw_mu_c ~ std_normal();
  // measurement noise
  raw_sigma_measure_noise_state ~ std_normal();
  raw_sigma_measure_noise_national ~ std_normal();
  measure_noise ~ std_normal();
  // raw_polling_error
  raw_polling_error ~ std_normal();
  // raw_delta
  raw_delta ~ std_normal();
  //*** likelihood
  n_democrat ~ binomial_logit(n_respondents, pi_democrat);
}

generated quantities {
  matrix[T, S] predicted_score;
  for (s in 1:S){
    predicted_score[1:current_T, s] = inv_logit(mu_a[1:current_T] +
                                                to_vector(mu_b[s, 1:current_T]));
    predicted_score[(current_T + 1):T, s] = inv_logit(to_vector(mu_b[s, (current_T + 1):T]));
  }
}
