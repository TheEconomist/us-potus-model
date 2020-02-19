data{
    int N;    // Number of polls
    int T;    // Number of days
    int S;    // Number of states (for which at least 1 poll is available) + 1
    int P;    // Number of pollsters
    int<lower = 1, upper = S> state[N]; // State index
    int<lower = 1, upper = T> day[N];   // Day index
    int<lower = 1, upper = P> poll[N];  // Pollster index
    real state_weights[S - 1]; // subtract 1 for national; based on 2012/16 turnouts
    
    // data
    int n_democrat[N];
    int n_respondents[N];
    
    // forward-backward
    int<lower = 1, upper = T> current_T;
    matrix[S - 1, S - 1] ss_correlation;
    
    // prior input
    real<lower = 0> prior_sigma_a;
    real<lower = 0> prior_sigma_b;
    vector[S - 1] mu_b_prior; // mu_b
    real<lower = 0> prior_sigma_mu_c; // mu_c
    real mu_alpha;    // delta_S = sum_{s = 2}^S(w_s * pi_s) - pi_n
    real sigma_alpha; // 0.1 in Kramp
    real<lower = 0> prior_sigma_measure_noise;

}

transformed data {
  matrix[S - 1, S - 1] cholesky_ss_correlation;
  vector[S] zero_vec;     // zero vector for polling error
  cholesky_ss_correlation = cholesky_decompose(ss_correlation);
}

parameters {
  real raw_alpha;             // corrects for possible discrepancies between national polls and the weighted average of state polls
  vector[S - 1] raw_polling_error;  // S state-specific polling errors (raw)
  real raw_poll_house[P];
  // mu_a
  real<lower = 0, upper = prior_sigma_a> sigma_a;
  real raw_mu_a[current_T];
  // mu_b
  real<lower = 0, upper = prior_sigma_b> sigma_b;
  real raw_mu_b[T, S - 1]; // S state-specific components at time T

  // mu_c
  real<lower = 0, upper = prior_sigma_mu_c> sigma_mu_c;
  vector[P] raw_mu_c;

  // u = measurement noise
  real <lower = 0, upper = prior_sigma_measure_noise> sigma_measure_noise_state;
  real <lower = 0, upper = prior_sigma_measure_noise> sigma_measure_noise_national;
  vector[N] measure_noise;
  
  // e polling error
  
}

transformed parameters {
  // parameters
    // alpha
  real alpha;
    // mu_a
  real mu_a[current_T];
    // mu_b
  vector[S - 1] mu_b[T];
    // mu_c
  vector[P] mu_c;
    // e - S - 1 state-specific polling errors
  vector[S - 1] polling_error;  

  // containers
  vector[current_T] sum_average_states; // set to current_T and have days without available data be ignored
  real pi_democrat[N];

  // construct parameters
  // alpha
  alpha = mu_alpha + raw_alpha * sigma_alpha;
  // polling error
  polling_error = cholesky_ss_correlation * raw_polling_error; // cholesky decomposition
  // mu_a
  mu_a[current_T] = 0;
  for (t in 1:(current_T - 1)) mu_a[current_T - t] = mu_a[current_T - t + 1] + raw_mu_a[current_T - t + 1] * sigma_a; 
 
  // mu_b
    // last (T)
  mu_b[T] = cholesky_ss_correlation * mu_b_prior;
    // forward from current poll (T - 1 to current_T)
  for (t in 1:(T - current_T)) mu_b[T - t] = cholesky_ss_correlation * mu_b[T - t + 1];
    // backward from current poll (current_T to 1)
  for (t in 1:(current_T - 1)) mu_b[current_T - t] = mu_b[current_T - t + 1] * sigma_b * sqrt(7);
  
  // mu_c
  mu_c = raw_mu_c * sigma_mu_c;
  
  for (t in 1:current_T){
    sum_average_states[t] = to_row_vector(state_weights) * inv_logit((mu_a[t] + mu_b[t] + polling_error));
  }

  // construct pi_democrat
  for (i in 1:N){
    if (state[i] == 51){
      // sum_average_states = weights times state polls trends
      // alpha              = discrepancy adjustment
      // mu_c               = polling house effect
      // measure_noise      = noise of the individual poll
      pi_democrat[i] = sum_average_states[day[i]] + alpha + mu_c[poll[i]] + sigma_measure_noise_national * measure_noise[i];
    } else {
      // mu_a               = national component
      // mu_b               = state component
      // mu_c               = poll component
      // measure_noise (u)  = state noise
      // polling_error (e)  = polling error term (state specific)
      pi_democrat[i] = mu_a[day[i]] + mu_b[day[i], state[i]] + 
                       mu_c[poll[i]] + measure_noise[state[i]] + sigma_measure_noise_state * polling_error[state[i]];    
    }
  }
}

model {
  // alpha
  target += std_normal_lpdf(raw_alpha);
  
  // measurement noise
  target += std_normal_lpdf(measure_noise);

  // mu_a
  target += uniform_lpdf(sigma_a | 0, 0.05);
  target += std_normal_lpdf(raw_mu_a);

  // mu_b
  target += uniform_lpdf(sigma_b | 0, 0.05);
  for (s_id in 1:(S - 1)) target += std_normal_lpdf(mu_b[:, s_id]);
  
  // mu_c
  target += std_normal_lpdf(raw_mu_c);
  
  
  target += binomial_logit_lpmf(n_democrat | n_respondents, pi_democrat);
}
