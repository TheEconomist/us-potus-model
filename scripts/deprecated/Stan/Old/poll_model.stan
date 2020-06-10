data{
    int S;    // Number of states (for which at least 1 poll is available) + 1
    int T;    // Number of days
    int N;    // Number of polls
    int W;    // Number of weeks
    int P;    // Number of pollsters
    int last_poll_W;
    int last_poll_T;
    int s[N]; // State index
    int t[N]; // Day index
    int w[N]; // Week index (weeks start on Tuesdays)
    int p[N]; // Pollster index
    int T_unique; // Number of (unique) days with at least one national poll
    int t_unique[N]; // = Which *unique national poll day index* does this poll correspond to?
    int unique_ts[T_unique]; 
    int unique_ws[T_unique];
    int week[last_poll_T];
    int day_of_week[last_poll_T];
    vector[S] state_weights;
    real alpha_prior;
    int n_clinton[N];
    int <lower = 0> n_respondents[N];
    vector [S] mu_b_prior;
    matrix [S-1, S-1] sigma_mu_b_end;
    matrix [S-1, S-1] sigma_walk_b_forecast;
    matrix [S-1, S-1] sigma_poll_error;
}

transformed data{
    // correlation matrixes for mu, sigma and correlated errors
    matrix [S-1, S-1] chol_sigma_walk_b_forecast;
    matrix [S-1, S-1] chol_sigma_mu_b_end;
    matrix [S-1, S-1] chol_sigma_poll_error;
    vector [S-1] zero_vec;
    int last_tuesday;
    // Cholesky decompositions to speed up sampling from multivariate normal.
    chol_sigma_walk_b_forecast = cholesky_decompose(sigma_walk_b_forecast);
    chol_sigma_mu_b_end = cholesky_decompose(sigma_mu_b_end);
    chol_sigma_poll_error = cholesky_decompose(sigma_poll_error);
    for (state in 1:(S-1)) zero_vec[state] = 0;
    last_tuesday = last_poll_T-day_of_week[last_poll_T];
}

parameters{
    vector[last_poll_T-1] delta_a;
    matrix[W-1,S] delta_b;
    vector[S] mu_b_end;
    vector[P] mu_c;
    real alpha_01;
    real<lower = 0, upper = 0.1> sigma_c;
    real u[N];
    real<lower = 0, upper = 0.1> sigma_u_national;
    real<lower = 0, upper = 0.1> sigma_u_state;
    real<lower = 0, upper = 0.05> sigma_walk_a_past;
    real<lower = 0, upper = 0.05> sigma_walk_b_past;
    vector[S-1] poll_error_01;
}

transformed parameters{
    real logit_clinton_hat[N];
    vector[last_poll_T] mu_a;
    matrix[W,S] mu_b;
    vector[T_unique] average_states;
    matrix[T_unique , S-1] matrix_inv_logit_mu_ab;
    vector[S-1] poll_error;
    real mu_a_t;
    real alpha;
    alpha = alpha_prior + alpha_01*0.2;
    poll_error = chol_sigma_poll_error * to_vector(poll_error_01);
    // Calculating mu_a: setting mu_a values to zero for the current week
    for (i in last_tuesday:last_poll_T)
        mu_a[i] = 0;
    // Calculating mu_a: reverse random walk
    for (i in 1:(last_tuesday- 1))
        mu_a[last_tuesday-i] = mu_a[last_tuesday-i+1] + sigma_walk_a_past * delta_a[last_tuesday-i];
    // Calculating mu_b (using the cholesky decompositions of covariance matrices)
    mu_b[W, 2:S] = to_row_vector(mu_b_prior[2:S] + chol_sigma_mu_b_end * to_vector(mu_b_end[2:S]));
    if (last_poll_W < W){
        for (wk in 1:(W - last_poll_W))
            mu_b[W - wk, 2:S] = mu_b[W - wk + 1, 2:S] + 
                                to_row_vector(chol_sigma_walk_b_forecast * to_vector(delta_b[W - wk, 2:S]));
    }
    for (wk in (W - last_poll_W + 1):(W-1))
        mu_b[W - wk, 2:S] = mu_b[W - wk + 1, 2:S] + sigma_walk_b_past * sqrt(7) * delta_b[W - wk, 2:S];
    for (wk in 1:W) 
        mu_b[wk, 1] = 0;
    // Creating a lookup table for national vote weighted average;
    // No need to compute values for every day:
    // Restricting the calculation to days in which there are national polls saves computation time.
    for(i in 1:T_unique){
        mu_a_t = mu_a[unique_ts[i]]; 
        for (state in 1:(S-1))
            // Careful here: state indices for poll_error are off by 1 relative to mu_b state indices.
            matrix_inv_logit_mu_ab[i, state] = inv_logit(mu_a_t + mu_b[unique_ws[i], state+1] + poll_error[state]);
    }
    average_states =  matrix_inv_logit_mu_ab * state_weights[2:S];
    // Calculating p_clinton_hat parameter for each national/state poll
    for(i in 1:N){
        if (s[i] == 1){
            // For national polls: 
            // p_clinton_hat is a function of: a national parameter **mu_a**,
            // the weighted average of the (interpolated) state parameters **mu_b**
            // an adjustment parameter **alpha** reflecting the fact that the average polled
            // state voter is not representative of the average US voter
            // and pollster house effects **mu_c**.
            logit_clinton_hat[i] = logit(average_states[t_unique[i]]) + alpha + sigma_c*mu_c[p[i]] + sigma_u_national*u[i];
        }
        else{
            // For state polls:
            // p_clinton_hat is a function of national and state parameters **mu_a**, **mu_b**
            // and pollster house effects **mu_c**
            logit_clinton_hat[i] = mu_a[t[i]] + mu_b[w[i], s[i]] + sigma_c*mu_c[p[i]] + sigma_u_state*u[i] 
                                 + poll_error[s[i]-1];
        }
    }
}

model{
    // mu_b_end, delta_a, & delta_b are drawn from Normal(0,1), and values for mu_a and mu_b
    // are calculated in the transformed parameters block; this speeds up convergence dramatically.
    // Prior of state parameters on election day mu_b
    mu_b_end[2:S] ~ normal(0, 1);
    // delta_a and delta_b are steps of the reverse random walks.
    delta_a ~ normal(0, 1); 
    for (wk in 1:(W-1))
        delta_b[W - wk, 2:S] ~ normal(0, 1);
    // Prior for the difference between national and weighted average of state parameters:
    alpha_01 ~ normal(0, 1);
    // Measurement error (one value per poll)
    u ~ normal(0, 1);
    // Polling error (one value per state; variance and correlation set by sigma_poll_error matrix)
    poll_error_01 ~ normal(0, 1);
    // Pollster house effects
    mu_c ~ normal(0, 1);
    // Likelihood of the model:
    n_clinton ~ binomial_logit(n_respondents, logit_clinton_hat);
}

generated quantities{
    matrix[last_tuesday + W - last_poll_W, S] predicted_score;
    // Predicted scores have *daily* values for past dates (since they depend on mu_b AND mu_a parameters),
    // but *weekly* values for future dates (since they only depend on mu_b).
    for (state in 2:S){
        // Backward estimates (daily)
        for (date in 1:last_tuesday){
            // Linear interpolation of mu_b values between previous and current week,
            // or current and next week
            // Using the following weights, depending on day_of_week:
            //         w-1    w       w+1
            // 0 Tue   3/7    4/7
            // 1 Wed   2/7    5/7
            // 2 Thu   1/7    6/7
            // 3 Fri   0      7/7
            // 4 Sat          6/7    1/7
            // 5 Sun          5/7    2/7
            // 6 Mon          4/7    3/7
            if (day_of_week[date] <= 3){ // 0=Sun, 1=Mon, 2=Tue, 3=Wed
                predicted_score[date, state] =
                           inv_logit(mu_a[date] + (1.0-(4+day_of_week[date])/7.0)*mu_b[max(week[date]-1, 1), state]
                                                +     ((4+day_of_week[date])/7.0)*mu_b[week[date], state]);
            }
            else{ // 4=Thu, 5=Fri, 6=Sat
                predicted_score[date, state] = // Linear interpolation between current and next week
                           inv_logit(mu_a[date] +     ((10-day_of_week[date])/7.0)*mu_b[week[date], state]
                                                + (1.0-(10-day_of_week[date])/7.0)*mu_b[min(week[date]+1, W), state]);
            }
            // predicted_score[date, state] = // Just a little bit of linear interpolation between weeks
            //            inv_logit(mu_a[date] + (1.0-day_of_week[date]/7.0)*mu_b[week[date], state]
            //                                 +     (day_of_week[date]/7.0)*mu_b[min(week[date]+1, W), state]);
        }
        // Forward estimates (weekly)
        for (date in (last_tuesday+1):(last_tuesday + W - last_poll_W))
            predicted_score[date, state] = inv_logit(mu_b[last_poll_W + date - last_tuesday, state]);
    }
    for (date in 1:(last_tuesday + W - last_poll_W))
        // National score: averaging state scores by state weights.
        predicted_score[date, 1] = predicted_score[date, 2:S] * state_weights[2:S];
}
