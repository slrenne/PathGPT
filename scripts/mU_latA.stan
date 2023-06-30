// 
data{
    int l_A; // length of the dataset
    int l_Pr; // length of the prompts
    int l_f; // length of the fiels
    int l_S; // length of the scenarios
    int P[l_A]; // Pathologist ID in answers
    int A[l_A]; // answer ID in answers
    int E[l_A]; // Error count ID in answers
    int U[l_A]; // usefulness evaluation ID in answers
    int S[l_A]; // scenario ID in answers
    int Pr[l_A]; // prompts id in answers
    int f[l_A];  // field ID in answers
}
parameters{
    real alpha_u;
    real alpha_e;
    vector[l_Pr] gamma;
    vector[l_S] delta;
    vector[l_f] beta_K;
    real<lower=0> sigma_K;
    real<lower=0> sigma_Pr;
    real<lower=0> sigma_S;
    matrix[l_f,4] z_u_P;
    vector[l_A] zeta_u;
    vector<lower=0>[l_f] sigma_u_P;
    cholesky_factor_corr[l_f] L_Rho_u_P;
}
transformed parameters{
    vector[l_A] mu; // common model to estimate K
    mu =  beta_K[f]*sigma_K + gamma[Pr]*sigma_Pr + delta[S]*sigma_S;

    matrix[4,l_f] beta_PF;
    beta_PF = (diag_pre_multiply(sigma_u_P, L_Rho_u_P) * z_u_P)';

    // model for usefulness
    vector[l_A] p;
    for(i in 1:l_A) {
      p[i] = alpha_u + mu[i] + beta_PF[P[i], f[i]];
    }

    // model for errors
    vector[l_A] lambda;
    for(i in 1:l_A) {
      lambda[i] = - 1 * alpha_e + mu[i] + beta_PF[P[i], f[i]];
    }
}
model{
    L_Rho_u_P ~ lkj_corr_cholesky( 2 );

    // hyperpriors of sigma
    sigma_u_P ~ exponential( 0.5 );
    sigma_S ~ exponential( 0.5 );
    sigma_Pr ~ exponential( 0.5 );
    sigma_K ~ exponential( 0.5 );
    
    // hyperpriors expressed as 'z-scores'
    beta_K ~ normal( 0 , 0.5 );
    delta ~ normal( 0 , 0.5 );
    gamma ~ normal( 0 , 0.5 );

    // priors for the intercepts 
    alpha_u ~ normal( 0 , 0.5 );  
    alpha_e ~ normal( 0 , 0.5 );

    // models
    U ~ bernoulli_logit( p );
    E ~ poisson_log( lambda );
}
generated quantities{
  matrix[l_f,l_f] Rho_u_P;
  Rho_u_P =  multiply_lower_tri_self_transpose(L_Rho_u_P);    
}
