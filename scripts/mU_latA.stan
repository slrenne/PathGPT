// 
data{
    int l_e; // length of the dataset (all the evaluations)
    int l_Pr; // length of the prompts
    int l_f; // length of the fiels
    int l_S; // length of the scenarios
    int l_a; // length of the answers
    int P[l_e]; // Pathologist ID in evaluation
    int A[l_e]; // answer ID in evaluation
    int E[l_e]; // Error count ID in evaluation
    int U[l_e]; // usefulness evaluation ID in evaluation
    int S[l_e]; // scenario ID in answers
    int Pr[l_e]; // prompts id in answers
    int f[l_e];  // field ID in answers
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
    vector<lower=0>[l_f] sigma_u_P;
    cholesky_factor_corr[l_f] L_Rho_u_P;
}
transformed parameters{
    // model of the answers
    vector[l_e] mu;
    mu =  beta_K[f]*sigma_K + gamma[Pr]*sigma_Pr + delta[S]*sigma_S;
    
    // VCV matrix pathology/field   
    matrix[4,l_f] beta_PF;
        beta_PF = (diag_pre_multiply(sigma_u_P, L_Rho_u_P) * z_u_P)';
    
    //moldel common to both GLMs
     for(i in 1:l_e) {
      mu[i] = mu[ i ] + beta_PF[P[i], f[i]];
    }
    
    // model of usefulness 
    vector[l_e] p;
     for(i in 1:l_e) {
      p[i] = alpha_u + mu[ i ];
    }
    
    // model of errors
    vector[l_e] lambda;
    for(i in 1:l_e) {
      lambda[i] = alpha_e - mu[ i ];
    }
}
model{
    L_Rho_u_P ~ lkj_corr_cholesky( 2 );
    sigma_u_P ~ exponential( 2 );
    sigma_S ~ exponential( 2 );
    sigma_Pr ~ exponential( 2 );
    sigma_K ~ exponential( 2 );
    to_vector( z_u_P ) ~ normal( 0, 0.5 );
    beta_K ~ normal( 0 , 0.5 );
    delta ~ normal( 0 , 0.5 );
    gamma ~ normal( 0 , 0.5 );
    alpha_u ~ normal( 0 , 0.5 );  
    alpha_e ~ normal( 0 , 0.5 );
    U ~ bernoulli_logit( p );
    E ~ poisson_log( lambda );
}
generated quantities{
  matrix[l_f,l_f] Rho_u_P;
  Rho_u_P =  multiply_lower_tri_self_transpose(L_Rho_u_P);
  
}
