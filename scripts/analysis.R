# set seed
set.seed(230628)

# library
library(rethinking)

# analysis
# let's create a vector for the promting strategy
Pr <-  as.factor(paste(A$Qt,A$Ref))
levels(Pr) #  "1 1" "1 2" "2 1" "2 2"
Pr <- as.integer(Pr)

# let's start from the model of the
dat<- list(
  E = E,
  U = U,       # usefulness
  A = Ev[,1],  # ID Answers
  P = Ev[,2],  # ID pathologist
  f = rep(A$f, 4),  # field ID  
  S = rep(A$S, 4),  # scenario ID
  Pr = rep(Pr, 4), # Prompt type
  l_f = length(unique(A$f)),
  l_Pr = length(unique(Pr))
)

m.u <- ulam(
  alist(
    U ~ bernoulli(p),
    logit(p) <- 
      alpha_u_bar + 
      zeta_u[A]*sigma_u_a + 
      beta_u[P,f] + 
      gamma_u[Pr]*sigma_u_Pr + 
      delta_u[S]*sigma_u_S +
      epsilon_u[f]*sigma_u_f,
    
    # adaptive priors - non-centered
    transpars> matrix[P,l_f]:beta_u <-
      compose_noncentered( sigma_u_P , L_Rho_u_P , z_u_P ),
    matrix[l_f,P]:z_u_P ~ normal( 0 , 1 ),    
    
    # fixed priors
    alpha_u_bar ~ normal(0,1.5),
    zeta_u[A] ~ normal(0,1.5),
    gamma_u[Pr] ~ normal(0,1.5),
    delta_u[S] ~ normal(0,1.5),
    epsilon_u[f] ~ normal(0,1.5),
    sigma_u_a ~ dexp(1),
    sigma_u_Pr ~ dexp(1),
    sigma_u_S ~ dexp(1),
    sigma_u_f ~ dexp(1),
    vector[l_f]:sigma_u_P ~ dexp(1),
    cholesky_factor_corr[l_f]:L_Rho_u_P ~ lkj_corr_cholesky( 2 ),
    
    # computing ordinary correlation matrix from Cholesky factors
    gq> matrix[l_f,l_f]:Rho_u_P <<- multiply_lower_tri_self_transpose(L_Rho_u_P)
    
    ) , data = dat, chains = 4, cores = 8, cmdstan = TRUE, threads = 2) 



m.e <- ulam(
  alist(
    E ~ dpois(lambda),
    log(lambda) <- -1 *alpha_e_bar + 
    zeta_u[A]*sigma_u_a + 
    beta_e[P,f] + 
    gamma_u[Pr]*sigma_u_Pr + 
    delta_u[S]*sigma_u_S +
    epsilon_u[f]*sigma_u_f,
    
    # adaptive priors - non-centered
    transpars> matrix[P,l_f]:beta_e <-
      compose_noncentered( sigma_e_P , L_Rho_e_P , z_e_P ),
    matrix[l_f,P]:z_e_P ~ normal( 0 , 1 ),    
    
    # fixed priors
    alpha_e_bar ~ normal(0,1.5),
    zeta_u[A] ~ normal(0,1.5),
    gamma_u[Pr] ~ normal(0,1.5),
    delta_u[S] ~ normal(0,1.5),
    epsilon_u[f] ~ normal(0,1.5),
    sigma_u_a ~ dexp(1),
    sigma_u_Pr ~ dexp(1),
    sigma_u_S ~ dexp(1),
    sigma_u_f ~ dexp(1),
    vector[l_f]:sigma_e_P ~ dexp(1),
    cholesky_factor_corr[l_f]:L_Rho_e_P ~ lkj_corr_cholesky( 2 ),
    
    # computing ordinary correlation matrix from Cholesky factors
    gq> matrix[l_f,l_f]:Rho_e_P <<- multiply_lower_tri_self_transpose(L_Rho_e_P)
    
    ) , data = dat, chains = 4, cores = 8, cmdstan = TRUE, threads = 2) 


m <- ulam(
  alist(
    
    # model of usefulness
    U ~ bernoulli(p), 
    
    # model of error counts
    E ~ dpois(lambda),
    
    # model of usefulness
    logit(p) <- 
      alpha_u_bar + 
      zeta_u[A]*sigma_u_a + 
      beta_u[P,f] + 
      gamma_u[Pr]*sigma_u_Pr + 
      delta_u[S]*sigma_u_S +
      epsilon_u[f]*sigma_u_f,
    
    # model of error counts
    log(lambda) <- -1 *alpha_e_bar + 
      zeta_u[A]*sigma_u_a + 
      beta_e[P,f] + 
      gamma_u[Pr]*sigma_u_Pr + 
      delta_u[S]*sigma_u_S +
      epsilon_u[f]*sigma_u_f,
    
    # adaptive priors - non-centered
    transpars> matrix[P,l_f]:beta_u <-
      compose_noncentered( sigma_u_P , L_Rho_u_P , z_u_P ),
    matrix[l_f,P]:z_u_P ~ normal( 0 , 1 ),  
    transpars> matrix[P,l_f]:beta_e <-
      compose_noncentered( sigma_e_P , L_Rho_e_P , z_e_P ),
    matrix[l_f,P]:z_e_P ~ normal( 0 , 1 ),   
    
    # fixed priors
    alpha_e_bar ~ normal(0,1.5),
    alpha_u_bar ~ normal(0,1.5),
    zeta_u[A] ~ normal(0,1.5),
    gamma_u[Pr] ~ normal(0,1.5),
    delta_u[S] ~ normal(0,1.5),
    epsilon_u[f] ~ normal(0,1.5),
    sigma_u_a ~ dexp(1),
    sigma_u_Pr ~ dexp(1),
    sigma_u_S ~ dexp(1),
    sigma_u_f ~ dexp(1),
    vector[l_f]:sigma_u_P ~ dexp(1),
    cholesky_factor_corr[l_f]:L_Rho_u_P ~ lkj_corr_cholesky( 2 ),
    vector[l_f]:sigma_e_P ~ dexp(1),
    cholesky_factor_corr[l_f]:L_Rho_e_P ~ lkj_corr_cholesky( 2 ),
    
    # computing ordinary correlation matrix from Cholesky factors
    gq> matrix[l_f,l_f]:Rho_u_P <<- multiply_lower_tri_self_transpose(L_Rho_u_P),
    gq> matrix[l_f,l_f]:Rho_e_P <<- multiply_lower_tri_self_transpose(L_Rho_e_P)
    
  ) , data = dat, chains = 4, cores = 8, cmdstan = TRUE, threads = 2) 
