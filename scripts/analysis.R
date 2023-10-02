# set seed
set.seed(230628)

# analysis
# let's create a vector for the prompting strategy
Pr <-  as.factor(paste(A$Qt,A$Ref))
levels(Pr) #  "1 1" "1 2" "2 1" "2 2"
Pr <- as.integer(Pr)

# let's start from the model of the
dat<- list(
  E = E,       # number of errors
  U = U,       # usefulness
  A = Ev[,1],  # ID Answers
  P = Ev[,2],  # ID pathologist
  f = rep(A$f, 4),  # field ID  
  S = rep(A$S, 4),  # scenario ID
  Pr = rep(Pr, 4), # Prompt type
  l_f = length(unique(A$f)),
  l_Pr = length(unique(Pr)),
  l_e = length(U),
  l_S = length(unique(A$S)),
  l_a = length(unique(Ev[,1])) 
)


# model written in stan 
m <- cstan( file = 'scripts/mU_latA.stan', 
            data = dat, 
            chains = 4, 
            cores = 4, 
            iter = 2000)

dashboard(m)
precis(m)
par(mfrow = c ( 1, 1)) # resetting graphical parameters

post <- extract.samples(m) # extract the posterior

# plotting coefficients recovery
lambdamean  <- vector()
for (i in 1:800) lambdamean[i] <-  mean(post$lambda[,i])
plot(log(lambda), lambdamean, 
     main = 'Lambda parameter', 
     xlab = 'True log(lambda)', 
     ylab = 'Recovered log(lambda)')

pmean  <- vector()
for (i in 1:800) pmean[i] <-  mean(post$p[,i])
plot(logit(p), pmean, 
     main = 'p parameter', 
     xlab = 'True logit(p)', 
     ylab = 'Recovered logit(p)')


A_link <- function( f, Pr, S){
  sim_A <- with(post, {
    beta_K[ , f]*sigma_K + gamma[ , Pr]*sigma_Pr + delta[ , S]*sigma_S})
  return(mean(sim_A))
}

simA <- mapply(A_link, f = dat$f[1:200], Pr = dat$Pr[1:200], S = dat$S[1:200])
plot(aA, simA, 
     main = 'A parameter', 
     xlab = 'True A', 
     ylab = 'Recovered A')

plot(N, ylim=c(-4.5,6), xlim = c(0.5,10.5),
     xlab = 'Field', ylab = 'Density', xaxt = 'null', 
     main = 'Latent Knowledge in each field')
idx <- order(K)
leg_text <- c('simulated','recovered')
legend('bottomright', legend = leg_text, 
       lwd = c(2,NA), pch = c(NA,23))


for(i in 1:10) {
y <- density(post$beta_K[,idx[i]] * post$sigma_K)$x
x <- density(post$beta_K[,idx[i]] * post$sigma_K)$y
polygon(i + x/2, y, col = scales::alpha(i,0.6), border = FALSE)
lines(i + x/2, y, lwd = 1)
polygon(i - x/2, y, col = scales::alpha(i,0.6), lwd = 2, border = FALSE)
lines(i - x/2, y, lwd = 1)
segments(x0 = i - 0.5,
         x1 = i + 0.5,
         y0 = K[idx[i]],
         col = 1,
         lwd = 2)
}
