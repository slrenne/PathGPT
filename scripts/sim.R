# set seed
set.seed(230423)

# library
library(rethinking)

# let's simulate the fields
# some fields  will be more covered
# in the trainig set of chatGPT, 
N <- 200 # total number of anwers
f <- 10L # different fields
S <- 50L # different scenarios
Qt <- 2L # max number of question types
Ref <- 2L # type of reference (with or without)
Pa <- 4L # number of pathologists
a0<- (-2) #alpha coefficient


# let's start simulating the answers

A <- expand.grid(1:S, 1:Qt, 1:Ref) # combine the three variables
A <- data.frame(A) #makes a df
colnames(A) <- c('S', 'Qt', 'Ref') # retitle the column names
A$f <- as.integer( trunc((A$S - 1)/5) + 1 ) # create also the fields

K <- rnorm(f) # knowledge simulated for each field
bS <- rnorm(S) # the effect of each scenario
P_coeff <- matrix(c(1,2,3,4), ncol =  2) # Prompt coefficient
P <- vector(length = N) # Prompt
for( i in 1 : N) P[i] <- P_coeff [A$Qt[i],A$Ref[i]]
aA <- rnorm(N, K[A$f] + P + bS[A$S]) # simulate the answer

# Now let's simulate the pathologists evaluation.
# simulate the pathologists' affinity to the field
Pa_f <- matrix(
  replicate(Pa, 
            sample((1:f - 5)/2, size = f, replace = FALSE)),
  ncol = 4
)

 N * Pa # total number of evaluations
Pa_coeff <- rnorm(Pa)
Ev <- data.frame(expand.grid(1:N,1:Pa))
mu <- vector(length = nrow((Ev)))
for (i in 1 : nrow(Ev)) {
  mu[i] <- aA[Ev[i,1]] + Pa_coeff[Ev[i,2]] - 2
}
# simulating usefulness
p <- inv_logit(mu)
U <- rbern(nrow(Ev), p)
hist(U)
# simulating number of errors
lambda <- exp(- mu)
E <- rpois(nrow(Ev), lambda)
dens(E)
plot(U,E)
