# set seed
set.seed(230423)

# library
library(rethinking)

# let's simulate the topics
# some topics  will be more covered
# in the trainig set of chatGTP, 
# we hence expect it to be important 
Tmax <- 10


N <- 1e5 # total number of questions 


a_bar <- rnorm( N , 0 , 1.5 )
sigma_A <- rexp( N, 1 )
sigma_B <- rexp( N , 1 )


# block interactions
mBT <- ulam(
    alist(
        H ~ bernoulli( p ) ,
        logit(p) <- b[S,R] + a[T],
      ## adaptive priors
        matrix[S,R]:b ~ dnorm( 0 , sigma_B ),
        a[T] ~ dnorm( a_bar , sigma_A ),
      ## hyper-priors
        a_bar ~ dnorm( 0 , 1.5 ),
        sigma_A ~ dexp(1),
        sigma_B ~ dexp(1)
    ) , data=dat , chains=4 , cores=4 )

mBTnc <- ulam(
    alist(
        P ~ bernoulli( p ) ,
        logit(p) <- a_bar + z_a[T]*sigma_A + z_b[S,R]*sigma_B ,
      ## adaptive priors
        matrix[S,R]:z_b ~ dnorm( 0 , 1 ),
        z_a[T] ~ dnorm( 0 , 1 ),
      ## hyper-priors
        a_bar ~ dnorm( 0 , 1.5 ),
        sigma_A ~ dexp(1),
        sigma_B ~ dexp(1),
        gq> vector[T]:a <<- a_bar + z_a*sigma_A,
        gq> matrix[S,R]:b <<- z_b*sigma_B
    ) , data=dat , chains=4 , cores=4 )

