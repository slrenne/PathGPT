# set seed
set.seed(230423)

# library
library(rethinking)

# let's simulate the fields
# some fields  will be more covered
# in the trainig set of chatGPT, 
N <- 1e5 
Fmax <- 5L

# the knowledge K of chatGPT will be therefore different for the different topics
# K will be a latent variable
Kbar <- rnorm(N)
# we will create 10 different knowledges related to the field

K <- matrix( ncol = Fmax , nrow = N)

for (i in 1 : Fmax) {
    for (j in 1 : N) {
      m <- i - 5 + Kbar[j]
      K[j,i] <- rnorm( 1, m /2 )
    }}


Title <- "Maximum knowledge differences among Fields"
plot(NULL, 
     xlim = c(-6,6),
     ylim = c(0,0.35),
     main = Title,
     xlab = "knowledge", 
     ylab = "Density")
abline(v = 0, lty = 2)
dens(K[,1], lwd = 3, main = Title, add = TRUE)
dens(K[,Fmax], add = TRUE, lwd = 3, col = 2)
dens(K[,Fmax] - K[,1], add = TRUE, lwd = 3, col = 3, show.HPDI = 0.89)
legend('topright', lwd = 3, col = 1:3, 
       legend = c('lowest',
                  'highest',
                  'contrast'))

# with the clinical scenario and the prompting strategy (open ended/ multiple choice
# and with or without references) we elicits different answers





 


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

summary(K)
