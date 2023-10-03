# set seed
set.seed(231003)

#data import
library(googlesheets4)
gs4_deauth() # #avoid GS4 to ask you to log in in google
url.db <- 'https://docs.google.com/spreadsheets/d/1L_DDxAoDRu1EYoPncjdbXfJmCSGcs-FESRuMs6i4rBo/edit?usp=sharing'
db <- read_sheet(url.db)

str(db)

# mean usefulness
mean(as.integer(factor(db$`was the advice useful?`))-1)
# number of errors 
mean(db$`Total number of error`)
round(proportions(table(db$`Total number of error`)),2)
propo
frequency()
hist(db$`Total number of error`)
hist

# let's start from the model of the
dat<- list(
  E = db$`Total number of error`,       # number of errors
  U = as.integer(
    as.factor(db$`was the advice useful?`)) - 1 ,       # usefulness
  A = db$`Question ID`,  # ID Answers
  P = as.integer(
    as.factor(db$`Email Address`)),  # ID pathologist
  f = as.integer(
    as.factor(db$Area)),  # field ID  
  S = as.integer(
    as.factor(db$`Scenario ID`)),  # scenario ID
  Pr = as.integer(
    as.factor(
      paste(db$type,db$reference))))  # Prompt type
dat$l_f <- length(unique(dat$f))
dat$l_Pr <- length(unique(dat$Pr))
dat$l_e <- length(dat$U)
dat$l_S <- length(unique(dat$S))
dat$l_a <- length(unique(dat$A)) 
dat$l_Pat <- length(unique(dat$P))


# model written in stan 
m <- cstan( file = 'scripts/mU_latA.stan', 
            data = dat, 
            chains = 4, 
            cores = 4, 
            iter = 2000)

dashboard(m)
precis(m)
precis(m,2,pars = 'sigma_u_P')
par(mfrow = c ( 1, 1)) # resetting graphical parameters
plot(precis(m))
plot(precis(m,2,pars = 'sigma_u_P'))

post <- extract.samples(m) # extract the posterior

# inspect the differences of the different 
# strategies at parameter level 
pic.size <- 2000

png('./figures/prompt_density.png', 
    width = pic.size, height = pic.size, res = 300)
plot(N, ylim = c(0,1.1), xlim = c(-1.5,1.5),
     xlab = '', ylab = 'Density', 
     main = 'Prompt modality')
abline(v = 0, lty = 2)
for (i in 1:4 ) dens(post$gamma[,i], col = i, lwd = 3, add = TRUE)
levels( as.factor(paste(db$type,db$reference)))
leg_text <- c('MC-NR', 'MC-R', 'OE-NR', 'OE-R')
legend('topleft',legend = leg_text, lwd = 3, col = 1:4)
dev.off()

U_link <- function( f, Pr, S){
  U <- with(post, {
    beta_K[ , f]*sigma_K })
  return(mean(sim_A))
}



plot(precis(m, 2, par = 'delta'))

plot(precis(m, 3, par = 'beta_PF'))

# the aim of the work is to assess the reliability 
# in addressing pathological problems/questions



simA <- mapply(A_link, f = dat$f[1:200], Pr = dat$Pr[1:200], S = dat$S[1:200])
plot(aA, simA, 
     main = 'A parameter', 
     xlab = 'True A', 
     ylab = 'Recovered A')

idx <- order(colMeans(post$beta_K))

png('./figures/latent_knowledge.png', 
    width = pic.size, height = pic.size, res = 300)
plot(N, ylim=c(-1.35,1.8), xlim = c(0.5,10.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Latent Knowledge in each field')
abline(h = 0)
s_factor <- 12 # scaling factor for graphics
for(i in 1:10) {
  y <- density(post$beta_K[,idx[i]] * post$sigma_K)$x
  x <- density(post$beta_K[,idx[i]] * post$sigma_K)$y
  polygon(i + x/s_factor, y, col = scales::alpha(i,0.6), border = FALSE)
  lines(i + x/s_factor, y, lwd = 1)
  polygon(i - x/s_factor, y, col = scales::alpha(i,0.6), lwd = 2, border = FALSE)
  lines(i - x/s_factor, y, lwd = 1)
}
axis(1, at = 1:10, labels = FALSE)
text(x = 1:10 - 0.1, -1.7,
     labels = levels(as.factor(db$Area))[idx],  
     srt=45,  adj=1,    xpd=TRUE)
dev.off()
