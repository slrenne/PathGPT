

#data import
#library(googlesheets4)
#gs4_deauth() # #avoid GS4 to ask you to log in in google
#url.db <- 'https://docs.google.com/spreadsheets/d/1L_DDxAoDRu1EYoPncjdbXfJmCSGcs-FESRuMs6i4rBo/edit?usp=sharing'
#db <- read_sheet(url.db)

library(vioplot)
# set seed
set.seed(231003)
library(rethinking)

db <- read.csv('PathGPT_Evaluation_Final_V1.csv',sep=";")


# let's start from the model of the
dat<- list(
  E = db$Total.number.of.error,       # number of errors
  U = as.integer(
    as.factor(db$was.the.advice.useful.)) - 1 ,       # usefulness
  A = db$Question.ID,  # ID Answers
  P = as.integer(
    as.factor(db$Username)),  # ID pathologist
  f = as.integer(
    as.factor(db$Area)),  # field ID  
  S = as.integer(
    as.factor(db$Scenario.ID)),  # scenario ID
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
m <- cstan( file = 'mU_latA.stan', 
            data = dat, 
            chains = 4, 
            cores = 4, 
            iter = 2000)

#dashboard(m)
#precis(m)
#precis(m,2,pars = 'sigma_u_P')
par(mfrow = c ( 1, 1)) # resetting graphical parameters
#plot(precis(m))
#plot(precis(m,2,pars = 'sigma_u_P'))

post <- extract.samples(m) # extract the posterior

#plot(precis(m, 3, par = 'beta_PF'))


A_link <- function( f, Pr, S){
  sim_A <- with(post, {
    beta_K[ , f]*sigma_K + gamma[ , Pr]*sigma_Pr + delta[ , S]*sigma_S})
}

l <- length(dat$E)
simA <- mapply(A_link, f = dat$f[1:l], Pr = dat$Pr[1:l], S = dat$S[1:l])

sim_p <- matrix(nrow = 4000, ncol = l)
for(i in 1:l ) sim_p[,i] <- post$alpha_u + simA[,i]
sim_p <- inv_logit(c(sim_p))
l <- length(sim_p)
sim_U <- rbern(l,sim_p)
HPDI(sim_p)
mean(sim_p)
png(filename="PlotFull.png", width=4000, height=3500, bg="white",res=300)
par(mfrow = c(2, 2),mar=c(8,6,4,3)+.1)


line = 1
cex = 2
side = 3
adj=-0.05

#png('./figures/usefulness.png', 
#    width = pic.size, height = pic.size, res = 300)

# FIRST PLOT -------------------------------------------------

plot(NULL, ylim = c(- 0.1,2.5), xlim = c(0,1),
     xlab = 'Probability', ylab = 'Density', 
     main = 'Usefulness')
abline(v = 0.5, lty = 2)
dens(sim_p, lwd = 3, add = TRUE, adj = 1, col = 'red')
useful <- data.frame(proportions(table(sim_U)))
useful[,2] <- useful[,2] * 2.5
for(i in 1:2) lines(i-1, useful[i,2], lwd=6, type = 'h')
axis(4, at = c(0,1.25,2.5), labels = c("0","0.5", "1"))
mtext("A", side=side, line=line, cex=cex, adj=adj)

# FIRST PLOT -------------------------------------------------

### to check 

l <- length(dat$E)
sim_lambda <- matrix(nrow = 4000, ncol = l)
for(i in 1:l ) sim_lambda[,i] <- post$alpha_e - simA[,i]
sim_lambda <- exp(c(sim_lambda))
l <- length(sim_lambda)
sim_E <- rpois(l,sim_lambda)
HPDI(sim_lambda)
mean(sim_lambda)


#png('./figures/accuracy.png', 
#    width = pic.size, height = pic.size, res = 300)
# SECOND PLOT -------------------------------------------------
plot(NULL, ylim = c(0 ,0.5), xlim = c(0,20),
     xlab = 'Probability', ylab = 'Density', 
     main = 'Accuracy')
dens(sim_lambda, lwd = 3, add = TRUE, adj = 1, col = 'red')
errors <- data.frame(proportions(table(sim_E)))
errors[,2] <- errors[,2] * 0.5
for(i in 1:21) lines(i-1, errors[i,2], lwd=6, type = 'h')
axis(4, at = c(0,0.25,.5), labels = c("0","0.5", "1"))
mtext("B", side=side, line=line, cex=cex, adj=adj)
# SECOND PLOT -------------------------------------------------


# THIRD PLOT ----------------------------------------------------

col <- c("#332288","#AA4499","#117733","#661100","#88CCEE","#DDCC77",
         "#CC6677","#999933","#44AA99","#882255")


idx <- order(colMeans(post$beta_K))
names = c("Urinary & Male\nGenital", "Female\nGenital", "Haematolymphoid",
          "CNS", "BST", "Endocrine and\nNeuroendocrine", "Breast", "Throacic",
          "Gastrointestinal","Skin")
#png('./figures/latent_knowledge.png', 
#    width = pic.size, height = pic.size, res = 300)
plot(NULL, ylim=c(-2,2), xlim = c(0.5,10.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Latent Knowledge in Each Field')
abline(h = 0, lty=2)
s_factor <- 2 # scaling factor for graphics
for(i in 1:10) {
  y <- density(post$beta_K[,idx[i]])$x
  x <- density(post$beta_K[,idx[i]])$y
  polygon(i + x/s_factor, y, col = scales::alpha(col[[i]],0.6), border = FALSE)
  lines(i + x/s_factor, y, lwd = 1)
  polygon(i - x/s_factor, y, col = scales::alpha(col[[i]],0.6), lwd = 2, border = FALSE)
  lines(i - x/s_factor, y, lwd = 1)
}
axis(1, at = 1:10, labels = FALSE)
text(x = 1:10, -2.3,
     labels = names,  
     srt=75,  adj=1,    xpd=TRUE)
mtext("C", side=side, line=line, cex=cex, adj=adj)

#levels(as.factor(db$Area))[idx]
# THIRD PLOT ----------------------------------------------------


# FOURTH PLOT -------------------------------------------------
pic.size <- 2000

idx <- order(colMeans(post$gamma))
#png('./figures/prompt_density.png', 
#    width = pic.size, height = pic.size, res = 300)
s_factor <- 3.2 # scaling factor for graphics
plot(NULL, ylim=c(-2.2,2.2), xlim = c(0.5,4.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Prompt Modality')
abline(h = 0, lty = 2)
for(i in 1:4) {
  y <- density(post$gamma[,idx[i]])$x
  x <- density(post$gamma[,idx[i]])$y
  polygon(i + x/s_factor, y, col = scales::alpha(col[[i]],0.6), border = FALSE)
  lines(i + x/s_factor, y, lwd = 1)
  polygon(i - x/s_factor, y, col = scales::alpha(col[[i]],0.6), lwd = 2, border = FALSE)
  lines(i - x/s_factor, y, lwd = 1)
  
  
  #vioplot::vioplot(post$gamma[,idx[i]],at=i,drawRect=FALSE,col = scales::alpha(col[[i]],0.6),add =TRUE)
  
}
levels(as.factor(paste(db$type,db$reference)))[idx]
leg_text <- c( 'Open Ended\nReference','Open Ended\nNo Reference', 
               'Multiple Choice\nReference', 'Multiple Choice\nNo Reference')
axis(1, at = 1:4, labels = FALSE)
text(x = 1:4, -2.6,
     labels = leg_text,  
     srt=75,  adj=1,    xpd=TRUE)
mtext("D", side=side, line=line, cex=cex, adj=adj)


# FOURTH PLOT -------------------------------------------------
dev.off()


# the aim of the work is to assess the reliability 
# in addressing pathological problems/questions




# ten color colorblinf friendly 
col <- c("#88CCEE","#CC6677","#DDCC77",
         "#117733", "#332288", "#AA4499", 
         "#44AA99", "#999933", "#882255", 
         "#661100")

field <- rep(1:10, each = 5 )
idx <- order(colMeans(post$delta))
y_text <- colMeans(post$delta)

#png('./figures/scenario_effect.png', 
#    width = pic.size*2, height = pic.size*1.4, res = 300)

plot(NULL, ylim=c(-1.6,1.9), xlim = c(0.5,50.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Effect of scenarios')
abline(h = 0)
s_factor <- 4 # scaling factor for graphics
for(i in 1:50) {
  y <- density(post$delta[,idx[i]])$x
  x <- density(post$delta[,idx[i]])$y
  polygon(i + x/s_factor, y, col = scales::alpha(col[field[idx[i]]],0.6), border = FALSE)
  lines(i + x/s_factor, y, lwd = 2)
  polygon(i - x/s_factor, y, col = scales::alpha(col[field[idx[i]]],0.6), lwd = 2, border = FALSE)
  lines(i - x/s_factor, y, lwd = 2)
  text(i, y_text[idx[i]], labels = idx[i])
}
lab <- c('Soft', 'Breast', 'Endocrine', 'Hemolymph', 
         'Uro', 'Gyn', 'GI', 'Thoracic', 'Skin', 'CNS')
legend('topleft', pch = rep(16,10), col = col, legend = lab)

