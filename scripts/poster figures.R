# run up to line 72 of 'real_data_analysis.R'
l <- length(dat$E)
f.width <- 15
f.heigh <- f.width/1.8
svg('./figures/figure3.svg', width = f.width, height = f.heigh)
par(mfrow = c(2,2))
sim_p <- matrix(nrow = 4000, ncol = l)
for(i in 1:l ) sim_p[,i] <- post$alpha_u + simA[,i]
sim_p <- inv_logit(c(sim_p))
l <- length(sim_p)
sim_U <- rbern(l,sim_p)
HPDI(sim_p)
mean(sim_p)
plot(NULL, ylim = c(- 0.1,2.5), xlim = c(0,1),
     xlab = 'Probability', ylab = 'Density', 
     main = 'Usefulness')
abline(v = 0.5, lty = 2)
dens(sim_p, lwd = 3, add = TRUE, adj = 1, col = 'red')
useful <- data.frame(proportions(table(sim_U)))
useful[,2] <- useful[,2] * 2.5
for(i in 1:2) lines(i-1, useful[i,2], lwd=6, type = 'h')
axis(4, at = c(0,1.25,2.5), labels = c("0","0.5", "1"))
mtext("A", side=3, line=1, cex=2, adj=-0.05)
l <- length(dat$E)
sim_lambda <- matrix(nrow = 4000, ncol = l)
for(i in 1:l ) sim_lambda[,i] <- post$alpha_e - simA[,i]
sim_lambda <- exp(c(sim_lambda))
l <- length(sim_lambda)
sim_E <- rpois(l,sim_lambda)
HPDI(sim_lambda)
mean(sim_lambda)
plot(NULL, ylim = c(0 ,0.5), xlim = c(0,20),
     xlab = 'Probability', ylab = 'Density', 
     main = 'Accuracy')
dens(sim_lambda, lwd = 3, add = TRUE, adj = 1, col = 'red')
errors <- data.frame(proportions(table(sim_E)))
errors[,2] <- errors[,2] * 0.5
for(i in 1:21) lines(i-1, errors[i,2], lwd=6, type = 'h')
axis(4, at = c(0,0.25,.5), labels = c("0","0.5", "1"))
mtext("B", side=3, line=1, cex=2, adj=-0.05)
idx <- order(colMeans(post$beta_K))
levels(as.factor(db$Area))[idx]
col <- c("#332288","#AA4499","#117733","#661100","#88CCEE","#DDCC77",
         "#CC6677","#999933","#44AA99","#882255")
new.lab = c("Urinary & Male\nGenital", "Female\nGenital", "Haematolymphoid",
            "CNS", "BST", "Endocrine and\nNeuroendocrine", "Breast", "Throacic",
            "Gastrointestinal","Skin")
par(mai = c(1.1, 0.8, 0.8, 0.4) + 0.02)
plot(NULL, ylim=c(-2,2), xlim = c(0.5,10.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Latent Knowledge in each field')
abline(h = 0)
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
text(x = 1:10, -2.6,
     labels = new.lab,  
     srt=40,  adj=1,    xpd=TRUE)
mtext("C", side=3, line=1, cex=2, adj=-0.05)
idx <- order(colMeans(post$gamma))
par(mai = c(1.1, 0.8, 0.8, 0.4) + 0.02)
s_factor <- 3.2 # scaling factor for graphics
plot(NULL, ylim=c(-2.2,2.2), xlim = c(0.5,4.5),
     xlab = '', ylab = 'Density', xaxt = 'null', 
     main = 'Prompt Modality')
abline(h = 0, lty = 2)
for(i in 1:4) {
  y <- density(post$gamma[,idx[i]])$x
  x <- density(post$gamma[,idx[i]])$y
  polygon(i + x/s_factor, y, col = scales::alpha(i,0.6), border = FALSE)
  lines(i + x/s_factor, y, lwd = 1)
  polygon(i - x/s_factor, y, col = scales::alpha(i,0.6), lwd = 2, border = FALSE)
  lines(i - x/s_factor, y, lwd = 1)
}
levels( as.factor(paste(db$type,db$reference)))[idx]
leg_text <- c( 'Open Ended\nReference','Open Ended\nNo Reference', 
               'Multiple Choice\nReference', 'Multiple Choice\nNo Reference')
text(x = 1:4, -3,
     labels = leg_text,  
     srt=0,  adj=0.5,    xpd=TRUE)
mtext("D", side=3, line=1, cex=2, adj=-0.05)
dev.off()


#--------------------------------



# figure 4 -------------------------------------------------------------- 
col <- c("#88CCEE","#CC6677","#DDCC77",
         "#117733", "#332288", "#AA4499", 
         "#44AA99", "#999933", "#882255", 
         "#661100")

field <- rep(1:10, each = 5 )
idx <- order(colMeans(post$delta))
y_text <- colMeans(post$delta)
svg('./figures/figure4.svg', 
    width = f.width, height = f.heigh)
par(mai = c(0, 0.8, 0.8, 0.4) + 0.02)
plot(NULL, ylim=c(-1.6,2.1), xlim = c(0.5,50.5),
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
new.lab = c( "BST","Breast","Endocrine and Neuroendocrine","Haematolymphoid",
             "Urinary & Male Genital", "Female Genital", "Gastrointestinal",
             "Throacic","Skin", "CNS")

legend('topleft', pch = rep(16,10), col = col, legend = new.lab)
dev.off()


url <- 'https://www.medrxiv.org/content/10.1101/2024.03.12.24304153v1'
code <- qrcode::qr_code(url)

svg('./figures/qr_code.svg')
plot(code)
dev.off()
