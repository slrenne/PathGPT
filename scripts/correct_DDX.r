# create a list of random number for 'correct' answer in DDX
# 

set.seed(230424)
a <- 1:5
a.seq <- sample(x = a, size = 150, replace = TRUE)
a.seq <- data.frame(a.seq)
write.csv(a.seq, 'order.csv')

