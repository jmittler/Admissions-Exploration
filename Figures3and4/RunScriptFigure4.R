set.seed(98195)

### Base simulation -- Committees evaluate applications perfectly
tiff_plot<- FALSE
Figure_Label = ""
n_reps <- 100
rand_table <- matrix(0,nrow=n_reps,ncol=15)
for (jj in 1:n_reps) {
  source("RandomParams.R")
  s8 <- 22.5;
  source("PreVsPostPredictors3.R")
  cat("r.squared_random_attending: ",r.squared_random_attending,"r.squared_attending: ",r.squared_attending,"\n")
  cat("cc_random_attending: ",cc_random_attending,"cc_attending: ",cc_attending,"\n")
  cat("pval_attending: ",pval_random_attending,"pval_attending: ",pval_attending,"\n")
  rand_table[jj,1] <- as.numeric(r.squared_random_attending)
  rand_table[jj,2] <- as.numeric(r.squared_attending)
  rand_table[jj,3] <- as.numeric(cc_random_attending)
  rand_table[jj,4] <- as.numeric(cc_attending)
  rand_table[jj,5] <- as.numeric(pval_random_attending)
  rand_table[jj,6] <- as.numeric(pval_attending)
  rand_table[jj,7] <- s1/(s1+s2+s3+s4+s5)
  rand_table[jj,8] <- w1/(w1+w2+w3+w4+w5+w6+w7)
  rand_table[jj,9] <- h1/(h1+h2+h3+h4+h5+h6+h7)
  rand_table[jj,10] <- as.numeric(r.squared6_random_attending)
  rand_table[jj,11] <- as.numeric(r.squared6_attending)
  rand_table[jj,12] <- as.numeric(cc6_random_attending) 
  rand_table[jj,13] <- as.numeric(cc6_attending)
  rand_table[jj,14] <- as.numeric(pval6_random_attending)
  rand_table[jj,15] <- as.numeric(pval6_attending)
}

tiff(filename = "Figure4_90chance_new.tiff",
     width = 4, height = 8, units = "in", pointsize = 12, res=1000,
     compression = "lzw")
par(mfrow=c(2,1))
x_vals <- c(3,7)
y_vals <- c(rand_table[1,3],rand_table[1,4])
plot(x_vals,y_vals,xaxt = "n", xlim = c(2,9), ylim=c(min(rand_table[,3],rand_table[,4]), max(rand_table[,3],rand_table[,4])),type="b",col="blue",
     xlab = "", ylab ="Regression coefficent [Factor 1]")

axis(1, at=c(3,7), labels=c("Random","Filtered"))
abline(h=0,col="grey")
for (jj in 2:n_reps) {
  y_vals <- c(rand_table[jj,3],rand_table[jj,4])
  if (y_vals[2] > y_vals[1]) lines(x_vals,y_vals,type= "b",col="red")
  if (y_vals[2] <= y_vals[1]) lines(x_vals,y_vals,type= "b",col="blue")
}
mtext("(a)",side=3,line=1.5,at=-20,cex=1.2)

y_vals <- c(rand_table[1,12],rand_table[1,13])
plot(x_vals,y_vals,xaxt = "n", xlim = c(2,9), ylim=c(min(rand_table[,12],rand_table[,13]), max(rand_table[,12],rand_table[,13])),type="b",col="blue",
     xlab = "", ylab ="Regression coefficent [Factor 6]")

axis(1, at=c(3,7), labels=c("Random","Filtered"))
abline(h=0,col="grey")
for (jj in 2:n_reps) {
  y_vals <- c(rand_table[jj,12],rand_table[jj,13])
  if (y_vals[2] > y_vals[1]) lines(x_vals,y_vals,type= "b",col="red")
  if (y_vals[2] <= y_vals[1]) lines(x_vals,y_vals,type= "b",col="blue")
}
mtext("(b)",side=3,line=1.5,at=-20,cex=1.2)
dev.off()

cat("Simulations for which Regr Coeff between GRE and performance increased after filtering\n")
for (jj in 1:n_reps) {
   if (rand_table[jj,3] < rand_table[jj,4]) {
     print(rand_table[jj,])
   }
}
cat("Number of simulations in which filtering resulted in a negative regression coefficient for the GRE : ",length(which(rand_table[,4]<0)),"\n")
cat("Number of simulations in which filtering resulted in a statistically significant negative regression coefficient for the GRE : ",
    length(which(rand_table[,4]<0 & rand_table[,6]<0.05)),"\n")
cat("Number of simulations in which filtering failed to yield a significantly positive relationship for the GRE : ",
     length(which(rand_table[,4]<0 | rand_table[,6]>=0.05)),"\n")
cat("Number of simulations in which filtering resulted in a statistically significant negative regression coefficient for the irrelevant variable : ",
     length(which(rand_table[,13]<0 & rand_table[,15]<0.05)),"\n")
cat("Number of simulations in which random filtering gave a statistically significant negative regression coefficient for the irrelevant variable : ",
    length(which(rand_table[,12]<0 & rand_table[,14]<0.05)),"\n")


