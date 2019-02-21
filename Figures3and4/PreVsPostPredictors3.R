### Code below assumes that authors of PLoS ONE-type papers have access to variables 1-4
library(MASS)
HighLowResults <- c(0,0,0,0,0,0,0,0)
mu <- c(0,0)  # Mean for variables 1 and 2
Sigma12 <- matrix(Corr12,nrow=2,ncol=2)+ diag(2)*(1-Corr12)
Sigma34 <- matrix(Corr34,nrow=2,ncol=2)+ diag(2)*(1-Corr34)

# Variables (as normal random variables)
if (new_simulation_data == 1) {
  V12 <- mvrnorm(Sample_size,mu,Sigma12)
  V34 <- mvrnorm(Sample_size,mu,Sigma34)
  Var1n <- V12[,1] # GRE 
  Var2n <- V12[,2] # GPA 
  Var3n <- V34[,1] # Research experience
  Var4n <- V34[,2] # Ability to work in lab 
  Var5n <- runif(Sample_size) # Subtle / unquantifable contributions to success correctly fished out by committee members
  Var6n <- runif(Sample_size) # Irrelevant/quirky criteria used by the admissions committee
  Var7n <- runif(Sample_size) # Irrelevant/quirky criteria used by Harvard's admissions committee 
  Var8n <- runif(Sample_size) # Unidentifiable and/or chance factors contributing to success
} else {
  if (length(Var1n) != Sample_size) cat("Error: Need to re-run with 'new_simulation_data' set to TRUE.\n")
}

# Variables expressed as rank within each category
Var1 <- rank(Var1n)/Sample_size; Var2 <- rank(Var2n)/Sample_size; Var3 <- rank(Var3n)/Sample_size;
Var4 <- rank(Var4n)/Sample_size; Var5 <- rank(Var5n)/Sample_size; Var6 <- rank(Var6n)/Sample_size;
Var7 <- rank(Var7n)/Sample_size; Var8 <- rank(Var8n)/Sample_size; 


Admissions_Criteria             <- w1*Var1 + w2*Var2 + w3*Var3 + w4*Var4 + w5*Var5 + w6*Var6 + w7*Var7 + w8*Var8
Harvards_Admissions_Criteria    <- h1*Var1 + h2*Var2 + h3*Var3 + h4*Var4 + h5*Var5 + h6*Var6 + h7*Var7 + h8*Var8
Grad_School_Performance         <- s1*Var1 + s2*Var2 + s3*Var3 + s4*Var4 + s5*Var5 + s6*Var6 + s7*Var7 + s8*Var8 
Observable <- Var1 + Var2 + Var3 + Var4  # Criteria used by GRE efficacy investigators


# Pre-admissions Regrelations using variable 1 only
#if (key_graphs_only) par(mfrow=c(2,1)) else par(mfrow=c(4,2))
jm_regr <- lm(Grad_School_Performance ~ Var1)
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (key_graphs_only == FALSE) {
  plot(Var1,Grad_School_Performance,xlab="Admissions criteria 1", ylab = "Performance in Graduate School", 
     main=paste("Correlations -- Admit everyone\n[N = ",Sample_size," Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
} 
# Pre-admissions correlations using all available variables
All_Observable_Variables <- Var1+Var2+Var3+Var4
jm_regr <- lm(Grad_School_Performance ~ All_Observable_Variables)
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (key_graphs_only == FALSE) {
  plot(Var1+Var2+Var3+Var4,Grad_School_Performance,xlab="Sum of all Observable admissions criteria",ylab = "Performance in Graduate School", 
     main=paste("Correlations -- Admit everyone\n [N = ",Sample_size,", Adj R2 = ",r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
}
#cat("***********Association between Variable 1 and Grad_School_Performance before filtering*********")
#print(cor.test(Var1,Grad_School_Performance))

# Post-admissions correlations using variable 1 only

admitted <- which(rank(Admissions_Criteria)/length(Admissions_Criteria) > Admissions_Cutoff) # Restrict admissions to the best
jm_regr <- lm(Grad_School_Performance[admitted] ~ Var1[admitted])
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (key_graphs_only == FALSE) {
  plot(Var1[admitted],Grad_School_Performance[admitted],xlab="Admissions criteria 1",ylab = "Performance in Graduate School", 
     main=paste("Correlations -- All admitted applicants attend\n[N = ",length(admitted),paste("(",round(100*length(admitted)/Sample_size,0),"%) Adj R2 = ",sep=""),
                r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
}
# Post-admissions correlations using all available variables
All_Observable_Variables <- Var1[admitted]+Var2[admitted]+Var3[admitted]+Var4[admitted]
jm_regr <- lm(Grad_School_Performance[admitted] ~ All_Observable_Variables)
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (key_graphs_only == FALSE) {
  plot(Var1[admitted]+Var2[admitted]+Var3[admitted]+Var4[admitted],Grad_School_Performance[admitted],xlab="Sum of all Admissions_Criteria admissions criteria",
     ylab = "Performance in Graduate School",  main=paste("Correlations -- All admitted applicants attend\n[N = ",length(admitted),
                                                          ", Adj R2 = ",r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
}
results <- summary(jm_regr)

#cat("**********Association between Variable 1 and Success after filtering out those with poor applications **********")
#print(cor.test(Var1[admitted],Grad_School_Performance[admitted]))

# Post-admissions, post-acceptance correlations using variable 1
attending <- which(rank(Admissions_Criteria)/length(Admissions_Criteria) > Admissions_Cutoff & 
                   rank(Harvards_Admissions_Criteria)/length(Harvards_Admissions_Criteria) < Attends_Harvard_Instead) # Restrict admissions to the best, but very best go to Harvard!

# Post-admissions, post-acceptance correlations using variable 1
num_attending <- length(attending)
random_attending <- sample(c(1:Sample_size),num_attending)

jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending])
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

r.squared_random_attending <- r.squared
cc_random_attending <- cc
pval_random_attending <- pval

if (tiff_plot == FALSE) {
   plot(Var1[random_attending],Grad_School_Performance[random_attending],xlab="Admissions criteria 1",ylab = "Performance in Graduate School",
        main=paste("Correlations -- Random attendees (Same N)\n[N = ",length(random_attending),", Adj R2 = ",r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
} else {
   #plot(Var1[random_attending],Grad_School_Performance[random_attending],xlim=c(0,1),ylim=c(0,10), xlab="Admissions criteria 1",ylab = "Performance")
   #legend(-0.08,10.7,paste("Regr Coeff = ",cc,", P = ",pval,sep=""),cex=0.8,text.col="red",bty="n")
} 

# Post-admissions, post-acceptance correlations using variable 6

jm_regr6_random_attending <- lm(Grad_School_Performance[random_attending] ~ Var6[random_attending])
results6_random_attending <- summary(jm_regr6_random_attending);
cc6_random_attending <- round(results6_random_attending$coefficients[2,1],2);

pval6_random_attending <- results6_random_attending$coefficients[2,4]
if (pval6_random_attending < 1e-99) pval6_random_attending = 1e-99
pval6_random_attending <- format(pval6_random_attending,digits = 3)

r.squared6_random_attending <- format(results6_random_attending$adj.r.squared,digits =2)

# Post-admissions, post-acceptance correlations using variable 1
jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending])
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

r.squared <- format(results$adj.r.squared,digits =2)
r.squared_attending <- r.squared
cc_attending <- cc
pval_attending <- pval

if (tiff_plot == FALSE) {
  plot(Var1[attending],Grad_School_Performance[attending],xlab="Admissions criteria 1",ylab = "Performance in Graduate School",
       main=paste("Correlations -- Attendees\n[N = ",length(attending),paste("(",round(100*length(attending)/Sample_size,0),"%) Adj R2 = ",sep=""),
                r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
} else {
  plot(Var1[attending],Grad_School_Performance[attending],xlim=c(0,1),xlab="Admissions criteria 1",ylab = "Performance")
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext(Figure_Label,side=3,line=1.5,at=-0.20,cex=1.2)
}
abline(jm_regr,col="red")


# Post-admissions, post-acceptance correlations using variable 6

jm_regr6_attending <- lm(Grad_School_Performance[attending] ~ Var6[attending])
results6_attending <- summary(jm_regr6_attending);

cc6_attending <- round(results6_attending$coefficients[2,1],2);

r.squared6_attending <- format(results6_attending$adj.r.squared,digits =2)

pval6_attending <- results6_attending$coefficients[2,4]
if (pval6_attending < 1e-99) pval6_attending = 1e-99
pval6_attending <- format(pval6_attending,digits = 3)

# Post-admissions, post-acceptance correlations all available variables
All_Observable_Variables <- Var1[attending]+Var2[attending]+Var3[attending]+Var4[attending]
jm_regr <- lm(Grad_School_Performance[attending] ~ All_Observable_Variables)
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)


if (key_graphs_only == FALSE) {plot(Var1[attending]+Var2[attending]+Var3[attending]+Var4[attending],Grad_School_Performance[attending],xlab="Sum of all Admissions_Criteria admissions criteria",
     ylab = "Performance in Graduate School", main=paste("Correlations -- Attendees\n[N = ",length(attending),", Adj R2 = ",r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
}
#cat("**********Association between Variable 1 and Success amonst those who attend **********")
#print(cor.test(Var1[attending],Grad_School_Performance[attending]))

#cat("**********Association between Variable 1 and Success after random subsetting with same sample size **********")
#print(cor.test(Var1[admitted],Grad_School_Performance[admitted]))



# Post-admissions, post-acceptance correlations all available variables
All_Observable_Variables <- Var1[random_attending]+Var2[random_attending]+Var3[random_attending]+Var4[random_attending]
jm_regr <- lm(Grad_School_Performance[random_attending] ~ All_Observable_Variables)
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (key_graphs_only == FALSE) {
  plot(Var1[random_attending]+Var2[random_attending]+Var3[random_attending]+Var4[random_attending],Grad_School_Performance[random_attending],xlab="Sum of all Admissions_Criteria admissions criteria",
     ylab = "Performance in Graduate School", main=paste("Correlations -- Random attendees (same N) \n[N = ",length(random_attending),", Adj R2 = ",r.squared,", Regr Coeff = ",cc,", P = ",pval,"]"))
  abline(jm_regr,col="red")
}
#cat("**********Association between Variable 1 and Success amonst those who attend **********")
#print(cor.test(Var1[random_attending],Grad_School_Performance[random_attending]))

##################################
##### Multivariate analyses ######
##################################
Table1 <- matrix(NA,nrow=4,ncol=4)
Table2 <- matrix(NA,nrow=4,ncol=4)
jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on random applicants using... var 1: Adjusted R2 = ",r.squared,"\n")
Table1[1,1] <- results$coefficients[2,4]

jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending] + Var2[random_attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on random applicants using vars 1-2: Adjusted R2 = ",r.squared,"\n")
Table1[1,2] <- results$coefficients[2,4]
Table1[2,2] <- results$coefficients[3,4]


jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending] + Var2[random_attending] + Var3[random_attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on random applicants using vars 1-3: Adjusted R2 = ",r.squared,"\n")
Table1[1,3] <- results$coefficients[2,4]
Table1[2,3] <- results$coefficients[3,4]
Table1[3,3] <- results$coefficients[4,4]

jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending] + Var2[random_attending] + Var3[random_attending] + Var4[random_attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on random applicants using vars 1-4: Adjusted R2 = ",r.squared,"\n")
Table1[1,4] <- results$coefficients[2,4]
Table1[2,4] <- results$coefficients[3,4]
Table1[3,4] <- results$coefficients[4,4]
Table1[4,4] <- results$coefficients[5,4]

# Multivariate analyses
jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on attendees using... var 1: Adjusted R2 = ",r.squared,"\n")
Table2[1,1] <- results$coefficients[2,4]

jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending] + Var2[attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on attendees using vars 1-2: Adjusted R2 = ",r.squared,"\n")
Table2[1,2] <- results$coefficients[2,4]
Table2[2,2] <- results$coefficients[3,4]

jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending] + Var2[attending] + Var3[attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on attendees using vars 1-3: Adjusted R2 = ",r.squared,"\n")
Table2[1,3] <- results$coefficients[2,4]
Table2[2,3] <- results$coefficients[3,4]
Table2[3,3] <- results$coefficients[4,4]

jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending] + Var2[attending] + Var3[attending] + Var4[attending])
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
cat("Multivariate regression on attendees using vars 1-4: Adjusted R2 = ",r.squared,"\n")
Table2[1,4] <- results$coefficients[2,4]
Table2[2,4] <- results$coefficients[3,4]
Table2[3,4] <- results$coefficients[4,4]
Table2[4,4] <- results$coefficients[5,4]
cat("Table 1 -- P-values for unfiltered data\n")
print(Table1)
cat("Table 2-- P-values for filtered data\n")
print(Table2)

#######################################################################
########### REPEAT MULTIVARIATE ANALYSES WITH SCALED VARIABLES ########
#######################################################################
library(QuantPsyc)
sTable1 <- matrix(NA,nrow=4,ncol=4)
sTable2 <- matrix(NA,nrow=4,ncol=4)
jm_regr <- lm(scale(Grad_School_Performance[random_attending]) ~ scale(Var1[random_attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable1[1,1] <- results$coefficients[2,1]

jm_regr <- lm(scale(Grad_School_Performance[random_attending]) ~ scale(Var1[random_attending]) + scale(Var2[random_attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable1[1,2] <- results$coefficients[2,1]
sTable1[2,2] <- results$coefficients[3,1]


jm_regr <- lm(scale(Grad_School_Performance[random_attending]) ~ scale(Var1[random_attending]) + scale(Var2[random_attending]) + scale(Var3[random_attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable1[1,3] <- results$coefficients[2,1]
sTable1[2,3] <- results$coefficients[3,1]
sTable1[3,3] <- results$coefficients[4,1]

jm_regr <- lm(scale(Grad_School_Performance[random_attending]) ~ scale(Var1[random_attending]) + scale(Var2[random_attending]) + scale(Var3[random_attending]) + scale(Var4[random_attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable1[1,4] <- results$coefficients[2,1]
sTable1[2,4] <- results$coefficients[3,1]
sTable1[3,4] <- results$coefficients[4,1]
sTable1[4,4] <- results$coefficients[5,1]

# Multivariate analyses
jm_regr <- lm(scale(Grad_School_Performance[attending]) ~ scale(Var1[attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable2[1,1] <- results$coefficients[2,1]

jm_regr <- lm(scale(Grad_School_Performance[attending]) ~ scale(Var1[attending]) + scale(Var2[attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable2[1,2] <- results$coefficients[2,1]
sTable2[2,2] <- results$coefficients[3,1]

jm_regr <- lm(scale(Grad_School_Performance[attending]) ~ scale(Var1[attending]) + scale(Var2[attending]) + scale(Var3[attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable2[1,3] <- results$coefficients[2,1]
sTable2[2,3] <- results$coefficients[3,1]
sTable2[3,3] <- results$coefficients[4,1]

jm_regr <- lm(scale(Grad_School_Performance[attending]) ~ scale(Var1[attending]) + scale(Var2[attending]) + scale(Var3[attending]) + scale(Var4[attending]))
results <- summary(jm_regr)
r.squared <- format(results$adj.r.squared,digits =2)
sTable2[1,4] <- results$coefficients[2,1]
sTable2[2,4] <- results$coefficients[3,1]
sTable2[3,4] <- results$coefficients[4,1]
sTable2[4,4] <- results$coefficients[5,1]
cat("Table 1 -- Standardized regression coefficients -- unfiltered data\n")
print(sTable1)
cat("Table 2 -- Standardized regression coefficients --filtered data\n")
print(sTable2)


if (show_gre_gpa_correlations) {
  # Plot GRE-GPA correlations
  par(mfrow=c(2,2))
  jm_regr <- lm(Var2[random_attending] ~ Var1[random_attending])
  results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
  if (pval < 1e-99) pval = 1e-99
  pval <- format(pval,digits = 3)
  r.squared <- format(results$adj.r.squared,digits =2)
  
  if (tiff_plot == FALSE) {
      plot(Var1[random_attending],Var2[random_attending],xlab="Admissions criteria 1", ylab = "Admissions Criteria 2", 
           main=paste("Correlations -- Random attendees\n[N = ",length(random_attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
  } else {
     plot(Var1[random_attending],Var2[random_attending],xlim=c(0,1),ylim=c(0,1.03), xlab="Admissions criteria 1",ylab = "Admissions Criteria 2",)
     legend(-0.08,1.09,paste("Regr Coeff = ",cc,", P = ",pval,sep=""),cex=0.8,text.col="red",bty="n")
     mtext("(a)",side=3,line=1.5,at=-0.2,cex=1.2)
  }
  abline(jm_regr,col="red")
 
  jm_regr <- lm(Var2[attending] ~ Var1[attending])
  results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
  if (pval < 1e-99) pval = 1e-99
  pval <- format(pval,digits = 3)
  r.squared <- format(results$adj.r.squared,digits =2)
  
  if (tiff_plot == FALSE) {
    plot(Var1[attending],Var2[attending],xlab="Admissions criteria 1", ylab = "Admissions Criteria 2", 
       main=paste("Correlations -- Attendees\n[N = ",length(attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
  } else {
      plot(Var1[attending],Var2[attending],xlim=c(0,1),ylim=c(0,1.03), xlab="Admissions criteria 1",ylab = "Admissions Criteria 2",)
      legend(-0.08,1.09,paste("Regr Coeff = ",cc,", P = ",pval,sep=""),cex=0.8,text.col="red",bty="n")
      mtext("(b)",side=3,line=1.5,at=-0.2,cex=1.2)
  } 
  abline(jm_regr,col="red")
  
  # Plot correlations between the GRE and research experience
  jm_regr <- lm(Var3[random_attending] ~ Var1[random_attending])
  results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
  if (pval < 1e-99) pval = 1e-99
  pval <- format(pval,digits = 3)
  r.squared <- format(results$adj.r.squared,digits =2)
  
  if (tiff_plot == FALSE) {
    plot(Var1[random_attending],Var3[random_attending],xlab="Admissions criteria 1", ylab = "Admissions Criteria 3", 
         main=paste("Correlations -- Random attendees\n[N = ",length(random_attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
  } else {
    plot(Var1[random_attending],Var3[random_attending],xlim=c(0,1),ylim=c(0,1.03), xlab="Admissions criteria 1",ylab = "Admissions Criteria 3",)
    legend(-0.08,1.09,paste("Regr Coeff = ",cc,", P = ",pval,sep=""),cex=0.8,text.col="red",bty="n")
    mtext("(c)",side=3,line=1.5,at=-0.2,cex=1.2)
  }
  abline(jm_regr,col="red")
  
  jm_regr <- lm(Var3[attending] ~ Var1[attending])
  results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
  if (pval < 1e-99) pval = 1e-99
  pval <- format(pval,digits = 3)
  r.squared <- format(results$adj.r.squared,digits =2)
  
  if (tiff_plot == FALSE) {
    plot(Var1[attending],Var3[attending],xlab="Admissions criteria 1", ylab = "Admissions Criteria 3", 
         main=paste("Correlations -- Attendees\n[N = ",length(attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
  } else {
    plot(Var1[attending],Var3[attending],xlim=c(0,1),ylim=c(0,1.03), xlab="Admissions criteria 1",ylab = "Admissions Criteria 3",)
    legend(-0.08,1.09,paste("Regr Coeff = ",cc,", P = ",pval,sep=""),cex=0.8,text.col="red",bty="n")
    mtext("(d)",side=3,line=1.5,at=-0.2,cex=1.2)
  } 
  abline(jm_regr,col="red")
}

cat("Average performance in graduate school : ", mean(Grad_School_Performance[attending]),"\n")
cat("Average GRE of applicants : ",mean(Var1),"\n")
cat("Average GRE of attendees  : ",mean(Var1[attending]),"\n")


#LowGRE <- which(rank(Admissions_Criteria)/length(Admissions_Criteria) > Admissions_Cutoff & 
#                               rank(Harvards_Admissions_Criteria)/length(Harvards_Admissions_Criteria) < Attends_Harvard_Instead & Var1 < 0.5) 
#HighGRE <- which(rank(Admissions_Criteria)/length(Admissions_Criteria) > Admissions_Cutoff & 
#                   rank(Harvards_Admissions_Criteria)/length(Harvards_Admissions_Criteria) < Attends_Harvard_Instead & Var1 >= 0.5) 
#MeanLowGRE <- mean(Var1[LowGRE])
#MeanHighGRE <- mean(Var1[HighGRE])
#HighLowResults[1] <- mean(Grad_School_Performance[LowGRE])
#HighLowResults[2] <- stdev(Grad_School_Performance[LowGRE])
#HighLowResults[3] <- mean(Grad_School_Performance[HighGRE])
#HighLowResults[4] <- stddev(Grad_School_Performance[HighGRE])
#random_attending <- sample(c(1:Sample_size),num_attending)



