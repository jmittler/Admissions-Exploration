set.seed(102)

### Base simulation -- Committees evaluate applications perfectly
Figure_Label = ""
source("ParamsFig1.R")
source("PreVsPostPredictors2.R")

tiff_plot <- TRUE
if (tiff_plot == TRUE) {
  tiff(filename = "Figure2.tiff",
     width = 7, height = 7, units = "in", pointsize = 12, res=300,
     compression = "lzw")
}
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
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(a)",side=3,line=1.9,at=-0.2,cex=1.2)
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
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(b)",side=3,line=1.9,at=-0.2,cex=1.2)
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
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(c)",side=3,line=1.9,at=-0.2,cex=1.2)
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
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(d)",side=3,line=1.9,at=-0.2,cex=1.2)
} 
abline(jm_regr,col="red")

if (tiff_plot == TRUE) dev.off()
