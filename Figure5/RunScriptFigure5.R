set.seed(98195)

tiff_plot <- TRUE
Figure_Label = ""
source("ParamsFig5.R")
source("PreVsPostPredictors2.R")


if (tiff_plot == TRUE) {
  tiff(filename = "Figure5v9.tiff",
     width = 3.5, height = 7, units = "in", pointsize = 12, res=1000,
     compression = "lzw")
}
# Plot GRE-GPA correlations
par(mfrow=c(2,1))
jm_regr <- lm(Grad_School_Performance[random_attending] ~ Var1[random_attending])
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (tiff_plot == FALSE) {
  plot(Var1[random_attending],Grad_School_Performance[random_attending],xlab="Admissions criteria 1", ylab = "Performance", 
       main=paste("Correlations -- Random attendees\n[N = ",length(random_attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
} else {
  plot(Var1[random_attending],Grad_School_Performance[random_attending],xlim=c(0,1),ylim=c(0,15.3), 
        xlab="Admissions criteria 1",ylab = "Performance",)
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(a)",side=3,line=1.9,at=-0.3,cex=1.2)
}
abline(jm_regr,col="red")

jm_regr <- lm(Grad_School_Performance[attending] ~ Var1[attending])
results <- summary(jm_regr); cc <- round(results$coefficients[2,1],2); pval <- results$coefficients[2,4]
if (pval < 1e-99) pval = 1e-99
pval <- format(pval,digits = 3)
r.squared <- format(results$adj.r.squared,digits =2)

if (tiff_plot == FALSE) {
  plot(Var1[attending],Grad_School_Performance[attending],xlab="Admissions criteria 1", ylab = "Admissions Criteria 2", 
       main=paste("Correlations -- Attendees\n[N = ",length(attending),", Adj R2 = ",r.squared,", Regr Coeff =",cc,", P = ",pval,"]"))
} else {
  plot(Var1[attending],Grad_School_Performance[attending],xlim=c(0,1),ylim=c(0,15.3), xlab="Admissions criteria 1",ylab = "Performance",)
  mtext(paste("Regr Coeff = ",cc,", P = ",pval,sep=""),side=3,line=0.7,at=0.50,cex=0.6,col="red",bty="n")
  mtext("(b)",side=3,line=1.9,at=-0.3,cex=1.2)
} 
abline(jm_regr,col="red")

if (tiff_plot == TRUE) dev.off()
