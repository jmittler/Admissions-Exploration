set.seed(1919)
tiff_plot = TRUE
if (tiff_plot) {
  tiff(filename = "Figure1_highN_variant.tiff",
       width = 7, height = 9, units = "in", pointsize = 12, res=300,
       compression = "lzw")
}

par(mfrow=c(3,2))
### Null Case -- No selection
Figure_Label = "(a)"
source("ParamsFig1_highN_variant.R")
Sample_size <- 0.07*Sample_size
Admissions_Cutoff <- 0.00 # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- 100.0  # Overall rank (using Harvard's criteria) above which students go elsewhere
source("PreVsPostPredictors.R")

### Base simulation -- Committees evaluate applications perfectly
cat("Case (b) -- near perfect evaluation of admissions criteria...\n")
Figure_Label = "(b)"
source("ParamsFig1_highN_variant.R")
source("PreVsPostPredictors.R")

### Committees over-estimate the importance of LORs
Figure_Label = "(c)"
source("ParamsFig1_highN_variant.R")
w3 <- 3*w3  # Committee members overweigh the importance of LORs
h3 <- 3*h3  # Committee members overweigh the importance of LORs
source("PreVsPostPredictors.R")

### Committees base their decisions on irrelvant factors
Figure_Label = "(d)"
source("ParamsFig1_highN_variant.R")
w6 <- 6*w6   # Committee members influenced by an irrelvant factor 
h7 <- 6*h7  # Committee members influenced by an irrelvant factor 
source("PreVsPostPredictors.R")

### Committees over-estimate the importance of GREs 
Figure_Label = "(e)"
source("ParamsFig1_highN_variant.R")
w1 <- 3*w1  # Committee members overweigh the importance of GRE scores
h1 <- 3*h1  # Committee members overweigh the importance of GRE scores
source("PreVsPostPredictors.R")


### Committees over-estimate the importance of GREs and irrelevant factors
Figure_Label = "(f)"
source("ParamsFig1_highN_variant.R")
w1 <- 2*w1  # Committee members overweigh the importance of GRE scores
h1 <- 2*h1  # Committee members overweigh the importance of GRE scores
w3 <- 2*w3  # Committee members overweigh the importance of LORs
h3 <- 2*h3  # Committee members overweigh the importance of LORs
source("PreVsPostPredictors.R")


if (tiff_plot) dev.off()