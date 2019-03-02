set.seed(19195)
tiff_plot = TRUE
if (tiff_plot) {
  tiff(filename = "Figure4.tiff",
       width = 7, height = 9, units = "in", pointsize = 12, res=300,
       compression = "lzw")
}

par(mfrow=c(2,1))

### Null Case -- No selection
Figure_Label = "       (a)"
source("ParamsFig4.R")
Sample_size <- 0.07*Sample_size
Admissions_Cutoff <- 0.00 # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- 100.0  # Overall rank (using Harvard's criteria) above which students go elsewhere
source("PreVsPostPredictors.R")

### Committees over-estimate the importance of GREs 
Figure_Label = "       (b)"
source("ParamsFig4.R")
w1 <- 3*w1  # Committee members overweigh the importance of GRE scores
h1 <- 3*h1  # Committee members overweigh the importance of GRE scores
source("PreVsPostPredictors.R")


if (tiff_plot) dev.off()