set.seed(98195)
tiff_plot = TRUE
if (tiff_plot) {
  tiff(filename = "Figure1.tiff",
       width = 7, height = 9, units = "in", pointsize = 12, res=1000,
       compression = "lzw")
}

par(mfrow=c(3,2))
### Null Case -- No selection
Figure_Label = "(a)"
source("BaseParams.R")
Sample_size <- 700
Admissions_Cutoff <- 0.00 # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- 100.0  # Overall rank (using Harvard's criteria) above which students go elsewhere
source("PreVsPostPredictors2.R")

### Base simulation -- Committees evaluate applications perfectly
Figure_Label = "(b)"
source("BaseParams.R")
source("PreVsPostPredictors2.R")

### Committees over-estimate the importance of experience and LORs
Figure_Label = "(c)"
source("BaseParams.R")
w4 <- 2*w4  # Committee members overweigh the importance of LORs
h4 <- 2*h4  # Committee members overweigh the importance of LORs
source("PreVsPostPredictors2.R")

### Committees base it decisions on irrelvant factors
Figure_Label = "(d)"
source("BaseParams.R")
w6 <- 1   # Committee members influenced by an irrelvant factor 
h6 <- 1   # Committee members influenced by an irrelvant factor 
w7 <- 1   # Committee members influenced by an irrelvant factor 
h7 <- 1
source("PreVsPostPredictors2.R")

### Committees over-estimate the importance of GRE Scores
Figure_Label = "(e)"
source("BaseParams.R")
w1 <- 1.5*w1  # Committee members overweigh the importance of GRE scores
h1 <- 1.5*h1  # Committee members overweigh the importance of GRE scores
source("PreVsPostPredictors2.R")


### Committees overweighs GRE and uses irrelvant factors
Figure_Label = "(f)"
source("BaseParams.R")
w6 <- 1   # Committee members influenced by an irrelvant factor 
h6 <- 1   # Committee members influenced by an irrelvant factor 
w7 <- 1   # Committee members influenced by an irrelvant factor 
h7 <- 1
w1 <- 1.5*w1  # Committee members overweigh the importance of GRE scores
h1 <- 1.5*h1  # Committee members overweigh the importance of GRE scores
source("PreVsPostPredictors2.R")

special_case = 0
if (special_case == 1) {
 ### Special case where students are admitted without regard to GRE scores
 #   Chosen to match key features of a minority student program at Vanderbilt where
 #   GRE scores were recorded, but where GRE were not used to influence admissions.
 source("BaseParams.R")
 w1 <- 0   # Note that h1 is assumed to be its usual value
 Admissions_Cutoff <- 0.55
 Attends_Harvard_Instead <- 0.50 # Assume that the best of these minority students are in high demand elsewhere
 plot_num = 4
 source("PreVsPostPredictors2.R")
}

if (tiff_plot) dev.off()