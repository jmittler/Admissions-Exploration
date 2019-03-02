# Assume that success in graduate school is the the sum of up to eight variables
Sample_size <- 3200
Admissions_Cutoff <- 0.80 # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- 0.87  # Overall rank at competing schoolsabove which students go elsewhere
show_gre_gpa_correlations <- FALSE
new_simulation_data <- 1
key_graphs_only <- TRUE

# Contributions (s1, s2,...) to success in graduate school / Weights (w1, w2, ...) used by admissions committee and Harvard's admissions committee
s1 <-  0.5;          w1 <- 1.5;          h1 <- 1.5;          # GREs  
s2 <-  runif(1,0,1); w2 <- runif(1,0,1); h2 <- runif(1,0,1); # GPA                      
s3 <-  runif(1,0,1); w3 <- runif(1,0,1); h3 <- runif(1,0,1); # LORs
s4 <-  runif(1,0,1); w4 <- runif(1,0,1); h4 <- runif(1,0,1); # Application statement
s5 <-  runif(1,0,1); w5 <- runif(1,0,1); h5 <- runif(1,0,1); # Subtle / unquantifable factors correctly intuited by committee members  
s6 <-  0.0;          w6 <- runif(1,0,1); h6 <- runif(1,0,1); # Irrelevant factors used by committee members
s7 <-  0.0;          w7 <- runif(1,0,1); h7 <- runif(1,0,1); # Irrelevant factors used competing admissions committee
s8 <-  runif(1,2.5,10); w8 <- 0.0;          h8 <- 0.0; # Chance effects

Admissions_Cutoff <- runif(1,0.50,0.80) # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- runif(1,0.8,0.95)  # Overall rank  at competing schools above which students go elsewhere


Corr12 <- runif(1, 0.0, 0.5) # Correlation between GRE and GPA
Corr34 <- runif(1, 0.0, 0.5) # Correlation between research experience and LORs
