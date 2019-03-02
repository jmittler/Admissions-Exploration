# Assume that success in graduate school is the the sum of up to eight variables
Sample_size <- 3200
Admissions_Cutoff <- 0.80 # Overall rank to get accepted into graduate program
Attends_Harvard_Instead <- 0.87  # Overall rank (using Harvard's criteria) above which students go elsewhere
show_gre_gpa_correlations <- FALSE
new_simulation_data <- 1
key_graphs_only <- TRUE

# Contributions (s1, s2,...) to success in graduate school / Weights (w1, w2, ...) used by admissions committee and Harvard's admissions committee
s1 <- 8.0; w1 <- 16.0; h1 <- 16.0; # GRE  
s2 <- 3.0; w2 <- 3.0; h2 <- 3.0; # GPA                      
s3 <- 3.0; w3 <- 3.0; h3 <- 3.0; # Ability to work in lab (as assessed by LORs)      
s4 <- 1.0; w4 <- 1.0; h4 <- 1.0; # Application statement
s5 <- 1.0; w5 <- 1.0; h5 <- 1.0; # Other (possibly unquantifable) factors identified by committee members  
s6 <- 0.0; w6 <- 0.5; h6 <- 0.0; # Irrelevant factors used by committee members
s7 <- 0.0; w7 <- 0.0; h7 <- 0.5; # Irrelevant factors used Harvard's admissions committee
s8 <- 8.0; w8 <- 0.0; h8 <- 0.0; # Chance effects

Corr12 <- 0.2 # Correlation between GRE and GPA
Corr34 <- 0.2 # Correlation between research experience and LORs
