# Isaac Racine
# Test the functions made in HW9
# 17 Mar 2021
#------------------------------------------------
library(tidyverse)
library(ggplot2)
library(gplots)

#-------------- Source files ---------------
source("HW9_functions.R")

#-------------- global variables ---------------
tree_data <- "AbovegroundData.csv"

#-------------- RUnning Functions ---------------
#read in and clean up data
#extracting only the columns we want
temp1 <- clean_data(tree_data)
#group the data
temp2 <- group_data(temp1)
#find parameters for each group and make into a vector
param_vector <- c(find_param_vals(temp2, "young"),
                  find_param_vals(temp2, "middle"),
                  find_param_vals(temp2, "old"))

multiple_sims <- data.frame()

#Question 3
for (x in 1:10) {
  

#simulate data with vector
  df <- normal_simulate(param_vector)
  #now run anova analysis and plot
  ANOVA_model(df)
  p_val <- ANOVA_model(df)[[1]][[5]][1]
  mean_sq <- ANOVA_model(df)[[1]][[3]][1]
  multiple_sims <- rbind(multiple_sims, list(x, mean_sq, p_val))
}
names(multiple_sims) <- list("ID", "Mean Square", "p-value")
print(multiple_sims)

#plot for last sample in dataframe
ANOVA_plot(df)

#-------------- Changing Mean ---------------
vec_change_mean <- change_mean(param_vector)
df_change_mean <- normal_simulate(vec_change_mean)
ANOVA_model(df_change_mean)
ANOVA_plot(df_change_mean)

#-------------- Changing Sample Size ---------------
vec_change_sample <- change_sample_size(param_vector)
df_change_sample <- normal_simulate(vec_change_sample)
ANOVA_model(df_change_sample)
ANOVA_plot(df_change_sample)


#-------------- Question 2 ---------------
#-------------- Doing it all again with Gamma ---------------
gamma_param <- c(find_gamma_params(temp2, "young"),
                 find_gamma_params(temp2, "middle"),
                 find_gamma_params(temp2, "old"))
df_gamma <- gamma_simulate(gamma_param)
ANOVA_model(df_gamma)
ANOVA_plot(df_gamma)

#-------------- Chaning Gamma Sample Size ---------------
vec_gamma_sample <- change_gamma_sample(gamma_param)
df_gamma_sample <- gamma_simulate(vec_gamma_sample)
ANOVA_model(df_gamma_sample)  
ANOVA_plot(df_gamma_sample)  

#PLot the means with 95% confidence intervals
ANOVA_mean_plot(df)
ANOVA_mean_plot(df_gamma)
ANOVA_mean_plot(df_gamma_sample)





