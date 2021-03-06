# Isaac Racine
# Make functions for what was done in HW 8
# 17 Mar 2021
#------------------------------------------------



#---------------------------------------
# FUNCTION clean_data
# description: will clean the data passed
# inputs: messy data frame
# outputs: clean data frame
########################################
library(tidyverse)
clean_data <- function(file_name = NULL) {
  
  if(is.null(file_name)) {  
    df <- data.frame(ID = 101:110,
                   var_a = runif(10),
                   var_b = runif(10))
} else {
    df <- read.table(file = file_name,
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = FALSE)
    df <- data.frame(1:nrow(df),df$Age, df$AGB) 
    names(df) <- list("ID","Age","Biomass")
    
    df <- df %>%
      mutate(Age = str_replace(Age, "OG", "200"))
    
    df$Age <- as.numeric(as.character(df$Age))
}
    
return(df)

} # end of clean_data
#---------------------------------------
  
  

#---------------------------------------
# FUNCTION group_data
# description: will place data into groups
# inputs: dataframe to group
# outputs: groupped dataframe
########################################
library(tidyverse)
group_data <- function(dataframe = NULL) {
  
  df <- dataframe
  df <- split(df, cut(df$Age, c(0,20,199,200), include.lowest = TRUE))
    
# function body

return(df)

} # end of group_data
#---------------------------------------
  
  

#---------------------------------------
# FUNCTION find_param_vals
# description: finds the parameter values for each group
# inputs: dataframe that wants parameters to be found
# outputs: return vector of length, mean, var
########################################
library(tidyverse)
find_param_vals <- function(dataframe = NULL, group = NULL) {
  
  df <- dataframe 
  if (group == "young") {
    young_num <- length(df$`[0,20]`$Biomass)
    young_mean <- mean(df$`[0,20]`$Biomass)
    young_var <- var(df$`[0,20]`$Biomass)/20
    param_vec <- c(young_num, young_mean, young_var)
  } else if (group == "middle"){
    middle_num <- length(df$`(20,199]`$Biomass)
    middle_mean <- mean(df$`(20,199]`$Biomass)
    middle_var <- var(df$`(20,199]`$Biomass)/90
    param_vec <- c(middle_num, middle_mean, middle_var)
  } else if (group == "old"){
    old_num <- length(df$`(199,200]`$Biomass)
    old_mean <- mean(df$`(199,200]`$Biomass)
    old_var <- var(df$`(199,200]`$Biomass)/100
    param_vec <- c(old_num, old_mean, old_var)
  }
  
# function body

return(param_vec)

} # end of find_param_vals
#---------------------------------------
  
  
  
  
  
  
  
  


#---------------------------------------
# FUNCTION normal_simulate
# description: will simulate random data under a normal
#              model
# inputs: vector of all the groups parameters
# outputs: simulated df
########################################
normal_simulate <- function(param_vec = NULL) {
  young_data <- rnorm(param_vec[1], param_vec[2], param_vec[3])
  middle_data <- rnorm(param_vec[4], param_vec[5], param_vec[6])
  old_data <- rnorm(param_vec[7], param_vec[8], param_vec[9])

  #combine the treatment randomly generated biomass
  #into one vector
  vec_biomass <- c(young_data, middle_data, old_data)
  
  #make vector to hold the treatment name
  vec_treatment <- c(rep("Young", param_vec[1]), rep("Middle", param_vec[4]), rep("Old", param_vec[7]))
  
  #Create the data frame and give it names
  rand_data <- data.frame(1:(param_vec[1]+param_vec[4]+param_vec[7]),
                          vec_treatment, vec_biomass)
  names(rand_data) <- list("ID", "Treatment", "Biomass")

return(rand_data)

} # end of normal_simulate
#---------------------------------------






#---------------------------------------
# FUNCTION ANOVA_model
# description: will run an ANOVA test on passed data
# inputs: data frame to be analyzed and variables to put where
# outputs: ANOVA results
########################################
ANOVA_model <- function(df = NULL) {
  ANOmodel <- aov(Biomass~Treatment,data=df)
# function body

return(summary(ANOmodel))

} # end of ANOVA_model
#---------------------------------------



#---------------------------------------
# FUNCTION ANOVA_plot
# description: make a plot on the random data generated
# inputs: dataframe of random data and which variales to use
# outputs: ANOVA plot
########################################
library(ggplot2)
ANOVA_plot <- function(df = NULL) {
  ANOplot <- ggplot(data=df, aes(x=Treatment, y=Biomass,
                                        fill = Treatment)) +
    geom_boxplot()
# function body

return(ANOplot)

} # end of ANOVA_plot
#---------------------------------------

#---------------------------------------
# FUNCTION change_mean
# description: change the means so the results are still significant
# inputs: param vector containg the rnorm parameters
# outputs: newly changed parameter vector
########################################
change_mean <- function(param_vec = NULL) {
  param_vec[2] <- param_vec[2]-70
  param_vec[8] <- param_vec[8]+70
# function body

return(param_vec)

} # end of change_mean
#---------------------------------------


#---------------------------------------
# FUNCTION change_sample_size
# description: change sample size so still significant
# inputs: parameter vector
# outputs: new parameter vector
########################################
change_sample_size <- function(param_vec = NULL) {
  param_vec[1] <- ceiling(param_vec[1]/2.7)
  param_vec[4] <- ceiling(param_vec[4]/2.7)
  param_vec[7] <- ceiling(param_vec[7]/2.7)
# function body

return(param_vec)

} # end of change_sample_size
#---------------------------------------

#---------------------------------------
# FUNCTION find_gamma_params
# description: finds parameters for gamma distribution
# inputs: 
# outputs: output_description
########################################
find_gamma_params <- function(dataframe = NULL, group = NULL) {
  
  df <- dataframe 
  if (group == "young") {
    young_num <- length(df$`[0,20]`$Biomass)
    young_mean <- mean(df$`[0,20]`$Biomass)
    young_var <- var(df$`[0,20]`$Biomass)
    young_shape <- (young_mean^2)/young_var
    young_scale <- young_var/young_mean
    param_vec <- c(young_num, young_shape, young_scale)
  } else if (group == "middle"){
    middle_num <- length(df$`(20,199]`$Biomass)
    middle_mean <- mean(df$`(20,199]`$Biomass)
    middle_var <- var(df$`(20,199]`$Biomass)
    middle_shape <- (middle_mean^2)/middle_var
    middle_scale <- middle_var/middle_mean
    param_vec <- c(middle_num, middle_shape, middle_scale)
  } else if (group == "old"){
    old_num <- length(df$`(199,200]`$Biomass)
    old_mean <- mean(df$`(199,200]`$Biomass)
    old_var <- var(df$`(199,200]`$Biomass)
    old_shape <- (old_mean^2)/old_var
    old_scale <- old_var/old_mean
    param_vec <- c(old_num, old_shape, old_scale)
  }
  
  # function body
  
return(param_vec)
} # end of find_gamma_params
#---------------------------------------
  
  
#---------------------------------------
# FUNCTION gamma_simulate
# description: simulates data under gamma
# inputs: vector of parameters for all groups
# outputs: simulated data
########################################
gamma_simulate <- function(param_vec = NULL) {
  
  young_data <- rgamma(param_vec[1], shape =param_vec[2], scale = param_vec[3])
  middle_data <- rgamma(param_vec[4],shape = param_vec[5], scale = param_vec[6])
  old_data <- rgamma(param_vec[7], shape = param_vec[8], scale = param_vec[9])
  
  #combine the treatment randomly generated biomass
  #into one vector
  vec_biomass <- c(young_data, middle_data, old_data)
  
  #make vector to hold the treatment name
  vec_treatment <- c(rep("Young", param_vec[1]), rep("Middle", param_vec[4]), rep("Old", param_vec[7]))
  
  #Create the data frame and give it names
  rand_data <- data.frame(1:(param_vec[1]+param_vec[4]+param_vec[7]),
                          vec_treatment, vec_biomass)
  names(rand_data) <- list("ID", "Treatment", "Biomass")
  
return(rand_data)



} # end of gamma_simulate
#---------------------------------------

#---------------------------------------
# FUNCTION ANOVA_mean_plot
# description: description
# inputs: input_description
# outputs: output_description
########################################
library("gplots")
ANOVA_mean_plot <- function(df = NULL) {
  df <- df
  ANOplot <- plotmeans(Biomass ~ Treatment, data = df,
            xlab = "Treatment", ylab = "Biomass",
            main="Mean Plot with 95% CI")
  

return(ANOplot)

} # end of ANOVA_mean_plot
#---------------------------------------


#---------------------------------------
# FUNCTION change_gamma_sample
# description: description
# inputs: input_description
# outputs: output_description
########################################
change_gamma_sample <- function(param_vec = NULL) {
  param_vec[1] <- ceiling(param_vec[1]/19)
  param_vec[4] <- ceiling(param_vec[4]/19)
  param_vec[7] <- ceiling(param_vec[7]/19)
# function body

return(param_vec)

} # end of change_gamma_sample
#---------------------------------------
