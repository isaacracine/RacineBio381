# Isaac Racine
# HW 10
# 24 Mar 2021
#------------------------------------------------ 

#-------------- Question 1 ---------------

#---------------------------------------
# FUNCTION zero_loop_counter
# description: will count all of the zeros in a vector using a for loop
# inputs: a numeric vector or matrix
# outputs: the number of elements that are euqal to 0
########################################
zero_loop_counter <- function(x = NULL) {
  if(is.null(x)){
    x <- c(rep(0:9, 3))}
  
  counter <- 0
  
  for(i in x){
    if(i == 0){
      counter = counter + 1}
  }
  
  
# function body

return(counter)

} # end of zero_loop_counter
#---------------------------------------

print(zero_loop_counter())
zero_loop_counter(c(0,0,1,1,2,0,0,0,4,5))
m <- matrix(rep(0:2, 4), nrow = 4)
zero_loop_counter(m)
                  

#-------------- Question 2 ---------------
#Use this format: length(vec[vec == 0])
z <- c(0:3, 3:0)
counter <- length(z[z==0])
print(counter)


#-------------- Question 3 ---------------

#---------------------------------------
# FUNCTION matrix_maker
# description: will make a matrix of passed dimensions, and each element of the matrix will be the product of the col and row num
# inputs: two integers representing the col and row number
# outputs: matrix with elements as the product of the row and col
########################################
matrix_maker <- function(row = NULL, col = NULL) {
  if(is.null(row) && is.null(col)){
    row = 2
    col = 2
    m <- matrix(nrow = 2, ncol = 2)
  } else {
    m <- matrix(nrow = row, ncol = col)}
  
  for (i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      m[i,j] <- i * j}}

return(m)

} # end of matrix_maker
#---------------------------------------
matrix_maker()
matrix_maker(6,4)
matrix_maker(4,8)

#-------------- Question 4 ---------------

#---------------------------------------
# FUNCTION read_data
# description: read in (or generate) data set for analysis
# inputs: file anem (or nothing as in this demo)
# outputs: 3 col data frame of obs data (ID, x, y)
########################################
read_data <- function(z = NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10 * rnorm(20)
    df <- data.frame(ID = seq_along(x_obs),
                     x_obs,
                     y_obs)
  } else {
    df <- read.table(file = z,
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE)}
  
  return(df)
  
} # end of read_data
#---------------------------------------


#---------------------------------------
# FUNCTION get_metric
# description: calculate metric for randomization test
# inputs: 2-col data frame for regression
# outputs: regression slope
########################################
get_metric <- function(z = NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10 * rnorm(20)
    z <- data.frame(ID = seq_along(x_obs), 
                    x_obs,
                    y_obs)}
  . <- lm(z[,3]~z[,2])  #3 col is y var, 2 is xvar
  . <- summary(.)
  
  . <- .$coefficients[2,1] # grabbing matrix and getting slope
  slope <- .
  
  return(slope)
  
} # end of get_metric
#---------------------------------------

#---------------------------------------
# FUNCTION shuffle_data
# description: randomize data for a regression analysis
# inputs: 3 col data frame (ID, xvar, yvar)
# outputs: 3 col data frame (ID, xvar, yvar)
########################################
shuffle_data <- function(z = NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10 * rnorm(20)
    z <- data.frame(ID = seq_along(x_obs),
                    x_obs,
                    y_obs)}
  z[,3] <- sample(z[,3])
  
  return(z)
  
} # end of shuffle_data
#---------------------------------------

#---------------------------------------
# FUNCTION get_pval
# description: calculate p-val from simulation
# inputs: list of observed metric and vector of simulated metrics
# outputs: lower and upper tail probability
########################################
get_pval <- function(z = NULL) {
  if(is.null(z)){
    z <- list(rnorm(1), rnorm(1000))}
  p_lower <- mean(z[[2]] <= z [[1]])
  #what is the proportion of the simulated values less than the obs values
  
  p_upper <- mean(z[[2]] >= z[[1]])
  
  return(c(pL = p_lower,pU = p_upper))
  
} # end of get_pval
#---------------------------------------


#---------------------------------------
# FUNCTION plot_ran_test
# description: create a ggplot of histogram of simulated values
# inputs: list of obs metric and vector simulated metrics
# outputs: saved ggplot graph
########################################
plot_ran_test <- function(z = NULL) {
  if(is.null(z)){
    z <- list(rnorm(1), rnorm(1000))}
  df <- data.frame(ID = seq_along(z[[2]]), sim_x = z [[2]])
  p1 <- ggplot(data = df, mapping = aes(x = sim_x))
  p1 + geom_histogram(mapping = aes(fill = I("goldenrod"),
                                    color = I("black"))) +
    geom_vline(aes(xintercept = z [[1]], col = "blue"))
  
  
  
} # end of plot_ran_test
#---------------------------------------

#set seed
set.seed(81)

#read data in
data <- read_data("CleanedAbovegroundData.csv")

# Remove one of the ID columns because now there are two
data <- data[-c(1)] 

# get the slope for regression
x_obs <- get_metric(data)

# declare number of samples
n_samples <- 1000

# set up empty vector for simulated slopes
x_sim <- rep(NA, n_samples)

#loop through to calculate several simulated means
for(i in seq_len(n_samples)){
  x_sim[i] <- get_metric(shuffle_data(data))}  

#make list of slopes
slopes <- list(x_obs, x_sim)
slopes
#get upper and lower tails
get_pval(slopes)

#plot the simulated slopes
plot_ran_test(slopes)

#-------------- Question 5 ---------------

reg <- lm(data$Biomass~data$Age)

# here is the p-value
summary(reg)$coefficients[2,4]


