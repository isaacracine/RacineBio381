# Isaac Racine
# HW 8
# 10 Mar 2021
#------------------------------------------------

library(ggplot2)
library(tidyverse)

#https://datadryad.org/stash/dataset/doi:10.5061/dryad.82vr4
#where I got my data

z <- read.table("AbovegroundData.csv",header=TRUE,sep=",", stringsAsFactors=FALSE)

#add ID column, index age and biomass to new dataframe
z <- data.frame(1:nrow(z),z$Age, z$AGB) 
names(z) <- list("ID","Age","Biomass")


#change Age's with OG to 200
#OG meant they were too old to detemine an age
#we will put in age of 200
z <- z %>%
  mutate(Age = str_replace(Age, "OG", "200"))



#change age from char to int
z$Age <- as.numeric(as.character(z$Age))

#see summary
summary(z)

#Can group ages into treatments
#0-20 is young
#21-199 is middle
#200+ is old
z <- split(z, cut(z$Age, c(0,20,199,200), include.lowest = TRUE))

print(z)

#Now that they're split here are their summary stats
#Group Young
print(z$`[0,20]`)
young_num <- length(z$`[0,20]`$Biomass)
young_mean <- mean(z$`[0,20]`$Biomass)
young_var <- var(z$`[0,20]`$Biomass)/20

#Group Middle
middle_num <- length(z$`(20,199]`$Biomass)
middle_mean <- mean(z$`(20,199]`$Biomass)
middle_var <- var(z$`(20,199]`$Biomass)/90

#Group Old
old_num <- length(z$`(199,200]`$Biomass)
old_mean <- mean(z$`(199,200]`$Biomass)
old_var <- var(z$`(199,200]`$Biomass)/100

#The variation of the groups were very large. To make the data actually significantly different I had to lessen the variation.
#-------------- Creating Simulated (Radnom) Data --------------
#First run the number of simulated data points needed 
#for each treatment group
young_data <- rnorm(young_num, young_mean, young_var)

middle_data <- rnorm(middle_num, middle_mean, middle_var)

old_data <- rnorm(old_num, old_mean, old_var)

#combine the treatment randomly generated biomass
#into one vector
vec_biomass <- c(young_data, middle_data, old_data)

#make vector to hold the treatment name
vec_treatment <- c(rep("Young", young_num), rep("Middle", middle_num), rep("Old", old_num))

#Create the data frame and give it names
rand_data <- data.frame(1:(young_num+middle_num+old_num),
                        vec_treatment, vec_biomass)
names(rand_data) <- list("ID", "Treatment", "Biomass")
print(rand_data)

#-------------- Make Model and PLot ---------------
#Here we will make an ANOVA test and plot
ANOmodel <- aov(Biomass~Treatment,data=rand_data)
print(summary(ANOmodel))

ANOplot <- ggplot(data=rand_data, aes(x=Treatment, y=Biomass,
                                      fill = Treatment)) +
  geom_boxplot()
print(ANOplot)

#-------------- Adjust Means ---------------
#Try to make means as close as together, 
#making effective size smaller
print(young_mean)
print(middle_mean)
print(old_mean)

#try to simulate data with same mean
young_data1 <- rnorm(young_num, middle_mean-70, young_var)
middle_data1 <- rnorm(middle_num, middle_mean, middle_var)
old_data1 <- rnorm(old_num, middle_mean+70, old_var)

#combine the treatment randomly generated biomass
#into one vector
vec_biomass1 <- c(young_data1, middle_data1, old_data1)

#Create the data frame and give it names
rand_data1 <- data.frame(1:(young_num+middle_num+old_num),
                        vec_treatment, vec_biomass1)
names(rand_data1) <- list("ID", "Treatment", "Biomass")

#Here we will make an ANOVA test and plot
ANOmodel <- aov(Biomass~Treatment,data=rand_data1)
print(summary(ANOmodel))

ANOplot <- ggplot(data=rand_data1, aes(x=Treatment, y=Biomass,
                                      fill = Treatment)) +
  geom_boxplot()
print(ANOplot)


#-------------- Testing Sample Size ---------------
#try to simulate data with smaller to see the minimum sample size needed to still have significant results
young_data2 <- rnorm(ceiling(young_num/2.7), young_mean, young_var)
middle_data2 <- rnorm(ceiling(middle_num/2.7), middle_mean, middle_var)
old_data2 <- rnorm(ceiling(old_num/2.7), old_mean, old_var)

#combine the treatment randomly generated biomass
#into one vector
vec_biomass2 <- c(young_data2, middle_data2, old_data2)
vec_treatment2 <- c(rep("Young", ceiling(young_num/2.7)), rep("Middle", ceiling(middle_num/2.7)), rep("Old", ceiling(old_num/2.7)))

#Create the data frame and give it names
rand_data2 <- data.frame(1:(ceiling(young_num/2.7)+ceiling(middle_num/2.7)+ceiling(old_num/2.7)),
                         vec_treatment2, vec_biomass2)
names(rand_data2) <- list("ID", "Treatment", "Biomass")

#Here we will make an ANOVA test and plot
ANOmodel <- aov(Biomass~Treatment,data=rand_data2)
print(summary(ANOmodel))

ANOplot <- ggplot(data=rand_data2, aes(x=Treatment, y=Biomass,
                                       fill = Treatment)) +
  geom_boxplot()
print(ANOplot)
