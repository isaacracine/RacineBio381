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
young_var <- var(z$`[0,20]`$Biomass)

#Group Middle
middle_num <- length(z$`(20,199]`$Biomass)
middle_mean <- mean(z$`(20,199]`$Biomass)
middle_var <- var(z$`(20,199]`$Biomass)

#Group Old
old_num <- length(z$`(199,200]`$Biomass)
old_mean <- mean(z$`(199,200]`$Biomass)
old_var <- var(z$`(199,200]`$Biomass)

#The variation of the groups were very large. To make the data actually significantly different I had to lessen the variation.
#-------------- Creating Simulated (Radnom) Data --------------
#First run the number of simulated data points needed 
#for each treatment group
#for gamma distribution must determine each groups shape and scale
young_shape <- (young_mean^2)/young_var
young_scale <- young_var/young_mean
young_data <- rgamma(young_num, shape = young_shape,
                     scale = young_scale)

middle_shape <- (middle_mean^2)/middle_var
middle_scale <- middle_var/middle_mean
middle_data <- rgamma(middle_num, shape = middle_shape,
                     scale = middle_scale)


old_shape <- (old_mean^2)/old_var
old_scale <- old_var/old_mean
old_data <- rgamma(old_num, shape = old_shape,
                     scale = old_scale)

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
young_data1 <- rgamma(young_num, middle_shape, young_scale)
middle_data1 <- rgamma(middle_num, middle_shape, middle_scale)
old_data1 <- rgamma(old_num, middle_shape, old_scale)

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

#The effect size can be 0 and still produce a result with a significant pattern. This must be due to the large sample sizes of each treatment, and due to their large differences in variation.

#-------------- Testing Sample Size ---------------
#try to simulate data with smaller to see the minimum sample size needed to still have significant results
young_data2 <- rgamma(ceiling(young_num/19), young_shape, young_scale)
middle_data2 <- rgamma(ceiling(middle_num/19), middle_shape, middle_scale)
old_data2 <- rgamma(ceiling(old_num/19), old_shape, old_scale)

#combine the treatment randomly generated biomass
#into one vector
vec_biomass2 <- c(young_data2, middle_data2, old_data2)
vec_treatment2 <- c(rep("Young", ceiling(young_num/19)), rep("Middle", ceiling(middle_num/19)), rep("Old", ceiling(old_num/19)))

#Create the data frame and give it names
rand_data2 <- data.frame(1:length(vec_treatment2),
                         vec_treatment2, vec_biomass2)
names(rand_data2) <- list("ID", "Treatment", "Biomass")

#Here we will make an ANOVA test and plot
ANOmodel <- aov(Biomass~Treatment,data=rand_data2)
print(summary(ANOmodel))

ANOplot <- ggplot(data=rand_data2, aes(x=Treatment, y=Biomass,
                                       fill = Treatment)) +
  geom_boxplot()
print(ANOplot)
