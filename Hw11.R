# Isaac Racine
# HW 11
# 31 Mar 2021
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

#don't split the data into treatments
unsplit_df <- z

#find values needed for gamma
unsplit_num <-(length(unsplit_df))
unsplit_mean <- mean(unsplit_df$Biomass)
unsplit_var <- var(unsplit_df$Biomass)


#-------------- Creating Simulated (Radnom) Data --------------
#Since the data is simulated all treatment groups will get the same parameters to see if by chance this could be the case
#for gamma distribution must determine shape and scale
unsplit_shpae <- (unsplit_mean^2) / unsplit_var
print(unsplit_shpae) #will be needed for later function

unsplit_scale <- unsplit_var/unsplit_mean
print(unsplit_scale) #will be needed for later function

#-------------- Make functions for simulating multiple dataframes ---------------

#use dim to find length of dataframe
#this will be used in the filebuilder function
#to determine how many rows hould be made
dim(z)

##################################################
# function: FileBuilder
# create a set of random files for regression
# input: fileN = number of files to create
#       : fileFolder = name of folder for random files
#       : fileSize = c(min,max) number of rows in file
#       : fileNA = number on average of NA values per column
# output: set of random files
#------------------------------------------------- 
FileBuilder <- function(fileN=100,
                        fileFolder="AbovegroundRandFile/",
                        fileSize=c(500,1334),
                        fileNA=3){
  for (i in seq_len(fileN)) {
    fileLength <- sample(fileSize[1]:fileSize[2],size=1) # get number of rows
    varX <- sample(c("Young", "Middle", "Old"), fileLength, replace = TRUE) # create random x
    varY <- rgamma(fileLength, shape = 0.5731653, scale = 246.8969) # create random Y 
    dF <- data.frame(varX,varY) # bind into a data frame
    # create label for file name with padded zeroes
    fileLabel <- paste(fileFolder,
                       "ranFile",
                       formatC(i,
                               width=3,
                               format="d",
                               flag="0"),
                       ".csv",sep="")
    
    # set up data file and incorporate time stamp and minimal metadata
    write.table(cat("# Simulated random data file for batch processing using aboveground biomasss data","\n",
                    "# timestamp: ",as.character(Sys.time()),"\n",
                    "# ISR","\n",
                    "# ------------------------", "\n",
                    "\n",
                    file=fileLabel,
                    row.names="",
                    col.names="",
                    sep=""))
    
    # now add the data frame
    write.table(x=dF,
                file=fileLabel,
                sep=",",
                row.names=FALSE,
                append=TRUE)
    
    
  }
}

#---------------------------------------
# FUNCTION regStats
# description: runs ANOVA
# inputs: 2-column data frame
# outputs: F-stat, p-value
########################################
regStats <- function(d = NULL) {
  
  #d$Biomass <-unlist(d$Biomass)
  
  . <- aov(d[,2] ~ d[,1], data = d)
  #. <- summary(.)
  statList <- list(p_value = summary(.)[[1]][1,5],
                   F_value = summary(.)[[1]][1,4])
# function body

return(statList)

} # end of regStats
#--------------------------------------


#-------------- Summary of all Samples ---------------


library(TeachingDemos)
char2seed("Freezing March")

#--------------------------------------------
# Global variables
fileFolder <- "AbovegroundRandFile/"
nFiles <- 100
fileOut <- "StatsSummary.csv"
#--------------------------------------------

# Create 100 random data sets
FileBuilder(fileN=nFiles)
fileNames <- list.files(path=fileFolder)


# Create data frame to hold file summary statistics
ID <- seq_along(fileNames)
fileName <- fileNames
p_val <- rep(NA,nFiles)
F_val <- rep(NA,nFiles)


statsOut <- data.frame(ID,fileName,p_val,F_val)
data <- read.table(file=paste(fileFolder,fileNames[1],sep=""),
                   sep=",",
                   header=TRUE)

# batch process by looping through individual files
for (i in seq_along(fileNames)) {
  data <- read.table(file=paste(fileFolder,fileNames[i],sep=""),
                     sep=",",
                     header=TRUE) # read in next data file

  dClean <- data[complete.cases(data),] # get clean cases
  
  . <- regStats(dClean) # pull regression stats from clean file
  statsOut[i,3:4] <- unlist(.) # unlist, copy into last 3 columns
  
}
# set up output file and incorporate time stamp and minimal metadata
write.table(cat("# Summary stats for ",
                "batch processing of regression models","\n",
                "# timestamp: ",as.character(Sys.time()),"\n",
                "# ISR","\n",
                "# ------------------------", "\n",
                "\n",
                file=fileOut,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=statsOut,
            file=fileOut,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)

print(statsOut)

#-------------- Make Bootstrap PLot ---------------
z_p_val <- 2.95e-8
# Know this from HW 8


p1 <- ggplot(data = statsOut, mapping = aes(x = statsOut$p_val))
p1 + geom_histogram(mapping = aes(fill = I("goldenrod"),
                                  color = I("black"))) +
  geom_vline(aes(xintercept = z_p_val, col = "blue"))

z_f_val <- 17.57

p2 <- ggplot(data = statsOut, mapping = aes(x =statsOut$F_val))
p2 + geom_histogram(mapping = aes(fill = I("goldenrod"),
                                  color = I("black"))) +
  geom_vline(aes(xintercept = z_f_val, col = "blue"))
