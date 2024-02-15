###############################################################################
### title: "Basic Data Import and Manipulation and Calculating Diversity"   ###
### author: Loralee Larios                                                  ###
### date created: Aug 10, 2021                                              ###    
### date last modified: Aug 16, 2021                                        ###
### output: NA                                                               ###
###############################################################################

## OBJECTIVES:  
## To learn to work with multiple data sets in R
## To learn how to calculate diversity metrics


## What are the three common types of ecological data? 
## Species, Environmental (Spatial/Temporal) , Treatment 
## The goal is often to see how species data vary in relation to the other data
## We often have different species response variables to capture changes
## The key to calculating different response variables is to understand how to work among
## multiple data sets

## The key to doing this in R is having "tidy data"
## What are tidy data?:
## Tidy data are data that are easy to transform and visualize. 
## The key idea is that variables are stored in a consistent way.
## Each variable forms a column
## Each observation forms a row
## Each type of observational unit forms a table

## For more information check out R for Data Science: https://r4ds.had.co.nz/tidy-data.html


## We will be using the tidyverse package for data manipulation and summarization
## it is a group of several packages including 'dplyr' ' tidyr' and 'ggplot2'


## some common functions that we will be using when manipulating data
## Functions from dplyr:
# filter: keep rows that match a criteria
# select: pick columns by name
# arrange: reorder rows
# mutate: add new variables
# summarize: reduce variables to values
# + group_by

## Functions from tidyr: 
# pivot_longer: convert many columns into variable/value pairs; akin to melt in reshape (previous gather in tidyr)
# pivot_wider: convert variable/value pairs into columns; akin to cast in reshape (previously spread in tidyr)
# separate: break one column into two (similar to strsplit)

# Load the tidyverse
library(tidyverse)


## Read in the raw data
## Also have a look at the intial data file
sp <- read_csv("BeeFlat_SpeciesInfo.csv")

## Read in Plot metadata
plot <- read_csv("BeeFlat_plotmetadata.csv")

## Read in species cover data file
d <- read_csv("CleanBeeFlat08062021_wide.csv")

## this asks R to print out the structure of the transect data frame
## it's the information that is loaded in the environment window in R studio
## this is helpful to check to make sure everything loaded correctly 
## (ie. numbers are numbers and not factors)
glimpse(sp)
glimpse(plot)
glimpse(d)

## For calculating diversity indices we will be using the 'vegan' package
##'vegan' has many functions that are relevant to community composition questions
library(vegan)
## each R package has vignettes that provide an overview of the package and 
## you can view the one for vegan
browseVignettes("vegan")

#################################################################### 
## Part 1: Calculating diversity & working with pipelines          #
####################################################################

## some common functions that we will be using when calculating diversity metrics
## Functions from vegan:
# specnumber: species richness as the number of unique species present in sampling unit
# diversity: the default diversity index is Shannon's diversity
# diversity(data, "inv") ## Simpson diversity

## some other calculations will be helpful when estimating other diversity indices
## Let's assume: 
# SR = species richness
# H.div = Shannon's diversity
## Then
# Eve <- H.div/log(SR) ## Pielou evenness


## let's look at our column information for the species data in a different way
names(d)
## 'NoNatVeg' is a column that covers bare ground 
## so it's not a column to include in species richness, we'll drop it below

## one of the objectives of working with 'tidy' script is that minimizes the number of objects
## and you are able to execute multiple commands in a single workflow

## let's try this for calculating diversity
## let's work through this sequentially so we can see how this works
## first we need to tell R what data columns we want to work with
sp.div<-  d %>%  #this line tells R to start with data frame d
  select(-NoNatVeg)%>% #drop column 'NoNatVeg'
  select(AVESPP:LUPALB) #now just select the species data columns

## NOTE: This is the start of a workflow! The pipeline function (%>%) says
## Take this dataframe and keep working on it! 
## You can keep chaining on as long as you like
## This avoids creating say a dat1 dataframe, and a dat2, and dat3, and dat4, 
##as you continue to manipulate the data

## let's add to that initial work flow to specify the diversity index calculations
## Note the '.' is telling R to use all the columns in the working data frame for the calculation
sp.div<-  d %>%  #this line tells R to start with data frame d
  select(-NoNatVeg)%>% #drop column 'NoNatVeg'
  select(AVESPP:LUPALB) %>% #now just select the species data columns
  mutate(H.div=diversity(.))%>% #create a new data column for the calculation of Shannon's Diversity
  mutate(spec.rich=specnumber(.)) %>% #create a new data column for species richness
  mutate(Even=H.div/log(spec.rich)) %>% #create a new data column for species evenness
  mutate(S.div=diversity(., "inv")) %>% #create a new data column for Simpson's diversity 
  bind_cols(.,select(d,Plot.key)) #This brings back in the plot.key column that was dropped earlier


## for community data we often want a site by species matrix, 
## to calculate diversity indices and conduct multivariate analysis
## but to summarize and for graphing sometimes we need a 'long' format
## and we need the grouping data information

######################################################################### 
## Part 2: Transposing data and calculating other response variables    #
#########################################################################