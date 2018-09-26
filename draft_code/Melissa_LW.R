# the following is the Rcode from Melissa for generating LW relationships


# first, I add and clean up the data in same way I already have:

# first I would like to read in the entered length-weight data #######
lw <- read.csv("2016_lw.csv", header = TRUE)

# read species code table
species_codes <- read.csv("species_codes.csv", header = TRUE)

# load packages
library(tidyverse)

# clean up dfs
species_codes <- species_codes %>%
  select(-PACFIN_CODE, -NOTES) %>% #this removed the two columns PACFIN_CODE and NOTES
  rename(Species_No= SPECIES) # changed column name to Species_No- tomatch lw df 
as_tibble() # just cleans up the df formatting
# clean up lw df
lw <- lw %>%
  rename(Species_No= Species_Code) %>% # renamed column to match species_codes df
  as_tibble() # cleaned up formatting

# merge dfs
# add species information from species_codes df to lw df - note will merge with key "Species_No" 
# note i only want to add the species info TO the lw data so this is a "left join"- want to add species names to LW data (which currently only has sp #)

lw <- lw %>% 
  left_join(species_codes, by= "Species_No")
# add two columns for log(W) and log(L)
lw <- lw %>%
  mutate(logW= log(Weight), logL= log(Std_Length))





#install.packages("FSA")

library(FSA) # "simple fisheries stock assessment methods"  #######

#LW linear fit (transformed lw)
lmm = lm(logW~logL, data=lw) # fit a linear model to the natural log transformed length and weight data 
summary(lmm)
coef(lmm) # checked coeffient values 

##prediction dataframe
predictWL = data.frame(L = seq(5,141,1)) # not sure why I need a blank series= could sort by instead?
predictWL$PredW = exp(lmm$coefficients[1])*predictWL$L^lmm$coefficients[2]

lmm$coefficients[1] # same as coef(lmm)["(Intercept)"]
range(lw$Std_Length) # our range is 5.65- 140.29 so as an example, going to try lengths 1-- 141


plot(lw$Std_Length, lw$Weight, 
  xlab = "Length (mm)", ylab = "Weight (g)",
  col=gray(level=0, alpha=0.2),pch=19)
lines(predictWL$L, predictWL$PredW,type='l',col="blue",lwd=3, lty = 1)

dev.off()


