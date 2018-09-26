###################### Length-Weight Data from 2016 Cruise ########################################################################
# Project by: Ily Iglesias
# Date started: 2/26/18: 
# Project description: Length-weight data relationships. For this project, I would like to plot the length-weight relationships of organisms
# collected on the 2016 rockfish recruitment and ecosystem assessment cruise.This relationship is best represented by W= aL^b (where W=weight, L=length, a= scaling coefficient, b=shape parameter for the body form of fishes)
# note: this relationship was established by Keys (1928)
# Data: I have two input data sets;
# species_codes: this is the page from the Access cruise dataset, the table "species_codes" and contains spp name information
# 2016_lw: this is the inputted length weight data
# Location: stored in my ilysa.iglesias drive under ROCKFISH//Projects/JRREACruise/LWData
####################################################################################################################################


# data import and cleaning   #######################################################################################################

# read in length weight data
lw <- read.csv("2016_lw.csv", header = TRUE)
# read species code table
species_codes <- read.csv("species_codes.csv", header = TRUE)

# load packages
library(tidyverse)

# clean up species_codes df
species_codes <- species_codes %>% # reference df
  select(-PACFIN_CODE, -NOTES) %>% #this removes the two columns PACFIN_CODE and NOTES
  rename(Species_No= SPECIES) # changed column name to Species_No- tomatch lw df 
  as_tibble() #  cleans up  df formatting

# clean up lw df
lw <- lw %>%
  rename(Species_No= Species_Code) %>% # renamed column to match species_codes df
  as_tibble() # cleaned up formatting

# merge dfs
# I want to add species information from species_codes df to lw df - note will merge with key "Species_No" 
# note i only want to add the species info TO the lw data so this is a "left join"- want to add species names to LW data (which currently only has sp #)

lw <- lw %>% 
  left_join(species_codes, by= "Species_No") 

lw$Species_No <- as.factor(lw$Species_No) # convert the species numbers into a factor- which they are


# Length weight equation ############################################################################################################
# W= a L ^b where W= weight, a= scaling coefficient ~condition, L= Length and b= shape parameter for the body form of a fish species

# transform allometric growh equation by taking the natural log of both sides
#  log (W) = log (a) + b* log (L)+E (where log(a)= intercept and b= slope and E=error)

# add two columns for log(W) and log(L)
lw <- lw %>%
  mutate(logW= log(Weight), logL= log(Std_Length))

# plot linear relationships between log transformed L and log transformed W data- for each species
# all species at once- using facets: 
#pdf("logwlogl_allspp.pdf")
ggplot(data= lw, mapping= aes(y= logW, x= logL)) + # defines our x and y values as well as the df
  geom_point(mapping = aes(color=SCI_NAME), show.legend = FALSE) + # colors points by species scientific name
  facet_wrap(~ SCI_NAME, nrow = 4) + # creates an individual plot for each species and since we have 24 spp, 4 rows of plots makes sense
  geom_smooth(method="lm", formula= y~ x, color= "black") + # plots linear models 
  xlab("Log Length mm") + ylab("Log Weight g") + ggtitle("Length-Weight") # add labels
#dev.off()



# Plotting LW data and predicted LW eq values for INDIVIDUAL spp. 

spp <- unique(lw$COMMON_NAME) # vector of indidivual species (common name) from our lw df
for (i in 1:length(spp)) {
  nest.i<- spp[i] # for species i within our vector of species "spp"
  species.i<- lw %>%
    filter(COMMON_NAME==nest.i) # filter out data for a specific species
    fit <- lm(formula= logW~ logL, data = species.i)  # fit log transformed w l data into linear model
    summary(fit) # this displays our intercept, slope values and R^2
  
  r<- range(species.i$Std_Length) #this defines the smallest and largest individual measured lengths per species
  predictWL = data.frame(L = seq(r[1],r[2],1)) # this sets the range specifically for the values we have measurements for- change these values to change the range of estimates
  predictWL$PredW = exp(fit$coefficients[1])*predictWL$L^fit$coefficients[2] # predicted dataframe: for defined L values, what is our predicted W based on our equation and est parameters
  
  # plot of W L data and our allometric growth equation with est parameters (a and b)
  file_name<- paste(nest.i, ".pdf", sep = "") # iterative file name definition
  pdf(paste0("/Volumes/ilysa.iglesias/ROCKFISH/Projects/JRREACruise/LWData/LW/spp_output/", file_name)) # this defines the location and name of output file
  plot(x=species.i$Std_Length, y=species.i$Weight, xlab = "Length (mm)", ylab = "Weight (g)", main = print(nest.i))
  lines(predictWL$L, predictWL$PredW, type = 'l', col="blue", lwd=3, lty=1) # this plots our equation
  legend("topleft", legend=paste0("W=",round(exp(fit$coefficients[1]), 5), "L^",round(fit$coefficients[2], 4)),
    col= "blue", lty=1, lwd=3, cex=0.8)
  dev.off()
}




