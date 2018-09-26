###################### Length-Weight Data from RREA Cruise ########################################################################
# Project by: Ily Iglesias
# Date completed: 9/26/18
# Project description: Length-weight data relationships. This project plots the length-weight relationships of organisms
# collected on the 2016 rockfish recruitment and ecosystem assessment cruise and later updated to include data from 2018. 
# the relationship between length and weight is best represented by W= aL^b (where W=weight, L=length, a= scaling coefficient, b=shape parameter for the body form of fishes) established by Keys (1928)

# Data: There are TWO spreadsheets needed to run this code:
# species_codes: this is the page from the Access cruise dataset, the table "species_codes"
# lw_data: this is the inputted length weight data I entered from paper datasheets-- if this is updated in the future, this would be the file to add new records of weight and length to

# update 09/26/18: I ensured that all of the code was running smoothly and moved copy of files to public drive- also backed up to github
 
####################################################################################################################################
# data import and cleaning   #######################################################################################################

# read in length weight data
lw <- read.csv("lw_data.csv", header = TRUE)
# read species code table
species_codes <- read.csv("species_codes.csv", header = TRUE)

# load packages
library(tidyverse)

# clean up species_codes df
species_codes <- species_codes %>% # reference df
  select(-PACFIN_CODE, -NOTES) %>% #this removes the two columns PACFIN_CODE and NOTES
  rename(Species_No= SPECIES)%>% # changed column name to Species_No- to match lw df 
  as_tibble() #  cleans up  df formatting

# clean up lw df
lw <- lw %>%
  rename(Species_No= Species_Code) %>% # renamed column to match species_codes df
  as_tibble() # cleaned up formatting

# merge dfs
# Add species information from species_codes df to lw df - note will merge with key "Species_No" 
# note i only want to add the species info TO the lw data so this is a "left join"- want to add species names to LW data (which currently only has sp #)

lw <- lw %>% 
  left_join(species_codes, by= "Species_No")%>%
  select(-M, -MATURITY_CODES) # remove two columns that we will not be needed for analysis

  lw$Species_No <- as.factor(lw$Species_No) # convert the species numbers into a factor- which they are
  rm(species_codes) # remove the species_codes df since this information is now merged with our df "lw"
  
  
# Length weight equation ############################################################################################################
# W= a L ^b where W= weight, a= scaling coefficient ~condition, L= Length and b= shape parameter for the body form of a given fish species

# transform allometric growh equation by taking the natural log of both sides
#  log (W) = log (a) + b* log (L)+E (where log(a)= intercept and b= slope and E=error)

# add two columns for log(W) and log(L)-- note default of function log() is natural log, for more see ?log()
lw <- lw %>%
      mutate(logW= log(Weight), logL= log(Std_Length))

# plot linear relationships between log transformed L and log transformed W data- for each species-- 
  # this plot "logwlogl_allspp.pdf" produces scatter plots of the log w and log l- should be linear-- this is a check on data quality

  # all species at once- using facets: 
pdf("logwlogl_allspp.pdf")
ggplot(data= lw, mapping= aes(y= logW, x= logL)) + # defines our x and y values as well as the df
  geom_point(mapping = aes(color=SCI_NAME), show.legend = FALSE) + # colors points by species scientific name
  facet_wrap(~ SCI_NAME, nrow = 4) + # creates an individual plot for each species and since we have 24 spp, 4 rows of plots makes sense
  geom_smooth(method="lm", formula= y~ x, color= "black") + # plots linear models 
  xlab("Log Length mm") + ylab("Log Weight g") + ggtitle("Length-Weight") # add labels
dev.off()



# Plotting LW data and predicted LW eq values for INDIVIDUAL spp. {FOR LOOP}

spp <- unique(lw$COMMON_NAME) # vector of all species (common name) from our lw df- note if this is your input, you will create a plot for EACH spp in the spreadsheet. 
# if converseley you only want to plot ONE species, not all spp individually, your input should be spp<- "COMMON NAME" ex spp<- "BLUE LANTERNFISH"
# spp<- "BLUE LANTERNFISH" will produce one plot for Blue Lanternfish

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
    col= "blue", lty=1, lwd=3, cex=0.8) # this displays the equation in the uppper left corner of each plot
  dev.off()
}




# for reference: table of the total number of measurements taken per species 
total_per_spp <- lw %>%
  group_by(COMMON_NAME) %>% # i grouped by species so each row is a species
  summarise(total_no_measure= n()) # this took the total number of obs for a given species and output a simple table
write.csv(total_per_spp, "total_per_spp.csv")

