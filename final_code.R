###################### Length-Weight Data from 2016 Cruise ########################################################################
# Project by: Ily Iglesias
# Date started: 2/26/18
# Project description: Length-weight data relationships. For this project, I would like to plot the length-weight relationships of organisms
# collected on the 2016 rockfish recruitment and ecosystem assessment cruise. JF who asked if I could take on this project, said that 
# the relationship between length and weight is best represented by W= aL^b (where W=weight, L=length, a= scaling coefficient, b=shape parameter for the body form of fishes)
# note: this relationship was established by Keys (1928)
# Data: I have two input data sets;
# species_codes: this is the page from the Access cruise dataset, the table "species_codes"
# lw_data: this is the inputted length weight data I entered from datasheets
# Location: stored in my ilysa.iglesias drive under ROCKFISH//Projects/JRREACruise/LWData
# update 3/2/18: John asked if I could tally the total number of measurements per species, so I calculated the total # of measurements per spp (df: total_per_spp) and added these values to the plot length_range
# update 5/31/18: I updated the input file so that it now includes measurements for length and weight taken from this year's cruise (RL 1802)- I also renamed this file (from 2016_lw) to lw_data 
  
  # todo: I now need to re-run this code with the updated values
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
# I want to add species information from species_codes df to lw df - note will merge with key "Species_No" 
# note i only want to add the species info TO the lw data so this is a "left join"- want to add species names to LW data (which currently only has sp #)

lw <- lw %>% 
  left_join(species_codes, by= "Species_No")%>%
  select(-M, -MATURITY_CODES) # remove two columns that we will not be needed for analysis

  lw$Species_No <- as.factor(lw$Species_No) # convert the species numbers into a factor- which they are
  
# Length weight equation ############################################################################################################
# W= a L ^b where W= weight, a= scaling coefficient ~condition, L= Length and b= shape parameter for the body form of a fish species

# transform allometric growh equation by taking the natural log of both sides
#  log (W) = log (a) + b* log (L)+E (where log(a)= intercept and b= slope and E=error)

# add two columns for log(W) and log(L)
lw <- lw %>%
      mutate(logW= log(Weight), logL= log(Std_Length))

# added 3/2/18: i would like to tally the total number of measurements taken per species and add this to the exisitng plot of max lengths and length ranges
total_per_spp <- lw %>%
                  group_by(COMMON_NAME) %>% # i grouped by species so each row is a species
                  summarise(total_no_measure= n()) # this took the total number of obs for a given species and output a simple table
        write.csv(total_per_spp, "total_per_spp.csv")
#filter(lw, COMMON_NAME=="KING-OF-THE-SALMON") # as a check I ensured that indeed there was only one record for Goby

# plot linear relationships between log transformed L and log transformed W data- for each species
# all species at once- using facets: 
pdf("logwlogl_allspp.pdf")
ggplot(data= lw, mapping= aes(y= logW, x= logL)) + # defines our x and y values as well as the df
  geom_point(mapping = aes(color=SCI_NAME), show.legend = FALSE) + # colors points by species scientific name
  facet_wrap(~ SCI_NAME, nrow = 4) + # creates an individual plot for each species and since we have 24 spp, 4 rows of plots makes sense
  geom_smooth(method="lm", formula= y~ x, color= "black") + # plots linear models 
  xlab("Log Length mm") + ylab("Log Weight g") + ggtitle("Length-Weight") # add labels
dev.off()

# Plotting LW data and predicted LW eq values for INDIVIDUAL spp. {FOR LOOP}

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
    col= "blue", lty=1, lwd=3, cex=0.8) # this displays the equation in the uppper left corner of each plot
  dev.off()
}

# UPDATE: 6/4/18: the above code was re-run to include values collected during leg one of the 2018 cruise- II

# UPDATE 03/05/2018 #######################################################################################################################
# In hopes that we could compile a list in the future of the weights and lengths that we still need..
# I looked up length info for each of the species we identified, and plotted the size range we collected as a line representing that range.

# these data came from fishbase.org-- and will be stored in a spreadsheet called sppLength
sci_names<- unique(lw$SCI_NAME) # create a vector of the species names we have lw data for
write.csv(sci_names, "sci_names.csv") # exported list of scientific names that we have lw data for-- then added manually max length info and baysian estimated a and b values for lw

# import csv
sppLength<- read.csv("sppLength.csv", header = TRUE) # imported csv with max length values from fishbase

# add length values per spp from lw df
maxL <- lw %>%
  select(Species=SCI_NAME, COMMON_NAME, Std_Length) %>% # selected 3 columns from lw df
  group_by(Species, COMMON_NAME) %>% # grouped data by Species (and included common name as well)
  summarise(minL= min(Std_Length), maxL=max(Std_Length))%>% #created two new columns which found the min and max length PER SPECIES
  left_join(sppLength, by="Species") %>% # joined the max length info from fish base (housed in df sppLength) to our maxL df (from lw data)
  filter(fish=="fish") %>% # because the inverts don't have info on max l from fishbase, I only selected fish spp
  mutate(Max_Length_mm= Max_Length*10)%>% # convert fishbase max length measurements to mm (to match other measured length values)
  # update 5/2/18: added total number of measurements per spp from df total_per_spp
  left_join(total_per_spp, by="COMMON_NAME") # this adds a column for the total number of measurements per fish spp


# plot of maximum length (values from fishbase) compared to our measured values from the cruise
#pdf("length_range.pdf")
ggplot(data = maxL) +
  geom_col(aes(x=COMMON_NAME, y=Max_Length_mm), color= "grey", alpha=0.5)+ # columns representing the fish base max length values in grey
  geom_linerange(aes(x= COMMON_NAME, ymin=minL, ymax= maxL), color="purple", lwd=4)+ # the range of measurements we took for weight
  geom_text(aes(x=COMMON_NAME, y=1000, label=total_no_measure)) +
  theme_bw()+ # removed background color
  coord_flip()+ #flipped the axis so spp names display clearly
  xlab("") + ylab("Length (mm)") #lables
#dev.off()








#NOTE: I was also able to plot these values via nls (non-linear least squares) in case I need to do this again!!
# nls- non-linear regression model- an alternative way to estimate parameters #######################
# i followed instructions from https://stackoverflow.com/questions/22664763/plotting-a-power-fit-of-for-y-axb
m <- nls(Weight~a*Std_Length^b, data = lw, start = list(a=1, b=1))
summary(m) # this reveals totally different values for intercept: 0.000004187 and slope: 3.29
coef(m)
# estimate goodness of fit? Not sure thorough what methods
cor(lw$Std_Length, predict(m)) # 0.85 so I think this is fairly decent

plot(lw$Std_Length, lw$Weight, xlab="Fish Length (mm)", ylab= "Fish Weight (g)") # this is a plot of length (x) and weight (y)
x_L <- sort(lw$Std_Length) # this sorts the length data- not sure why i need this but without doing this step I get a very strange zig zag pattern
lines(x_L, predict(m, list(Std_Length=x_L)), col="purple", lwd=4) # this plots the line for our x= x_L lengths and predicted y (weight) values based on our estimated parameters from the nls model


# I will eventually have to update these data again after the second leg of the cruise- will need to re run at that time!

