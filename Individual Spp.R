# LW Relationships for INDIVIDUAL SPECIES 

# want to automate the creation of the same processes undertaken in lw_code and melissa_LW but for EACH species

# first I would like to read, clean and organize length-weight data as before... ######
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

lw$Species_No <- as.factor(lw$Species_No) # convert the species numbers into a factor- which they are





# Length weight equation
# W= a L ^b where W= weight, a= scaling coefficient ~condition, L= Length and b= shape parameter for the body form of a fish species

# transform allometric growh equation by taking the natural log of both sides
#  log (W) = log (a) + b* log (L)+E (where log(a)= intercept and b= slope and E=error)

# add two columns for log(W) and log(L)
lw <- lw %>%
  mutate(logW= log(Weight), logL= log(Std_Length))






# Plotting LW data and LWeq for INDIVIDUAL spp. based on Melissa's method-- for loop for ea spp #######

# select one species ex. medusafish

spp <- unique(lw$COMMON_NAME) # vector of indidivual species (common name) from our lw df
for (i in 1:length(spp)) {
nest.i<- spp[i] # for species i of our vector of species spp
species.i<- lw %>%
              filter(COMMON_NAME==nest.i) # filter out data for a specific species
  fit <- lm(formula= logW~ logL, data = species.i)  # fit log transformed w l data into linear model
  summary(fit) # this displays our intercept, slope values and R^2
  
  r<- range(species.i$Std_Length) #this defines the smallest and largest individual per species
  predictWL = data.frame(L = seq(r[1],r[2],1)) # this sets the range specifically for the values we have measurements for- change these values to change the range of estimates
  predictWL$PredW = exp(fit$coefficients[1])*predictWL$L^fit$coefficients[2] # predicted dataframe: for defined L values, what is our predicted W based on our equation
  
   # plot of W L data and our allometric growth equation with est parameters (a and b)
file_name<- paste(nest.i, ".pdf", sep = "") # iterative file name definition
pdf(paste0("/Volumes/ilysa.iglesias/ROCKFISH/Projects/JRREACruise/LWData/LW/spp_output/", file_name)) # this defines the location and name of output file
  plot(x=species.i$Std_Length, y=species.i$Weight, xlab = "Length (mm)", ylab = "Weight (g)", main = print(nest.i))
  lines(predictWL$L, predictWL$PredW, type = 'l', col="blue", lwd=3, lty=1)
  legend("topleft", legend=paste0("W=",round(exp(fit$coefficients[1]), 5), "L^",round(fit$coefficients[2], 4)),
    col= "blue", lty=1, lwd=3, cex=0.8)
  dev.off()
}

# if using ggplot, use 
#fun_lw<- function(x,y,a=exp(coef(fit)["(Intercept)"]),b=coef(fit)["logL"]) {y=a*x^b} # this is our lw equation with a and b defined from log transformed linear model
#stat_function(fun = fun_lw, geom = "line", color="cornflowerblue", lwd=2)



# UPDATE 03/05/2018 ##########
# John asked that I compile a list (maybe a list? not sure what form)- to show what data we have so far! In hopes that we could compile a list in the future of the 
# weights and lengths we still need....
# I looked up length info for each of the species we identified and plotted the size range we collected as a line representing the range.

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
    mutate(Max_Length_mm= Max_Length*10) # convert fishbase max length measurements to mm (to match other measured length values)


# plot of maximum length (values from fishbase) compared to our measured values from the cruise
pdf("length_range.pdf")
ggplot(data = maxL) +
  geom_col(aes(x=COMMON_NAME, y=Max_Length_mm), color= "grey", alpha=0.5)+ # columns representing the fish base max length values in grey
  geom_linerange(aes(x= COMMON_NAME, ymin=minL, ymax= maxL), color="purple", lwd=4)+ # the range of measurements we took for weight
  theme_bw()+ # removed background color
  coord_flip()+ #flipped the axis so spp names display clearly
  xlab("") + ylab("Length (mm)") #lables
dev.off()
