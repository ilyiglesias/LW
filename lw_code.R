###################### Length-Weight Data from 2016 Cruise ########################################################################
  # Project by: Ily Iglesias
  # Date started: 2/26/18: 
  # Project description: Length-weight data relationships. For this project, I would like to plot the length-weight relationships of organisms
  # collected on the 2016 rockfish recruitment and ecosystem assessment cruise. JF who asked if I could take on this project, said that 
  # the relationship between length and weight is best represented by W= aL^b (where W=weight, L=length, a= scaling coefficient, b=shape parameter for the body form of fishes)
  # note: this relationship was established by Keys (1928)
  # Data: I have two input data sets;
        # species_codes: this is the page from the Access cruise dataset, the table "species_codes"
        # 2016_lw: this is the inputted length weight data I entered from datasheets
  # Location: stored in my ilysa.iglesias drive under ROCKFISH//Projects/JRREACruise/LWData
####################################################################################################################################

# first I would like to read in the entered length-weight data
lw <- read.csv("2016_lw.csv", header = TRUE)

# read species code table
species_codes <- read.csv("species_codes.csv", header = TRUE)

# load packages
library(tidyverse)

# clean up dfs #########
species_codes <- species_codes %>%
          select(-PACFIN_CODE, -NOTES) %>% #this removed the two columns PACFIN_CODE and NOTES
          rename(Species_No= SPECIES) # changed column name to Species_No- tomatch lw df 
          as_tibble() # just cleans up the df formatting
# clean up lw df
lw <- lw %>%
      rename(Species_No= Species_Code) %>% # renamed column to match species_codes df
      as_tibble() # cleaned up formatting

getwd() # want to remember this path
# "/Volumes/ilysa.iglesias/ROCKFISH/Projects/JRREACruise/LWData/LW"




# merge dfs ######
# add species information from species_codes df to lw df - note will merge with key "Species_No" 
  # note i only want to add the species info TO the lw data so this is a "left join"- want to add species names to LW data (which currently only has sp #)

  lw <- lw %>% 
   left_join(species_codes, by= "Species_No")



# Length weight equation ########
  # W= a L ^b where W= weight, a= scaling coefficient ~condition, L= Length and b= shape parameter for the body form of a fish species

# LINEAR TRANSFORMATION ######
# There are multiple ways to calculate LW relationship-- This is the most basic
# I researched instructions for how to complete this online through a range of sources including: http://www.hafro.is/~einarhj/education/tcrenv2016/pre/r-models.pdf https://www.researchgate.net/post/How_to_Calculate_Fish_Length_Weight_Relationship and http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
# and http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf

# transform allometric growh equation by taking the natural log of both sides
      #  log (W) = log (a) + b* log (L)+E (where log(a)= intercept and b= slope and E=error)

# add two columns for log(W) and log(L)
lw <- lw %>%
    mutate(logW= log(Weight), logL= log(Std_Length))

# fit a linear model to our LW data
fit <- lm(formula= logW~ logL, data = lw) 
summary(fit) # this displays our intercept, slope values and R^2= 0.7715

coef(fit) # this prints out our intercept and slope values:
#(Intercept)        logL 
#-9.015215    2.446952 

# referencing model parameters ########
#a: exp(coef(fit)["(Intercept)"]) # this is the linear intercept back-transformed via a=e^intercept
#b: coef(fit)["logL"] # this is the slope of the linear, log transfomed wl data and does not need to be transformed


# plot transformed data along with fitted linear model: (ALL SPECIES COMBINED-)########
ggplot(data= lw, mapping= aes(y= logW, x= logL)) + # defines our x and y values as well as the df
  geom_point(mapping=aes(color=SCI_NAME)) + # plot our linear points for log length and log w- and colors points based on scientific name categories
  geom_smooth(method="lm", formula= y~ x, color= "black") + # plots our line 
  xlab("Log Length mm") + ylab("Log Weight g") + ggtitle("Transformed Length-Weight")



  # plot all species at once- using facets: NOTE this plots all Log(e) W values against Log(e)L - for EACH SPECIES #######
#pdf("logwlogl_allspp.pdf")
ggplot(data= lw, mapping= aes(y= logW, x= logL)) + # defines our x and y values as well as the df
  geom_point(mapping = aes(color=SCI_NAME), show.legend = FALSE) + 
  facet_wrap(~ SCI_NAME, nrow = 4) +
  geom_smooth(method="lm", formula= y~ x, color= "black") + # plots linear models 
  xlab("Log Length mm") + ylab("Log Weight g") + ggtitle("Length-Weight")
#dev.off()

# length(unique(lw$SCI_NAME)) #Note 24 total species

# lw function #######
fun_lw<- function(x,y,a=exp(coef(fit)["(Intercept)"]),b=coef(fit)["logL"]) {y=a*x^b} # this is our lw equation with a and b defined from log transformed linear model


# plotting the ACTUAL (LW) relationship ########  with equation annotation (that could use some work)
#pdf("lw_eq.pdf")
ggplot(data = lw, mapping = aes(y= Weight, x=Std_Length))+ # define x y note these are the real values
  geom_point(mapping = aes(alpha=1/10), color="bisque4", show.legend = FALSE) + #plots our LW points with transparency
  stat_function(fun = fun_lw, geom = "line", color="cornflowerblue", lwd=2) + # this draws our LW equation using the parameters a and b defined by the log transformed LW data
  facet_wrap(~ SCI_NAME, nrow = 4) +
  xlab("Length mm") + ylab("Weight g") + ggtitle("LW relationship with est parameters values")+ # add labels for x, y and main
  theme_classic() + # removed grid lines 
  annotate(geom = "text",x = 50,y = 40,label = paste0("W =", "(", round(exp(coef(fit)["(Intercept)"]), 5), ")"," ", "L", "^", round(coef(fit)["logL"], 4)), col = "grey25") # label equation
#dev.off()









############################################ II you are here 2/27 ###########################################################################
# need to work on the label :) so the carrot isn't showing!!! ###############################################################################
# hmmm, the stat_function line of code plots our equation for W=aL^b based on the parameters derived from the linear model. Unfortuantley it doesn't appear to fit well.

# another great source on curve fitting:
# http://www.css.cornell.edu/faculty/dgr2/teach/R/R_CurveFit.pdf
?predict() # for tomorrow: melissa's code uses predict, will need to take a peek!
  
  
# nls- non-linear regression model- an alternative way to estimate parameters #######################
  # i followed instructions from https://stackoverflow.com/questions/22664763/plotting-a-power-fit-of-for-y-axb
m <- nls(Weight~a*Std_Length^b, data = lw, start = list(a=1, b=1))
summary(m) # this reveals totally different values for intercept: 0.000004187 and slope: 3.29
coef(m)
# estimate goodness of fit? Not sure thorough what methods
cor(lw$Std_Length, predict(m)) # 0.85 so I think this is fairly decent
jpeg("nls.jpg")
plot(lw$Std_Length, lw$Weight, xlab="Fish Length (mm)", ylab= "Fish Weight (g)") # this is a plot of length (x) and weight (y)
x_L <- sort(lw$Std_Length) # this sorts the length data- not sure why i need this but without doing this step I get a very strange zig zag pattern
lines(x_L, predict(m, list(Std_Length=x_L)), col="purple", lwd=4) # this plots the line for our x= x_L lengths and predicted y (weight) values based on our estimated parameters from the nls model

dev.off() # delete all plot figures 




# also referenced: https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/


# ONE other version NOAA method https://www.pifsc.noaa.gov/library/pubs/admin/PIFSC_Admin_Rep_12-03.pdf
  # logW = logA + BlogL ====  bo + b1 LogL + e  - note bo= linear intercept, b1= slope and e= residual error

# https://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
# ugggg over my head, use function mle() 






rm(list = ls())

















