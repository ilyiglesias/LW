



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





