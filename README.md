Length Weight data from RREA cruise 

Created by Ily Iglesias (ilysa.iglesias@noaa.gov)
Date completed: 9/26/18
Project description: This project plots the length-weight relationships of organisms collected on the rockfish recruitment and ecosystem assessment cruise (2016 data and later updated to include data from 2018.) 

In order to use this code, you need:
1. file: "final_code.R" the R script which contains all the code you need to plot allometric growth for ea spp
2. file: "lw_data.csv" the input length and weight data entered from paper datasheets-- 
NOTE: if you wish to update this project, this is the file you would add additional length, weight data to
3. file: species_codes.csv these are the conventional species codes used by the groundfish analysis team (exported from larger cruise database) we use these to add species common names to spp code info
4. create a folder called "spp_output" in your R project file (it should already be there if you got this R project from II)
5. about 5 minutes: the code is fairly straight forward and well documented, so just press run and you should be good to go! I hope you find this helpful!

Note that if you select run, this code will generate a plot for EACH species within the database. If you want to create a plot for a specific spp of interest, follow instructions in line 79
