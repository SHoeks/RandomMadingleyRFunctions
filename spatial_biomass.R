##-------------------------------------------------------##
## Script for testing resterizing vegetation output data ##
##-------------------------------------------------------##

library('MadingleyR')

# source temporary function for making raster data for autotrophs
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/make_spatial_biomass_raster_autotrophs.R")

# Load MadingleyR default inputs
sptl_inp = madingley_inputs("spatial inputs")
chrt_def = madingley_inputs("cohort definition")
stck_def = madingley_inputs("stock definition")
mdl_prms = madingley_inputs("model parameters") 

# set spatial window
spatial_window = c(-11, 35, 35, 60)
plot_spatialwindow(spatial_window)

# init model
m_data = madingley_init(spatial_window = spatial_window,
                        cohort_def = chrt_def,
                        stock_def = stck_def,
                        spatial_inputs = sptl_inp,
                        max_cohort = 100)

# run test sim
m_data2 = madingley_run(years = 5,
                        out_dir = "/Volumes/WorkSSD/Phd/Papers/Projects_MadingleyR/",
                        madingley_data = m_data,
                        cohort_def = chrt_def,
                        stock_def = stck_def,
                        spatial_inputs = sptl_inp, 
                        max_cohort = 100)

# load and create spatial biomass raster autotrophs 
# this loads and rasterizes the autotroph data for the last 12 months of the simulation
autotr_biomass = make_spatial_biomass_raster_autotrophs(m_data2)
plot(log10(autotr_biomass), zlim=c(9,11)) # make plot from rasters, in kg

# check if values are correct (compare rasters against stock (autotroph) time line)
plot_timelines(m_data2) # plot the time line
test = c(); for(i in 1:12) test = c(test,sum(autotr_biomass[[i]][])) # sum the value of all rasters
months = sort(sort(m_data2$time_line_stocks$Month, decreasing = TRUE)[1:12]) + 1 # get months
lines(months,log10(test), col="red", lwd = 3) # add time line extracted from rasters



