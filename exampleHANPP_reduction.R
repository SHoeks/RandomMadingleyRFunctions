# load MadingleyR package
library(MadingleyR)

# set spatial window and load spatial rasters
spatial_window = c(0, 3, 45, 48)
plot_spatialwindow(spatial_window)
sp_in = madingley_inputs('spatial inputs') # load default inputs, including hanpp layer

# source function raster data for autotrophs
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/make_spatial_biomass_raster_autotrophs.R")

# NEW! source combined timeline function
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/plot_combined_timelines2.R")

# NEW! source function to crop spatial rasters using spatial window
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/crop_spatial_rasters_to_window.r")

# when you're using a small spatial window (e.g. 10x10 grid cells) this function crops the raster
# helps to speed up loading times and makes working with the inputs easier
sp_in_crop = crop_spatial_rasters_to_window(inputs = sp_in, spatial_window = spatial_window)

# initialise model, hanpp layer not yet relevant here
mdata = madingley_init(spatial_window = spatial_window, 
                       spatial_inputs = sp_in_crop, 
                       max_cohort = 100)

# run spin-up (without hanpp)
mdata2 = madingley_run(madingley_data = mdata, 
                       years = 50, 
                       spatial_inputs = sp_in_crop, 
                       max_cohort = 100)

# this should be your final hanpp layer, to one you want to work towards (sp_inputs_SSP2.6_2100$hanpp?)
final_hanpp = sp_in$hanpp
final_hanpp[] = sample(seq(0.1,0.3,0.1),length(final_hanpp[]),replace = TRUE) 
final_hanpp = crop_spatial_rasters_to_window(final_hanpp, spatial_window = spatial_window)

# prebuild hanpp reduction rasters
reduct_hanpp_values = matrix(seq(1,0.0,-0.1),ncol=length(final_hanpp[]),nrow=11)
for(c in 1:ncol(reduct_hanpp_values)){
  for(r in 1:nrow(reduct_hanpp_values)) reduct_hanpp_values[r,c]=ifelse(reduct_hanpp_values[r,c]<final_hanpp[c],final_hanpp[c],reduct_hanpp_values[r,c])
}
# each row in this matrix represents the hanpp values of one hanpp raster at a specific time in the following loop
# so each column in the matrix represents a single grid cell and the hanpp values over time

# run reduction loop
mdata_list = list(mdata2)
for(i in 1:nrow(reduct_hanpp_values)){
  
  # set hanpp
  print(paste("HANPP reduction:",i,"/",nrow(reduct_hanpp_values)))
  sp_in_crop$hanpp[] = reduct_hanpp_values[i,] # set values using row in raster
    
  # run model
  mdata_list[[i+1]] = madingley_run(madingley_data = mdata_list[[i]], 
                         years = 10, # run for 10 years? 
                         spatial_inputs = sp_in_crop, 
                         max_cohort = 100,
                         apply_hanpp = 1,
                         silenced = TRUE)
  
}

# run mandingley continuation
sp_in_crop$hanpp[] = final_hanpp[] 
mdata_list[[length(mdata_list)+1]] = madingley_run(madingley_data = mdata_list[[length(mdata_list)]], 
                       years = 20, 
                       spatial_inputs = sp_in_crop, 
                       max_cohort = 100, 
                       apply_hanpp = 1)


# plot combined timeline
plot_combined_timelines(mdata_list, legend=TRUE, plot=TRUE) # plot=FASLE returns timeline data.frame




