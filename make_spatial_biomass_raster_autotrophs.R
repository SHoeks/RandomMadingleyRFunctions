make_spatial_biomass_raster_autotrophs = function(md){
  
  # get spatial window
  sw = md$spatial_window
  e = raster::extent(sw[1], sw[2], sw[3], sw[4])
  
  # check if out_dir was specified manually within madingley_run()
  if(!is.null(md$out_path)){ # out_dir specified manually
    tdo = md$out_path
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='/')  tdo=substr(tdo,1,nchar(tdo)-1)
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='\\') tdo=substr(tdo,1,nchar(tdo)-1)
    if(dir.exists(paste0(tdo,md$out_dir_name))) {
      out_dir = tdo
      cat(paste0("loading inputs from: ",out_dir,md$out_dir_name))
    }
  }else{ # use default output dir
    out_dir = tempdir()
    cat(paste0("loading inputs from: ",out_dir,md$out_dir_name))
  }
  
  # check if dir exists
  if(!dir.exists(paste0(out_dir,md$out_dir_name))) stop("Unable to find output folder: ",paste0(out_dir,md$out_dir_name))
  
  # default color pal
  pal = col
  
  # get data paths
  snames = grep("StockProperties",list.files(paste0(out_dir,md$out_dir_name,'stock_properties/'),full.names=T),value = T)
  
  # check if required files were exported
  if(!length(snames)>1) stop("Required files were not exported during model run")
  
  # select last 12 csvs
  snames = snames[1:(length(snames)-1)]; snames = snames[(length(snames)-11):length(snames)] 
  smonths = sort(sort(md$time_line_stocks$Month, decreasing = TRUE)[1:12])
  
  # create empty raster brick
  r_empty = new("RasterLayer", data = new(".SingleLayerData", values = logical(0), offset = 0, gain = 1, 
            inmemory = FALSE, fromdisk = TRUE, isfactor = FALSE, attributes = list(), haveminmax = TRUE, 
            min = 0, max = 1, band = 1L, unit = "", names = "r_empty"), 
            extent = new("Extent", xmin = -180, xmax = 180, ymin = -90, ymax = 90), rotated = FALSE, 
            rotation = new(".Rotation", geotrans = numeric(0), transfun = function () 
            NULL), ncols = 360L, nrows = 180L, crs = new("CRS", projargs = NA_character_), history = list(), z = list())
  values(r_empty) = 0
  r_empty = crop(r_empty,e)
  if(md$grid_size!=1) {
    r_sized = raster::raster(nrow=dim(r_empty)[1]*(1/md$grid_size),ncol=dim(r_empty)[2]*(1/md$grid_size))
    extent(r_sized) = c(sw[1], sw[2], sw[3], sw[4])
    r_empty = raster::resample(r_empty,r_sized,"bilinear")
  }
  m_r_empty = raster::as.matrix(r_empty)
  r_empty_stack = list()
  for(i in 1:length(snames)) {
    r_empty_stack[[i]] = r_empty
    names(r_empty_stack[[i]]) = paste0("sim_month_",smonths[i])
  }
  
  # process files
  csv_out_path = paste0(out_dir,md$out_dir_name,'Data_proc_biomass.csv')
  
  # insert raster values
  lat = 1:nrow(m_r_empty); long = 1:ncol(m_r_empty)
  for(mnth in 1:length(snames)){
    bio = aggregate(TotalBiomass~GridcellIndex,read.csv(snames[mnth]),FUN=sum) # read and process data
    m_r_empty[] = 0 # reset insert matrix
    gc_counter = 0 # reset gc counter
    for(i in lat){
      for(j in long){
        if(!all(!bio$GridcellIndex==gc_counter)){
          val = bio$TotalBiomass[bio$GridcellIndex==gc_counter]
          if(is.infinite(val)){
            m_r_empty[i,j] = 0
          }else{
            m_r_empty[i,j] = val
          }
        }else{
          m_r_empty[i,j] = 0
        }
        gc_counter = gc_counter+1
      }
    }
    r_empty_stack[[mnth]][] = apply(m_r_empty, 2, rev) # flip matrix vertically
  }
  
  return(brick(r_empty_stack))
}
