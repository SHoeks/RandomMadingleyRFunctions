get_data = function(n_years_to_av,removalBodyMassThresholds,phases_timelines,
                    maddata1,maddata2,maddata3){

  
  BMafterRemoval_H = removalBodyMassThresholds["BMafterRemoval_H"]/1000
  BMafterRemoval_C = removalBodyMassThresholds["BMafterRemoval_C"]/1000
  BMafterRemoval_O = removalBodyMassThresholds["BMafterRemoval_O"]/1000
  
  # endo herbivores
  herb_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==0,] # keep only herbivores endotherm. fg = 0
    if(nrow(tl)==0){
      herb_mean_last_months_kg = c(herb_mean_last_months_kg,0)
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      herb_mean_last_months_kg = c(herb_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
    }

  }
  
  # endo carnivores
  carn_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==1,] # keep only carnivores endotherm. fg = 2
    if(nrow(tl)==0){
      carn_mean_last_months_kg = c(carn_mean_last_months_kg,0)
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      carn_mean_last_months_kg = c(carn_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
    }

  }
  
  # endo omnivores
  omni_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==2,] # keep only omnivores endotherm. fg = 1
    if(nrow(tl)==0){
      omni_mean_last_months_kg = c(omni_mean_last_months_kg,0)
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      omni_mean_last_months_kg = c(omni_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
    }
  }
  
  # endo all heterotrophs
  all_heterotrophs_mean_last_months_kg = omni_mean_last_months_kg + carn_mean_last_months_kg + herb_mean_last_months_kg
  
  # mega endo herbivores only
  herb_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==0,] # keep only herbivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_H),] # get mega herbs only
    if(nrow(tl)==0){
      herb_mega_mean_last_months_kg = c(herb_mega_mean_last_months_kg,0) # insert mean of monthly biomass
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      herb_mega_mean_last_months_kg = c(herb_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomassesses
    }
  }
  
  # mega endo carnivores only
  carn_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==1,] # keep only carnivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_C),] # get mega carnivores only
    if(nrow(tl)==0){
      carn_mega_mean_last_months_kg = c(carn_mega_mean_last_months_kg,0) # insert mean of monthly biomassesses
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      carn_mega_mean_last_months_kg = c(carn_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
    }
  }
  
  # mega endo omnivores only
  omni_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(phases_timelines[[i]])
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==2,] # keep only carnivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_O),] # get mega carnivores only
    if(nrow(tl)==0){
      omni_mega_mean_last_months_kg = c(omni_mega_mean_last_months_kg,0) # insert mean of monthly biomassesses
    }else{
      tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
      omni_mega_mean_last_months_kg = c(omni_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
    }
  }
  
  # autotrophs
  #load("Phase_5_Stabilisation.Rdata")
  t1<-m_data1$time_line_stocks
  t2<-m_data2$time_line_stocks
  t3<-m_data3$time_line_stocks
  t1<-aggregate(TotalStockBiomass~Year,FUN=mean,data=t1)
  t2<-aggregate(TotalStockBiomass~Year,FUN=mean,data=t2)
  t3<-aggregate(TotalStockBiomass~Year,FUN=mean,data=t3)
  t1 = t1[t1$Year%in%(max(t1$Year)-n_years_to_av):max(t1$Year),]
  t2 = t2[t2$Year%in%(max(t2$Year)-n_years_to_av):max(t2$Year),]
  t3 = t3[t3$Year%in%(max(t3$Year)-n_years_to_av):max(t3$Year),]
  t1 = mean(t1$TotalStockBiomass)
  t2 = mean(t2$TotalStockBiomass)
  t3 = mean(t3$TotalStockBiomass)
  veg = c(t1,t2,t3)
  
  values = c(veg,
             all_heterotrophs_mean_last_months_kg,
             herb_mean_last_months_kg,
             carn_mean_last_months_kg,
             omni_mean_last_months_kg,
             herb_mega_mean_last_months_kg,
             carn_mega_mean_last_months_kg,
             omni_mega_mean_last_months_kg)
  
  values_rat = c(veg[1]/veg,
                 all_heterotrophs_mean_last_months_kg[1]/all_heterotrophs_mean_last_months_kg,
                 herb_mean_last_months_kg[1]/herb_mean_last_months_kg,
                 carn_mean_last_months_kg[1]/carn_mean_last_months_kg,
                 omni_mean_last_months_kg[1]/omni_mean_last_months_kg,
                 herb_mega_mean_last_months_kg[1]/herb_mega_mean_last_months_kg,
                 carn_mega_mean_last_months_kg[1]/carn_mega_mean_last_months_kg,
                 omni_mega_mean_last_months_kg[1]/omni_mega_mean_last_months_kg)
  values_rat = 1/values_rat
  
  values_log = log10(values)
  values_log[is.infinite(values_log)] = 0
  values[is.infinite(values)] = 0
  
  group = c(rep("Autotrophs",3),
            rep("All Endotherms",3),
            rep("Herbivores",3),
            rep("Carnivores",3),
            rep("Omnivores",3),
            rep("Mega Herbivores",3),
            rep("Mega Carnivores",3),
            rep("Mega Omnivores",3))
  
  Scenario = rep(c("1. Spin-up","2. After removel","3. After Reintroduction"),8)
  scenario_number = rep(c("1","2","3"),8)
  
  
  df = data.frame(cbind(values,values_log,group,Scenario,scenario_number))
  
  df$values = as.numeric(df$values)
  df$values_rat = as.numeric(values_rat)
  df$values_log = as.numeric(values_log)
  df$values_perc = (values_rat-1)*100
  df$values_perc2 = (values_rat)*100
  df$order = nrow(df):1
  df$myFacet = df$group != "Autotrophs"
  
  # setwd(currentwd) # reset wd
  df$Scenario = as.character(df$Scenario)
  df$Scenario[df$Scenario=="1. Spin-up"]= "1. Initial state"
  df$Scenario[df$Scenario=="2. After removel"]= "2. Post removal"
  df$Scenario[df$Scenario=="3. After Reintroduction"]= "3. Post reintroduction"
  
  
  return(df)
}

createSingleCohort = function(cohortAdultMass_kg, gc_index, functional_group, isAdult = 0, sp_inputs, cohort_def=0) {
  
  
  # example:
  # library(MadingleyR)
  # sp_inputs = madingley_inputs("spatial inputs")
  # createSingleCohort(cohortAdultMass_kg=200,gc_index=0,functional_group=0,isAdult=1,sp_inputs=sp_inputs)
  
  if(cohort_def==0) cohort_def = MadingleyR::madingley_inputs('cohort definition')
  
  
  functional_group_index=functional_group+1
  
  TrophicIndex = 0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Herbivore") TrophicIndex = 2.0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Carnivore") TrophicIndex = 3.0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Omnivore")  TrophicIndex = 2.5
  
  
  optimalPreyBodySizeRatio = 0
  if( cohort_def$DEFINITION_Endo.Ectotherm[functional_group_index] == "Endotherm" && cohortAdultMass_kg >= 21 && 
      cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Carnivore" ) {
    
    optimalPreyBodySizeRatio = max( 0.01, rnorm( 1, 1, 0.02 ) );
    
  }else{
    
    optimalPreyBodySizeRatio = max( 0.01, rnorm( 1, 0.1, 0.02 ) );
    
  }   
  #print(optimalPreyBodySizeRatio)
  
  options(warn=-1)
  cohortAdultMass = cohortAdultMass_kg*1000
  expectedLnAdultMassRatio = 2.24 + 0.13 * log( cohortAdultMass );
  cohortAdultMassRatio = 1.0 + 10 * rnorm(n=1, mean=expectedLnAdultMassRatio, sd=0.5)
  cohortJuvenileMass = cohortAdultMass * 1.0 / cohortAdultMassRatio;
  NewBiomass = ( 3300. / 1000 ) * 100 * 3000 * 0.6^log10( cohortJuvenileMass ) * ( mean(area(sp_inputs$Endo_H_max)[]/10000) )
  options(warn=0)
  
  out = data.frame(
    GridcellIndex = gc_index,
    FunctionalGroupIndex = functional_group,
    JuvenileMass = cohortJuvenileMass,
    AdultMass = cohortAdultMass,
    IndividualBodyMass = cohortJuvenileMass,
    CohortAbundance = NewBiomass,
    LogOptimalPreyBodySizeRatio = log(optimalPreyBodySizeRatio),
    BirthTimeStep = 0,
    ProportionTimeActive = 0.5,
    TrophicIndex = TrophicIndex,
    IndividualReproductivePotentialMass = 0,
    MaturityTimeStep = 0,
    IsAdult = 0,
    AgeMonths = 0,                      
    TimeStepsJuviline = 0,            
    TimeStepsAdult = 0  
  )
  
  if(isAdult==1) {
    out$IndividualBodyMass = cohortAdultMass
    out$IsAdult = 1
    out$TimeStepsAdult = 1
    out$AgeMonths = 1
    out$TimeStepsJuviline = 1
  }
  
  return(out)
  
}

plotBinnedTimelines = function(madingley_data,plot=TRUE,only_above_zero=FALSE){
    
    cdef = madingley_data$cohort_def
    
    if(class(madingley_data$out_path)=="NULL"){
      dir_path = paste0(tempdir(),madingley_data$out_dir_name,"/cohort_properties/") # make dir name
    }else{
      dir_path = paste0(madingley_data$out_path,madingley_data$out_dir_name,"/cohort_properties/") # make dir name
    }
    
    out_paths = list.files(dir_path,pattern='.csv',full.names = T) # list cohort output files
    out_paths = grep("UserDefinedBinnedCohortStatistics",out_paths,value = T) # grep only BinnedCohortStatistics
    data = lapply(out_paths,read.csv) # read all data
    
    print(dir_path)
    
    for(i in 1:length(data)){ 
      
      data[[i]]$Month = i # add months
      data[[i]]$Group = 1:nrow(data[[i]]) # add group id
      
    }
    
    data = do.call("rbind", data) # rbind all data in list
    n_groups = max(data$Group) # get number of groups
    n_months = max(data$Month) # total number of monhts
    max_fg = max(data$FunctionalGroupIndex) # fg end
    min_fg = min(data$FunctionalGroupIndex) # fg start
    
    output_data = list()
    
    # plot data
    for(fg in min_fg:max_fg){
      
      data_fg = data[data$FunctionalGroupIndex == fg,]
      par(mfrow=c(2,4))
      
      for(bin in unique(data_fg$Group)){
        
        data_fg_bin = data_fg[data_fg$Group == bin,]
        TotalBiomass_kg = log10(data_fg_bin$TotalBiomass_kg)
        TotalBiomass_kg = ifelse(is.infinite(TotalBiomass_kg),0,TotalBiomass_kg)
        
        if(plot){
          if(min_fg==0) {fg_data = fg + 1} else {fg_data = fg}
          diet = cdef$DEFINITION_Nutrition.source[fg_data]
          therm = cdef$DEFINITION_Endo.Ectotherm[fg_data]
          main1 = paste(diet," ",therm," (",fg,")",'\n bin = ',
                        data_fg_bin$LowerBodyMassBin[1],' to ',
                        data_fg_bin$UpperBodyMassBin[1],'kg',sep='')
        }
        if(plot) {
          if(only_above_zero){
            if( sum(TotalBiomass_kg)>0){
              plot(data_fg_bin$Month,TotalBiomass_kg,type='l',main=main1,xlab="month",ylab='log10 biomass kg',col = 'darkgrey')
            }
          }else{
            plot(data_fg_bin$Month,TotalBiomass_kg,type='l',main=main1,xlab="month",ylab='log10 biomass kg',col = 'darkgrey')
          }
        }
      }
      
      if(!plot){
        if(min_fg==0) {fg_data = fg + 1} else {fg_data = fg}
        data_fg$Group = NULL
        data_fg$LowerLog10Bin = log10(data_fg$LowerBodyMassBin)
        data_fg$LowerBodyMassBin = NULL
        data_fg$UpperLog10Bin = log10(data_fg$UpperBodyMassBin)
        data_fg$UpperBodyMassBin = NULL
        output_data[[fg_data]] = data_fg
      }
    }
    
    par(mfrow=c(1,1))
    
    if(!plot){
      return(output_data)
    }
    
    
}

merge_tl = function(timelines_data_xx, start_year = 0){
  tl_data = do.call("rbind", timelines_data_xx)
  tl_data$FunctionalGroupIndex = tl_data$FunctionalGroupIndex + 1
  tl_data$Year<-round(tl_data$Month/12)
  tl_data<-aggregate(TotalBiomass_kg ~ LowerLog10Bin+Year+FunctionalGroupIndex, data=tl_data, FUN=mean)
  tl_data$Year = tl_data$Year + start_year
  return(tl_data)
}

merge_tl_list = function(timelines_data_xx, start_year = 0){
  
  new_list = list()
  for(i in 1:length(timelines_data_xx)) {
    new_list[[i]] = do.call("rbind", timelines_data_xx[[i]])
    if(i>1){
      new_list[[i]]$Month = new_list[[i]]$Month + max(new_list[[i-1]]$Month)
    }
  }
  
  tl_data = do.call("rbind", new_list)
  tl_data$FunctionalGroupIndex = tl_data$FunctionalGroupIndex + 1
  tl_data$Year<-round(tl_data$Month/12)
  tl_data<-aggregate(TotalBiomass_kg ~ LowerLog10Bin+Year+FunctionalGroupIndex, data=tl_data, FUN=mean)
  tl_data$Year = tl_data$Year + start_year
  return(tl_data)
}

make_plot = function(tldata,FG_select,maxY,Intervals,SizeIntervalLines){
  
  colvect<-c("#a6cee3", "#1f78b4", '#b2df8a', '#33a02c', '#fdbf6f', '#ff7f00', '#fb9a99', '#e31a1c')
  
  H1p<-ggplot(subset(tldata,FunctionalGroupIndex==FG_select), aes(x=Year, y=TotalBiomass_kg, col=as.factor(10^LowerLog10Bin), alpha=0.5)) +
    geom_line() +
    coord_cartesian(ylim=c(0, maxY)) +
    geom_vline(xintercept=Intervals, linetype="dashed", size=SizeIntervalLines)  +
    scale_colour_manual(values=colvect) +
    ggtitle("Herbivores endotherm") + 
    ylim(0, maxY)
  return(H1p)
  
}

make_multipanal_plot = function(tldata,FG_select,Intervals,SizeIntervalLines,Titles=c("Herbivores endotherm","Carnivores endotherm","Omnivores endotherm")){
  
  
  colvect<-c("#a6cee3", "#1f78b4", '#b2df8a', '#33a02c', '#fdbf6f', '#ff7f00', '#fb9a99', '#e31a1c')
  
  plotz = list()
  for(i in FG_select) {
    maxY = max(subset(tldata,FunctionalGroupIndex==i)$TotalBiomass_kg)
    plotz[[i]]<-ggplot(subset(tldata,FunctionalGroupIndex==i), aes(x=Year, y=TotalBiomass_kg, col=as.factor(10^LowerLog10Bin), alpha=0.5)) +
      geom_line() +
      coord_cartesian(ylim=c(0, maxY)) +
      geom_vline(xintercept=Intervals, linetype="dashed", size=SizeIntervalLines)  +
      scale_colour_manual(values=colvect) +
      ggtitle(Titles[i]) + 
      ylim(0, maxY)
  }
  ggarrange(plotz[[1]], plotz[[2]],plotz[[3]], labels=c("(a)","(b)", "(c)"), font.label=list(size=15, face="plain"), nrow=1, ncol=3, common.legend=TRUE)
  
  
  
  
}

FW_abs = function(madingley_data,max_flows=10,color=T,sample_data=100,min_log_scale=5,max_log_scale=11,...){
  
  # check if out_dir was specified manually within madingley_run()
  if(!is.null(madingley_data$out_path)){ # out_dir specified manually
    tdo = madingley_data$out_path
    # remove slashes from out_dir
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='/')  tdo=substr(tdo,1,nchar(tdo)-1)
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='\\') tdo=substr(tdo,1,nchar(tdo)-1)
    if(dir.exists(paste0(tdo,madingley_data$out_dir_name))) {
      out_dir = tdo
      cat(paste0("loading inputs from: ",out_dir,madingley_data$out_dir_name))
    }
  }else{ # use default output dir
    out_dir = tempdir()
    cat(paste0("loading inputs from: ",out_dir,madingley_data$out_dir_name))
  }
  
  
  # check if dir exists
  if(!dir.exists(paste0(out_dir,madingley_data$out_dir_name))){ # not exist
    stop("Unable to find output folder")
  }
  
  # paths
  # needed madingley_data!!
  if(length(list.files(paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)))==0){
    base_path = paste0(gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)
  }else{
    base_path = paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)
  }
  
  # determine sample fraction
  sample_fraction = sample_data/100
  
  # make file paths
  cohort_path = paste0(base_path,'cohort_properties')
  flow_path = paste0(base_path,'consumption_statistics')
  
  # list files
  flow_files = list.files(flow_path,pattern="PreyBinnedFoodwebConnections",full.names = T)
  bins_files = list.files(cohort_path,pattern="BinnedCohortStatistics",full.names = T)
  
  # check if required files were exported
  if(!length(flow_files)>1){ # not exist
    stop("Required files were not exported during model run")
  }
  if(!length(bins_files)>1){ # not exist
    stop("Required files were not exported during model run")
  }
  
  # init loop
  iterator = 1:length(list.files(flow_path,pattern="PreyBinnedFoodwebConnections"))
  flows_l = list()
  bins_l = list()
  
  # loop over time steps
  for(timestep in iterator){
    
    # list files and load the last 120 csvs
    flow_files = list.files(flow_path,pattern="PreyBinnedFoodwebConnections",full.names = T)
    bins_files = list.files(cohort_path,pattern="BinnedCohortStatistics",full.names = T)
    #cat("Processing: ",flow_files[timestep])
    #cat("Processing: ",bins_files[timestep])
    flows = read.csv(flow_files[timestep])
    bins = read.csv(bins_files[timestep])
    
    # sample data
    if(sample_fraction!=1){
      nrow_flows = nrow(flows); nrow_bins = nrow(bins)
      flows_sample = sample(1:nrow_flows,round(nrow_flows*sample_fraction))
      bins_sample = sample(1:nrow_bins,round(nrow_bins*sample_fraction))
      flows = flows[flows_sample,]
      bins = bins[bins_sample,]
    }
    
    # process bins
    bins = aggregate(bins,by=list(bins$FunctionalGroupIndex,bins$LowerBodyMassBin,bins$UpperBodyMassBin),FUN=sum)
    bins = bins[bins$TotalBiomass_kg!=0,]
    bins[,c("FunctionalGroupIndex","LowerBodyMassBin","UpperBodyMassBin")] = NULL
    names(bins)[1:3] = c("FunctionalGroupIndex","LowerBodyMassBin","UpperBodyMassBin")
    
    guilds = as.vector(madingley_data$cohort_def$DEFINITION_Nutrition.source)
    guilds = guilds[bins$FunctionalGroupIndex+1]
    bins$guilds = guilds
    thermo = as.vector(madingley_data$cohort_def$DEFINITION_Endo.Ectotherm)
    thermo = thermo[bins$FunctionalGroupIndex+1]
    bins$thermo = thermo
    guilds[guilds == "Herbivore"] = 1
    guilds[guilds == "Omnivore"] = 2
    guilds[guilds == "Carnivore"] = 3
    thermo[thermo == "Endotherm"] = 0.5
    thermo[thermo == "Ectotherm"] = 0
    bins$y = as.numeric(guilds) + as.numeric(thermo)
    bins$x = log10(bins$LowerBodyMassBin) + 0.5
    bins = bins[,c("x","y","TotalAbundance","TotalBiomass_kg")]
    
    # process flows
    flows$Prey_Diet = as.vector(flows$Prey_Diet)
    flows$Prey_Thermoregulation = as.vector(flows$Prey_Thermoregulation)
    flows[flows$Prey_Thermoregulation == "Ecto",]$Prey_Thermoregulation = 0
    flows[flows$Prey_Thermoregulation == "Endo",]$Prey_Thermoregulation = 0.5
    flows[flows$Prey_Diet == "H",]$Prey_Diet = 1
    flows[flows$Prey_Diet == "O",]$Prey_Diet = 2
    flows[flows$Prey_Diet == "C",]$Prey_Diet = 3
    flows$Prey_Diet = as.numeric(flows$Prey_Diet)
    flows$Prey_Thermoregulation = as.numeric(flows$Prey_Thermoregulation)
    flows$y_prey = flows$Prey_Diet + flows$Prey_Thermoregulation
    
    flows$Prey_UpperBodyMassBin = flows$Prey_LowerBodyMassBin*10
    flows$LOG10Prey_LowerBodyMassBin = log10(flows$Prey_LowerBodyMassBin)
    flows$LOG10Prey_UpperBodyMassBin = log10(flows$Prey_UpperBodyMassBin)
    flows$x_prey = flows$LOG10Prey_LowerBodyMassBin + 0.5
    
    bins_size = c(1 %o% 10^(-10:10))
    bin_alloc = rep(NA,length(flows$Pred_LowerBodyMassBin))
    for(row in 1:nrow(flows)) bin_alloc[row] = which(flows$Pred_AdlBM[row]<bins_size)[1]
    flows$Pred_LowerBodyMassBin = bins_size[bin_alloc-1]
    flows$Pred_UpperBodyMassBin = bins_size[bin_alloc]
    flows$LOG10Pred_LowerBodyMassBin = log10(flows$Pred_LowerBodyMassBin)
    flows$LOG10Pred_UpperBodyMassBin = log10(flows$Pred_UpperBodyMassBin)
    flows$x_pred = flows$LOG10Pred_LowerBodyMassBin + 0.5
    
    guilds = as.vector(madingley_data$cohort_def$DEFINITION_Nutrition.source)
    guilds = guilds[flows$Pred_FunctionalGroupIndex+1]
    thermo = as.vector(madingley_data$cohort_def$DEFINITION_Endo.Ectotherm)
    thermo = thermo[flows$Pred_FunctionalGroupIndex+1]
    guilds[guilds == "Omnivore"] = 2
    guilds[guilds == "Carnivore"] = 3
    thermo[thermo == "Endotherm"] = 0.5
    thermo[thermo == "Ectotherm"] = 0
    flows$y_pred = as.numeric(guilds) + as.numeric(thermo)
    
    flows = flows[,c("x_pred","y_pred","x_prey","y_prey","Prey_BiomassConsumed")]
    flows = aggregate(flows,by=list(flows[,"x_pred"],flows[,"y_pred"],flows[,"x_prey"],flows[,"y_prey"]),FUN=sum)
    flows = cbind(flows[,c(1:4)],flows$Prey_BiomassConsumed)
    names(flows) = c("x_pred","y_pred","x_prey","y_prey","Prey_BiomassConsumed")
    
    # put in list
    flows_l[[timestep]] = flows
    bins_l[[timestep]] = bins
    
  }
  
  flows = do.call(rbind,flows_l)
  bins = do.call(rbind,bins_l)
  
  # average over years
  flows = aggregate(flows,by=list(flows[,"x_pred"],flows[,"y_pred"],flows[,"x_prey"],flows[,"y_prey"]),FUN=mean)
  flows = flows[,grep("Group",names(flows),invert = T)]
  bins = aggregate(bins,by=list(bins[,"x"],bins[,"y"]),FUN=mean,drop=T)
  bins = bins[,grep("Group",names(bins),invert = T)]
  bins$x = bins$x + 3
  bins$points_cex = bins$TotalBiomass_kg # /max(bins$TotalBiomass_kg)
  
  # filter
  if(max_flows<8){
    flows$PredID = paste0(flows$x_pred,"_",flows$y_pred)
    filder_max_con = max_flows
    filer_ids = names(table(flows$PredID))[table(flows$PredID)>filder_max_con]
    for(i in 1:length(filer_ids)){
      #print(filer_ids[i])
      idx = which(flows$PredID==filer_ids[i])
      nidx = length(idx)
      idx2 = idx[order(flows[idx,]$Prey_BiomassConsumed)[(nidx-filder_max_con):nidx]]
      
      if(i==1) {
        new_flows =  flows[idx2,]
      }else{
        new_flows = rbind(new_flows,flows[idx2,])
      }
    }
    idx = which(flows$PredID%in%names(table(flows$PredID))[table(flows$PredID)<=filder_max_con])
    #cat(idx)
    flows = rbind(new_flows,flows[idx,])
  }
  
  norm_min_range = 1
  norm_max_range = 15
  flows$lwd_norm = (flows$Prey_BiomassConsumed-min(flows$Prey_BiomassConsumed))/
    (max(flows$Prey_BiomassConsumed)-min(flows$Prey_BiomassConsumed)) * (norm_max_range-norm_min_range) + norm_min_range
  plot(c(-1.7,8),c(0.7,4),col="white",xaxs="i", yaxs="i",axes=F,xlab="",ylab="",...)
  arrowcolors = rev(paste0("grey",44:81))
  norm_min_range = 1
  norm_max_range = length(arrowcolors)
  flows$lwd_norm_color = round((flows$Prey_BiomassConsumed-min(flows$Prey_BiomassConsumed))/
                                 (max(flows$Prey_BiomassConsumed)-min(flows$Prey_BiomassConsumed)) * (norm_max_range-norm_min_range) + norm_min_range )
  flows$lwd_norm_color = arrowcolors[flows$lwd_norm_color]
  
  x_min_val = -0.5
  for(i in 1:nrow(flows)){
    if(flows$x_prey[i]>x_min_val&flows$x_pred[i]>x_min_val){
      try(
        # diagram::curvedarrow(
        #   c(flows$x_prey[i],flows$y_prey[i]),
        #   c(flows$x_pred[i],flows$y_pred[i]),
        #   lwd = flows$lwd_norm[i],
        #   lty = 1,
        #   lcol = flows$lwd_norm_color[i],
        #   arr.width = 1e-20,
        #   arr.pos = 1,
        #   curve = 0.1,
        #   dr = 0.0001,
        #   arr.col="blue",
        #   arr.lwd=1e-20,
        #   endhead = FALSE,
        #   segment = c(0, 1))
        add_food_web_connection(
          c(flows$x_prey[i],flows$y_prey[i]),
          c(flows$x_pred[i],flows$y_pred[i]),
          lwd = flows$lwd_norm[i],
          lty = 1,
          lcol = flows$lwd_norm_color[i],
          arr.width = 1e-20,
          arr.pos = 1,
          curve = 0.1,
          dr = 0.0001,
          arr.col="blue",
          arr.lwd=1e-20,
          endhead = FALSE,
          segment = c(0, 1))
        ,silent = TRUE)
    }
  }
  
  # quant = quantile(bins$points_cex,0.8)
  # bins$points_cex[bins$points_cex>quant]=quant
  # norm_min_range = 2.0
  # norm_max_range = 8.0
  # bins$points_cex_norm = (bins$points_cex-min(bins$points_cex))/
  #   (max(bins$points_cex)-min(bins$points_cex)) * (norm_max_range-norm_min_range) + norm_min_range
  
  bins$points_cex_norm = round(log10(bins$points_cex)*100)/100
  bins$points_cex_norm = ifelse(bins$points_cex_norm>max_log_scale,max_log_scale,bins$points_cex_norm)
  bins$points_cex_norm = ifelse(bins$points_cex_norm<min_log_scale,min_log_scale,bins$points_cex_norm)
  
  org_vals = seq(min_log_scale,max_log_scale,0.01)
  norm_vals = seq(0,10,10/length(org_vals))
  get = match(round(bins$points_cex_norm, digits = 3),round(org_vals,digits = 3))
  bins$points_cex_norm_og = bins$points_cex_norm
  bins$points_cex_norm = norm_vals[get]
  
  p_colors = c("#00B358","#FF1300","#104BA9")
  
  for(i in 1:nrow(bins)){
    if(bins$x[i]>(-0.5)){
      if(color){
        if(bins$y[i]<2){
          points(x=bins$x[i],y=bins$y[i],cex=bins$points_cex_norm[i],
                 col=rgb(0,0,0,0.001),bg=rgb(0,179/255,88/255,0.5),type="p",pch=21) #
        }else if(bins$y[i]<3){
          points(x=bins$x[i],y=bins$y[i],cex=bins$points_cex_norm[i],
                 col=rgb(0,0,0,0.001),bg=rgb(16/255,75/255,169/255,0.5),type="p",pch=21) #
        }else{
          points(x=bins$x[i],y=bins$y[i],cex=bins$points_cex_norm[i],
                 col=rgb(0,0,0,0.001),bg=rgb(255/255,19/255,0,0.5),type="p",pch=21) #
        }
      }else{
        points(x=bins$x[i],y=bins$y[i],cex=bins$points_cex_norm[i],
               col=rgb(0,0,0,0.001),bg=rgb(0,0,0,0.5),type="p",pch=21) #
      }
      
    }
  }
  
  xlabs = c(expression(paste("10"^"-3"*"-10"^"-2")),
            expression(paste("10"^"-2"*"-10"^"-1")),
            expression(paste("10"^"-1"*"-10"^"0")),
            expression(paste("10"^"0"*"-10"^"1")),
            expression(paste("10"^"1"*"-10"^"2")),
            expression(paste("10"^"2"*"-10"^"3")),
            expression(paste("10"^"3"*"-10"^"4")))
  
  #plot(-3:3+3,1:7) # used for testing
  text(cex=0.9, x=c(-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5)+3, y=0.45, xlabs, xpd=TRUE, srt=45)
  axis(1, at=c(-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5)+3,padj=0.5,cex.axis=0.9,labels = F)
  
  mtext("Body mass bin [kg]",side = 1,at = 3.5,padj = 5.5)
  
  axis(2,at=c(1.0,1.5,2.0,2.5,3.0,3.5),labels=rep(" ",6),pos=-0.1)
  
  ycat_font_size = 1
  x_placement = -1
  text(x_placement,1.0,"Ectotherm\nherbivore",cex=ycat_font_size, xpd=T)
  text(x_placement,1.5,"Endotherm\nherbivore",cex=ycat_font_size, xpd=T)
  text(x_placement,2.0,"Ectotherm\nomnivore",cex=ycat_font_size, xpd=T)
  text(x_placement,2.5,"Endotherm\nomnivore",cex=ycat_font_size, xpd=T)
  text(x_placement,3.0,"Ectotherm\ncarnivore",cex=ycat_font_size, xpd=T)
  text(x_placement,3.5,"Endotherm\ncarnivore",cex=ycat_font_size, xpd=T)
  
  #return(bins)
  
}

CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)

scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}
#https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/dotbar.html

get_binned_fw_data_abs = function(madingley_data, years_end = 5){
  
  # check if out_dir was specified manually within madingley_run()
  if(!is.null(madingley_data$out_path)){ # out_dir specified manually
    tdo = madingley_data$out_path
    # remove slashes from out_dir
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='/')  tdo=substr(tdo,1,nchar(tdo)-1)
    if(substr(tdo,(nchar(tdo)+1)-1,nchar(tdo))=='\\') tdo=substr(tdo,1,nchar(tdo)-1)
    if(dir.exists(paste0(tdo,madingley_data$out_dir_name))) {
      out_dir = tdo
      cat(paste0("loading inputs from: ",out_dir,madingley_data$out_dir_name))
    }
  }else{ # use default output dir
    out_dir = tempdir()
    cat(paste0("loading inputs from: ",out_dir,madingley_data$out_dir_name))
  }
  
  
  # check if dir exists
  if(!dir.exists(paste0(out_dir,madingley_data$out_dir_name))){ # not exist
    stop("Unable to find output folder")
  }
  
  # paths
  # needed madingley_data!!
  if(length(list.files(paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)))==0){
    base_path = paste0(gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)
  }else{
    base_path = paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),madingley_data$out_dir_name)
  }
  
  # make file paths
  cohort_path = paste0(base_path,'cohort_properties')
  bins_files = list.files(cohort_path,pattern="UserDefinedBinnedCohortStatistics",full.names = T)
  
  # check if required files were exported
  if(!length(bins_files)>1){ # not exist
    stop("Required files were not exported during model run")
  }
  
  # init loop
  start_files = length(bins_files) - years_end*12
  end_files = length(bins_files)
  iterator = start_files:end_files
  bins_l = list()
  
  # loop over time steps
  for(timestep in iterator){
    
    # list files and load the last 120 csvs
    bins_files = list.files(cohort_path,pattern="UserDefinedBinnedCohortStatistics",full.names = T)
    bins = read.csv(bins_files[timestep])
    
    # process bins
    bins = aggregate(bins,by=list(bins$FunctionalGroupIndex,bins$LowerBodyMassBin,bins$UpperBodyMassBin),FUN=sum)
    bins = bins[bins$TotalBiomass_kg!=0,]
    bins[,c("FunctionalGroupIndex","LowerBodyMassBin","UpperBodyMassBin")] = NULL
    names(bins)[1:3] = c("FunctionalGroupIndex","LowerBodyMassBin","UpperBodyMassBin")
    
    guilds = as.vector(madingley_data$cohort_def$DEFINITION_Nutrition.source)
    guilds = guilds[bins$FunctionalGroupIndex+1]
    bins$guilds = guilds
    thermo = as.vector(madingley_data$cohort_def$DEFINITION_Endo.Ectotherm)
    thermo = thermo[bins$FunctionalGroupIndex+1]
    bins$thermo = thermo
    guilds[guilds == "Herbivore"] = 1
    guilds[guilds == "Omnivore"] = 2
    guilds[guilds == "Carnivore"] = 3
    thermo[thermo == "Endotherm"] = 0.5
    thermo[thermo == "Ectotherm"] = 0
    bins$y = as.numeric(guilds) + as.numeric(thermo)
    bins$x = bins$LowerBodyMassBin
    
    # put in list
    bins_l[[timestep]] = bins
    
  }
  
  bins = do.call(rbind,bins_l)
  bins = bins[,c("LowerBodyMassBin","UpperBodyMassBin","TotalAbundance","TotalBiomass_kg","guilds","thermo")]
  
  # average over years
  bins = aggregate(.~LowerBodyMassBin+UpperBodyMassBin+guilds+thermo, data=bins,FUN=mean)
  
  bins$ID = paste0(bins$thermo,"_",bins$guilds,"_",log10(bins$LowerBodyMassBin))
  
  return(bins)
  
}

get_model_params = function() {
  Activity_Parameters = c(6.61, 1.6, 1.51, 1.53)
  Activity_names = rep("Activity_Parameters", length(Activity_Parameters))
  Dispersal_Parameters = c(0.0278, 0.48, 50000, 0.0278, 0.48, 
                           0.8)
  Dispersal_names = rep("Dispersal_Parameters", length(Dispersal_Parameters))
  EatingCarnivory_Parameters = c(0.5, 0.7, 0.5, 0.7, 1, 6e-06, 
                                 1, 0.7)
  EatingCarnivory_names = rep("EatingCarnivory_Parameters", 
                              length(EatingCarnivory_Parameters))
  EatingCarnivory_Parameters = c(EatingCarnivory_Parameters, 
                                 0.1)
  EatingCarnivory_names = c(EatingCarnivory_names, "EatingOmnivory_Parameters")
  EatingHerbivory_Parameters = c(0.7, 0.7, 0.7, 0.7, 1, 1e-11, 
                                 1, 2.1, 1)
  EatingHerbivory_names = c(rep("EatingHerbivory_Parameters", 
                                length(EatingHerbivory_Parameters)))
  MetabolismEctotherm_Parameters = c(0.88, 1.48984e+11, 0.69, 
                                     8.617e-05, 41918272883, 0.69, 0.036697248)
  MetabolismEctotherm_names = rep("MetabolismEctotherm_Parameters", 
                                  length(MetabolismEctotherm_Parameters))
  MetabolismEndotherm_Parameters = c(0.7, 908090839730, 0.69, 
                                     8.617e-05, 0.0366972, 37)
  MetabolismEndotherm_names = rep("MetabolismEndotherm_Parameters", 
                                  length(MetabolismEndotherm_Parameters))
  MetabolismHeterotroph_Parameters = c(0.71, 0.69, 8.617e-05)
  MetabolismHeterotroph_names = rep("MetabolismHeterotroph_Parameters", 
                                    length(MetabolismHeterotroph_Parameters))
  Mortality_Parameters = c(0.001, 0.003, 0.6, 0.05, 1)
  Mortality_names = rep("Mortality_Parameters", length(Mortality_Parameters))
  Reproduction_Parameters = c(1.5, 0.25, 0.05, 0.5)
  Reproduction_names = rep("Reproduction_Parameters", length(Reproduction_Parameters))
  VegetationModel_Parameters = c(0.961644704, 0.237468183, 
                                 0.100597089, 0.001184101, 7.154615416, 1.270782192, -1.828591558, 
                                 0.844864063, 0.040273936, 1.013070062, 0.020575964, -1.195235464, 
                                 0.04309283, -1.478393163, 0.139462774, -4.395910091, 
                                 0.362742634, 0.388125108, 19.98393943, 1.148698636, 8.419032427, 
                                 0.01, 24, 0.01, 24, 0.01, 12, 1, 0.001, 2, 2.26032940698105e-06, 
                                 0.476, 0.213)
  VegetationModel_names = rep("VegetationModel_Parameters", 
                              length(VegetationModel_Parameters))
  All_values = c(Activity_Parameters, Dispersal_Parameters, 
                 EatingCarnivory_Parameters, EatingHerbivory_Parameters, 
                 MetabolismEctotherm_Parameters, MetabolismEndotherm_Parameters, 
                 MetabolismHeterotroph_Parameters, Mortality_Parameters, 
                 Reproduction_Parameters, VegetationModel_Parameters)
  ALL_params = c(Activity_names, Dispersal_names, EatingCarnivory_names, 
                 EatingHerbivory_names, MetabolismEctotherm_names, MetabolismEndotherm_names, 
                 MetabolismHeterotroph_names, Mortality_names, Reproduction_names, 
                 VegetationModel_names)
  ModelParameters = data.frame(params = ALL_params, values = All_values)
  ModelParameters$notes = NA
  i = 1
  ModelParameters$notes[i] = "Activity: Terrestrial Warming Tolerance Intercept"
  i = i + 1
  ModelParameters$notes[i] = "Activity: Terrestrial Warming Tolerance Slope"
  i = i + 1
  ModelParameters$notes[i] = "Activity: Terrestrial TSM Intercept"
  i = i + 1
  ModelParameters$notes[i] = "Activity: Terrestrial TSM Slope"
  i = i + 1
  ModelParameters$notes[i] = "Diffusive Dispersal: Speed Body Mass Scalar"
  i = i + 1
  ModelParameters$notes[i] = "Diffusive Dispersal: Speed Body Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Responsive Dispersal: Density Threshold Scaling"
  i = i + 1
  ModelParameters$notes[i] = "Responsive Dispersal: Speed Body Mass Scalar"
  i = i + 1
  ModelParameters$notes[i] = "Responsive Dispersal: Speed Body Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Responsive Dispersal: Starvation Dispersal Body Mass Threshold"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Handling Time Scalar Terrestrial"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Handling Time Exponent Terrestrial"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Handling Time Scalar Marine (not applicable to current version)"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Handling Time Exponent Marine (not applicable to current version)"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Referenc eMass"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Kill Rate Constant"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Kill Rate Constant Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Eating Carnivory: Feeding Preference Standard Deviation"
  i = i + 1
  ModelParameters$notes[i] = "Eating Omnivory: Max Allowed Prey Ratio Omnivores"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Handling Time Scalar Terrestrial"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Handling Time Scalar Marine (not applicable to current version)"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Handling Time Exponent Terrestrial"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Handling Time Exponent Marine (not applicable to current version)"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Reference Mass"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Herbivory Rate Constant"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Herbivory Rate Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Attack Rate Exponent Terrestrial"
  i = i + 1
  ModelParameters$notes[i] = "Eating Herbivory: Fraction Edible Stock Mass"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Metabolism Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Normalization Constant"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Activation Energy"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Boltzmann Constant"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Normalization Constant BMR"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Basal Metabolism Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Ectotherm: Energy Scalar"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Metabolism Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Normalization Constant"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Activation Energy"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Boltzmann Constant"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Energy Scalar"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Endotherm: Endotherm Body Temperature"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Heterotroph: Metabolism Mass Exponent"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Heterotroph: Activation Energy"
  i = i + 1
  ModelParameters$notes[i] = "Metabolism Heterotroph: Boltzmann Constant"
  i = i + 1
  ModelParameters$notes[i] = "Mortality Background: Mortailty Rate"
  i = i + 1
  ModelParameters$notes[i] = "Mortality Senescence: Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Mortality Starvation: Logistic Inflection Point"
  i = i + 1
  ModelParameters$notes[i] = "Mortality Starvation: Logistic Scaling Parameter"
  i = i + 1
  ModelParameters$notes[i] = "Mortality Starvation: Maximum Starvation Rate"
  i = i + 1
  ModelParameters$notes[i] = "Reproduction: Mass Ratio Threshold"
  i = i + 1
  ModelParameters$notes[i] = "Reproduction: Mass Evolution Probability Threshold"
  i = i + 1
  ModelParameters$notes[i] = "Reproduction: Mass Evolution Standard Deviation"
  i = i + 1
  ModelParameters$notes[i] = "Reproduction: Semelparity Adult Mass Allocation"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Miami NPP, Max NPP"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Miami NPP, T1NPP"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Miami NPP, T2NPP"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Miami NPP, PNPP"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Fraction Structure Scalar"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Fraction Evergreen A"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Fraction Evergreen B"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Calculate Fraction Evergreen C"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Evergreen Annual Leaf Mortality Slope "
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Evergreen Annual Leaf Mortality Intercept"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Deciduous Annual Leaf Mortality Slope"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Deciduous Annual Leaf Mortality Intercept"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Fine Root Mortality Rate Slope"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Fine Root Mortality Rate Intercept"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Structural Mortality P2"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Structural Mortality P1"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Leaf Carbon Fixation, MaxFracStruct"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Half Saturation Fire Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Scalar Fire Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: NPP Half Saturation Fire Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: NPP Scalar Fire Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Min Evergreen Annual Leaf Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Max Evergreen Annual Leaf Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Min Deciduous Annual Leaf Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Max Deciduous Annual Leaf Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Min Fine Root Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Max Fine Root Mortality Rate"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Max Structural Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Min Structural Mortality"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Base Scalar Fire"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Min Return Interval"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Mass Carbon Per Mass Leaf Dry Matter"
  i = i + 1
  ModelParameters$notes[i] = "Terrestrial Carbon: Mass Leaf Dry Matter Per Mass Leaf Wet Matter"
  i = i + 1
  return(ModelParameters)
}

removeLargeBodiedCohorts = function( cohorts, n_to_remove, fgs, removalBodyMassThresholds ){

  tmp_cohorts = cohorts
  
  # idx all cohorts to rm per fg
  idx_rm = list()
  for(i in 1:length(fgs)) idx_rm[[i]] = which(tmp_cohorts$FunctionalGroupIndex == fgs[i] & tmp_cohorts$AdultMass>removalBodyMassThresholds[i])
  
  # idx all cohorts to rm
  idx_remaining = c()
  for(i in 1:length(fgs)) idx_remaining = c(idx_remaining,idx_rm[[i]])
  
  # remove largest first 
  idx_rm_sample = list()
  for(i in 1:length(fgs)) idx_rm_sample[[i]] = idx_rm[[i]][order(tmp_cohorts$AdultMass[idx_rm[[i]]],decreasing = TRUE)][1:n_to_remove_per_year]
  
  # idx all sample cohorts to rm
  idx_to_remove_sample = c()
  for(i in 1:length(fgs)) idx_to_remove_sample = c(idx_to_remove_sample,idx_rm_sample[[i]])
  idx_to_remove_sample = idx_to_remove_sample[complete.cases(idx_to_remove_sample)]
  
  # remove cohorts
  tmp_cohorts = tmp_cohorts[-idx_to_remove_sample,] 
  
  # print info
  print(paste("Removing",length(idx_to_remove_sample),"cohorts,",length(idx_remaining)-length(idx_to_remove_sample),"above body mass threshold remaining"))

  # return cohorts and number of cohorts removed
  return(list(tmp_cohorts,length(idx_to_remove_sample)))
}

madingley_init_wrapper = function(ModelSetup, sp_inputs) {
  
  return(MadingleyRewilding::madingley_init( cohort_def=ModelSetup$chrt_def,
                         stock_def=ModelSetup$stck_def, 
                         spatial_inputs=ModelSetup$sp_inputs, 
                         spatial_window=ModelSetup$spatial_window, 
                         max_cohort = ModelSetup$MaxCohorts
                         )
         )

}

madingley_run_wrapper = function(m_data, ModelSetup, sp_inputs, years, exportTimestep, silenced = TRUE) {

  exportTimestep = years + exportTimestep
  
  return(MadingleyRewilding::madingley_run( cohort_def=ModelSetup$chrt_def,
                        stock_def=ModelSetup$stck_def, 
                        spatial_inputs=ModelSetup$sp_inputs, 
                        MinPreyDensityPerHect=ModelSetup$MnPreyDens, 
                        MinPreyDensityMaxBodyMassAff_KG=ModelSetup$MinPreyDensThresh, 
                        madingley_data = m_data,
                        out_dir = ModelSetup$outDIR, 
                        years = years, 
                        max_cohort = ModelSetup$MaxCohorts, 
                        output_timestep = c(0,exportTimestep,exportTimestep,exportTimestep),
                        model_parameters = ModelSetup$mdl_prms, 
                        silenced=silenced, 
                        parallel = ModelSetup$RunParallel,
                        cohort_output_bins = ModelSetup$OutBins
                        )
         )

}



DownloadLoadHalfDegreeInputs = function(wd) {
  
  if(substr(wd,(nchar(wd)+1)-1,nchar(wd))=='/')  wd=substr(wd,1,nchar(wd)-1)
  if(substr(wd,(nchar(wd)+1)-1,nchar(wd))=='\\') wd=substr(wd,1,nchar(wd)-1)
  
  zip_path = paste0(wd,"/0.5degree.zip")
  rasters_path = paste0(wd,"/MadingleyR_0.5degree_inputs-master")
  
  if(!file.exists(paste0(wd,"/0.5degree.zip"))){
    cat('Downloading zip from github repository: ')
    download.file(url = "https://github.com/SHoeks/MadingleyR_0.5degree_inputs/archive/master.zip", destfile = zip_path); 
  }else{
    cat('Zip already downloaded \n')
  }
  if(!file.exists(paste0(wd,"/MadingleyR_0.5degree_inputs-master"))){
    cat('Extracting zip \n')
    setwd(wd)
    unzip('0.5degree.zip')
  }else{
    cat('Zip already extracted \n')
  }
  
  if (!"rgdal" %in% installed.packages()[, "Package"]) {
    stop("Package 'rgdal' not installed")
  }
  else {
    require(rgdal)
  }
  if (!"raster" %in% installed.packages()[, "Package"]) {
    stop("Package 'raster' not installed")
  }
  else {
    require(raster)
  }
  input = list()
  
  spatial_path = paste0(rasters_path)
  file_names = list.files(spatial_path, full.names = T, recursive = T)
  list_names = gsub("\\..*", "", list.files(spatial_path, full.names = F, recursive = T))
  FILES_sp = c("realm_classification", "land_mask", "hanpp", 
               "available_water_capacity") #, "Ecto_max", "Endo_C_max", "Endo_H_max", "Endo_O_max")
  FILES_sp_temp = c("terrestrial_net_primary_productivity", 
                    "near-surface_temperature", "precipitation", "ground_frost_frequency", 
                    "diurnal_temperature_range")
  for (i in FILES_sp) {
    if (length(grep(i, file_names, value = T)) != 1) {
      stop("Could not find raster: ", i, ".tif \n")
    }
  }
  for (i in FILES_sp_temp) {
    if (length(grep(i, file_names, value = T)) != 12) {
      stop("Could not find raster all 12 monthly rasters containing data on: ", 
           i, "\n")
    }
  }
  cat("Reading default input rasters from: ", spatial_path)
  for (i in FILES_sp) {
    file_name = grep(i, file_names, value = T)
    cat(".")
    input[[i]] = raster(file_name)
  }
  for (i in FILES_sp_temp) {
    file_name = grep(i, file_names, value = T)
    if (length(grep("_1.tif", file_name, value = T)) == 
        0) {
      file_name_sort = file_name
    }
    else {
      if (substr(spatial_path, nchar(spatial_path), 
                 nchar(spatial_path)) == "/") {
        file_name_sort = paste0(spatial_path, i, "_", 
                                1:12)
      }
      else {
        file_name_sort = paste0(spatial_path, "/", 
                                i, "_", 1:12, ".tif")
      }
    }
    if (length(file_name_sort) == 12) {
      input[[i]] = brick(lapply(file_name_sort, raster))
    }
    cat(".")
  }
  cat("\n")
  
  # no max body mass raster available yet, setting single value
  input$Ecto_max = input$Endo_C_max = input$Endo_H_max = input$Endo_O_max = input$land_mask
  input$Ecto_max[] = 50000
  input$Endo_C_max[] = 200000
  input$Endo_H_max[] = 1000000
  input$Endo_O_max[] = 500000
  
  return(input)
  
}

SetSingleValueForAllRasters = function(sp_inputs_crop,select_location){
  
  sp_input_select_values = list()
  for(i in 1:length(sp_inputs)) {
    
    # get value at select_location
    sp_input_select_values[[i]] = extract(sp_inputs_crop[[i]],select_location)
    
    # if brick
    if(class(sp_inputs_crop[[i]])=="RasterBrick"){
      for(j in 1:12){
        sp_inputs_crop[[i]][[j]][] = sp_input_select_values[[i]][j]
      }
    }else{
      sp_inputs_crop[[i]][] = sp_input_select_values[[i]][1]
    }
  }
  return(sp_inputs_crop)
}

get_cohorts_to_remove_and_reintroduce = function(cohorts, removalBodyMassThresholds) {
  
  H_Cohorts_to_remove<-cohorts[cohorts$FunctionalGroupIndex==0 & 
                                 cohorts$AdultMass>removalBodyMassThresholds["BMafterRemoval_H"],]
  C_Cohorts_to_remove<-cohorts[cohorts$FunctionalGroupIndex==1 & 
                                 cohorts$AdultMass>removalBodyMassThresholds["BMafterRemoval_C"],]
  O_Cohorts_to_remove<-cohorts[cohorts$FunctionalGroupIndex==2 & 
                                 cohorts$AdultMass>removalBodyMassThresholds["BMafterRemoval_O"],]
  
  Cohorts_to_remove<-rbind(H_Cohorts_to_remove, C_Cohorts_to_remove, O_Cohorts_to_remove)

  return(Cohorts_to_remove)
}

prepare_cohorts_to_reintroduce = function(Cohorts_to_reintroduce){

  #reduce n of cohorts to reintroduce by aggregating by adult body mass, gridcell and functional group
  HeaderOrder<-names(Cohorts_to_reintroduce)
  Cohorts_to_reintroduce$AdultMass2<-round(Cohorts_to_reintroduce$AdultMass/1000)*1000
  Cohorts_to_reintroduce<-aggregate(. ~ GridcellIndex + FunctionalGroupIndex + AdultMass2, FUN=mean, data=Cohorts_to_reintroduce)
  Cohorts_to_reintroduce<-Cohorts_to_reintroduce[,HeaderOrder]
  Cohorts_to_reintroduce$AdultMass2 = NULL
  
  # change Cohorts_to_reintroduce properties
  Cohorts_to_reintroduce$IndividualReproductivePotentialMass = 0
  Cohorts_to_reintroduce$MaturityTimeStep = 0
  Cohorts_to_reintroduce$IsAdult = 0
  Cohorts_to_reintroduce$AgeMonths = 0 
  Cohorts_to_reintroduce$TimeStepsJuviline = 0
  Cohorts_to_reintroduce$TimeStepsAdult = 0
  Cohorts_to_reintroduce$IndividualBodyMass = (Cohorts_to_reintroduce$JuvenileMass + Cohorts_to_reintroduce$AdultMass) / 2
  Cohorts_to_reintroduce$CohortAbundance = nAnimalsToReintroduce
  
  # split and sort by adult bm
  H_Cohorts_to_reintro = subset(Cohorts_to_reintroduce, FunctionalGroupIndex==0)
  C_Cohorts_to_reintro = subset(Cohorts_to_reintroduce, FunctionalGroupIndex==1)
  O_Cohorts_to_reintro = subset(Cohorts_to_reintroduce, FunctionalGroupIndex==2)
  H_Cohorts_to_reintro = H_Cohorts_to_reintro[order(H_Cohorts_to_reintro$GridcellIndex, H_Cohorts_to_reintro$AdultMass, decreasing=FALSE),]
  C_Cohorts_to_reintro = C_Cohorts_to_reintro[order(C_Cohorts_to_reintro$GridcellIndex, C_Cohorts_to_reintro$AdultMass, decreasing=FALSE),]
  O_Cohorts_to_reintro = O_Cohorts_to_reintro[order(O_Cohorts_to_reintro$GridcellIndex, O_Cohorts_to_reintro$AdultMass, decreasing=FALSE),]
  
  return(list(H_Cohorts_to_reintro,C_Cohorts_to_reintro,O_Cohorts_to_reintro))
}


get_cohorts_to_reintroduce = function(A_Cohorts_to_reintro,fg_string="omnivore"){

  if(nrow(A_Cohorts_to_reintro)>0){
    A_select = !duplicated(A_Cohorts_to_reintro$GridcellIndex) # select fist (smallest) bm cohort from each cell
    A_reintro_now = A_Cohorts_to_reintro[A_select,] # put selected in own data.frame
    A_Cohorts_to_reintro = A_Cohorts_to_reintro[!A_select,] # remove selected from cohorts to reintroduce
    #A_reintro_now$BirthTimeStep = round(mean(reintro_m_data[[tracker]]$cohorts$BirthTimeStep)) # update cohort properties
    print(paste("reintroducing:",nrow(A_reintro_now),fg_string,"cohorts, remaining:",nrow(A_Cohorts_to_reintro)))
  }else{
    A_reintro_now = A_Cohorts_to_reintro
    print(paste("no more",fg_string,"cohorts to reintroduce"))
  }

  return(list(Cohorts_to_reintro_later=A_Cohorts_to_reintro,reintro_now=A_reintro_now))
}

remove_cohorts = function(m_data_in, trckr, rmThresholds, gc_ids, n_rm=2) {
  
  TotRemain = 0
  
  for(j in 1:length(gc_ids)){
    
    cell<-gc_ids[j]
    
    for (d in 0:2) { #for each trophic group
      
      Cohorts_toRemove_tmp<-m_data_in[[trckr]]$cohorts[m_data_in[[trckr]]$cohorts$GridcellIndex==cell & 
                                                         m_data_in[[trckr]]$cohorts$FunctionalGroupIndex==d & 
                                                         m_data_in[[trckr]]$cohorts$AdultMass>rmThresholds[paste0('BMafterRemoval_', ifelse(d==0, 'H', ifelse(d==1, 'C', 'O')))],]
      Cohorts_toRemove_tmp<-Cohorts_toRemove_tmp[order(Cohorts_toRemove_tmp$AdultMass, decreasing=TRUE),]#sort by mass
      BM_toRemove<-unique(Cohorts_toRemove_tmp$AdultMass)
      if(length(BM_toRemove)==0){assign(paste0('BM_toRemove_', d), c()); next} #if nothing to remove, skip
      if(length(BM_toRemove)>n_rm) {
        BM_toRemove2<-BM_toRemove[1:n_rm] #remove the first X starting from the largest
      } else {BM_toRemove2<-BM_toRemove} #if less remaining than those removed, all are removed
      ind_rem<-which(m_data_in[[trckr]]$cohorts$AdultMass %in% BM_toRemove2 & m_data_in[[trckr]]$cohorts$GridcellIndex==cell & m_data_in[[trckr]]$cohorts$FunctionalGroupIndex==d)
      if(length(ind_rem)>0){
        m_data_in[[trckr]]$cohorts = m_data_in[[trckr]]$cohorts[-ind_rem,]
      }
      BM_toRemove<-BM_toRemove[!BM_toRemove %in% BM_toRemove2]
      
      if(cell==max(gc_ids)){
        print(paste0('Trophic group ', d, ': ', length(BM_toRemove), ' left'))
      }
      assign(paste0('BM_toRemove_', d), BM_toRemove)
      
    }#closes loop through diets
    TotRemain = TotRemain + length(BM_toRemove_0) + length(BM_toRemove_1) + length(BM_toRemove_2)
    
  } #end loop through cells

  return(list(removal_m_data = m_data_in, TotalRemaining = TotRemain))
  
}



