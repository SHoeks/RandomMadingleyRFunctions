get_data = function(wd,n_years_to_av,BMafterRemoval_H,BMafterRemoval_C,BMafterRemoval_O,phases_timelines){

  currentwd = getwd()
  setwd(wd)
  rdata_paths = list.files(pattern = "Timelines.Rdata")
  for(i in rdata_paths) load(i)
  
  # endo herbivores
  herb_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==0,] # keep only herbivores endotherm. fg = 0
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    herb_mean_last_months_kg = c(herb_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # endo carnivores
  carn_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==1,] # keep only carnivores endotherm. fg = 2
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    carn_mean_last_months_kg = c(carn_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # endo omnivores
  omni_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==2,] # keep only omnivores endotherm. fg = 1
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    omni_mean_last_months_kg = c(omni_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # endo all heterotrophs
  all_heterotrophs_mean_last_months_kg = omni_mean_last_months_kg + carn_mean_last_months_kg + herb_mean_last_months_kg
  
  # mega endo herbivores only
  herb_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==0,] # keep only herbivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_H),] # get mega herbs only
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    herb_mega_mean_last_months_kg = c(herb_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # mega endo carnivores only
  carn_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==1,] # keep only carnivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_C),] # get mega carnivores only
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    carn_mega_mean_last_months_kg = c(carn_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # mega endo omnivores only
  omni_mega_mean_last_months_kg = c()
  for(i in 1:length(phases_timelines)){
    tl = rbind.fill(get(phases_timelines[i]))
    months_to_keep = (max(tl$Month)-n_years_to_av*12+1):max(tl$Month)
    tl = tl[tl$Month%in%months_to_keep,] # keep last n_years_to_av only
    tl = tl[tl$FunctionalGroupIndex==2,] # keep only carnivores endotherm. fg = 0
    tl = tl[tl$LowerLog10Bin>=log10(BMafterRemoval_O),] # get mega carnivores only
    tl = aggregate(TotalBiomass_kg~Month,data=tl,FUN=sum) # calc total biomass in each month
    omni_mega_mean_last_months_kg = c(omni_mega_mean_last_months_kg,mean(tl$TotalBiomass_kg)) # insert mean of monthly biomasses
  }
  
  # autotrophs
  load("Phase_5_Stabilisation.Rdata")
  t1<-m_data2$time_line_stocks
  t2<-m_data3$time_line_stocks
  t3<-m_data4$time_line_stocks
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
  
  setwd(currentwd) # reset wd
  
  return(df)
}
