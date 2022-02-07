
get_rewildEU_FD_data = function(wd,scenario,location,replica){
  
  # set wd
  setwd(wd)
  
  # out 1 = spin up, 
  # out 3 = post-removeal
  # out 4 or 5 = post-reintro (depending on scanario)
  out_idx = c(1,3,4)
  if(scenario=="_ho_first_carn_later_") out_idx = c(1,3,5)
  
  # get replica
  drs = list.files(pattern = paste0(location,scenario))
  drs = paste0(wd,"/",drs)
  dr = drs[replica]
  setwd(dr)
  print(dr)
  
  # select output folder
  outs = list.files(pattern = "madingley_outs")
  
  # initial data
  setwd(paste0(outs[out_idx[1]],"/cohort_properties"))
  files = list.files(pattern = "FullCohortProperties")
  files = files[files!="FullCohortProperties_99999.csv"]
  files = files[order(files, decreasing = TRUE)][1:12]
  data_init = do.call(rbind,lapply(files,read.csv))
  dim(data_init); setwd(dr)
  data_init = data_init[,c("AdultMass","TrophicIndex","FunctionalGroupIndex")]
  data_init$Phase = "init"
  
  # post-removal data
  setwd(paste0(outs[out_idx[2]],"/cohort_properties"))
  files = list.files(pattern = "FullCohortProperties")
  files = files[files!="FullCohortProperties_99999.csv"]
  files = files[order(files, decreasing = TRUE)][1:12]
  data_remv = do.call(rbind,lapply(files,read.csv))
  dim(data_remv); setwd(dr)
  data_remv = data_remv[,c("AdultMass","TrophicIndex","FunctionalGroupIndex")]
  data_remv$Phase = "p_remv"
  
  # post-reintroduction data
  setwd(paste0(outs[out_idx[3]],"/cohort_properties"))
  files = list.files(pattern = "FullCohortProperties")
  files = files[files!="FullCohortProperties_99999.csv"]
  files = files[order(files, decreasing = TRUE)][1:12]
  data_rein = do.call(rbind,lapply(files,read.csv))
  dim(data_rein); setwd(dr)
  data_rein = data_rein[,c("AdultMass","TrophicIndex","FunctionalGroupIndex")]
  data_rein$Phase = "p_rein"
  
  # reset wd
  setwd(wd)
  
  return(rbind(data_init,data_remv,data_rein))
}




