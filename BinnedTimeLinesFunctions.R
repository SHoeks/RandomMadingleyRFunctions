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
