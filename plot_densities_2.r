plot_densities_v2 = function(madingley_data,weighted="biomass",by_traits=c("DEFINITION_Endo.Ectotherm","Nutrition.source"),
                          col=c("#74add1","#a50026","#fdae61","#74add1","#a50026","#fdae61"),plot=TRUE,multipanel=TRUE,...){
  
  # store current par settings
  opar = par(no.readonly = TRUE)
  
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
  
  
  # get base path
  md = madingley_data
  if(length(list.files(paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),md$out_dir_name)))==0){
    base_path = paste0(gsub("//", "/", out_dir, fixed=TRUE),md$out_dir_name)
  }else{
    base_path = paste0('/private',gsub("//", "/", out_dir, fixed=TRUE),md$out_dir_name)
  }
  
  # cons path
  cohort_path = paste0(base_path,'cohort_properties')
  cfiles = list.files(cohort_path,pattern="FullCohortProperties",full.names = T)
  
  c=lapply(cfiles,read.csv)
  c = do.call(rbind,c)
  c = c[,c("FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
  
  selected_fgs = which(md$cohort_def$PROPERTY_Initial.number.of.GridCellCohorts>0)-1
  cols = c()
  for(i in by_traits) cols = c(cols,grep(i,names(md$cohort_def)))
  
  cohort_def = md$cohort_def[selected_fgs+1,cols]
  
  if(length(cohort_def)==length(selected_fgs)){
    LOOP_END = length(cohort_def)
    VEC = TRUE
  }else{
    LOOP_END = nrow(cohort_def)
    VEC = FALSE
  }
  
  main_labs = c()
  for(i in 1:LOOP_END){
    for(k in 1:length(cols)){
      if(k==1){
        if(VEC){
          temp = paste(as.character(cohort_def[i]))
        }else{
          temp = paste(as.character(cohort_def[i,k]))
        }
      }else{
        if(VEC){
          temp = paste(temp,as.character(cohort_def[i]))
        }else{
          temp = paste(temp,as.character(cohort_def[i,k]))
        }
      }
      
    }
    main_labs = c(main_labs,temp)
  }
  
  main_labs = data.frame(selected_fgs,main_labs)
  
  c$main_labs_idx = match(c$FunctionalGroupIndex,main_labs$selected_fgs)
  c$main_labs = main_labs$main_labs[c$main_labs_idx]
  n_main_labs = length(unique(as.vector(main_labs$main_labs)))
  
  if(multipanel){
    if(n_main_labs==6){
      par(mfrow=c(3,2),oma = c(0, 0, 0, 0))
      par(mar = c(5,4,4,2))
    }
    
    if(n_main_labs==3){
      par(mfrow=c(3,1),oma = c(0, 0, 0, 0))
      par(mar = c(5,4,4,2))
    }
  }
  
  # temporary turn of warnings
  owarn = options()$warn # store orinigal value for warnings
  options(warn=-1)
  
  output=list()
  
  fg_counter = 1
  
  for(nn in 1:n_main_labs){
    
    main_lab_sel = main_labs$main_labs[nn]
    main_lab_sel_list = gsub(" ", "", main_lab_sel, fixed = TRUE)
    c_select = c[c$main_labs==main_lab_sel,]
    if(nrow(c_select)==0) next # functional group not present, moving to next loop iteration
    factor = 10
    c_select$mass_class = (round(log10(c_select$IndividualBodyMass)*factor)/factor)-2
    
    if(weighted=="biomass"){
      
      c_select$biomass = c_select$CohortAbundance*c_select$IndividualBodyMass
      
      #out_temp = h_helper1(c_select$mass_class, c_select$biomass, breaks = seq(0,20,0.5),plot = F)
      
      if(plot) h_helper1(x=c_select$mass_class, w=c_select$biomass,ylab="density",
                         breaks = seq(floor(min(c_select$mass_class)),ceiling(max(c_select$mass_class)),0.5),
                         xlab="log10(Cohort Body mass [g])",main=paste(main_lab_sel,"biomass"),plot = T,col=col[nn])
      
      
      
      if(plot==FALSE) {
        temp_out = h_helper1(x=c_select$mass_class, w=c_select$biomass,breaks = seq(floor(min(c_select$mass_class)),ceiling(max(c_select$mass_class)),0.5),plot = F)
        temp_out = data.frame(mids=temp_out$mids,
                              lows=temp_out$breaks[1:length(temp_out$mids)],
                              highs=temp_out$breaks[2:length(temp_out$breaks)],
                              counts=temp_out$counts,density=temp_out$density)
        output[[main_lab_sel_list]] = temp_out
      }
      
    }else{
      
      
      #out_temp = h_helper1(x=c_select$mass_class, w=c_select$CohortAbundance,breaks = seq(0,20,0.5),plot = F)
      
      
      if(plot) h_helper1(x = c_select$mass_class, w= c_select$CohortAbundance,ylab="density",
                         breaks = seq(floor(min(c_select$mass_class)),ceiling(max(c_select$mass_class)),0.5),
                         xlab="log10(Cohort Body mass [g])",main=paste(main_lab_sel,"abundance"),plot = T,col=col[nn])
      
      
      if(plot==FALSE) {
        temp_out = h_helper1(x=c_select$mass_class, w= c_select$CohortAbundance,
                             breaks = seq(floor(min(c_select$mass_class)),ceiling(max(c_select$mass_class)),0.5),
                             plot = F)
        temp_out = data.frame(mids=temp_out$mids,
                              lows=temp_out$breaks[1:length(temp_out$mids)],
                              highs=temp_out$breaks[2:length(temp_out$breaks)],
                              counts=temp_out$counts,density=temp_out$density)
        output[[main_lab_sel_list]] = temp_out
      }
    }
    fg_counter = fg_counter+1
  }
  
  # return to original settings
  par(opar)
  options(warn=owarn)
  
  if(plot==FALSE) return(output)
}

h_helper1<-function(x,w,breaks="Sturges",col=NULL,plot=TRUE,freq=TRUE,ylab=NULL,xaxis=TRUE,ylim=NA,...) {
  
  if(missing(x))
    stop("Usage: weighted.hist(x,...) vector of values x required")
  if(missing(w)) w<-rep(1,length(x))
  breaks<-h_helper2(x,breaks)
  width<-diff(breaks)
  diffx<-diff(range(x))
  equidist<-sum(width-width[1]) < diffx/1000
  nbreaks<-length(breaks)-1
  # make sure that the last break is greater than the maximum value
  lastbreak<-breaks[nbreaks+1]
  breaks[nbreaks+1]<-breaks[nbreaks+1]+diffx/1000
  if(diff(range(breaks)) < diffx)
    warning("Not all values will be included in the histogram")
  counts<-rep(0,nbreaks)
  for(bin in 1:nbreaks)
    counts[bin]<-sum(w[x >= breaks[bin] & x < breaks[bin+1]])
  density<-counts/sum(counts)
  if(freq) {
    if(is.null(ylab)) ylab<-"Frequency"
    heights<-counts
    if(!equidist)
      warning("Areas will not relate to frequencies")
  }
  else {
    if(!equidist) {
      heights<-density*mean(width)/width
      heights<-heights/sum(heights)
    }
    else heights<-density
    if(is.null(ylab)) ylab<-"Density"
  }
  if(plot) {
    if(is.null(col)) col<-par("bg")
    if(is.na(ylim)) ylim<-c(0,1.1*max(heights,na.rm=TRUE))
    mids<-barplot(heights,width=width,col=col,space=0,ylim=ylim,ylab=ylab,...)
    tickpos<-c(mids-width/2,mids[length(mids)]+width[length(width)]/2)
    if(xaxis) axis(1,at=tickpos,labels=signif(c(breaks[1:nbreaks],lastbreak),3))
  }
  else mids<-breaks[-length(breaks)]+width/2
  invisible(list(breaks=breaks,counts=counts,density=density,
                 mids=mids,xname=deparse(substitute(x)),equidist=equidist))
}

h_helper2<-function(x,breaks) {
  # if a break computing function name is passed
  if(is.character(breaks))
    nbreaks<-do.call(paste("nclass",breaks,sep=".",collapse=""),list(x))
  # if breaks is numeric
  if(is.numeric(breaks)) {
    # if just the number of breaks is passed
    if(length(breaks) == 1) {
      nbreaks<-breaks
    }
    # otherwise assume that breaks specifies the breakpoints
    else return(breaks)
  }
  breakinc<-diff(range(x))/nbreaks
  breaks<-c(min(x),rep(breakinc,nbreaks))
  breaks<-cumsum(breaks)
  return(breaks)
}
