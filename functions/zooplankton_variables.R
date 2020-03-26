
##################################################################################################
#######This function aggregates variables from an operational zooplankton data service.###########
##################################################################################################                                                               

##################################################################################################




#' @param Year values betwen 1956 and 2013
#' @param season  "Jan_Mar", "Apr_Jun",  "Jul_Sep", "Oct_Dec"
#' @param species "Acartia", "Calanus_finmarchicus, "Calanus_helgolandicus",
#'  "Metridia_lucens","Temora_longicornis", "Large_copepods","Small_copepods"
#'  @param time_bin "1_year" or "10_years"
#'  @param max_rel_error 0, 0.3 or 0.5
#' 

zooplankton_variables<-function(season,Year,species,max_rel_error,time_bin){
  ###set results location
  
  data.path <-"results/data/zooplankton/"
  plot.path <-"results/plots/zooplankton/"
 
  Year<-as.factor(Year)
  sp<-c("Acartia","Calanus_finmarchicus","Calanus_helgolandicus",
        "Metridia_lucens","Temora_longicornis","Large_copepods","Small_copepods"
        )

  x<-list()
  x1<-list()
  x2<-list()
  xx<-list()
  xx1<-list()
  xx2<-list()
  for(i in 1 :length(sp)){
  x[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_one_year.grd")
   x1[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_L1_one_year.grd")
   x2[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_L2_one_year.grd")
   xx[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_ten_years.grd")
   xx1[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_L1_ten_years.grd")
   xx2[[i]]<-paste0(Data_location,"Template_Ecosystem/data/zooplankton/",sp[i],"/Ab_L2_ten_years.grd")
  }
  
  #####################Reading in data ##############################
  if(species=="Acartia" & max_rel_error == 0 & time_bin=="1_year"){
  d<-brick(x[1])
  }
  else if (species=="Acartia" & max_rel_error == 0 & time_bin=="10_year"){
  d<-brick(xx[1])
  }
  else if(species=="Acartia" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[1])
  }
  else if (species=="Acartia" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[1])
  }
  else if(species=="Acartia" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[1])
  }
  else if (species=="Acartia" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[1])
  }
  else if(species=="Calanus_finmarchicus" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[2])
  }
  else if (species=="Calanus_finmarchicus" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[2])
  }
  else if(species=="Calanus_finmarchicus" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[2])
  }
  else if (species=="Calanus_finmarchicus" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[2])
  }
  else if(species=="Calanus_finmarchicus" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[2])
  }
  else if (species=="Calanus_finmarchicus" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[2])
  }
  else if(species=="Calanus_helgolandicus" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[3])
  }
  else if (species=="Calanus_helgolandicus" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[3])
  }
  else if(species=="Calanus_helgolandicus" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[3])
  }
  else if (species=="Calanus_helgolandicus" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[3])
  }
  else if(species=="Calanus_helgolandicus" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[3])
  }
  else if (species=="Calanus_helgolandicus" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[3])
  }
  else if(species=="Metridia_lucens" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[4])
  }
  else if (species=="Metridia_lucens" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[4])
  }
  else if(species=="Metridia_lucens" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[4])
  }
  else if (species=="Metridia_lucens" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[4])
  }
  else if(species=="Metridia_lucens" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[4])
  }
  else if (species=="Metridia_lucens" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[4])
  }
  else if(species=="Temora_longicornis" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[5])
  }
  else if (species=="Temora_longicornis" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[5])
  }
  else if(species=="Temora_longicornis" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[5])
  }
  else if (species=="Temora_longicornis" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[5])
  }
  else if(species=="Temora_longicornis" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[5])
  }
  else if (species=="Temora_longicornis" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[5])
  }
  else if(species=="Large_copepods" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[6])
  }
  else if (species=="Large_copepods" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[6])
  }
  else if(species=="Large_copepods" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[6])
  }
  else if (species=="Large_copepods" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[6])
  }
  else if(species=="Large_copepods" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[6])
  }
  else if (species=="Large_copepods" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[6])
  }
  
  else if(species=="Small_copepods" & max_rel_error == 0 & time_bin=="1_year"){
    d<-brick(x[7])
  }
  else if (species=="Small_copepods" & max_rel_error == 0 & time_bin=="10_year"){
    d<-brick(xx[7])
  }
  else if(species=="Small_copepods" & max_rel_error == 0.3 & time_bin=="1_year"){
    d<-brick(x1[7])
  }
  else if (species=="Small_copepods" & max_rel_error == 0.3 & time_bin=="10_year"){
    d<-brick(xx1[7])
  }
  else if(species=="Small_copepods" & max_rel_error == 0.5 & time_bin=="1_year"){
    d<-brick(x2[7])
  }
  else if (species=="Small_copepods" & max_rel_error == 0.5 & time_bin=="10_year"){
    d<-brick(xx2[7])
  } 

  
  ###################### layers location matrix in rasterbrick##################
  ##############################################################################
  s<-c("Jan_Mar","Apr_Jun","Jul_Sep","Oct_Dec")
  y<-1958:2013
  
  ####location matrix in rasterbrick month by year####
  lev<-matrix(nrow = length(s),ncol=length(y))##4 seasons by 56 years
  for(i in 2:4){
    lev[1,]<-seq(1,224,by=4)
    lev[i,]<-seq(1+(i-1),224+(i-1),by=4)}
  rownames(lev)<-s
  colnames(lev)<-y

#######################crop raster to selected extent#######################################
    e<-as(extent(xmin,xmax,ymin,ymax),'SpatialPolygons')
    crs(e)<-crs(d)
    slice<-crop(d,e)
    ########################combine summurixed data#########
    all_data<-list()
    data<-list()
    for(m in 1:length(season)){
      for(i in 1:length(season)){
        f<-list()
        for(j in 1:length(Year)){
          d <-subset(slice,lev[season[i],Year[j]])
         mean<-as.data.frame(cellStats(d, stat='mean', na.rm=TRUE))
         # mean<-mean(getValues(d),na.rm=T)
          rownames(mean)<-1:dim(mean)[1]
          names(mean)<-"mean"
          sd<-as.data.frame(cellStats(d, stat='sd', na.rm=TRUE))
          rownames(sd)<-1:dim(sd)[1]
          names(sd)<-"sd"
          d1<-cbind(mean,sd)
          d1$sd_low<-d1$mean-d1$sd
          d1$sd_high<-d1$mean+d1$sd
          d1$season<-season[i]
          d1$Year<-Year[j]
          d1$species<-species
          f[[j]]<-d1
        }
        data[[i]]<-f
      }
      all_data[[m]]<-ldply(data[[m]])}
    combined<-ldply(all_data)
    
    ##################save to result folder############################
    write.csv(combined,paste0(data.path,species,".csv"),row.names=F)
    ##################plot by selected species################
  
    temp_plt<-ggplot(data = combined, aes(x = Year, y = mean)) +
      ylab("Average log abundance")+
      xlab("Time") +
      # xlim(xmin, NA) +
      geom_line(aes(group=1)) +
      geom_errorbar(aes(ymin = sd_low, ymax = sd_high), 
                    size = 0.3, width = 0.3) +
      geom_point()+
      facet_wrap( ~season, ncol= 1, scales = "free_y") +
      theme_bw() +
      theme(strip.background = element_blank())
    
    ggsave(paste0(plot.path,species,".png"))
    print(temp_plt)
    
    
   
}