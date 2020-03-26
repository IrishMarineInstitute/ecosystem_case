
##################################################################################################
#######This function aggregates variables from Copernicus Marine Service .########################
##################################################################################################                                                               

##################################################################################################



#' @param month  between January and December
#' @param year values betwen 1992 and 2018
#' @param data_type "physics" or "biogeochemistry"
#' @param parameter Specific to "physics" variables
#' either
      # "thetao"   Temperature
      # "bottomT"  Sea floor potential temperature
      # "so"       Salinity 
      # "zos"      Sea surface height
      # "uo"       Eastward velocity 
      # "vo"       Northward velocity 
      # "mlotst"   Ocean mixed layer thickness defined by density 

#'                 Specific to "biogeochemistry" variables
#' either
      # "chl"	    Mass Concentration of Chlorophyll in Sea Water
      # "phyc" 	  Mole Concentration of Phytoplankton Expressed as Carbon in Sea Water
      # "o2"  	  Mole Concentration of Dissolved Oxygen in Sea Water
      # "no3"	    Mole Concentration of Nitrate in Sea Water
      # "po4"	    Mole Concentration of Phosphate in Sea Water
      # "si"	    Mole Concentration of Silicate in Sea Water
      # "fe"	    Mole Concentration of Iron in Sea Water
      # "nh4"	    Mole Concentration of Ammonium in Sea Water
      # "nppv"	  Net Primary Productivity of Carbon
      # "zeu"	    Euphotic Zone Depth
#' @param summary_table returns summary table TRUE or FALSE (FALSE by default)
#' @param spatial_plot TRUE or FALSE (TRUE by default)returns
#'  spatial plots for selected month, year and parameter
##########################################################################################################
##########################################################################################################







copernicus_ocean_variables<-function(month,year,data_type,parameter,violin_plot=TRUE,spatial_plot=TRUE){
  ###set results location
data.path <-"results/data/copernicus_ocean_variables/"
plot.path <-"results/plots/copernicus_ocean_variables/"
 

  

year<-as.character(year)

#####check if parameter correctly choosen for data_type variable  
if(data_type=="physics" & !parameter %in% c("thetao","bottomT","so","zos","uo","vo","mlotst")){
  stop('parameter not specific to "physics" data_type')
}

if(data_type=="biogeochemistry" & !parameter %in% c("chl","phyc","o2","no3","po4","si","fe","nh4",
                                                    "nppv","zeu")){
  stop('parameter not specific to "biogeochemistry" data_type')
}

#####################Reading in data ##############################
if(data_type=="physics"){
ncpath<-paste0(Data_location,"Template_Ecosystem/data/copernicus_phys_monthly.nc")
full_names<-read.csv(paste0(Data_location,'Template_Ecosystem/data/PhysDesc.csv'))
}

else if(data_type=="biogeochemistry"){
ncpath<-paste0(Data_location,"Template_Ecosystem/data/copernicus_bio_monthly.nc")
full_names<-read.csv(paste0(Data_location,'Template_Ecosystem/data/BioChemDesc.csv'))
}
####################################################################
##############Spatial plot defining color scheme###################
 if(data_type=="physics" & parameter %in% c("thetao","bottomT")){
   coll<-rerddap::colors$temperature
}
 if(data_type=="physics" & parameter %in% c("so")){
   coll<-rerddap::colors$salinity
 }
if(data_type=="physics" & parameter %in% c("uo","vo")){
  coll<-rerddap::colors$velocity
}
if(data_type=="physics" & parameter %in% c("zos","mlotst")){
  coll<-rerddap::colors$density
}

 if(data_type=="biogeochemistry" & parameter %in% c("chl","phyc")){
   coll<-rerddap::colors$chlorophyll
 }
 if(data_type=="biogeochemistry" & parameter %in% c("o2")){
   coll<-rerddap::colors$oxygen}
 
if(data_type=="biogeochemistry" & parameter %in% c("no3","po4","si","fe","nh4")){
  coll<-rerddap::colors$vorticity
  
}
if(data_type=="biogeochemistry" & parameter %in% c("nppv","zeu")){
  coll<-rerddap::colors$vorticity
  
}
###################### layers location matrix in rasterbrick##################
##############################################################################
m<-c("January","February","March","April","May","June","July",
         "August","September","October","November","December")
y<-1992:2018

####location matrix in rasterbrick month by year####
lev<-matrix(nrow = length(m),ncol=length(y))##12 month by 27 years
for(i in 2:length(m)){
  lev[1,]<-seq(1,length(m)*length(y)-(length(m)-1),by=length(m))
  lev[i,]<-seq(1+(i-1),length(m)*length(y)-(length(m)-1)+(i-1),by=length(m))}
rownames(lev)<-m
colnames(lev)<-y

######Setting names for raster layers###
h<-list()
for(i in 1:length(y)){
 h[[i]]<-paste0(substring(m,1,3),y[i])}
h_names<-unlist(h)


#######################read in raster brick#################################################

raster <- brick(ncpath,varname=parameter)
names(raster)<-h_names
#######################crop raster to selected extent#######################################
e<-as(extent(xmin,xmax,ymin,ymax),'SpatialPolygons')
crs(e)<-crs(raster)
slice<-crop(raster,e)

########################combine summurixed data#########
all_data<-list()
data<-list()
for(m in 1:length(month)){
for(i in 1:length(month)){
  f<-list()
  for(j in 1:length(year)){
  d <-subset(slice,lev[month[i],year[j]])
  mean<-as.data.frame(cellStats(d, stat='mean', na.rm=TRUE))
  rownames(mean)<-1:dim(mean)[1]
  names(mean)<-"mean"
  sd<-as.data.frame(cellStats(d, stat='sd', na.rm=TRUE))
  rownames(sd)<-1:dim(sd)[1]
  names(sd)<-"sd"
  d1<-cbind(mean,sd)
  d1$sd_low<-d1$mean-d1$sd
  d1$sd_high<-d1$mean+d1$sd
  d1$month<-month[i]
  d1$year<-year[j]
  d1$parameter<-parameter
  f[[j]]<-d1
 }
  data[[i]]<-f
}
all_data[[m]]<-ldply(data[[m]])}
combined<-ldply(all_data)
combined$month <- factor(combined$month, levels = month)
##################save to result folder############################
write.csv(combined,paste0(data.path,parameter,".csv"),row.names=F)

##################plot by selected parameter################
d<-filter(combined,parameter==parameter)
 temp_plt<-ggplot(data = d, aes(x = year, y = mean)) +
  ylab(paste("Mean",filter(full_names,Name==parameter)[1,2]))+
   xlab("Year") +
   # xlim(xmin, NA) +
   geom_line(aes(group=1)) +
   geom_errorbar(aes(ymin = sd_low, ymax = sd_high), 
                 size = 0.3, width = 0.3) +
   geom_point()+
   facet_wrap( ~month, nrow = 4, scales = "fixed") +
   theme_bw() +
   theme(strip.background = element_blank())
 
 ggsave(paste0(plot.path,parameter,".png"))
 print(temp_plt)
 

 dd<-subset(slice,lev[month,year])
 
###############violin plot################
 
if(violin_plot==TRUE){
  # dd<-subset(slice,lev[month,year])
  p<-bwplot(dd,main=paste(filter(full_names,Name==parameter)[1,2]))
  print(p)
}
 ###############spatial plot################
if(spatial_plot==TRUE){
   # dd<-subset(slice,lev[month,year])
    p1<-levelplot(dd,contour=TRUE,col.regions=coll,main=paste(filter(full_names,Name==parameter)[1,2]))
    print(p1)
}
 
 
 
  png(filename =paste0(plot.path,parameter,"_Spatial.png"))
 print(levelplot(dd,contour=TRUE,col.regions=coll,main=paste(filter(full_names,Name==parameter)[1,2])))
 whatever <-dev.off()
  
  png(filename =paste0(plot.path,parameter,"_violin.png"))
 print(bwplot(dd,main=paste(filter(full_names,Name==parameter)[1,2])))
 whatever <-dev.off()
  
#  ###############summary table################
#  if(summary_table==TRUE){
#    t1 <- tableGrob(combined, theme=ttheme_minimal(), rows=NULL, cols=colnames(combined))
#    
#   grid.arrange(t1, ncol=1)
#   
# }

}


