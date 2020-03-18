################################################################################################
#######     This function aggregates data for seabed habitats mapping. Data that can be ########
#######     accessed by 'Biozone', 'Substate', 'Folk_5', 'EUNIS', 'MSFD_BBHT' for the   ########
#######     Irish coast.                                                                ########
################################################################################################
#' @param xmin lower limit of Longitude
#' @param xmax upper limit of Longitude
#' @param ymin lower limit of Latitude
#' @param ymax upper limit of Latitude
#' @param Biozone TRUE or FALSE (TRUE by default)
#' @param Substate TRUE or FALSE (TRUE by default)
#' @param Folk_5 TRUE or FALSE (TRUE by default)
#' @param EUNIS TRUE or FALSE (TRUE by default)
#' @param MSFD_BBHT TRUE or FALSE (TRUE by default)
#' @param save_table_as by default TRUE save both "csv" and  "xlsx" other options
#' # save_table_as="csv" or save_table_as="xlsx"
#' @param map TRUE or FALSE (FALSE by default) including /excluding leaflet map with layer for each attribute 



seabed_habitats_summary <- function(xmin,xmax,ymin,ymax,Biozone=TRUE,Substrate=TRUE,Folk_5=TRUE,EUNIS=TRUE,MSFD_BBHT=TRUE,save_table_as=TRUE,map=FALSE) {
  ###set results location
data.path <-"results/data/seabed_habitats_summary/"

  
###1. Subset coordinates by required xmin,xmax,ymin and ymax
coord<-coordinates(sb2019)
cut<-cbind(c(xmin,xmax),c(ymin,ymax))
sb2019.subset <- sb2019[coord[,1] > cut[1,1] &
coord[,1] < cut[2,1] &
coord[,2] > cut[1,2] &
coord[,2] < cut[2,2],]
###2. Save subsetted shapefile in the results folder
####Save subsetted shapefile
writeOGR(sb2019.subset, dsn="results/data/shapefile",layer="sb2019_subset", driver="ESRI Shapefile",check_exists=T,overwrite_layer=T)



####3. Summarize polygons area
dd<-sb2019.subset@data
dd$area_km2<-round(area(sb2019.subset)/1000000,0)
if (Biozone==TRUE) {
d<-dd %>%
group_by(Biozone) %>%
dplyr::summarize(area_km2 = sum(area_km2)) %>%
mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))

#save data to the results folder
if(save_table_as==TRUE){
write.csv(d,paste0(data.path,"Biozone.csv"),row.names=F)
write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Biozone",append=TRUE)}
if(save_table_as=="csv"){
write.csv(d,paste0(data.path,"Biozone.csv"),row.names=F)}

if(save_table_as=="xlsx"){
write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Biozone",append=TRUE)
}



#print summary

kable(d)%>%
print()

 

}
if (Substrate==TRUE) {
d<-dd  %>%
group_by(Substrate) %>%
dplyr::summarize(area_km2 = sum(area_km2)) %>%
mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))

#save data to the results folder
if(save_table_as==TRUE){
  write.csv(d,paste0(data.path,"Substrate.csv"),row.names=F)
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Substrate",append=TRUE)}
if(save_table_as=="csv"){
  write.csv(d,paste0(data.path,"Substrate.csv"),row.names=F)}
else if(save_table_as=="xlsx"){
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Substrate",append=TRUE)
}



#summary

kable(d)%>%
print()
}

if (Folk_5 == TRUE) {
d<-dd %>%
group_by(Folk_5) %>%
dplyr::summarize(area_km2 = sum(area_km2)) %>%
mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))

#save data to the results folder
if(save_table_as==TRUE){
  write.csv(d,paste0(data.path,"Folk_5.csv"),row.names=F)
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Folk_5",append=TRUE)}

if(save_table_as=="csv"){
  write.csv(d,paste0(data.path,"Folk_5.csv"),row.names=F)}
else if(save_table_as=="xlsx"){
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "Folk_5",append=TRUE)
}

#summary

kable(d)%>%
print()

}

if (EUNIS == TRUE) {
d<-dd  %>%
group_by(EUNIS) %>%
dplyr::summarize(area_km2 = sum(area_km2)) %>%
mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))

#save data to the results folder
if(save_table_as==TRUE){
  write.csv(d,paste0(data.path,"EUNIS.csv"),row.names=F)
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "EUNIS",append=TRUE)}

if(save_table_as=="csv"){
  write.csv(d,paste0(data.path,"EUNIS.csv"),row.names=F)}
else if(save_table_as=="xlsx"){
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "EUNIS",append=TRUE)
}


#summary

kable(d)%>%
print()
}
if (MSFD_BBHT == TRUE) {
d<-dd  %>%
group_by(MSFD_BBHT) %>%
dplyr::summarize(area_km2 = sum(area_km2)) %>%
mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))

#save data to the results folder
if(save_table_as==TRUE){
  write.csv(d,paste0(data.path,"MSFD_BBHT.csv"),row.names=F)
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "MSFD_BBHT",append=TRUE)}

if(save_table_as=="csv"){
  write.csv(d,paste0(data.path,"MSFD_BBHT.csv"),row.names=F)}
else if(save_table_as=="xlsx"){
  write.xlsx(d, file =paste0(data.path,"seabed_summary.xlsx") ,
             sheetName = "MSFD_BBHT",append=TRUE)
}

#summary

 kable(d)%>%
 print()
}


# ###4. return results
if(map==T){
t<-names(sb2019.subset@data)[1:5]
pal<-list()
for(i in 1:5){
  n <-length(levels(sb2019.subset[[t[i]]]))
  palette <- distinctColorPalette(n)
  pal[[i]] <- colorFactor(palette,sb2019.subset[[t[i]]])}
#map<-
  leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap)%>%
  addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
              layers = "0",
              options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
              group  = "ICES Areas")%>%
addPolygons(data = sb2019.subset,color="black", weight = 1,fillColor=~pal[[1]](sb2019.subset$Biozone),
            fillOpacity = 0.5,popup=paste("<b>Biozone:</b> ",sb2019.subset$Biozone),group = "Biozone")%>%
 addPolygons(data = sb2019.subset,color="black", weight = 1,fillColor=~pal[[2]](sb2019.subset$Substrate),
 fillOpacity = 0.5,popup=paste("<b>Substrate:</b> ",sb2019.subset$Substrate),group = "Substrate")%>%
 addPolygons(data = sb2019.subset,color="black", weight = 1,fillColor=~pal[[3]](sb2019.subset$Folk_5),
fillOpacity = 0.5,popup=paste("<b>Folk_5:</b> ",sb2019.subset$Folk_5),group = "Folk_5")%>%
 addPolygons(data = sb2019.subset,color="black", weight = 1,fillColor=~pal[[4]](sb2019.subset$EUNIS),fillOpacity = 0.5,
 popup=paste("<b>EUNIS:</b> ",sb2019.subset$EUNIS),group  = "EUNIS")%>%
 addPolygons(data = sb2019.subset,color="black", weight = 1,fillColor=~pal[[5]](sb2019.subset$MSFD_BBHT),
 fillOpacity = 0.5,popup=paste("<b>MSFD_BBHT:</b> ",sb2019.subset$MSFD_BBHT),group  = "MSFD_BBHT")%>%
 addLayersControl(baseGroups = c("Biozone","Substrate","Folk_5","EUNIS","MSFD_BBHT"),
 options = layersControlOptions(collapsed = FALSE))%>%return()

}
}







