


#' Generate ExpoApp_Totals, ExpoApp_min ,and Quality Analysis Report
#'
#' It is the function to generate a 10 seconds and 1 minute simplified ExpoApp data.
#' @param ExpoApp It is the ExpoApp RData object
# @param Build Character variable with the path to the Spatio-Temporal Clustering function. This function reduce the cloud of points around places to a one point per place and time. It also enriches the data with information about OpenStreetMap's green spaces.
# @param EPSG_code Numeric variable with the desired projected coordinate reference system of the study area.
# @param Buffer Numeric variable with the desired minimum radius of the buffer to be used in the clustering algorithm.
#' @param Time.zone Character variable with the time zone of the study area (e.g. "Australia/Melbourne").
# @param Clustering A logical variable (TRUE/FALSE) indicating if applying yes/no the clustering algorithm.
#' @param output_dir The folder where we want to store the 10 seconds and 1 minute simplified ExpoApp datasets and the Quality Analysis Report.
#' @param save_ExpoApp_totals A logical variable (TRUE/FALSE) indicating if we want to save the 10 seconds simplified ExpoApp dataset.
#' @param save_ExpoApp_min A logical variable (TRUE/FALSE) indicating if we want to save the 1 minute simplified ExpoApp dataset.
#' @param save_html A logical variable (TRUE/FALSE) indicating if we want to save the Quality Analysis Report. 
#' @param open_html A logical variable (TRUE/FALSE) indicating if we want to open the Quality Analysis Report in the browser. 
#' @param ... optional arguments of function.

#' @return It returns a data.table object with the time, accelerometry and location information at 10 seconds resolution. 
#' This simplified ExpoApp dataset is still preliminar. So far, it is useful only for the Quality Analysis Report. 
#' 
#' If save_ExpoApp_totals, save_ExpoApp_min and save_html are TRUE a 10 seconds RData, 1 minute RData and quality report html files are
#' saved at output_dir folder.
#'
#' @examples
#' # ExpoApp geolocation information is encrypted to ensure the confidentiality of participants
#' # in case they lose the pheno. Using your password and the below link, you can 
#' # download SensorLab2-1.2.2 tool. It contains a jar file, a decrypt key and example datasets.
#' # Please, download, unzip and save SensorLab2-1.2.2 into your desired path.
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' load(file.path(Lab_folder,"ExpoApp.IDddg.RData"))
#' ls()
#' result <- reduce_expoapp(ExpoApp=expoapp,output_dir=getwd(), Time.zone = "Australia/Melbourne",
#'                          save_ExpoApp_totals = TRUE, save_ExpoApp_min = FALSE, save_html = TRUE, 
#'                          open_html = TRUE)
#' 
#' # If you have imported Expoapp Data using import_expoapp and you are in the same sesion, 
#' you don't need to load the data.
#' sapply(expoapp,head)
#' result <- reduce_expoapp(ExpoApp=expoapp,output_dir=getwd(), Time.zone = "Australia/Melbourne",
#'                          save_ExpoApp_totals = TRUE, save_ExpoApp_min = FALSE, save_html = TRUE, 
#'                          open_html = TRUE)
#' result
#' gps <- result[!is.na(latitude),]
#' gps_sf <- sf::st_as_sf(gps,coords=c("longitude","latitude"),crs=4326)
#' mapview::mapview(gps_sf,zcol="mets",layer.name="METs")

#' @export

reduce_expoapp <- function(ExpoApp = NULL , Time.zone = "Australia/Melbourne",
#                           Build = NULL, EPSG_code = 25832, Buffer = 150, Clustering = FALSE,                           
                           output_dir = getwd(), save_ExpoApp_totals = FALSE, 
                           save_ExpoApp_min = FALSE, save_html = TRUE,
                           open_html = TRUE,...){
  epo <- acc <- date.min <- axis1 <- V <- steps <- mets <- day <- latitude <- Mets <- NULL
  
  id <- ExpoApp$settings$characteristics[ExpoApp$settings$V1=='ID']

  Clustering = FALSE
  
#  if(Clustering==TRUE){
#    gis.expo(Build=Build,Decrypted=a,EPSG_code=EPSG_code ,Buffer=Buffer ,Time.zone=Time.zone)
#    getwd()
#    gps <- mobile.gps(list.files(gsub("decrypted","processed",a),full.names=T))
#  }
    
  gps <- copy(ExpoApp$gps)
  names(gps) <- tolower(names(gps))
  gps[,date := as.POSIXct(trunc((epo/1e4),0)*10,origin="1970-01-01",tz=Time.zone)]
  setkey(gps,date)
  gps <- gps[gps[ , .I[which.min(acc)], by = date]$V1]
  gps_min <- copy(gps)
  gps_min[,date.min:=as.POSIXct(format(date,format="%Y-%m-%d %H:%M"))]
  gps_min <- gps_min[gps_min[ , .I[which.min(acc)], by = date.min]$V1]
  
  acce <- copy(ExpoApp$acce)
  acce <- axes2vectors(x=acce,lista=FALSE,Time.zone=Time.zone)
  acce.date <- data.table(Date=seq(min(acce$Date),max(acce$Date),by="10 secs"))
  acce <- merge(acce.date,acce,by="Date",all.x=T)
  acce[,axis1 := ifelse(V<0.27,0,-48.08 + 211.81*(V^0.95))]
  acce[,steps := 1]
  acti<- pa.acti2(acce)
  acti_min <- acti$raw[ ,list(Mets=mean(mets,na.rm=T)), by = "date.min"]
  
  aux <- merge(acti$raw,gps,by="date",all.x=T)
  aux[,day := factor(substr(date.min,1,10))]
  aux[,hour := factor(substr(date.min,12,13))]
  aux[,time := time2decimal(date.min)]
  
  expoapp.totals <- aux
  if(save_ExpoApp_totals == TRUE){
    save(expoapp.totals,file=file.path(output_dir,paste0("ExpoApp_totals_",id,".RData")))
  }
  
  
  aux_min <- merge(acti_min,gps_min,by="date.min",all.x=T)
  aux_min[,day := factor(substr(date.min,1,10))]
  aux_min[,hour := factor(substr(date.min,12,13))]
  aux_min[,time := time2decimal(date.min)]
  
  expoapp.min <- aux_min
  if(save_ExpoApp_min == TRUE){
    save(expoapp.min,file=file.path(output_dir,paste0("ExpoApp_min_",id,".RData")))
  }
  
  
  aux.sf <- aux_min[!is.na(latitude),]
  aux.sf <- st_as_sf(data.frame(aux.sf),coords=c("longitude","latitude"),crs=4326)
  aux.sf <- aux.sf[,c("date","day","acc","pro","sat","Mets")]
  
  times <- rbindlist(list(
    Recorded = table2frame(round(table(aux_min$day[!is.na(aux_min$Mets)],useNA="ifany")/60,1)),
    Wearing = round(acti$control[1,1:(ncol(acti$control)-1)],1),
    ALL_Location = table2frame(round(table(aux_min$day[!is.na(aux_min$pro)])/60,1)),
    GPS_Location = table2frame(round(table(aux_min$day[aux_min$pro%in%'gps'])/60,1)),
    NET_Location = table2frame(round(table(aux_min$day[aux_min$pro%in%'network'])/60,1))
  ),idcol = "Type", fill=TRUE)
  
  nolocation <- completeness_expoapp(aux_min)
  
  #https://plot.ly/r/custom-buttons/#update-button
  pa_plot <- ggplot(aux_min,aes(time,Mets))+xlim(c(0,24))+geom_path()+facet_wrap(~day)+ theme_bw()
  
  #https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  #https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
  #https://cran.r-project.org/web/packages/ggspatial/vignettes/ggspatial.html
  #gps_plot <- ggplot() + annotation_map_tile(type = "osm",zoom=12) + geom_sf(data=aux.sf, colour = "red", fill = NA, inherit.aes = FALSE)
  
  #https://stackoverflow.com/questions/34331964/mapview-error-in-knitr-r-markdown-document
  gps_plot<- mapview::mapview(aux.sf,layer.name="ExpoApp",zcol = "day", burst= T)
  
  result <- list(
    settings = copy(ExpoApp$settings),
    times = times,
    nolocation = nolocation,
    pa_plot = pa_plot,
    gps_plot = gps_plot,
    notes = copy(ExpoApp$notes))
  
  print_expoapp(result,output_dir = output_dir,save_html = save_html)
  
  return(expoapp.totals)
}