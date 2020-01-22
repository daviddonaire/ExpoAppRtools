#' Untar, Decrypt and Save ExpoApp data.
#' 
#' This function untars ExpoApp data, decrypts ExpoApp geolocation file, and saves the ExpoApp untared and in RData format.
#' @param file Character variable with the path to the tar.gz file of ExpoApp data.
#' @param SensorLab Character variable with the path to the SensorLab folder. SensorLab folder has to contain the jar and the pem files.
#' @param save_RData A logical variable (TRUE/FALSE) indicating if we want to save the RData file of ExpoApp data.
#' @param save_untar A logical variable (TRUE/FALSE) indicating if we want to save the untar file of ExpoApp data.
#' @param ... optional arguments to FUN.
#'
#' @return value
#'
#' @examples
#' # Using your password and the below link, you can download SensorLab2-1.2.2 tool.
#' # It contains a jar file and an example dataset.
#' # Please, unzip and save it into your desired path.
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' targz_file <- file.path(Lab_folder,"ExpoApp.IDddg.tar.gz")
#'
#' expoapp <- import_expoapp(file = targz_file, SensorLab = Lab_folder,
#'                         save_RData = FALSE, save_untar=FALSE)
#' sapply(expoapp,head)
#'
#' gps_sf <- sf::st_as_sf(expoapp$gps,coords=c("LONGITUDE","LATITUDE"),crs=4326)
#' mapview::mapview(gps_sf)
#' # see Expoapp_resum to generate the 10 seconds and 1 minute simplified Expoapp files.
#' @export

import_expoapp <- function(file = NULL, SensorLab = NULL,
                           save_RData= TRUE, save_untar = TRUE,...){
  EPO <- V1 <- NULL
  
  
  if(is.null(file)){
    stop("Error: file is not a Character variable with the path to the tar.gz file of ExpoApp data.")
  }
  
  if(is.null(SensorLab)){
    stop("Error: SensorLab is not a Character variable with the path to the SensorLab folder.")
    
  }
  if(all(c("priv8.pem","SensorLab2-1.2.2-jar-with-dependencies.jar")%in%list.files(SensorLab))==FALSE){
    stop("Error: SensorLab doesn't contain the jar and the pem files.")
  }
  
  inicio <- getwd()
  
  if(save_untar==FALSE){
    td <- tempdir()
  }else{
    td <- sub("/([^/]*)$", "",file)
  }
  
  ## REMOVE EXISTING UNTARED FOLDERS
  unlink(gsub(".tar.gz","",file),recursive=T)
  
  ## UNTAR TAR.GZ
  untar(file,exdir = td)
  
  ## LIST OF PATH TO DATASETS
  dir_expoapp <- untar(file,list=T,exdir = td)
  dir2_expoapp <- list(acce = file.path(td,grep("ACC",grep("csv",dir_expoapp,value=T),value=T)),
                       gps = file.path(td,grep("GPS",grep("csv",dir_expoapp,value=T),value=T)),
                       bar = file.path(td,grep("BAR",grep("csv",dir_expoapp,value=T),value=T)),
                       ui = file.path(td,grep("UI",grep("csv",dir_expoapp,value=T),value=T)),
                       logcat = file.path(td,grep("logcat",dir_expoapp,value=T)))
  
  ## READING ACCELEROMETRY
  if(length(dir2_expoapp$acce)!=0){
    acce <- rbindlist(lapply(dir2_expoapp$acce,fread), use.names = TRUE)
    setkey(acce, EPO)
  }else {acce <- NULL}
  
  ## READING & CLEANNING SPATIAL DATA.
  a <- decrypt_expoapp(dir2_expoapp$gps,SensorLab=SensorLab)
  
  if(length(dir2_expoapp$gps)!=0){
    gps <- read_gps_expoapp(file=list.files(a,full.names = TRUE))
  }else {gps <- NULL}
  
  ## READING BAROMETRIC DATA
  if(length(dir2_expoapp$bar)!=0){
    bar <- read.csv(dir2_expoapp$bar)
  }else {bar <- NULL}
  
  ## READING UI DATA
  if(length(dir2_expoapp$ui)!=0){
    ui <- read.csv(dir2_expoapp$ui)
  }else {ui<- NULL}
  
  ## READING SETTINGS
  settings <- fread(dir2_expoapp$logcat,fill=T,nrows=15,sep=":")
  settings$characteristics <- gsub(" NA  ","",apply(settings[,2:ncol(settings)],1,paste,collapse=' '))
  settings <- settings[,c('V1','characteristics')]
  
  ## READING NOTES
  notes <- fread(dir2_expoapp$logcat,fill=T)
  notes <- notes[V1%in%c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'),]
  notes <- unique(notes)
  
  ## REMOVING OR SAVING UNTARED FILES
  if(save_untar==FALSE){
    unlink(file.path(td,dir_expoapp[1]),recursive=T)
  }else{
    print(paste("Untared file of ExpoApp saved at",file.path(td,dir_expoapp[1])))
  }
  
  
  ## GENERATING EXPOAPP OBJECT
  expoapp <- list(acce=acce,
                  gps=gps,
                  bar=bar,
                  ui=ui,
                  settings=settings,
                  notes=notes)
  
  setwd(inicio)
  
  ## SAVE RDATA
  if(save_RData=="TRUE"){
    save(expoapp,file=gsub(".tar.gz",".RData",file))
    print(paste("RData file of ExpoApp saved at",gsub(".tar.gz",".RData",file)))
  }
  
  ## RETURNING RESULTS  
  return(expoapp)
  
}