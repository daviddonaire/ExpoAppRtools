
#' Decrypt a Geolocation ExpoApp File
#'
#' It is a function to decode the geolocation file of ExpoApp.
#'
#' @param file The path to the encrypted ExpoApp GPS data is stored.
#' @param output_dir The path where we want to store the desencrypted ExpoApp gps data.
#' @param SensorLab The path to the SensorLab folder with the jar file.
#' @param ... optional arguments of function.
#'
#' @return The function returns the path to where decrypted file is stored. This function saves a decrypted file of the ExpoApp gps data.
#'
#'
#' @import bit64
#' @import fansi
#' @import rmarkdown
#' @import installr
#' @import knitr
#' @import sf
#' @import ggplot2
#' @import ggspatial
#' @import data.table
#' @import mapview
#' @import RSQLite
#' @import cluster
#' @import PhysicalActivity
#' @import grDevices
#' @import stats
#' @import utils

#'
#' @examples
#' # ExpoApp geolocation information is encrypted to ensure the confidentiality of participants
#' # in case they lose the pheno. Using your password and the below link, you can 
#' # download SensorLab2-1.2.2 tool. It contains a jar file, a decrypt key and example datasets.
#' # Please, download, unzip and save SensorLab2-1.2.2 into your desired path.
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' encryted_file <- file.path(Lab_folder,"ExpoApp.GPS.IDddg.csv")
#' output_dir <-"C:/Users/ddonaire/Documents"
#' 
#' decrypt_expoapp(file = encrypted_file, SensorLab = Lab_folder, output_dir=output_dir)
#'
#' @export

decrypt_expoapp <- function(file=NULL, output_dir=NULL,SensorLab=SensorLab,...){
  if(is.null(output_dir)){
    output_dir <- file.path(sub("/([^/]*)$", "",file),"decrypted")
  }
  dir_home <- getwd()
  b <- file.path(output_dir,"temp")
  #d <- file.path(SensorLab,"SensorLab2-1.1.8-jar-with-dependencies.jar")
  d <- file.path(SensorLab,"SensorLab2-1.2.2-jar-with-dependencies.jar")
  e <- file.path(SensorLab,"priv8.pem")
  #e <- file.path(SensorLab,"rsapublic.pem")
  
  suppressWarnings(dir.create(output_dir,recursive = FALSE, mode = "0777"))
  dir.create(b, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  file.copy(from=file,to=b)
  file.copy(from=d,to=b)
  file.copy(from=e,to=b)
  setwd(b)
  fe <- gsub(".csv","",grep("csv",list.files(),value=T))
  #g <- paste("java -cp SensorLab2-1.1.8-jar-with-dependencies.jar com.ateknea.exposomics.expoapp.utils.LocationDecrypter",
  #grep("csv",list.files(),value=T))
  g <- paste("java -cp SensorLab2-1.2.2-jar-with-dependencies.jar com.ateknea.exposomics.expoapp.utils.LocationRSADecrypter",
             grep("csv",list.files(),value=T))
  system(g,show.output.on.console = FALSE)
  h <- paste(fe,"_decrypted.csv",sep="")
  file.rename("decrypted.csv",h)
  file.copy(h,output_dir)
  setwd(dir_home)
  unlink(b,recursive=T)
  return(output_dir)
}


#' Decrypt a Geolocation ExpoApp Files List
#'
#' It is a function to decrypt at once all ExpoApp gps files stored into the same folder.
#' @param gps_dir The path where the encrypted ExpoApp GPS files are stored.
#' @param output_dir The path where we want to store the desencrypted ExpoApp gps data.
#' @param SensorLab The path to the SensorLab folder with the jar file.
#' @param ... optional arguments of function.
#'
#' @return The function returns the path to where the decrypted files are stored. 
#' This function decrypts all ExpoApp gps files stored in one folder and saves the decrypted file at the output_dir.
#' 
#' @export

decrypt_expoapp_list <- function(gps_dir=NULL,output_dir=NULL,SensorLab=NULL,...){
  
  if(is.null(output_dir)){
    output_dir <- sub("/([^/]*)$", "",gps_dir)
  }
  res <-lapply(list.files(gps_dir,full.names = TRUE),function(y)decrypt_expoapp(y,output_dir=raw,SensorLab))
  ifelse(is.list(res),paste("Decrypted files saved in ",file.path(paste(head(strsplit(raw,"/")[[1]],-1),collapse="/"),"decrypted")),"ERROR")
}