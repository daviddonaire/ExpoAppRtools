
#' Decrypt a Geolocation ExpoApp File
#'
#' It is a function to decode the geolocation file of ExpoApp.
#'
#' @param file The path to the encrypted ExpoApp GPS data is stored.
#' @param output_dir The path where we want to store the desencrypted ExpoApp gps data.
#' @param SensorLab The path to the SensorLab folder with the jar file.
#' @param ... optional arguments of function.
#'
#' @return value
#'
#'
#' @import bit64
#' @import fansi
#' @import rmarkdown
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
#' # Using your password and the below link, you can download SensorLab2-1.2.2 tool.
#' # It contains a jar file and an example dataset.
#' # Please, unzip and save in your desired path
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' gps_file <- system.file("extdata", "ExpoApp.GPS.IDddg.csv", package = "ExpoAppRtools")
#' decrypt_expoapp(file = gps_file, SensorLab = Lab_folder)
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
#' @return value
#' @export

decrypt_expoapp_list <- function(gps_dir=NULL,output_dir=NULL,SensorLab=NULL,...){

  if(is.null(output_dir)){
    output_dir <- sub("/([^/]*)$", "",gps_dir)
  }
  res <-lapply(list.files(gps_dir,full.names = TRUE),function(y)decrypt_expoapp(y,output_dir=raw,SensorLab))
  ifelse(is.list(res),paste("Decrypted files saved in ",file.path(paste(head(strsplit(raw,"/")[[1]],-1),collapse="/"),"decrypted")),"ERROR")
}

#' Read a Decrypted Geolocation ExpoApp File
#'
#' It is a function to read gps data from ExpoApp.
#' @param file It is a string variable with the path to the decripted ExpoApp GPS data.
#' @param ... optional arguments of function.
#'
#' @return value
#' @examples
#' # Using your password and the below link, you can download SensorLab2-1.2.2 tool.
#' # It contains a jar file and an example dataset.
#' # Please, unzip and save it into your desired path.
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' gps_file <- file.path(Lab_folder,"ExpoApp.GPS.IDddg_decrypted.csv")
#'
#' gps_data <- read_gps_expoapp(file = gps_file)
#' head(gps_data)
#' gps_sf <- sf::st_as_sf(gps_data,coords=c("LONGITUDE","LATITUDE"),crs=4326)
#' mapview::mapview(gps_sf)
#'
#' @export

read_gps_expoapp <- function(file=NULL,...){
  EPO <- NULL

  gps <- fread(file,sep=",",skip=1)[,1:7]
  names(gps) <- c("EPO","TIMESTAMP","LATITUDE","LONGITUDE","ACC","PRO","SAT")
  setkey(gps,EPO)
  return(gps)
}


#' gis.expo
#'
#' It is the function to clustering the geolocation at places
#'
#' @param Build This the function to clustering. It also enrich data with information about green spaces from OpenStreetMap.
#' @param Decrypted This is the path to the desencryted gps data from ExpoApp.
#' @param EPSG_code This is the desired projected coordinate reference system of the study area.
#' @param Buffer This is the desired minimum buffer used in the clustering algorithm.
#' @param Time.zone The time zone of the study area.
#' @param ... optional arguments of function.
#'
#' @return value It is a sqlite database with the gps data from ExpoApp cleaned and enriched with information about green spaces.
#' @export

gis.expo <- function(Build=NULL,Decrypted=NULL,EPSG_code=25832,Buffer=150,Time.zone="Europe/Rome",...){
  setwd(Build)
  Processed <- file.path(paste(head(strsplit(Decrypted,"/")[[1]],-1),collapse="/"),"processed")
  suppressWarnings(dir.create(Processed, recursive = FALSE, mode = "0777"))
  k1 <- paste('expoapp \"',Processed,'" \"',Decrypted,'" \"',sep="")
  k2 <- substr(k1,1,nchar(k1)-2)
  k3 <- paste('\"',Time.zone,'\"',sep="")
  k4 <- paste(k2,EPSG_code,Buffer,k3)
  system(k4)
}


#' mobile.gps
#'
#' It is the function to simplify and convert the sqlite obtained from clustering function to RData.
#' @param x It is the sqlite file database generated from the gis.expo function.
#' @param ... optional arguments of function.
#'
#' @return value
#' @export

mobile.gps <- function(x,...){
  z <- strsplit(x,"/")[[1]]
  z1 <- paste(strsplit(tail(z,1),"[.]")[[1]][1],"RData",sep=".")
  z2 <- paste(c("RData",z1),collapse="/")
  z3 <- file.path(paste(head(z,-2),collapse="/"),"RData")
  suppressWarnings(dir.create(z3, recursive = FALSE, mode = "0777"))
  zz <- file.path(paste(head(z,-2),collapse="/"),z2)

  drv <- dbDriver("SQLite")
  tfile <-  x
  conn <- dbConnect(drv, dbname = tfile)
  dbListTables(conn)

  mob<- dbGetQuery(conn, "SELECT * FROM mobile10sec_result")
  place <- dbGetQuery(conn,"SELECT * FROM places_result")
  mob_raw <- dbGetQuery(conn, "SELECT * FROM mobile")
  #mob<- data.table(dbReadTable(conn, "mobile10sec_result"))
  #place<- data.table(dbReadTable(conn, "places_result"))
  #mob_raw <- data.table(dbReadTable(conn, "mobile")

  dbDisconnect(conn)
  dbUnloadDriver(drv)

  mob$lon <- as.numeric(mob$lon)
  mob$lat <- as.numeric(mob$lat)

  sapply(place$id_place,function(x){
    mob$greenblue[mob$place==x] <<- place$greenblue[place$id_place==x]
    mob$lon[mob$place==x] <<- place$lon[place$id_place==x]
    mob$lat[mob$place==x] <<- place$lat[place$id_place==x]
  })

  names(mob)[names(mob)%in%"lat"] <- "lat.cor"
  names(mob)[names(mob)%in%"lon"] <- "lon.cor"

  mobile <- merge(mob,mob_raw,by="epoch_measuring",all.x=T)

  mobile <- mobile[c("epoch_measuring","localdate.x","localdate.y","lat.cor","lon.cor","lat","lon","accuracy","provider","numsatellites","cluster","place","greenblue")]
  mobile <- mobile[!duplicated(mobile),]

  save(mobile,file=zz)
  mobile
}


#' mobile.gps.list is the function to convert all clustering sqlite files to RData files.
#' @param processed It is the folder with all stored sqlite databases generated from the gps.expo.
#' @param ... optional arguments of function.
#'
#' @return value
#' @export

mobile.gps.list <- function(processed=NULL,...){
  aux <- lapply(list.files(processed,full.names = TRUE),function(x){print(x);mobile.gps(x)})
  names(aux) <- gsub(".sqlite","",list.files(processed))
  aux
}


#' Convert Accelerometry to Forces
#'
#' It is the function to convert the x,y,z axes accelerometry measures to 2 vector forces (Vertical & Horizontal).
#' @param x An data.table object with the accelerometer data from the ExpoApp data.It is generated using import_expoapp function.
#' @param path It is the path directory to the accelerometer folder of ExpoApp data.
#' @param lista Logical variable (TRUE,FALSE). If it is TRUE the function returns two elements the raw data and the 10 second forces dataset. If it false return only the 10 second resolution dataset with forces.
#' @param Time.zone Character variable with the time zone information.
#' @param ... optional arguments of function.
#'
#' @return If lista is TRUE, it returns a list of two data.tables. The first is the raw data (3 axes accelerations) and the second is processed (2 vector forces).
#'If lista is FALSE, it returns a data.table with the 2 vector forces.
#'
#' @export

axes2vectors<-function(x=NULL,path,lista=TRUE,Time.zone="Australia/Melbourne",...){
  EPO <- date.time <- date.time2 <- odd <- measure <- g <- dif.time <- mean.x <- NULL
  X <- mean.y <- Y <- mean.z <- Z <- dif.x <- dif.y <- dif.z <- p3 <- d <- lx  <- NULL
  ly <- lz <- k <- Av <- Av.s <- date2 <- NULL

  if(is.null(x)){
    x <- rbindlist(lapply(path,fread))
  }
  setkey(x, EPO)

  x[,date.time:= EPO/1e3]
  x[,date.time2 := round(date.time,0)]
  x[,odd := date.time2%%2!=0]
  x[,date.time2:=ifelse(odd==T,date.time2-1,date.time2)]

  setkey(x, date.time2)
  x[,measure:= .N ,by=date.time2]
  x[,g := 9.80655]

  values<-rle(x$date.time2)$values

  if(length(values)>1){
    x <- x[,dif.time := tail(date.time,1)-head(date.time,1),by=date.time2]
    x <- x[,mean.x := mean(X,na.rm=T),by=date.time2]
    x <- x[,mean.y := mean(Y,na.rm=T),by=date.time2]
    x <- x[,mean.z := mean(Z,na.rm=T),by=date.time2]
  }else{
    x$dif.time<-tail(x$date.time,1)-head(x$date.time,1)
    x$mean.x<-mean(x$X)
    x$mean.y<-mean(x$Y)
    x$mean.z<-mean(x$Z)
  }


  x[,dif.x := mean.x - X]
  x[,dif.y := mean.y - Y]
  x[,dif.z := mean.z - Z]
  x[,p3 := mean.x^2 + mean.y^2 + mean.z^2]
  x[,d := (mean.x*dif.x) + (mean.y*dif.y) + (mean.z*dif.z)]
  x[,lx := (d/p3) * mean.x]
  x[,ly := (d/p3) * mean.y]
  x[,lz := (d/p3) * mean.z]
  x[,k := lx^2 + ly^2 + lz^2]
  x[,Av := sqrt(k)]

  if(length(values)>1){
    x <- x[,Av.s := sum(Av,na.rm=T),by=date.time2]
  }else{
    x$Av.s<-sum(x$Av)
  }

  x[,Av.s := Av.s/(measure/dif.time)/g]

  result <- x[,mean(Av.s,na.rm=T),by=date.time2]
  names(result) <- c("date.time2","Av.s")

  result[,date2 := as.POSIXct(trunc((date.time2/10),0)*10
                              ,origin="1970-01-01",tz=Time.zone)]

  setkey(result, date2)

  result <- result[,sum(Av.s,na.rm=T),by=date2]

  names(result)<-c("Date","V")

  if(isTRUE(lista)){
    result <- list(acce_raw=x,acce_vec=result)
  }else{
    result
  }
  return(result)
}

#' Convert Forces to ActiGraph Counts, METs and Estimates Wearing Time.
#' 
#' @param ace A data.table with the accelerometer data from ExpoApp.
#' @param x The path, including file name, where we want to save the time-series of METs
#' @param plot A logical variable (TRUE/FALSE) indicating png plot with the time-series of METs.
#' @param population A character variable ("adults","childhood") indicating if participant was an adult or kid.
#' @param ... optional arguments of table function.
#'
#' @return value
#' @export

pa.acti2 <-function(ace,x,plot=FALSE,population="adults",...){
  cont <- axis1.1 <- axis1 <- axis1.2 <- axis1.3 <- axis1.4 <- axis1.5 <- NULL
  axis1._1 <- axis1._2 <- axis1._3 <- axis1._4 <- axis1._5 <- NULL
  cvf5 <- cvf4 <- cvf3 <- cvf2 <- cvf1 <- cvb5 <- CV <- NULL
  . <- mets <- steps <- NULL

  ace[,cont:=1:nrow(ace)]

  ace[,axis1.1  := as.numeric(c(tail(axis1,-1),NA))]
  ace[,axis1.2  := as.numeric(c(tail(axis1,-2),rep(NA,2)))]
  ace[,axis1.3  := as.numeric(c(tail(axis1,-3),rep(NA,3)))]
  ace[,axis1.4  := as.numeric(c(tail(axis1,-4),rep(NA,4)))]
  ace[,axis1.5  := as.numeric(c(tail(axis1,-5),rep(NA,5)))]
  ace[,axis1._1 := as.numeric(c(NA,head(axis1,-1)))]
  ace[,axis1._2 := as.numeric(c(rep(NA,2),head(axis1,-2)))]
  ace[,axis1._3 := as.numeric(c(rep(NA,3),head(axis1,-3)))]
  ace[,axis1._4 := as.numeric(c(rep(NA,4),head(axis1,-4)))]
  ace[,axis1._5 := as.numeric(c(rep(NA,5),head(axis1,-5)))]

  ace[,cvf5:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1","axis1.1","axis1.2","axis1.3","axis1.4","axis1.5")]
  ace[,cvf4:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1._1","axis1","axis1.1","axis1.2","axis1.3","axis1.4")]
  ace[,cvf3:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1._2","axis1._1","axis1","axis1.1","axis1.2","axis1.3")]
  ace[,cvf2:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1._3","axis1._2","axis1._1","axis1","axis1.1","axis1.2")]
  ace[,cvf1:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1._4","axis1._3","axis1._2","axis1._1","axis1","axis1.1")]
  ace[,cvb5:= as.numeric(apply(.SD,1,function(x)sd(x,na.rm=T)/mean(x,na.rm=T))),.SDcols= c("axis1._5","axis1._4","axis1._3","axis1._2","axis1._1","axis1")]

  suppressWarnings(suppressMessages(ace[,CV := apply(.SD,1,min,na.rm=T),.SDcols=c("cvb5","cvf1","cvf2","cvf3","cvf4","cvf5")]))
  ace[,CV := ifelse(is.na(CV)|is.infinite(CV),0,CV)]


  if(population=="adults"){
    ace$mets[ace$axis1<=8 & !is.na(ace$axis1)] <- 1
    ace$mets[ace$axis1>8 & !is.na(ace$axis1) & ace$CV>10] <- 0.749395+
      (0.716431*(log(ace$axis1[ace$axis1>8 & !is.na(ace$axis1) & ace$CV>10])))-
      (0.179874*(log(ace$axis1[ace$axis1>8 & !is.na(ace$axis1) & ace$CV>10]))^2)+
      (0.033173*(log(ace$axis1[ace$axis1>8 & !is.na(ace$axis1) & ace$CV>10]))^3)
    ace$mets[ace$axis1>8 & !is.na(ace$axis1) & ace$CV<=10] <-
      2.294275*(exp(0.00084679*ace$axis1[ace$axis1>8 & !is.na(ace$axis1) & ace$CV<=10]))
  }

  if(population=="childhood"){
    ace$mets [ace$axis1<=25 & !is.na(ace$axis1)] <- 1
    ace$mets [ace$axis1>25 & !is.na(ace$axis1) & ace$CV<=35] <-
      1.982 * exp(0.00101*ace$axis1[ace$axis1>25 & !is.na(ace$axis1) & ace$CV<=35])
    ace$mets [ace$axis1>25 & !is.na(ace$axis1) & ace$CV>35] <- 2.842 +
      (0.00288*ace$axis1 [ace$axis1>25 & !is.na(ace$axis1) & ace$CV>35])
  }


  acti<-ace
  rm(ace)
  names(acti)[1]<-"date"
  acti$date2 <- as.POSIXct(format(acti$date,format="%Y-%m-%d %H:%M"))
  #acti2<-aggregate(subset(acti,select=c("mets")),by=list(acti$date2),FUN=mean)
  #acti2<-aggregate(mets~date2,data=acti,FUN=mean,na.rm=T)
  acti2 <- acti[,.(mets=mean(mets,na.rm=T)),by="date2"]
  names(acti2)<-c("date","mets.acti")
  #acti3<-aggregate(cbind(axis1,steps)~date2,data=acti,FUN=sum,na.rm=T)
  acti3 <- acti[,.(axis1=sum(axis1,na.rm=T),steps=sum(steps,na.rm=T)),by="date2"]
  names(acti3)<-c("date","axis1","steps")
  acti4<-merge(acti3,acti2,by="date")

  #grup <- rle(acti4$axis1)


  acti4$date.ch <- as.character(acti4$date)
  k.wear <- as.data.frame(unclass(rle(acti4$axis1>1)))

  if(k.wear$lengths[k.wear$values==T][which.max(k.wear$lengths[k.wear$values==T])]>10)
    acti4$wear <- wearingMarking(acti4,TS="date.ch",cts="axis1",perMinuteCts=1)$wearing
  if(k.wear$lengths[k.wear$values==T][which.max(k.wear$lengths[k.wear$values==T])]<10)
    acti4$wear <- "nw"

  acti4$wearing<-acti4$mets.acti
  acti4$wearing[acti4$wear=="nw"]<-NA

  acti4$day <- format(acti4$date,format="%Y-%m-%d")
  acti4$worn[!is.na(acti4$wearing)==T]<- "Wearing Device, hours"
  acti4$time.pa[acti4$wearing>=1.5 & acti4$wearing<3 & !is.na(acti4$wearing)] <- "Light Physical Activity (1.5-3 METs), hours"
  acti4$time.pa[acti4$wearing>=3 & acti4$wearing<6 & !is.na(acti4$wearing)] <- "Moderate Physical Activity (3-6 METs), hours"
  acti4$time.pa[acti4$wearing>=6 & !is.na(acti4$wearing)] <- "Vigorous Physical Activity (>6 METs), hours"
  acti4

  #anado wearing & METs al raw.data
  acti <- merge(acti,acti4[,c("date","wear","wearing")],by.x="date2",by.y="date",all.x=T)
  acti <- acti[,c("date2","date","V","axis1","mets","wear","wearing")]
  names(acti) <- c("date.min","date","V","axis1","mets","wear","wearing")

  if(plot==TRUE){
    png(sub("agd","png",x),width = 800, height = 600)
    plot(acti4$date,acti4$mets.acti,type="l",xlab="Date",ylab="Intensity (METs)")
    dev.off()
  }

  WEARING <-as.data.frame(unclass(round(table(acti4$worn,acti4$day)/60,2)))
  PA <-as.data.frame(unclass(round(table(acti4$time.pa,acti4$day)/60,2)))
  STEPS <- with(acti4,tapply(steps,day,sum))
  control <- rbind(WEARING,PA)
  control$TOTAL <- rowSums (control, na.rm = FALSE, dims = 1)

  res <- list(raw=acti,
              control=control)
  return(res)
}


#' Evaluation of Data Completeness of an ExpoApp session.
#'
#' It is the function to assess the number of minutes with accelerometer but no location. The off printed identify when the smartphone was turn off.
#' @param aux The data.table with ExpoApp information at minute resolution.
#' @param ... optional arguments of table function.
#'
#' @return value
#' @export

completeness_expoapp <- function(aux,...){
  completeness <- Mets <- pro <- NULL

  aux[,completeness := ifelse(is.na(Mets) & is.na(pro), -1,
                             ifelse(is.na(pro) & !is.na(Mets),1,NA))]
  res <- aux[,ifelse(sum(completeness,na.rm=T)<= -59,"off",as.character(round(sum(completeness[completeness>0],na.rm=T),0))),by=c('day','hour')]
  res.width <- dcast(res, day ~ hour, value.var="V1")
  res.width[is.na(res.width)] <- '-'
  return(res.width)
}


#' time2decimal is the function to convert times (HH:MM) to decimals.
#' @param date A character o POSIxt variable with format ("yyyy-mm-dd hh:mm:ss").
#' @param ... optional arguments of table function.
#'
#' @return value
#' @export

time2decimal <- function(date="2019-12-12 08:20:30",...){
  x <- as.POSIXlt(date)
  res <- apply(cbind(x$hour, x$min/60, x$sec/3600),1,sum,na.rm=T)
  return(res)
}


#' table2frame is the function to convert tables to data.frames.
#' @param x A table object.
#' @param ... optional arguments of table function.
#'
#' @return A data.frame with the same shape (nrow*ncols) of the table object.
#' @export

table2frame <- function(x,...){
  as.data.frame(t(as.matrix(x)))
}


#' Untar, Decrypt and Save ExpoApp data.
#' 
#' This function untars ExpoApp data, decrypts ExpoApp geolocation file, and saves the ExpoApp untared and in RData format.
#' @param file Character variable with the path to the tar.gz file of ExpoApp data.
#' @param SensorLab Character variable with the path to the SensorLab folder. SensorLab folder has to contain the jar and the pem files.
#' @param Build Character variable with the path to the Spatio-Temporal Clustering function. This function reduce the cloud of points around places to a one point per place and time. It also enriches the data with information about OpenStreetMap's green spaces.
#' @param EPSG_code Numeric variable with the desired projected coordinate reference system of the study area.
#' @param Buffer Numeric variable with the desired minimum radius of the buffer to be used in the clustering algorithm.
#' @param Time.zone Character variable with the time zone of the study area (e.g. "Australia/Melbourne").
#' @param Clustering A logical variable (TRUE/FALSE) indicating if applying yes/no the clustering algorithm.
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
#'                         Time.zone = "Australia/Melbourne", 
#'                         Clustering = FALSE, save_RData = FALSE, save_untar=FALSE)
#' sapply(expoapp,head)
#'
#' gps_sf <- sf::st_as_sf(expoapp$gps,coords=c("LONGITUDE","LATITUDE"),crs=4326)
#' mapview::mapview(gps_sf)
#' # see Expoapp_resum to generate the 10 seconds and 1 minute simplified Expoapp files.
#' @export

import_expoapp <- function(file = NULL, SensorLab = NULL,
                         Build = NULL, EPSG_code = 25832, Buffer = 150,
                         Time.zone = "Europe/Rome", Clustering = FALSE,
                         save_RData= TRUE, save_untar = TRUE,...){
  EPO <- V1 <- NULL

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
  if(length(dir2_expoapp$acce!=0)){
    acce <- rbindlist(lapply(dir2_expoapp$acce,fread), use.names = TRUE)
    setkey(acce, EPO)
  }else {acce <- NULL}

  ## READING & CLEANNING SPATIAL DATA
  a <- decrypt_expoapp(dir2_expoapp$gps,SensorLab=SensorLab)
  
  if(Clustering==TRUE){
    gis.expo(Build=Build,Decrypted=a,EPSG_code=EPSG_code ,Buffer=Buffer ,Time.zone=Time.zone)
    getwd()
    gps <- mobile.gps(list.files(gsub("decrypted","processed",a),full.names=T))
  }else{
    gps <- read_gps_expoapp(file=list.files(a))
  }

  ## READING BAROMETRIC DATA
  if(length(dir2_expoapp$bar!=0)){
    bar <- read.csv(dir2_expoapp$bar)
  }else {bar <- NULL}

  ## READING UI DATA
  if(length(dir2_expoapp$ui!=0)){
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


#' print_expoapp
#' 
#' It is the function to generate the html with the data analysis of the ExpoApp data.
#' @param result It is a list object with the times, settings, notes, gps_plot, pa_plot, and nolocation.
#' Times is a data.table with the information of the quality of the data.
#' Settings are the software details of the phone and settings of the ExpoApp session.
#' Notes is the logcat from ExpoApp with all the infromation about battery of the phone.
#' Gps_plot is a mapview object with the gps information from ExpoApp.
#' PA_plot is a ggplot object with the METs time-series from ExpoApp.
#' Nolocation is the temporal completeness of the geolocation of the Expoapp session.
#' @param output_dir The folder where we want to store the html file with the quality analysis of ExpoApp session.
#' @param ... optional arguments to the function.
#'
#' @return value
#' @export

print_expoapp <- function(result, output_dir = NULL,...){
  expoapp_text1 <- c("#' ---","#' title: ExpoApp Quality Data Analysis","#' author: David Donaire-Gonzalez","#' date: January 8th, 2019",
                     "#' output:","#'    html_document:","#'      toc: true","#'      highlight: zenburn","#' ---"," ","#' ## Phone & ExpoApp Settings","#'",
                     " ",'#+ results="asis",echo=FALSE, size="tiny" ')
  expoapp_text2 <- c("\n#'"," ","#' ## Evaluation of Data Completeness:","#' Recorded, Wearing, and with All, GPS and NETWORK location",
                     "#'"," ",'#+ results="asis",echo=FALSE, size="tiny" ',"knitr::kable(result$times)","#'"," ","#' ## ExpoApp Physical Activity Plot",
                     "#'"," ","#+ fig.width=9, fig.height=4,echo=FALSE ","result$pa_plot ","#'"," ","#' ## ExpoApp Map ","#'"," ","#+ fig.width=9, fig.height=6, echo=FALSE ",
                     "result$gps_plot@map ","#'"," ","#' ## Minutes with accelerometer but no location","#' (off: smartphone turn off) ","#'"," ","#+ results='asis',echo=FALSE ","knitr::kable(result$nolocation) ",
                     "#'"," "," ","#' ## ExpoApp logcat ","#'"," ","#+ results='asis',echo=FALSE ")
  expoapp_text3 <- c("\n#'"," ","#+ echo=FALSE ","#https://rmarkdown.rstudio.com/articles_report_from_r_script.html ",
                     "#http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/ ",
                     "#https://kbroman.org/knitr_knutshell/pages/Rmarkdown.html ",
                     "#https://stackoverflow.com/questions/48370425/pass-code-to-input-of-rmarkdown-render-function-instead-of-a-file ",
                     " ",'#rmarkdown::render("C:/ACU/1_Projects/iMAP/Scripts/ExpoApp/Test_Improving_R_mobile_tools.R") ',"#'"," ")

  tmp <- file.path(output_dir,"expoapp_summary.R")
  cat(paste0(expoapp_text1,collapse="\n"),file=tmp)
  cat(sapply(apply(result$settings,1,paste0,collapse=": "),function(x)paste("\n#'",x,"  ")),file=tmp,append=T)
  cat(paste0(expoapp_text2,collapse="\n"),file=tmp,append=T)
  cat(sapply(apply(result$notes,1,paste0,collapse=" "),function(x)paste("\n#'",x,"  ")),file=tmp,append=T)
  cat(paste0(expoapp_text3,collapse="\n"),file=tmp,append=T)

  rmarkdown::render(tmp,output_dir = output_dir)
  unlink(tmp)
  browseURL(file.path(output_dir,"expoapp_summary.html"))
}


#' Generate ExpoApp_Totals, ExpoApp_min ,and Quality Analysis Report
#'
#' It is the function to generate a 10 seconds and 1 minute simplified ExpoApp data.
#' @param ExpoApp It is the ExpoApp RData object
#' @param acce_lista A logical variable (TRUE/FALSE) indicating if the accelerometer output should have raw and vectors or only vectors.
#' @param Time.zone The time zone of the study area.
#' @param output_dir The folder where we want to store the 10 seconds and 1 minute simplified ExpoApp data.
#' @param ... optional arguments of function.

#' @return value
#'
#' @examples
#' # ExpoApp geolocation information is encrypted to ensure the confidentiality of participants in case they lose the pheno.
#' # Using your password and the below link, you can download SensorLab2-1.2.2 tool.
#' # It contains a decrypt key and example datasets.
#' # Please, download, unzip and save SensorLab2-1.2.2 into your desired path.
#'
#' browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#' Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#' load(file.path(Lab_folder,"ExpoApp.IDddg.RData"))
#' ls()
#' result <- reduce_expoapp(ExpoApp=expoapp,output_dir=getwd())
#' sapply(result,class)
#' @export

reduce_expoapp <- function(ExpoApp = NULL ,acce_lista = FALSE, Time.zone = "Australia/Melbourne",
                       output_dir = getwd(),...){
  epo <- acc <- date.min <- axis1 <- V <- steps <- mets <- day <- latitude <- Mets <- NULL

  id <- ExpoApp$settings$characteristics[ExpoApp$settings$V1=='ID']

  gps <- copy(ExpoApp$gps)
  names(gps) <- tolower(names(gps))
  gps[,date := as.POSIXct(trunc((epo/1e4),0)*10,origin="1970-01-01",tz=Time.zone)]
  setkey(gps,date)
  gps <- gps[gps[ , .I[which.min(acc)], by = date]$V1]
  gps_min <- copy(gps)
  gps_min[,date.min:=as.POSIXct(format(date,format="%Y-%m-%d %H:%M"))]
  gps_min <- gps_min[gps_min[ , .I[which.min(acc)], by = date.min]$V1]

  acce <- copy(ExpoApp$acce)
  acce <- axes2vectors(x=acce,lista=acce_lista,Time.zone=Time.zone)
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
  save(expoapp.totals,file=file.path(output_dir,paste0("ExpoApp_totals_",id,".RData")))

  aux_min <- merge(acti_min,gps_min,by="date.min",all.x=T)
  aux_min[,day := factor(substr(date.min,1,10))]
  aux_min[,hour := factor(substr(date.min,12,13))]
  aux_min[,time := time2decimal(date.min)]

  expoapp.min <- aux_min
  save(expoapp.min,file=file.path(output_dir,paste0("ExpoApp_min_",id,".RData")))

  aux.sf <- aux_min[!is.na(latitude),]
  #aux.sf[,N:=sum(.N,na.rm=T),by="day"]
  #aux.sf <- aux.sf[N>1,]
  aux.sf <- st_as_sf(data.frame(aux.sf),coords=c("longitude","latitude"),crs=4326)

  times <- rbindlist(list(
    Recorded = table2frame(round(table(aux_min$day[!is.na(aux_min$Mets)],useNA="ifany")/60,1)),
    Wearing = round(acti$control[1,1:(ncol(acti$control)-1)],1),
    ALL_Location = table2frame(round(table(aux_min$day[!is.na(aux_min$pro)])/60,1)),
    GPS_Location = table2frame(round(table(aux_min$day[aux_min$pro%in%'gps'])/60,1)),
    NET_Location = table2frame(round(table(aux_min$day[aux_min$pro%in%'network'])/60,1))
  ),idcol = "Type", fill=TRUE)

  nolocation <- completeness_expoapp(aux_min)

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

  print_expoapp(result,output_dir = output_dir)

  return(result)
}

