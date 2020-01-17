
#Read a Decrypted Geolocation ExpoApp File
#
#It is a function to read gps data from ExpoApp.
#@param file It is a string variable with the path to the decripted ExpoApp GPS data.
#@param ... optional arguments of function.
#
#@return value
#@examples
## Using your password and the below link, you can download SensorLab2-1.2.2 tool.
## It contains a jar file and an example dataset.
## Please, unzip and save it into your desired path.
#
#browseURL("https://cloudstor.aarnet.edu.au/plus/s/5kPnaEyzuRB4cpH")
#Lab_folder <-"C:/Users/ddonaire/Documents/SensorLab2-1.2.2"
#gps_file <- file.path(Lab_folder,"ExpoApp.GPS.IDddg_decrypted.csv")
#
#gps_data <- read_gps_expoapp(file = gps_file)
#head(gps_data)
#gps_sf <- sf::st_as_sf(gps_data,coords=c("LONGITUDE","LATITUDE"),crs=4326)
#mapview::mapview(gps_sf)
#


read_gps_expoapp <- function(file=NULL,...){
  EPO <- NULL

  gps <- fread(file,sep=",",skip=1)[,1:7]
  names(gps) <- c("EPO","TIMESTAMP","LATITUDE","LONGITUDE","ACC","PRO","SAT")
  setkey(gps,EPO)
  return(gps)
}


#gis.expo
#
#It is the function to clustering the geolocation at places
#
#@param Build This the function to clustering. It also enrich data with information about green spaces from OpenStreetMap.
#@param Decrypted This is the path to the desencryted gps data from ExpoApp.
#@param EPSG_code This is the desired projected coordinate reference system of the study area.
#@param Buffer This is the desired minimum buffer used in the clustering algorithm.
#@param Time.zone The time zone of the study area.
#@param ... optional arguments of function.
#
#@return value It is a sqlite database with the gps data from ExpoApp cleaned and enriched with information about green spaces.

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


#mobile.gps
#
#It is the function to simplify and convert the sqlite obtained from clustering function to RData.
#@param x It is the sqlite file database generated from the gis.expo function.
#@param ... optional arguments of function.
#
#@return value

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


#mobile.gps.list is the function to convert all clustering sqlite files to RData files.
#@param processed It is the folder with all stored sqlite databases generated from the gps.expo.
#@param ... optional arguments of function.
#
#@return value

mobile.gps.list <- function(processed=NULL,...){
  aux <- lapply(list.files(processed,full.names = TRUE),function(x){print(x);mobile.gps(x)})
  names(aux) <- gsub(".sqlite","",list.files(processed))
  aux
}


#Convert Accelerometry to Forces
#
#It is the function to convert the x,y,z axes accelerometry measures to 2 vector forces (Vertical & Horizontal).
#@param x An data.table object with the accelerometer data from the ExpoApp data.It is generated using import_expoapp function.
#@param path It is the path directory to the accelerometer folder of ExpoApp data.
#@param lista Logical variable (TRUE,FALSE). If it is TRUE the function returns two elements the raw data and the 10 second forces dataset. If it false return only the 10 second resolution dataset with forces.
#@param Time.zone Character variable with the time zone information.
#@param ... optional arguments of function.
#
#@return If lista is TRUE, it returns a list of two data.tables. The first is the raw data (3 axes accelerations) and the second is processed (2 vector forces).
#If lista is FALSE, it returns a data.table with the 2 vector forces.
#

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

#Convert Forces to ActiGraph Counts, METs and Estimates Wearing Time.
#
#@param ace A data.table with the accelerometer data from ExpoApp.
#@param x The path, including file name, where we want to save the time-series of METs
#@param plot A logical variable (TRUE/FALSE) indicating png plot with the time-series of METs.
#@param population A character variable ("adults","childhood") indicating if participant was an adult or kid.
#@param ... optional arguments of table function.
#
#@return value

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


#Evaluation of Data Completeness of an ExpoApp session.
#
#It is the function to assess the number of minutes with accelerometer but no location. The off printed identify when the smartphone was turn off.
#@param aux The data.table with ExpoApp information at minute resolution.
#@param ... optional arguments of table function.
#
#@return value

completeness_expoapp <- function(aux,...){
  completeness <- Mets <- pro <- NULL

  aux[,completeness := ifelse(is.na(Mets) & is.na(pro), -1,
                             ifelse(is.na(pro) & !is.na(Mets),1,NA))]
  res <- aux[,ifelse(sum(completeness,na.rm=T)<= -59,"off",as.character(round(sum(completeness[completeness>0],na.rm=T),0))),by=c('day','hour')]
  res.width <- dcast(res, day ~ hour, value.var="V1")
  res.width[is.na(res.width)] <- '-'
  return(res.width)
}


#time2decimal is the function to convert times (HH:MM) to decimals.
#@param date A character o POSIxt variable with format ("yyyy-mm-dd hh:mm:ss").
#@param ... optional arguments of table function.
#
#@return value

time2decimal <- function(date="2019-12-12 08:20:30",...){
  x <- as.POSIXlt(date)
  res <- apply(cbind(x$hour, x$min/60, x$sec/3600),1,sum,na.rm=T)
  return(res)
}


#table2frame is the function to convert tables to data.frames.
#@param x A table object.
#@param ... optional arguments of table function.
#
#@return A data.frame with the same shape (nrow*ncols) of the table object.

table2frame <- function(x,...){
  as.data.frame(t(as.matrix(x)))
}










