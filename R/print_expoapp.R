#' Generate ExpoApp Quality Data Analysis Report in html
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
#' @param open_html A logical variable (TRUE/FALSE) indicating if we want to open the Quality Analysis Report in the browser. 
#' @param save_html A logical variable (TRUE/FALSE) indicating if we want to save the Quality Analysis Report. 
#' @param ... optional arguments to the function.
#'
#' @return value

print_expoapp <- function(result, output_dir = NULL, open_html = TRUE, save_html = TRUE,...){
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
  
  id <- result$settings$characteristics[result$settings$V1 == "ID"]
  Rname <- paste0("Expoapp_Report_",id,".R")
  Hname <- paste0("Expoapp_Report_",id,".html")
  
  tmp <- file.path(output_dir,Rname)
  cat(paste0(expoapp_text1,collapse="\n"),file=tmp)
  cat(sapply(apply(result$settings,1,paste0,collapse=": "),function(x)paste("\n#'",x,"  ")),file=tmp,append=T)
  cat(paste0(expoapp_text2,collapse="\n"),file=tmp,append=T)
  cat(sapply(apply(result$notes,1,paste0,collapse=" "),function(x)paste("\n#'",x,"  ")),file=tmp,append=T)
  cat(paste0(expoapp_text3,collapse="\n"),file=tmp,append=T)
  
  if(rmarkdown::pandoc_available()==FALSE){
    stop("Error: pandoc 2.7.2 is not installed. Use the following link to installed. \n https://github.com/jgm/pandoc/releases/download/2.7.2/pandoc-2.7.2-windows-x86_64.msi")
  }
  
  rmarkdown::render(tmp,output_dir = output_dir)
  unlink(tmp)
  
  if(open_html == TRUE){
    browseURL(file.path(output_dir,Hname))    
  }
  
  if(save_html == FALSE){
    unlink(file.path(output_dir,Hname))
  }
  
}