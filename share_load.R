### Share load should be sourced by both ui and server.
##  load library --------------------
library(rjson)
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
#library(geosphere)
library(magrittr)
library(shinycssloaders)
library(quantmod)
library(xts)
options(spinner.color="#006272")
library(timevis)
#library(comtradr)
library(memoise)
#library(networkD3)
library(promises)
library(future)
library(reshape2)
plan(multiprocess)

### use memoise package for ct_search in comtradr ----
#m_ct_search <- memoise::memoise(ct_search)

## load functions
source('helper_funs.R', encoding = "UTF-8")

## load concordance
load("./CSP_data.rdata")

list_nozares<-list(c('Ieguves rūpniecība [B]','Apstrādes rūpniecība [C]','Pārtikas, dzērienu un tabakas produktu ražošana [C10-12]',
                     'Vieglā rūpniecība [C13-15]','Kokapstrāde [C16]','Papīra ražošana un poligrāfija [C17-18]','Ķīmiskā rūpniecība [C19-22]',
                     'Nemetālisko minerālu ražošana [C23]','Metālu un gatavo metālizstrādājumu ražošana [C24-25]','Datoru, elektrisko un citu iekārtu ražošana [C26-28]',
                     'Transportlīdzkļu ražošana [C29-30]','Mēbeļu un citu produktu ražošana [C31-32]','Iekārtu un ierīču remonts un uzstādīšana [C33]',
                     'Elektroenerģija, gāzes apgāde un siltumapgāde [C35]'),
                   c('Pārtikas produktu ražošana [C10]','Dzērienu ražošana [C11]','Tabakas izstrādājumu ražošana [C12]','Tekstilizstrādājumu ražošana [C13]',
                     'Apģērbu ražošana [C14]','Ādas un ādas izstrādājumu ražošana [C15]','Koksnes, koka un korķa izstrādājumu ražošana [C16]',
                     'Papīra un papīra izstrādājumu ražošana [C17]','Poligrāfija un ierakstu reproducēšana [C18]','Naftas produktu pārstrāde [C19]',
                     'Ķīmisko produktu ražošana [C20]','Farmaceitisko produktu ražošana [C21]','Gumijas un plastmasas izstrādājumu ražošana [C22]',
                     'Nemetālisko minerālu ražošana [C23]','Metālu ražošana [C24]','Gatavo metālizstrādājumu ražošana [C25]', 'Datoru, optisko un elektronisko iekārtu ražošana [C26]',
                     'Elektrisko iekārtu ražošana [C27]','Citu iekārtu ražošana [C28]','Automobiļu, piekabju un puspiekabju ražošana [C29]','Citu transportlīdzekļu ražošana [C30]',
                     'Mēbeļu ražošana [C31]','Cita veida ražošana [C32]','Iekārtu un ierīču remonts un uzstādīšana [C33]'))
names(list_nozares)<-c('Līdzīgo nozaru grupa','Apstrādes rūpniecība')


## setup global variables
actualYear <- substr(datacal_df$Periods[nrow(datacal_df)],1,4)
actualMonth_NR <- substr(datacal_df$Periods[nrow(datacal_df)],6,7)
month_var<-c('01','02','03','04', '05','06','07','08','09','10','11','12')
month_names<-c('janvārī','februārī','martā',
               'aprīlī','maijā','jūnijā',
               'jūlijā','augustā','septembrī',
               'oktobrī','novembrī','decembrī')
monthly<-c('janvāri','februāri','martu',
           'aprīli','maiju','jūniju',
           'jūliju','augustu','septembri',
           'oktobri','novembri','decembri')
for (i in 1:12){
  if (actualMonth_NR==month_var[i]){
    actualMonth=month_names[i]
    actualMon=monthly[i]
  }
}


#maxYear <- tolower(paste0(dtf_shiny_full$Note[1],' ', max(dtf_shiny_full$Year)))
#maxYear <- gsub('q1', 'March', maxYear)
#maxYear <- gsub('q2', 'June', maxYear)
#maxYear <- gsub('q3', 'September', maxYear)
#maxYear <- gsub('q4', 'December', maxYear)

## Stats NZ's monthly update link -- update very month
SNZ_link <- "https://stat.gov.lv/lv/meklet?Search=%22%22&DataSource=%22publications%22&PublicationType=%5B%22press_release%22%5D&Themes=%222374%22"

# Highchart options
global <- getOption("highcharter.lang")
global$contextButtonTitle<-'Grafika satura izvēlne'
global$downloadJPEG<-'Lejupielādēt JPEG attēlu'
global$downloadPDF<-'Lejupielādēt PDF datni'
global$downloadPNG<-'Lejupielādēt PNG attēlu'
global$downloadSVG<-'Lejupielādēt SVG vektorgrafiku'
global$drillUpText<-'Atpakaļ uz {series.name}'
global$loading<-'Ielādē...'
global$months<-c('Janvāris','Februāris','Marts','Aprīlis','Maijs', 'Jūnijs', 'Jūlijs', 'Augusts', 'Septembris','Oktobris','Novembris', 'Decembris')
global$noData<-'Nav datu, ko attēlot'
global$printChart<-'Izdrukāt grafiku'
global$resetZoom<-'Atiestatīt pietuvinājumu'
global$resetZoomTitle<-'Attiestatīt pietuvinājumu līmenī 1:1'
global$weekdays<-c('Svētdiena','Pirmdiena','Otrdiena','Trešdiena','Ceturtdiena','Piektdiena','Sestdiena')
options(highcharter.lang = global)


#Tabulas iekrāsošanas funkcija
color_from_middle <- function (data, color1,color2) 
{
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
} 

# Funkcija skaistākiem Value Box

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75); font-size: 12px"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-small icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# Tēma ValueBox sparkline
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}
