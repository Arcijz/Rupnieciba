#### LIST nozares ------------------
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



#### vb style -----------------
VB_style <- function(msg = 'Sveiki', style="font-size: 100%;"){
   tags$p( msg , style = style )
}

### CAGR function------------------
CAGR <- 
   function (ratio, period, digits = 1) {
      round((exp(log(ratio)/period) - 1) * 100, digits)
      }
      
## rgb to hex function---------------      
GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

## how to text for commodity intelligence report --------------
howto_ci <- function(){
   fluidRow(
      tags$h1('Kā darboties ar produktu informāciju:', style = "font-family: 'Arial"),
      tags$ol(
         tags$li( "Atlasi definētās nozaru grupas, vai izveido sarakstu ar saviem analizējamajiem produktiem:",
                  tags$ol(type="a",
                          tags$li(tags$b("Definēto produktu grupas ", style = "font-family: 'Arial"), "izveidoja CSP darbinieki", style = "font-family: 'Arial"),
                          tags$li(tags$b("Pašdefinētās ", style = "font-family: 'Arial"), 
                                  "produktu grupas izveido lietotāji .csv formāta failā, kur pirmā kolonna satur PRODCOM kodus, bet otrā kolonna grupas nosaukumu.",
                                  tags$b("Tu vari lejupielādēt paraugu no ", tags$a(href="HS_group_template.csv", "šejienes", target = "_blank", style = "font-family: 'Arial"), ", un modificēt pēc savām vajadzībām." , style = "font-family: 'Arial")
                                  , style = "font-family: 'Arial")
                  )
         ),
         tags$li("Meklē vai augšupielādē:",
                 tags$ol(type='a',
                         tags$li( "Meklē un atlasi vienu vai vairākas definētās produktu grupas" , style = "font-family: 'Arial"),
                         tags$li( "Augšupielādē sevis definētās produktu grupas pareizā formātā" , style = "font-family: 'Arial")
                         , style = "font-family: 'Arial")
         ),
         tags$li(tags$b("Lūdzu nospied ATIESTATĪT pirms jaunas atskaites izveides!", style = "font-family: 'Arial")),
         tags$li( "Detalizētākām instrukcijām par šīs darbpaneļa sadaļas izmantošanu, lūdzu apmeklē " ,
                  tags$a( "šo lapu",
                          href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                          target = "_blank", style = "font-family: 'Arial"),
                  ".", style = "font-family: 'Arial"
                  )
      )
   )
}

## how to text for country intelligence report --------------
howto_country <- function(){
   fluidRow(
      tags$h1('Kā izmantot nozares analīzes sadaļu:', style = "font-family: 'Arial"),
      tags$ol(
         tags$li( "Izvēlies vienu vai vairākas rūpniecības apakšnozares", style = "font-family: 'Arial"),
         tags$li( "Meklē vai izvēlies TIKAI VIENU ierakstu no līdzīgo nozaru grupas, vai arī vairākas individuālās rūpniecības nozares. ", style = "font-family: 'Arial"),
         tags$li(tags$b("Lūdzu nospied ATIESTATĪT pirms jaunas atskaites izveides!", style = "font-family: 'Arial"))
      )
   )
}

## how to text for hs finder tab ----------------
howto_hs_finder <- function(){
   #fluidRow(
      tags$h1('How to:')
      tags$ol(
         tags$li( "Search for either HS code or commodity names in the search box or the filter box above each column. Note that regular expression rules are built in all search boxes."),
         tags$li( "Show either the first 10 entries or the entire table."),
         tags$li( "Click one or multiple rows to generate a report."),
         tags$li( "The report is default to be based on exports but can be changed to imports by using the radio button on the left." ),
         tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
                  tags$a( "here",
                          href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                          target = "_blank"),
                  "."
         )
         #tags$li( "Copy to clipboard or export as a CSV file for later use")
      )
   #)
}

## contact for help ----------------
contact <- function(){
   fluidRow(
      h2( "Kontakti" ),
      tags$p("Rūpniecības darbpaneļa darbības uzlabošanai noderēs Jūsu atsauksmes, ieteikumi, konstruktīvā kritika. Tos vari nosūtīt uz ", 
             tags$a( href="mailto:admin@arcijz.id.lv",
                     "admin@arcijz.id.lv",
                     target = '_blank'),
             "."
      )
   )
}

## data_source of the report ---------------------
data_source <- function(){
   fluidRow(
      h2(paste0('Kas ir datu avoti?')),
      tags$ol(
         tags$li( "Dati tiek iegūti no Oficiālās statistikas portāla, izmantojot API iespējas. Par to vairāk iespējams uzzināt: ", 
                  tags$a(tags$i("Oficiālās statistikas portāla API"),
                         href = "https://stat.gov.lv/lv/api-un-kodu-vardnicas/api",
                         target = "_blank"), 
                  ". Tiek izmantota informācija no sadaļām: ",
                  tags$a(tags$i("Rūpniecība"),
                         href = "https://stat.gov.lv/lv/statistikas-temas/noz/rupnieciba",
                         target = "_blank"),
                  " un ",
                  tags$a(tags$i("Uzņēmējdarbības prognozes (konjunktūras rādītāji)"),
                         href = "https://stat.gov.lv/lv/statistikas-temas/valsts-ekonomika/konjunktura",
                         target = "_blank")
         )
      #   tags$li("Goods exports and imports by country and commodity are sourced and compiled from ",
      #           tags$a("the overseas merchiandise trade datasets",
      #                  href = "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx",
      #                  target = "_blank"),
      #           " from Statistics New Zealand."
      #   ),
      #   tags$li("Services exports and imports by country are sourced from ",
      #           tags$a(tags$i("BPM6 Services by country, year ended in quarter (Qrtly-Mar/Jun/Sep/Dec)") ,
      #                  href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
      #                  target = "_blank"),
      #           ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand. For countries whose data are not available from this source, ",
      #           tags$a( tags$i("Goods and services trade by country: Year ended Qtr Year map data CSV"),
      #                   href = "https://www.stats.govt.nz/information-releases/goods-and-services-trade-by-country-year-ended-june-2018",
      #                   target = "_blank" ),
      #           "is then used."
      #           ),
      #tags$li("Data used in the global trade analysis are sourced from ",
      #        tags$a(tags$i("UN Comtrade, International Trade Statistics Database") ,
      #               href = "https://comtrade.un.org/",
      #               target = "_blank"),
      #        ", by using its ", 
      #        tags$a(tags$i("API"),
      #               href = "https://comtrade.un.org/data/dev/portal",
      #               target = "_blank"),
      #        " via an R package called ",
      #        tags$a(tags$i("comtradr"),
      #               href = "https://cran.r-project.org/web/packages/comtradr/index.html",
      #               target = "_blank"),
      #        ". Please note that the maximum number of queries is 100 per hour."
      #        ),
      #tags$li("Directional basis stock of direct investment are sourced from ",
      #        tags$a(tags$i("BPM6 Annual, Directional basis stock of direct investment by country (Annual-Mar)") ,
      #               href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
      #               target = "_blank"),
      #        ", a table under Economic indicators and International Investment Position - IIP from Inforshare Statistics New Zealand."),
      #tags$li("New Zealand visitor travelling overseas data is sourced from ",
      #        tags$a(tags$i("NZ-resident traveller departures by EVERY country of main dest and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
      #               href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
      #               target = "_blank"),
      #        ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand."),
      #tags$li("Foreign visitor travelling to New Zealand data is sourced from ",
      #        tags$a(tags$i("Visitor arrivals by EVERY country of residence and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
      #               href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
      #               target = "_blank"),
      #        ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand.")
      )
   )
}


## when the dashobard will be updated ---------------------
when_update <- function() {
   fluidRow(
      h2(paste0('Kad darbpanelī iekļautā informācija tiks atjaunota?')),
      tags$p(
         "Darbpanelī iekļautā informācija tiek atjaunota dienā, kad tiek publicēta ikmēneša Centrālās statistikas pārvaldes preses relīze par situāciju Latvijas rūpniecībā.
         Tā kā datu imports panelī šobrīd notiek ar pāris manuālām darbībām, datu atjaunošana atkarīga no manām iespējām attiecīgajā mirklī veikt datu atjaunošanu."
      )
   )
}

## hs_code_explain of the report ---------------------
hs_code_explain <- function() {
   fluidRow(
      h2(paste0('Kas ir CN un PRODCOM kodi?')),
      tags$p(
         "The Harmonized Commodity Description and Coding System, also known as the Harmonized System (HS) of tariff nomenclature is an internationally standardized system of names and numbers to classify traded products. It came into effect in 1988 and has since been developed and maintained by the World Customs Organization (WCO) (formerly the Customs Co-operation Council), an independent intergovernmental organization based in Brussels, Belgium, with over 200 member countries."
         ),
      tags$p("More information on New Zealand harmonised system classification can be found ", 
             tags$a( href="http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS2017.aspx",
                     "here.",
                     target = '_blank') 
             ),
      tags$p("In addition, the Customs Service are responsible for the classification of goods at the border. More information can be found ", 
             tags$a( href="https://www.customs.govt.nz/business/tariffs/tariff-classifications-and-rates/",
                     "here.",
                     target = '_blank') )
   )
}

### trade terms explained-----------------
trade_terms <- function(){
   fluidRow(
      h2(paste0('Ko nozīmē šie termini?')),
      #tags$ol(
         tags$li( tags$b("Trade balance, trade surplus or trade deficit:"), tags$br(),
                  "The balance of trade, commercial balance, or net exports (sometimes symbolized as NX), is the difference between the monetary value of a nation's exports and imports over a certain period. Sometimes a distinction is made between a balance of trade for goods versus one for services. If a country exports a greater value than it imports, it has a trade surplus, if a country imports a greater value than it exports, it has a trade deficit."
         ),
         tags$li( tags$b("Two-way trade:"), tags$br(),
                 "The sum of total exports and imports."
         )
      #)
   )
}

### confidential trade data --------------------
confidential_trade_data <- function(){
   fluidRow(
      h2(paste0('Kādi dati ir konfidencionāli?')),
      tags$p(
         "International Merchandise Trade Statistics confidentiality policy can be found ",
         tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality/international-merchandise-trade-confidentiality-policy.aspx",
                 "here. ",
                 target = '_blank') ,
         "Confidential items in overseas trade and cargo statistics can be found ",
          tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality.aspx",
                 "here, ",
                 target = '_blank'),
         "where you also find the confidential items for both exports and imports."
      )
   )
}

### Urgent updates explained-----------------
urgent_updates <- function(){
   fluidRow(
      h2(paste0('Pēdējā atjaunošana:')),
      tags$li( "Pēdējo reizi dati atjaunoti 8. martā līdz ar preses relīzes par janvāri publicēšanu, kas atrodama ",
              tags$a( href="https://stat.gov.lv/lv/statistikas-temas/noz/rupnieciba/preses-relizes/6262-rupniecibas-produkcija-2021-gada-janvari",
                      "šeit.",
                      target = '_blank'))
   )
}



# var conall = $(this.chart.container).parents(".shinyjs-hide).find("#CIExportPercentLine");
## shared legend for highchager ---------------------
sharelegend = JS('function(event){
                  var vis = this.visible;
                 var conall = $(this.chart.container).parents(".row").find("div.highchart");
                 for(var i = 0; i < conall.length; i++){
                    var hc = $(conall[i]).highcharts();
                    var series = hc.get(this.options.id);
                    if(series){
                       if(vis){
                          series.hide();
                        } else{
                          series.show();
                        }
                     }
                 }
                 return false;
                 }')

sharelegend = JS("function (event) {
                    var visibility = this.visible ? 'visible' : 'hidden';
                 if (!confirm('The series is currently ' +
                 visibility + '. Do you want to change that?')) {
                 return false;
                 }
                 }")



######## map heat map -----------------
hc_add_series_treemap2 = with(environment(hctreemap),
                              ## Modified `hc_add_series_treemap`
                              ## names colorValue correctly for connection to `hc_colorAxis`
                              function (hc, tm, ...)
                              {
                                 assertthat::assert_that( is.highchart(hc),is.list(tm))
                                 df <- tm$tm %>% tbl_df() %>% select_("-x0", "-y0", "-w", 
                                                                      "-h", "-stdErr", "-vColorValue") %>% rename_(value = "vSize", 
                                                                                                                   colorValue = "vColor") %>% purrr::map_if(is.factor, as.character) %>% 
                                    data.frame(stringsAsFactors = FALSE) %>% tbl_df()
                                 ndepth <- which(names(df) == "value") - 1
                                 ds <- map_df(seq(ndepth), function(lvl) {
                                    df2 <- df %>% filter_(sprintf("level == %s", lvl)) %>% 
                                       rename_(name = names(df)[lvl]) %>% mutate_(id = "highcharter::str_to_id(name)")
                                    if (lvl > 1) {
                                       df2 <- df2 %>% mutate_(parent = names(df)[lvl - 1], 
                                                              parent = "highcharter::str_to_id(parent)")
                                    }
                                    else {
                                       df2 <- df2 %>% mutate_(parent = NA)
                                    }
                                    df2
                                 })
                                 ds <- list_parse(ds)
                                 ds <- map(ds, function(x) {
                                    if (is.na(x$parent)) 
                                       x$parent <- NULL
                                    x
                                 })
                                 hc %>% hc_add_series(data = ds, type = "treemap", ...)
                              }
)


# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
   paste("Atsauksmēm & ieteikumiem")
}

customSentence_share <- function(numItems, type) {
   paste("Tev patīk? Tad padalies!")
}

##
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
   type <- match.arg(type)
   if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
   items <- c(list(...), .list)
   lapply(items, shinydashboard:::tagAssert, type = "li")
   dropdownClass <- paste0("dropdown ", type, "-menu")
   if (is.null(icon)) {
      icon <- switch(type, messages = shiny::icon("envelope"), 
                     notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
   }
   numItems <- length(items)
   if (is.null(badgeStatus)) {
      badge <- NULL
   }
   else {
      badge <- tags$span(class = paste0("label label-", badgeStatus), 
                    numItems)
   }
   tags$li(
      class = dropdownClass, 
      a(
         href = "#", 
         class = "dropdown-toggle", 
         `data-toggle` = "dropdown", 
         icon, 
         badge
      ), 
      tags$ul(
         class = "dropdown-menu", 
         tags$li(
            class = "header", 
            customSentence(numItems, type)
         ), 
         tags$li(
            tags$ul(class = "menu", items)
         )
      )
   )
}