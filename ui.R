#### Jaunzēlandes tirdzniecības analīzes rīks, pielāgots LV rūpniecības vajadzībām.

tags$li(class="dropdown",tags$style(list("*{font-family: 'Arial'}")))
####
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
options(spinner.color="#006272")
library(timevis)

## Datu ielāde no CSP un rīkā glabātajiem arhīviem...
load("./CSP_data.rdata")

## Būs nepieciešams izveidot rīku, kas no CSP datu ielases noteiks pašreizējo gadu un mēnesi.
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

maxYear_lb <- paste0( str_to_sentence(substr(actualMonth, 1, 3 ))," ", substr(actualYear, nchar(actualYear)-1, nchar(actualYear)))  


## Palīgfunkciju ielāde
source('helper_funs.R', encoding = "UTF-8")


## build ui.R -----------------------------------
## 1. Galvene -------------------------------
header <- 
   dashboardHeader( 
                     title = HTML("LATVIJAS RŪPNIECĪBA"),
                    
                    disable = FALSE, 
                    titleWidth  = 325,
                    dropdownMenuCustom( type = 'message',
                                        customSentence = customSentence,
                                  messageItem(
                                     from = "admin@arcijz.id.lv",#'Atsauksmēm un ieteikumiem',
                                     message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                     icon = icon("envelope"),
                                     href = "mailto:admin@arcijz.id.lv"
                                  ),
                                  icon = icon('comment')
                                 ),
                    # Dalīšanās linki tālākā attīstības posmā jāpilnveido...
                    dropdownMenuCustom( type = 'message',
                                        customSentence = customSentence_share,
                                  icon = icon("share-alt"),
                                  messageItem(
                                     from = 'Twitter',
                                     message = "",
                                     icon = icon("twitter"),
                                     href = "https://twitter.com/intent/tweet?text=Latvijas%20rupniecības%20darbpanelis&url=https://shiny.kalasniks.lv/shiny/rupnieciba/"
                                  ),
                                  messageItem(
                                     from = 'Facebook',
                                     message = "",
                                     icon = icon("facebook"),
                                     href = "https://www.facebook.com/"
                                  ),
                                  messageItem(
                                     from = 'LinkedIn',
                                     message = "",
                                     icon = icon("linkedin"),
                                     href = "https://www.linkedin.com/sharing/share-offsite/?url=https://shiny.kalasniks.lv/shiny/rupnieciba/"
                                  )
                                  )
                    
   )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://shiny.kalasniks.lv/shiny/rupnieciba/',
                                             tags$img(src='MBIELogo/dashboard-217-2.png'),
                                             target = '_blank', align = 'left') #,height='67',width='228.6'



## 2. sānu panelis------------------------------
siderbar <- 
   dashboardSidebar( 
      tags$style(HTML("*{font-family: 'Arial'}")),
      width = 225,
      sidebarMenu(
         id = 'sidebar',
         style = "position: relative; overflow: visible;",
         #style = "position: relative; overflow: visible; overflow-y:scroll",
         #style = 'height: 90vh; overflow-y: auto;',
         ## 1st tab show the Main dashboard -----------
         menuItem( "Īstermiņš", tabName = 'dashboard', icon = icon('dashboard'),
                   badgeLabel = maxYear_lb, badgeColor = "orange" ),
         
         ## add conditional panel to show more
         # conditionalPanel( "input.sidebar === 'dashboard'",
         #                   actionButton("btn_show_more",
         #                                paste0(' Show more details'),
         #                                icon = icon('chevron-circle-down'),
         #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
         #                                ) 
         #                   ),
         
         
         
         ## 2nd Second tab shows the country/region level tab --------------
         menuItem("Nozaru analīze", tabName = 'country_intel', icon = icon('globe') ),
         div( id = 'sidebar_cr',
              conditionalPanel("input.sidebar === 'country_intel'",
                               selectizeInput("select_country",
                                              "Izvēlies vienu vai vairākas interesējošās nozares", 
                                              choices =  list_nozares, 
                                              selected = NULL,  width = "200px",
                                              multiple = T), #,
                               #actionButton('btn_country','Submit')
                               
                               ## action button to build report
                               actionButton('btn_build_country_report', 
                                            paste0('Izveidot atskaiti'),
                                            icon = icon('wrench')),
                               
                               ## reset side bar selectoin
                               actionButton('btn_reset_cr',
                                            'Atiestatīt',
                                            icon = icon('refresh') )
                               
              )),
         
         ## 5th tab Data source, definition , i.e., help ---------------
         menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
         
         ## 6th tab monthly update ----------------------
         menuItem( "Preses relīzes", tabName = 'monthly_update', icon = icon('bell'),
                   badgeLabel = "jauns", badgeColor = "green" )
         )
   )

## 3. body --------------------------------
body <- dashboardBody( tags$style(HTML("*{font-family: 'Arial'}")),
   
   ## 3.0. CSS styles in header ----------------------------
   tags$head(

      tags$script("document.title = 'Latvijas Rūpniecības statistikas panelis'"),

      ### Styles 
      tags$style(HTML("*{font-family: 'Arial'}")),
      tags$style(HTML(".small-box {height: 175px}")),
      tags$style(HTML(".fa { font-size: 35px; }")),
      tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
      tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
      tags$style(HTML(".fa-globe { font-size: 20px; }")),
      tags$style(HTML(".fa-barcode { font-size: 20px; }")),
      tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
      tags$style(HTML(".fa-wrench { font-size: 15px; }")),
      tags$style(HTML(".fa-refresh { font-size: 15px; }")),
      tags$style(HTML(".fa-search { font-size: 15px; }")),
      tags$style(HTML(".fa-comment { font-size: 20px; }")),
      tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
      tags$style(HTML(".fa-envelope { font-size: 20px; }")),
      tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
      tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
      tags$style(HTML(".fa-bell { font-size: 17px; }")),
      tags$style(HTML(".fa-check { font-size: 14px; }")),
      tags$style(HTML(".fa-times { font-size: 14px; }")),
      
      tags$style(HTML(".fa-twitter { font-size: 25px; }")),      #color:red;
      tags$style(HTML(".fa-facebook { font-size: 25px; }")),
      tags$style(HTML(".fa-linkedin { font-size: 25px; }")),
      
      ## modify the dashboard's skin color
      tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #009999;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #009999;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #009999;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #009999;
                                 }
                       ')
                ),
      
      ## modify icon size in the sub side bar menu
      tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
                  )) ,
      
      tags$style( HTML("hr {border-top: 1px solid #009999;}") ),
      
      ## to not show error message in shiny
      tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
      tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
      
      ## heand dropdown menu size
      #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
      tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
      tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
      tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
      ),
       
   ## 3.1 Dashboard body --------------
   tabItems(
      ## 3.1 Main dashboard ----------------------------------------------------------
      tabItem( tabName = 'dashboard',
               ## contents for the dashboard tab
               div(id = 'main_wait_message',
                   h1('Lūdzu uzgaidiet! Aplikācija apstrādā statistikas datus ...',
                      style = "color:darkblue; font-family: 'Arial" , align = "center" ) ,
                   tags$hr()
                   ),
               
               # 1.1 Export/import board ---------------------------
               #div(class = 'scroller_anchor'),
               #div(class = 'scroller', ) ,
               
               h1(paste0("Situācija Latvijas rūpniecībā ", actualYear,". gada ",actualMonth), style = "font-family: 'Arial") ,
               h2(paste0('Rūpniecībā kopā (BCD)'), style = "font-family: 'Arial"),
               fluidRow(
                  valueBoxOutput("BCD_CaltBox") %>% withSpinner(type=4),
                  valueBoxOutput("BCD_SeztBox"),
                  valueBoxOutput("BCD_gadaBox")
               ),

               h2(paste0("Ieguves rūpniecība (B)"), style = "font-family: 'Arial"),
               fluidRow(
                  valueBoxOutput("B_CaltBox") ,
                  valueBoxOutput("B_SeztBox") ,
                  valueBoxOutput("B_gadaBox")
                  ),

               h2(paste0("Apstrādes rūpniecība (C)"), style = "font-family: 'Arial"),
               fluidRow(
                  valueBoxOutput("C_CaltBox") ,
                  valueBoxOutput("C_SeztBox") ,
                  valueBoxOutput("C_gadaBox")
                  ) ,
               
               h2(paste0("Elektroenerģija un gāzes apgāde (D)"), style = "font-family: 'Arial"),
               fluidRow(
                  valueBoxOutput("D_CaltBox") ,
                  valueBoxOutput("D_SeztBox") ,
                  valueBoxOutput("D_gadaBox")
               ) ,
               
               ## 1.2 Time series plot ----------------------------------------
               h2(paste0("Rūpniecības attīstība un detalizētāka apstrādes rūpniecības nozaru attīstība"), style = "font-family: 'Arial"),
               fluidRow( column( width = 6,h4("Rūpniecības pamatnozaru sezonāli izlīdzinātie indeksi", align = 'center'), highchartOutput('BCDLineHc') ),
                         column( width = 6,h4("Apstrādes rūpniecības apakšnozaru pārmaiņas pret iepr. gada atb. mēnesi", align = 'center'),h5("(procentos)", align = 'center'), highchartOutput('CBarHc') )
                         ),
               

               ## 1.3 Table shows growth rate ---------------------------------
               h2(paste0("Īstermiņā un ilgtermiņā notiekošā apstrādes rūpniecības izaugsme"), style = "font-family: 'Arial"),
               p("Pēdējais mēnesis; pēdējie 3 mēneši; pēdējie 6 mēneši; no gada sākuma; pēdējie 12 mēneši pret iepriekšējā gada atbilstošo periodu") ,
               fluidRow( dataTableOutput('GrowthTab')  ),
               
               div( id = 'message_to_show_more',
                    tags$hr(),
                    tags$h3( "Nospiežot 'Parādīt vairāk informācijas' iespējams attēlot papildus informāciju par rūpniecības attīstību raksturojošiem rādītājiem." , style = "font-family: 'Arial"),
                    actionButton("btn_show_more",
                                 paste0(' Parādīt vairāk informācijas'),
                                 icon = icon('chevron-circle-down'),
                                 style='padding-top:3px; padding-bottom:3px;padding-left:5px;padding-right:5px;font-size:120% '
                    ) 
                    ),
               
               div( id = "show_more_detail" ) ,
               
               shinyjs::hidden( div( id = "load_more_message",
                                     tags$hr(),
                                     tags$h1("Ielādē...", align = "center", style = "font-family: 'Arial")  )
                               )

               ),
      
       ## 3.3 country intellgence -----------------------------------------------------
      tabItem( tabName = 'country_intel',
               ## 3.3.1 Help text first -------------- 
               div(id = 'country_howto',
                   howto_country() ) ,
               
               ## 3... wait message ------
               hidden(
                  div( id = 'wait_message_country_intel',
                    h2( "Šobrīd tiek sagatavota atskaite tikai un vienīgi Tev ....." , style = "font-family: 'Arial")
                    )),
               
               ## 3... div to holder created UIs ------
               tags$div( id = 'country_name' ),
               tags$div( id = 'country_info' ),
               tags$div( id = 'country_trade_summary' ),
               tags$div( id = 'country_appendix' ) 
      ),
      
      ## 3.5 Help and info -------------------------------
      tabItem( tabName = 'help',
               ## 3.5.1 Data sources ---------------
               div( id = 'help_contact',
                    contact() ),
               
               div( id = 'help_data_source',
                    data_source() ),
               
               div( id = 'when_to_update',
                    when_update() ), 
               
               #div( id = 'help_hs_code',
               #     hs_code_explain() ),
               
               #div( id = 'help_trade_term',
               #     trade_terms() ),
               
               #div( id = 'help_confidential_data',
               #     confidential_trade_data() ),
               
               div( id = 'help_urgent_update',
                    urgent_updates() )
               ),
      
      ## 3.6 Monthly update from Stats NZ --------------
      tabItem( tabName = 'monthly_update',
               div( id = 'monthly_update',
                    fluidRow( htmlOutput('MonthlyUpdate') )
               ))
   )
)



## put UI together --------------------
ui <- 
   dashboardPage(header, siderbar, body )
