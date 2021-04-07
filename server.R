
## Ielādē failu, kas kopīgs gan UI, gan SERVER
source('share_load.R', encoding = "UTF-8")

## Servera puse aplikācijai: server.R
server <- 
   function(input, output, session) {
      ## I. Galvenais darbpanelis (īstermiņa statistika) -----------------------------
      i_prog <- 1
      tot_step <- 12
      
      # 1. Vērtību kastītes  ---------------------------------------------------------
      ## Progresa līknes...
      withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
         # Palielina progresa līknes garumu, un atjauno detaļu tekstu.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1

      
         ## IEGUVES RŪPNIECĪBA VALUEBOX
      Temp<-dplyr::filter(datacal_df, Nozare=="B")
      Temp[is.na(Temp)] <- 0
      hc1 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "column", hcaes(Periods, Kalendarie), name = "Izmaiņas pret atb. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      B_cal<-dplyr::filter(Temp, Nozare=="B" & Periods==Temp$Periods[nrow(Temp)])
      output$B_CaltBox <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(B_cal$Kalendarie[1]>0,'+',''),B_cal$Kalendarie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējā gada atbilstošo mēnesi"),
         sparkobj = hc1,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse(B_cal$Kalendarie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse(B_cal$Kalendarie[1]>0, 'green', 'red' ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
         
         #renderValueBox({
         #valueBox(
         #   VB_style(paste0(ifelse(B_cal$Kalendarie[1]>0,'+',''),B_cal$Kalendarie[1], " %"),  "font-size: 60%;"  ),
         #   "Izlaide pret iepriekšējā gada atbilstošo mēnesi", 
         #   icon = icon(ifelse(B_cal$Kalendarie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'), 
         #   color = ifelse(B_cal$Kalendarie[1]>0, 'green', 'red' ),
         #   href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
         #)
      #})
      
      ###
      Temp<-dplyr::filter(datasez_df, Nozare=="B")
      Temp[is.na(Temp)] <- 0
      hc2 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "line", hcaes(Periods, Sezonalie), name = "Izmaiņas pret iepr. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      B_sez<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$B_SeztBox <- renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(B_sez$Sezonalie[1]>0,'+',''),B_sez$Sezonalie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējo mēnesi"),
         sparkobj = hc2,
         subtitle = NULL,
         info = "Pēc sezonāli izlīdzinātiem datiem",
         icon = icon(ifelse(B_sez$Sezonalie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),# icon("sign-out"),
         width = 4,
         color = ifelse(B_sez$Sezonalie[1]>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
         
         #renderValueBox({
         #valueBox(
         #   VB_style(paste0(ifelse(B_sez$Sezonalie[1]>0,'+',''),B_sez$Sezonalie[1], " %"),  "font-size: 60%;"  ),
         #   "Izlaide pret iepriekšējo mēnesi",  
         #   icon = icon(ifelse(B_sez$Sezonalie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),# icon("sign-out"),
         #   color = ifelse(B_sez$Sezonalie[1]>0, 'lime', 'maroon'  ),
         #   href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
         #)
      #})
      
      ###
      Temp<-datagada_df
      Temp[is.na(Temp)] <- 0
      Temp2<-dplyr::filter(datagada_all_df, Nozare == "B")
      Temp2<-dplyr::filter(Temp2, substr(Periods, 6,7)==substr(Temp2$Periods[nrow(Temp2)],6,7))
      Temp2$`No gada sakuma`<-round(Temp2$`No gada sakuma`-100,1)
      hc3 <- hchart(Temp2[(nrow(Temp2)-18):nrow(Temp2),], "column", hcaes(Periods, `No gada sakuma`), name = "Izmaiņas no gada sākuma pret iepr. gada atb. periodu")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      B_gada<-dplyr::filter(Temp, Nozare=="B", )
      output$B_gadaBox  <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse((B_gada$`No gada sakuma`[1]-100)>0,'+',''),round((B_gada$`No gada sakuma`[1]-100),1), " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide no gada sākuma pret iepriekšējo gadu"),
         sparkobj = hc3,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse((B_gada$`No gada sakuma`[1]-100)>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse((B_gada$`No gada sakuma`[1]-100)>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI030m/"
      ))
         
     #    renderValueBox({
     #    valueBox(
     #       VB_style(paste0(ifelse((B_gada$`No gada sakuma`[1]-100)>0,'+',''),round((B_gada$`No gada sakuma`[1]-100),1), " %"),  "font-size: 100%;"  ),
     #       "Izlaide no gada sākuma pret iepriekšējo gadu", 
     #       icon = icon(ifelse((B_gada$`No gada sakuma`[1]-100)>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
     #       color = ifelse((B_gada$`No gada sakuma`[1]-100)>0, 'lime', 'maroon'  ),
     #       href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI030m/"
     #    )
     # })
      
      ## APSTRĀDES RŪPNIECĪBA VALUEBOX
      
      Temp<-dplyr::filter(datacal_df, Nozare=="C")
      Temp[is.na(Temp)] <- 0
      hc4 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "column", hcaes(Periods, Kalendarie), name = "Izmaiņas pret atb. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      C_cal<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$C_CaltBox <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(C_cal$Kalendarie[1]>0,'+',''),C_cal$Kalendarie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējā gada atbilstošo mēnesi"),
         sparkobj = hc4,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse(C_cal$Kalendarie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse(C_cal$Kalendarie[1]>0, 'green', 'red' ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-dplyr::filter(datasez_df, Nozare=="C")
      Temp[is.na(Temp)] <- 0
      hc5 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "line", hcaes(Periods, Sezonalie), name = "Izmaiņas pret iepr. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      C_sez<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$C_SeztBox <- renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(C_sez$Sezonalie[1]>0,'+',''),C_sez$Sezonalie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējo mēnesi"),
         sparkobj = hc5,
         subtitle = NULL,
         info = "Pēc sezonāli izlīdzinātiem datiem",
         icon = icon(ifelse(C_sez$Sezonalie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),# icon("sign-out"),
         width = 4,
         color = ifelse(C_sez$Sezonalie[1]>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-datagada_df
      Temp[is.na(Temp)] <- 0
      Temp2<-dplyr::filter(datagada_all_df, Nozare == "C")
      Temp2<-dplyr::filter(Temp2, substr(Periods, 6,7)==substr(Temp2$Periods[nrow(Temp2)],6,7))
      Temp2$`No gada sakuma`<-round(Temp2$`No gada sakuma`-100 ,1)
      hc6 <- hchart(Temp2[(nrow(Temp2)-18):nrow(Temp2),], "column", hcaes(Periods, `No gada sakuma`), name = "Izmaiņas no gada sākuma pret iepr. gada atb. periodu")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      C_gada<-dplyr::filter(Temp, Nozare=="C", )
      output$C_gadaBox  <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse((C_gada$`No gada sakuma`[1]-100)>0,'+',''),round((C_gada$`No gada sakuma`[1]-100),1), " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide no gada sākuma pret iepriekšējo gadu"),
         sparkobj = hc6,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse((C_gada$`No gada sakuma`[1]-100)>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse((C_gada$`No gada sakuma`[1]-100)>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI030m/"
      ))
      
      ## ELEKTROENERĢIJAS UN GĀZES APGĀDES VALUEBOX
      Temp<-dplyr::filter(datacal_df, Nozare=="D_X_D353")
      Temp[is.na(Temp)] <- 0
      hc7 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "column", hcaes(Periods, Kalendarie), name = "Izmaiņas pret atb. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      D_cal<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$D_CaltBox <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(D_cal$Kalendarie[1]>0,'+',''),D_cal$Kalendarie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējā gada atbilstošo mēnesi"),
         sparkobj = hc7,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse(D_cal$Kalendarie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse(D_cal$Kalendarie[1]>0, 'green', 'red' ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-dplyr::filter(datasez_df, Nozare=="D_X_D353")
      Temp[is.na(Temp)] <- 0
      hc8 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "line", hcaes(Periods, Sezonalie), name = "Izmaiņas pret iepr. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      D_sez<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$D_SeztBox <- renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(D_sez$Sezonalie[1]>0,'+',''),D_sez$Sezonalie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējo mēnesi"),
         sparkobj = hc8,
         subtitle = NULL,
         info = "Pēc sezonāli izlīdzinātiem datiem",
         icon = icon(ifelse(D_sez$Sezonalie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),# icon("sign-out"),
         width = 4,
         color = ifelse(D_sez$Sezonalie[1]>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-datagada_df
      Temp[is.na(Temp)] <- 0
      Temp2<-dplyr::filter(datagada_all_df, Nozare == "D_X_D353")
      Temp2<-dplyr::filter(Temp2, substr(Periods, 6,7)==substr(Temp2$Periods[nrow(Temp2)],6,7))
      Temp2$`No gada sakuma`<-round(Temp2$`No gada sakuma`-100,1)
      hc9 <- hchart(Temp2[(nrow(Temp2)-18):nrow(Temp2),], "column", hcaes(Periods, `No gada sakuma`), name = "Izmaiņas no gada sākuma pret iepr. gada atb. periodu")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      D_gada<-dplyr::filter(Temp, Nozare=="D_X_D353", )
      output$D_gadaBox  <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse((D_gada$`No gada sakuma`[1]-100)>0,'+',''),round((D_gada$`No gada sakuma`[1]-100),1), " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide no gada sākuma pret iepriekšējo gadu"),
         sparkobj = hc9,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse((D_gada$`No gada sakuma`[1]-100)>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse((D_gada$`No gada sakuma`[1]-100)>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI030m/"
      ))
      
      ## KOPĒJĀS RŪPNIECĪBAS VALUEBOX
      Temp<-dplyr::filter(datacal_df, Nozare=="B_C_D")
      Temp[is.na(Temp)] <- 0
      hc10 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "column", hcaes(Periods, Kalendarie), name = "Izmaiņas pret atb. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      BCD_cal<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$BCD_CaltBox <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(BCD_cal$Kalendarie[1]>0,'+',''),BCD_cal$Kalendarie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējā gada atbilstošo mēnesi"),
         sparkobj = hc10,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse(BCD_cal$Kalendarie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse(BCD_cal$Kalendarie[1]>0, 'green', 'red' ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-dplyr::filter(datasez_df, Nozare=="B_C_D")
      Temp[is.na(Temp)] <- 0
      hc11 <- hchart(Temp[(nrow(Temp)-18):nrow(Temp),], "line", hcaes(Periods, Sezonalie), name = "Izmaiņas pret iepr. mēn.")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      BCD_sez<-dplyr::filter(Temp, Periods==Temp$Periods[nrow(Temp)])
      output$BCD_SeztBox <- renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse(BCD_sez$Sezonalie[1]>0,'+',''),BCD_sez$Sezonalie[1], " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide pret iepriekšējo mēnesi"),
         sparkobj = hc11,
         subtitle = NULL,
         info = "Pēc sezonāli izlīdzinātiem datiem",
         icon = icon(ifelse(BCD_sez$Sezonalie[1]>0,'arrow-up','arrow-down'), lib = 'glyphicon'),# icon("sign-out"),
         width = 4,
         color = ifelse(BCD_sez$Sezonalie[1]>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
      ))
      
      ###
      Temp<-datagada_df
      Temp[is.na(Temp)] <- 0
      Temp2<-dplyr::filter(datagada_all_df, Nozare == "B_C_D")
      Temp2<-dplyr::filter(Temp2, substr(Periods, 6,7)==substr(Temp2$Periods[nrow(Temp2)],6,7))
      Temp2$`No gada sakuma`<-round(Temp2$`No gada sakuma`-100 ,1)
      hc12 <- hchart(Temp2[(nrow(Temp2)-18):nrow(Temp2),], "column", hcaes(Periods, `No gada sakuma`), name = "Izmaiņas no gada sākuma pret iepr. gada atb. periodu")  %>% 
         hc_size(height = 75) %>% 
         hc_credits(enabled = FALSE) %>% 
         hc_add_theme(hc_theme_sparkline_vb()) 
      BCD_gada<-dplyr::filter(Temp, Nozare=="B_C_D", )
      output$BCD_gadaBox  <-renderValueBox( valueBoxSpark(
         value = VB_style(paste0(ifelse((BCD_gada$`No gada sakuma`[1]-100)>0,'+',''),round((BCD_gada$`No gada sakuma`[1]-100),1), " %"),  "font-size: 100%;"  ),
         title = toupper("Izlaide no gada sākuma pret iepriekšējo gadu"),
         sparkobj = hc12,
         subtitle = NULL,
         info = "Pēc kalendāri izlīdzinātiem datiem",
         icon = icon(ifelse((BCD_gada$`No gada sakuma`[1]-100)>0,'arrow-up','arrow-down'), lib = 'glyphicon'),
         width = 4,
         color = ifelse((BCD_gada$`No gada sakuma`[1]-100)>0, 'green', 'red'  ),
         href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI030m/"
      ))
      
      
      # 2. 4 Rūpniecības sadaļu izlaides līniju grafiks  -----------------------------------------------------------------
      withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      filtnozares=c('B_C_D', 'B', 'C', 'D_X_D353')
      tmp_dtf <-
         sezonalie_df %>%
         filter(Nozare %in% filtnozares) %>%
         mutate(Periods, as.Date(paste0(substr(Periods,1,4),'-',substr(Periods,6,7),'-01'), format = "%Y-%m-%d"))
      names(tmp_dtf)[4]<-'Datums'
      tmp_BCD<-dplyr::filter(tmp_dtf, Nozare=='B_C_D')
      tmp_BCD <- xts(tmp_BCD[c(-1,-2,-4)], order.by=as.POSIXct(tmp_BCD$Datums))
      tmp_B<-dplyr::filter(tmp_dtf, Nozare=='B')
      tmp_B<- xts(tmp_B[c(-1,-2,-4)], order.by=as.POSIXct(tmp_B$Datums))
      tmp_C<-dplyr::filter(tmp_dtf, Nozare=='C')
      tmp_C<-xts(tmp_C[c(-1,-2,-4)], order.by=as.POSIXct(tmp_C$Datums))
      tmp_D<-dplyr::filter(tmp_dtf, Nozare=='D_X_D353')
      tmp_D<-xts(tmp_D[c(-1,-2,-4)], order.by=as.POSIXct(tmp_D$Datums))
      
      output$BCDLineHc <-renderHighchart({
         highchart(type = "stock") %>%
            hc_add_series(tmp_BCD, type='line', name = 'Rūpniecība kopā [BCD]', color='green', lineWidth=6, marker = list(radius = 2), visible = TRUE)%>%
            hc_add_series(tmp_B, type='line', name = 'Ieguves rūpniecība [B]', color = 'blue', dashStyle='ShortDash',  marker = list(radius = 2), visible = FALSE)%>%
            hc_add_series(tmp_C, type='line', name = 'Apstrādes rūpniecība [C]', color = 'red', dashStyle='ShortDash', marker = list(radius = 2), visible = FALSE)%>%
            hc_add_series(tmp_D, type='line', name = 'Elektroenerģija un gāzes apgāde [D]', color = 'brown', dashStyle='ShortDash', marker = list(radius = 2), visible = FALSE)%>%
            hc_xAxis(labels = list(format = '{value:%b %y}') ) %>%
            hc_yAxis( title = list(text = "RPI, 2015=100") ) %>%
            hc_tooltip(valueDecimals = 1, crosshairs = FALSE, split = FALSE, shared = TRUE) %>%
            hc_credits(
               enabled = TRUE,
               text = "Centrālā statistikas pārvalde | RUI020m Rūpniecības produkcijas apjoma indeksi un pārmaiņas nozarēs un ražošanas pamatgrupējumos",
               href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
            ) %>%
            hc_rangeSelector( buttons = list(
               list(type = 'all', text = 'Viss'),
               list(type = 'year', count = 10, text = '10G'),
               list(type = 'year', count = 5, text = '5G'),
               list(type = 'year', count = 2, text = '2G'),
               list(type = 'year', count = 1, text = 'Gads'),
               list(type = 'month', count = 6, text = '1/2G')
            ), selected = 2)%>% 
            hc_legend(enabled = TRUE)
      })
      
      # 2.1 Grafiks ar līniju grafiku izmaiņām pret iepriekšējā gada atbilstošo mēnesi  -----------------------------------------------------------------
      withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      filtnozares=c('C','C10','C11','C13','C14','C16','C17','C18','C20','C22','C23','C25','C27','C28','C29','C31','C32','C33')
      filtnozares_NOS=c('Apstrādes rūpniecība [C]', '--Pārtikas produktu ražošana [C10]', '--Dzērienu ražošana [C11]',
                    '--Tekstilizstrādājumu ražošana [C13]','--Apģērbu ražošana [C14]','--Koksnes, koka un korķa izstrādājumu ražošana, izmņemot mēbeles [C16]',
                    '--Papīra un papīra izstrādājumu ražošana [C17]','--Poligrāfija un ierakstu reproducēšana [C18]','--Ķīmisko vielu un ķīmisko produktu ražošana [C20]',
                    '--Gumijas un plastmasas izstrādājumu ražošana [C22]','--Nemetālisko minerālu izstrādājumu ražošana [C23]','--Gatavo metālizstrādājumu ražošana [C25]',
                    '--Elektrisko iekārtu ražošana [C27]','--Citur neklasificētu iekārtu, mehānismu un darba mašīnu ražošana [C28]','--Automobiļu, piekabju un puspiekabju ražošana [C29]',
                    '--Mēbeļu ražošana [C31]','--Cita veida ražošana [C32]','--Iekārtu un ierīču remonts un uzstādīšana [C33]')
      filtruBAZE<-data.frame(filtnozares, filtnozares_NOS)
      names(filtruBAZE)[1]<-'Nozare'
      
      
      tmp_dtf2 <-
         datacal_df %>%
         filter(Nozare %in% filtruBAZE$Nozare ) %>%
         filter(Periods==datacal_df$Periods[(nrow(datacal_df))])
      tmp_dtf2<-merge(tmp_dtf2, filtruBAZE, by='Nozare', all.x = TRUE)
      tmp_dtf2$negativie <- ifelse(tmp_dtf2$Kalendarie>=0, NA, tmp_dtf2$Kalendarie)
      tmp_dtf2<-tmp_dtf2[order(-tmp_dtf2$Kalendarie),]
      tmp_dtf2$pozitivie<-ifelse(tmp_dtf2$Kalendarie<0, NA, tmp_dtf2$Kalendarie)
      
      output$CBarHc <-renderHighchart({
         highchart() %>%
            hc_chart(type = 'bar') %>%
            hc_yAxis(title = list(text = "Izmaiņas pret iepriekšējā gada atbilstošo mēnesi"))%>%
            hc_xAxis(categories=tmp_dtf2$filtnozares_NOS, title = list(text = "") )%>%
            hc_add_series(name=paste0('Pret ',as.numeric(actualYear)-1,'. gada ',actualMon), data=tmp_dtf2$pozitivie, stack=paste0('Pret ',as.numeric(actualYear)-1,'. gada ',actualMon), color='#8bbc21' )%>%
            hc_add_series(name=paste0('Pret ',as.numeric(actualYear)-1,'. gada ',actualMon), data=tmp_dtf2$negativie, stack=paste0('Pret ',as.numeric(actualYear)-1,'. gada ',actualMon), color='#910000' )%>%
            hc_plotOptions(bar = list(
               #stacking="normal",
               pointWidth=10,
               dataLabels = list(enabled = TRUE)
            ))%>%
            hc_credits(
               enabled = TRUE,
               text = "Centrālā statistikas pārvalde | RUI020m Rūpniecības produkcijas apjoma indeksi un pārmaiņas nozarēs un ražošanas pamatgrupējumos",
               href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI020m/"
            ) %>%
            hc_legend(enabled = FALSE)
      })
      

      # 3. Attīstība dažāda ilguma periodos ---------------------------------------------------
      withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      filtnozares2<-c('B_C_D', 'B', unlist(filtnozares), 'D_X_D353')
      filtnozares2_nos<-c('Rūpniecība kopā [BCD]','Ieguves rūpniecība [B]',unlist(filtnozares_NOS),'Elektroenerģija un gāzes apgāde [D]')
      filtruBAZE2<-data.frame(filtnozares2, filtnozares2_nos)
      names(filtruBAZE2)[1]<-'Nozare'
      tmp_dtf3<-filter(svari_df, ((Nozare %in% filtnozares2)&(Periods==actualYear)))
      tmp_dtf3<-tmp_dtf3[,-2]
      tmp_dtf3[2]<-tmp_dtf3[2]/100
      izmainas<-filter(kalendarie_df, Nozare %in% filtnozares2)
      izmainas<-dcast(izmainas, Nozare ~ Periods)
      adtf<-filter(datagada_df, ((Nozare %in% izmainas$Nozare)&(Periods==datagada_df$Periods[(nrow(datagada_df))])))
      adtf[3]<-round((adtf[3]-100)/100,3)
      izmainas2<-data.frame(izmainas$Nozare, M1<-round(izmainas[,ncol(izmainas)]/izmainas[,ncol(izmainas)-12]-1,3),
                            M3<-round(rowMeans(izmainas[,(ncol(izmainas)-2):ncol(izmainas)])/rowMeans(izmainas[,(ncol(izmainas)-14):(ncol(izmainas)-12)])-1,3), 
                            M6<-round(rowMeans(izmainas[,(ncol(izmainas)-5):ncol(izmainas)])/rowMeans(izmainas[,(ncol(izmainas)-17):(ncol(izmainas)-12)])-1,3),
                            M12<-round(rowMeans(izmainas[,(ncol(izmainas)-11):ncol(izmainas)])/rowMeans(izmainas[,(ncol(izmainas)-23):(ncol(izmainas)-12)])-1,3))
      names(izmainas2)[1]<-'Nozare'
      izmainas2<-merge(izmainas2, adtf[,c(1,3)], by='Nozare', all.x=TRUE)
      names(izmainas2)<-c('Nozare','M1','M3','M6','M12', 'No gada sākuma')
      tmp_dtf3<-merge(tmp_dtf3, izmainas2, by='Nozare', all.x=TRUE)
      tmp_dtf3<-merge(filtruBAZE2, tmp_dtf3, by='Nozare',all.x = TRUE)
      tmp_dtf3<-tmp_dtf3[,-1]
      names(tmp_dtf3)<-c("Nozare", 'Svars, % no NACE sadaļas', 'mēnesis', 'ceturksnis', 'pusgads', 'gads', 'no gada sākuma')
      #tmp_dtf3<-tmp_dtf3[match(filtnozares2, tmp_dtf3$Nozare),]
      
      output$GrowthTab <- renderDataTable({
         datatable( tmp_dtf3,
                    rownames = F,
                    extensions = 'Buttons',
                    options = list(dom = 'Bt', 
                                   pageLength=21,
                                   buttons = c('copy', 'excel', 'pdf', 'print'),
                                   scrollX = TRUE) ,
                    colnames=c("Nozare", 'Svars, % no NACE sadaļas', 'mēnesis', 'ceturksnis', 'pusgads', 'gads', 'no gada sākuma')
                   ) %>%
            formatStyle(columns = 'Nozare',
                        target = 'row',
                        fontWeight = styleEqual(c('Rūpniecība kopā [BCD]', 'Ieguves rūpniecība [B]',
                                                  'Apstrādes rūpniecība [C]', 'Elektroenerģija un gāzes apgāde [D]'), c('bold','bold', 'bold','bold')),
                        backgroundColor = styleEqual(c('Rūpniecība kopā [BCD]', 'Ieguves rūpniecība [B]',
                                                       'Apstrādes rūpniecība [C]', 'Elektroenerģija un gāzes apgāde [D]'), c('lightgrey','lightgrey','lightgrey','lightgrey'))
                       ) %>%
            formatStyle(
               c('mēnesis', 'ceturksnis', 'pusgads', 'gads', 'no gada sākuma'),
               background=color_from_middle(c(tmp_dtf3$mēnesis,tmp_dtf3$ceturksnis, tmp_dtf3$pusgads, tmp_dtf3$gads, tmp_dtf3$`no gada sākuma`),'#910000','#8bbc21'),
               #background = styleColorBar( c(0,max(c(tmp_dtf3$mēnesis,tmp_dtf3$ceturksnis, tmp_dtf3$pusgads, tmp_dtf3$gads, tmp_dtf3$`no gada sākuma`))*2) , 'lightblue'),
               backgroundSize = '100% 90%',
               backgroundRepeat = 'no-repeat',
               backgroundPosition = 'center'
            ) %>%
            formatPercentage( c('Svars, % no NACE sadaļas','mēnesis', 'ceturksnis', 'pusgads', 'gads', 'no gada sākuma'),digit = 1 ) %>%
            formatStyle( columns = c("Nozare", 'Svars, % no NACE sadaļas', 'mēnesis', 'ceturksnis', 'pusgads', 'gads', 'no gada sākuma'), `font-size`= '115%' )
            #formatCurrency( columns = c('Value'), mark = " ", digits = 0)
      })


      ## Vieta pogai, kas parāda vairāk info... -- 
      removeUI( selector = '#main_wait_message' )
      
      # 7.10  Show more button --------------------
      observeEvent( input$btn_show_more,
                    {
                       ## disable the buttone ---
                       shinyjs::disable("btn_show_more")
                       ## --- hide message to show more -----
                       shinyjs::hide(id = 'message_to_show_more')
                       ## --- show loading message ---
                       shinyjs::show( id = "load_more_message" )
                       
                       # 4. Treemap , kas parāda rūpniecības struktūru un pēdējā ceturkšņa izmaiņas ------------------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       dtf_for_treemap<-filter(svari_df, Periods==max(Periods))
                       izmainas3<-dcast(kalendarie_df, Nozare ~ Periods)
                       dtf_for_treemap<-merge(dtf_for_treemap, izmainas3[,c(1, (ncol(izmainas3)-14):ncol(izmainas3))], by='Nozare', all.x=TRUE)
                       Izm3men<-rowMeans(dtf_for_treemap[,c((ncol(dtf_for_treemap)-2):ncol(dtf_for_treemap))])/rowMeans(dtf_for_treemap[,c((ncol(dtf_for_treemap)-14):(ncol(dtf_for_treemap)-12))])*100-100
                       Svari2<-as.numeric(dtf_for_treemap$Svari)/82*100
                       dtf_for_treemap<-cbind(dtf_for_treemap, 'Izm3men'=round(Izm3men,1), 'Svari'=round(Svari2,1))
                       dtf_for_treemap<-dtf_for_treemap[,c(1,19, 20)]
                       dtf_for_treemap<-filter(dtf_for_treemap, !is.na(Izm3men))
                       dtf_for_treemap<-filter(dtf_for_treemap, !Nozare %in% c('D_X_D353', 'B', 'B_C_D', 'C'))
                       #dtf_for_treemap<-rbind(dtf_for_treemap, c('KONFIDENCIONĀLĀS NOZARES', '0', round(100-sum(as.numeric(dtf_for_treemap$Svari)),1)))
                       dtf_for_treemap[, 2] <- as.numeric(dtf_for_treemap[, 2] )
                       dtf_for_treemap[, 3] <- as.numeric(dtf_for_treemap[, 3] )
                       NozaruBAZE<-data.frame('Nozare'<-c('C10','C11','C13','C14','C15','C16', 'C17','C18','C20','C22','C23','C24','C25','C26','C27','C28','C29','C30','C31','C32', 'C33'),
                                              'Nozares nos'<-c('[C10] Pārtikas produktu ražošana','[C11] Dzērienu ražošana','[C13] Tekstilizstrādājumu ražošana',
                                                               '[C14] Apģērbu ražošana','[C15] Ādas un ādas izstrādājumu ražošana','[C16] Koksnes un koka izstrādājumu ražošana',
                                                               '[C17] Papīra ražošana','[C18] Poligrāfija un ierakstu reproducēšana','[C20] Ķīmisko vielu ražošana',
                                                               '[C22] Gumijas un plastmasas izstrādājumu ražošana','[C23] Nemetālisko minerālu ražošana','[C24] Metālu ražošana',
                                                               '[C25] Gatavo metālizstrādājumu ražošana','[C26] Datoru, optisko un elektronisko iekārtu ražošana',
                                                               '[C27] Elektrisko iekārtu ražošana','[C28] Citu mašīnu un iekārtu ražošana', '[C29] Automobiļu, puspiekabju un piekabju ražošana',
                                                               '[C30] Citu transportlīdzekļu ražošana','[C31] Mēbeļu ražošana','[C32] Cita ražošana','[C33] Iekārtu remonts un uzstādīšana'))
                       names(NozaruBAZE)<-c('Nozare','Nozares nosaukums')
                       dtf_for_treemap<-merge(dtf_for_treemap, NozaruBAZE, by='Nozare', all.x = TRUE)
                       
                       pdf(NULL)
                          tmp_tm_ex=treemap(dtf_for_treemap,
                                            index='Nozares nosaukums',
                                            vSize = 'Svari',
                                            vColor = 'Izm3men',
                                            type = 'value',
                                            fun.aggregate = "weighted.mean",
                                            draw = FALSE)
                       
                       output$KeyExTM <- renderHighchart({
                          highchart() %>%
                             hc_add_series_treemap2( tmp_tm_ex , #hctreemap
                                                     allowDrillToNode = TRUE,
                                                     layoutAlgorithm = "squarified",
                                                     levelIsConstant = FALSE,
                                                     levels = list(list(level = 1,
                                                                        dataLabels = list(enabled = TRUE,
                                                                                          style = list(fontSize = '16px',
                                                                                                       fontWeight = 'normal')
                                                                        )
                                                                   )
                                                     )
                             ) %>%
                             hc_chart(backgroundColor = NULL, plotBorderColor = "#555", plotBorderWidth = 2) %>%
                             hc_title(text = "Apstrādes rūpniecības izaugsme pēdējos 3 mēnešos, salīdzinājumā ar iepriekšējā gada atbilstošo periodu") %>%
                             hc_subtitle(text = "Kvadrāta krāsa attēlo nozares pārmaiņas, kvadrāta lielums nozares daļu pievienotajā vērtībā (%)") %>%
                             hc_exporting(enabled = FALSE, formAttributes = list(target = "_blank")) %>%
                             hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                                        Daļa no pievienotās vērtības: {point.value:,.1f} % <br>
                                        Pēdējo 3 mēn. izmaiņas pret atb. periodu: {point.colorValue:,.1f} %") %>% 
                             hc_colorAxis(minColor = tmp_tm_ex$tm$color[which.min(tmp_tm_ex$tm$vColorValue)],
                                          maxColor = tmp_tm_ex$tm$color[which.max(tmp_tm_ex$tm$vColorValue)] ,
                                          labels = list(format = "{value}%", useHTML = TRUE), reversed = FALSE
                             ) %>%
                             hc_credits(
                                enabled = TRUE,
                                text = "Centrālā statistikas pārvalde | RUI020m RUI010 Produkcijas apjoma un apgrozījuma indeksi",
                                href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/?tablelist=true"
                             ) %>%
                             hc_legend(align = "right", layout = "vertical", verticalAlign = "top",
                                       reversed = TRUE , y = 70, symbolHeight = 250, itemMarginTop = 10)
                       })
                       
                       
                       # 5.0 Sezonālie apgrozījuma indeksi -----------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       dtf_for_apgroz<-filter(sez_apgr_df, Virziens == "TOVT")
                       dtf_for_apgroz<-filter(dtf_for_apgroz, Nozare %in% c("B", "C","C10", "C16","C25"))
                       periodi<-c(sez_apgr_df$Periods[(nrow(sez_apgr_df)-20):nrow(sez_apgr_df)])
                       dtf_for_apgroz<-filter(dtf_for_apgroz, Periods %in% periodi)
                       COMPfiltrs<-data.frame(c("B", "C","C10", "C16","C25"),c('Ieguves rūpniecība [B]','Apstrādes rūpniecība [C]','Pārtikas produktu ražošana [C10]','Koksnes un koka izstrādājumu ražošana [C16]','Gatavo metālizstrādājumu ražošana [C25]'))
                       names(COMPfiltrs)<-c('Nozare','Nozare2')
                       dtf_for_apgroz<-merge(dtf_for_apgroz, COMPfiltrs, by='Nozare', all.x = TRUE)
                       
                       ### Apgrozījuma līniju grafiks
                       output$KeyApgrLine <- renderHighchart({
                          highchart() %>%
                             #hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  dtf_for_apgroz[,c(3:5)] ,
                                            mapping = hcaes(  x = Periods, y = Sezonalie, group = Nozare2 ),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c(T, F, F, F, F)
                                            ) %>%
                             hc_xAxis( categories = c( unique( dtf_for_apgroz$Periods) ) ) %>%
                             hc_yAxis( title = list(text = "Indekss, 2015=100")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: {point.y} "),
                                        headerFormat = '<span style="font-size: 13px">Periods: {point.key}</span>'
                             ) %>%
                             hc_credits(
                                enabled = TRUE,
                                text = "Centrālā statistikas pārvalde | RUI040c Apgrozījuma indeksi un pārmaiņas nozarēs",
                                href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/RUI040c/"
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 5.0.1 5 gadi eksports un vietējais tirgus -------------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       
                       ### Grafiks pēdējo 5 gadu apgrozījuma vietējā tirgū un eksportā attīstība !!!!!!!!!!!!!!!!!!!!! UZLABOJAMS AR EKSPORTA/VIETĒJĀ PROCENTIEM !!!!!!!!!!!!!!!!!!!
                       df_for_area<-filter(apgr_nauda_df, Nozare == 'C')
                       df_for_area<-filter(df_for_area, Periods %in% periodi)
                       df_for_area2<-melt(data = df_for_area, id.vars = c('Periods'),
                                          measure.vars = c('Apgrozijums_nauda', 'Vietejais', 'Eksports',
                                                           'Vietejais_nauda','Eksports_nauda'))
                       dtf_for_area3<-filter(df_for_area2, variable %in% c('Vietejais_nauda','Eksports_nauda'))
                       
                       output$KeyApgrPercent <- 
                             renderHighchart({
                                highchart() %>%
                                   hc_plotOptions(area = list(
                                      stacking = "normal")
                                   )%>%
                                   hc_xAxis( categories = c( unique( dtf_for_area3$Periods) ) ) %>%
                                   hc_add_series( data =  filter(dtf_for_area3, variable == 'Eksports_nauda')$value ,
                                                  type = 'area',
                                                  marker = list(symbol = 'circle'),
                                                  name = 'Eksporta apgrozījums'
                                   ) %>%
                                   hc_add_series( data =  filter(dtf_for_area3, variable == 'Vietejais_nauda')$value ,
                                                  type = 'area',
                                                  marker = list(symbol = 'square'),
                                                  name = 'Apgrozījums vietējā tirgū'
                                   ) %>%
                                   hc_chart(type='area') %>%
                                   hc_yAxis( title = list(text = "Apgrozījums, tūkst. EUR")  ) %>%
                                   hc_tooltip(table = TRUE,
                                              sort = TRUE,
                                              pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                    " {series.name}: {point.y} tūkst. EUR"),
                                              headerFormat = '<span style="font-size: 13px">Periods: {point.key}</span>'
                                   ) %>%
                                   hc_credits(
                                      enabled = TRUE,
                                      text = "Centrālā statistikas pārvalde | RUA030c Apstrādes rūpniecības produkcijas izlaide un apgrozījums nozarēs (tūkst. eiro; procentos)",
                                      href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUA/RUA030c/"
                                   ) %>%
                                   hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) 
                             })
                       
                       
                       # 5.1 Ekonomikas sentimenta un konfidences indekss -----------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       menesi_5g<-c(sentimenta_df$Periods[(nrow(sentimenta_df)-60):nrow(sentimenta_df)])
                       timeline_5g<-as.Date(paste0(substr(menesi_5g,1,4),'-',substr(menesi_5g,6,7),'-01'), format = "%Y-%m-%d")
                       
                       tmp_dtf_sentiments<-filter(sentimenta_df[,c(1,3)], Periods %in% menesi_5g)
                       tmp_dtf_sentiments$Sentimenta_izm<-round(tmp_dtf_sentiments$Sentimenta_izm,0)
                       tmp_dtf_sentiments<-cbind(tmp_dtf_sentiments, timeline_5g)
                       #tmp_dtf_sentiments <- xts(tmp_dtf_sentiments[,c(-1)], order.by=as.POSIXct(tmp_dtf_sentiments$timeline_5g ))
                       tmp_dtf_sentiments <- tmp_dtf_sentiments[,c(-1)]
                       tmp_dtf_konfidence<-filter(konfidences_df[,c(3,4)], Periods %in% menesi_5g)
                       tmp_dtf_konfidence<-cbind(tmp_dtf_konfidence, timeline_5g)
                       tmp_dtf_konfidence<- tmp_dtf_konfidence[,c(-1)]
                       
                       ### Ekonomikas sentimenta indeksa un rūpniecības konfidences rādītājs
                       output$SentimentLine <- renderHighchart({
                          highchart() %>%
                             hc_add_series( data = tmp_dtf_sentiments, hcaes(x = timeline_5g, y = Sentimenta_izm),
                                            type = 'line', marker = list(symbol = 'circle'),
                                            name='Ekonomikas sentimenta indekss'
                             ) %>%
                             hc_add_series( data = tmp_dtf_konfidence, hcaes(x = timeline_5g, y = Konfidence),
                                            type = 'line', marker = list(symbol = 'triangle'),
                                            name='Rūpniecības konfidences rādītājs'
                             ) %>%
                             hc_xAxis( labels = list(format = '{value:%b %y}'), categories = timeline_5g ) %>%
                             hc_yAxis( title = list(text = " "), 
                                       labels = list( format = "{value:,.0f}")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: {point.y}"),
                                        headerFormat = '<span style="font-size: 13px">Periods: {point.key:%B %Y}</span>'
                             ) %>%
                             hc_credits(
                                enabled = TRUE,
                                text = "Centrālā statistikas pārvalde | KRE010m KRE020m Ekonomikas sentimenta un konfidences rādītāji",
                                href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__VEK__KR__KRE/?tablelist=true"
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 25, y = -15 )
                       })
                       
                       # 5.1.1 Detalizētāki konjunktūras grafiki -----------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                      
                       ### Rūpniecības konjunktūras rādītāji nākotnes attīstībai un šī brīža situācijai
                       tmp_konjunktur<-filter(konjunkturas_df, Periods %in% menesi_5g)
                       tmp_konjunktur<-cbind(tmp_konjunktur, timelines5g<-as.Date(paste0(substr(tmp_konjunktur$Periods,1,4),'-',substr(tmp_konjunktur$Periods,6,7),'-01'), format = "%Y-%m-%d"))
                       names(tmp_konjunktur)[5]<-'Periods_as_Date'
                       tmp_konjunktur2<-na.omit(tmp_konjunktur)
                       KONJUNKTURA<-data.frame(c('BSEXN3','CBS','CI_IND','EEXN3','EVEN3','EVEN3_CIS','EVEN3_EU','PEN3'),
                                               c('Gaidāmā saimnieciskā darbība turpmākajos 3 mēnešos','Kopējā saimnieciskā darbība šobrīd',
                                                 'Konfidences rādītājs rūpniecībā','Gaidāmā nodarbinātība turpmākajos 3 mēnešos',
                                                 'Eksporta pasūtījumi turpmākajos 3 mēnešos','Eksporta pasūtījumi uz NVS turpmākajos 3 mēnešos',
                                                 'Eksporta pasūtījumi uz ES turpmākajos 3 mēnešos','Gaidāmā ražošanas aktivitāte turpmākajos 3 mēnešos'))
                       names(KONJUNKTURA)<-c('Rādītājs','Rādītājs2')
                       tmp_konjunktur2<-merge(tmp_konjunktur2, KONJUNKTURA, by='Rādītājs',all.x = TRUE)
                       
                        output$KonjunkturLine<- 
                             renderHighchart({
                                highchart()%>%
                                hc_add_series( data =  tmp_konjunktur2,
                                                  mapping = hcaes(  x = Periods_as_Date, y = Konjunktura, group = Rādītājs2 ),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle') ,
                                                  visible = c(F,F,F,F,F,T,T,F)
                                   ) %>%
                                   hc_xAxis( labels = list(format = '{value:%b %y}'), categories = unique(tmp_konjunktur$Periods_as_Date) ) %>%
                                   hc_yAxis( title = list(text = " "), 
                                             labels = list( format = "{value:,.0f}")  ) %>%
                                   hc_plotOptions(line = list(
                                      dataLabels = list(enabled = F),
                                      #stacking = "normal",
                                      enableMouseTracking = T)
                                   )%>%
                                   hc_tooltip(table = TRUE,
                                              sort = TRUE,
                                              pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                    " {series.name}: {point.y}"),
                                              headerFormat = '<span style="font-size: 13px">Periods: {point.key:%B %Y}</span>'
                                   ) %>%
                                   hc_credits(
                                      enabled = TRUE,
                                      text = "Centrālā statistikas pārvalde | KRR030m Konjunktūras rādītāji apstrādes rūpniecībā pa uzņēmumu lieluma grupām (saldo, procentos)",
                                      href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__VEK__KR__KRR/KRR030m/"
                                   ) %>%
                                   hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'bottom', floating = T, x = 25, y = -50 )
                             })
                       
                       # 6. Global trading partners glance ---------------------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       output$TradeMap <- 
                          renderUI({
                             tags$iframe(#srcdoc = paste(readLines("www/Twoway_trade_by_country.html"), 
                                #               collapse = '\n'),
                                src = "Twoway_trade_by_country.html",
                                height="550px", width="100%")
                          })
                       
                       # 7.0 Devumi ----------------------------
                       withProgress(message = 'Ielādē...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       
                       ### Devumu grafiks un devumu grafika aprēķins
                       tmp_df_devumiem<-filter(datacal_df, substr(Periods,1,4)>=2013 )
                       tmp_df_devumiem<-filter(tmp_df_devumiem, Nozare %in% unique(svari_df$Nozare))
                       tmp_df_devumiem<-filter(tmp_df_devumiem, !Nozare %in% c('B_C_D', 'B', 'D_X_D353'))
                       #devumiem2<-dcast(tmp_df_devumiem, Nozare ~ Periods)
                       svari2_df<-dcast(svari_df, Nozare ~ Periods)
                       svari3_df<-svari2_df
                       for (i in 2:ncol(svari2_df)){
                          for (j in 1:nrow(svari2_df)){
                             svari3_df[j,i]=svari2_df[j,i]/svari2_df[2,i]
                          }
                       }
                       svari3_df<-melt(svari3_df, id.vars = c('Nozare'), measure.vars = names(svari3_df)[2:ncol(svari2_df)])
                       names(svari3_df)<-c('Nozare','Gads','Svars')
                       tmp_df_devumiem$Gads<-substr(tmp_df_devumiem$Periods,1,4)
                       tmp_df_devumiem<-merge(tmp_df_devumiem, svari3_df, by=c('Nozare','Gads'), all.x = TRUE)
                       tmp_df_devumiem$Devums = round(tmp_df_devumiem$Kalendarie*tmp_df_devumiem$Svars,3)
                       devumiem2<-dcast(tmp_df_devumiem[,c(1,3,6)], Nozare ~ Periods)
                       konfidencialajam<-c('Konfidencionālās nozares un statistiskā kļūda')
                       dev_partika<-c('C10_12 Pārtikas un dzērienu ražošana')
                       dev_viegla<-c('C13_15 Vieglā rūpniecība')
                       dev_kokapstr<-c('C16 Kokapstrāde')
                       dev_polig<-c('C17_18 Papīra ražošana un poligrāfija')
                       dev_kimiska<-c('C19_22 Ķīmiskā rūpniecība')
                       dev_nemetaliska<-c('C23 Nemetālisko minerālu izstr. ražošana')
                       dev_metaliska<-c('C24_25 Metālu un metālizstrādājumu ražošana')
                       dev_masinbuve<-c('C26_28 Datoru, elektrisko un citu iekārtu ražošana')
                       dev_transportbuve<-('C29_30 Automobiļu un citu transportlīdzekļu ražošana')
                       dev_cita<-('C31_32 Mēbeļu un citu produktu ražošana')
                       dev_iekartu<-c('C33 Iekārtu remonts un uzstādīšana')
                       plus <- function(x) {
                          if(all(is.na(x))){
                             c(x[0],NA)} else {
                                sum(x,na.rm = TRUE)}
                       }
                       for (i in 2:ncol(devumiem2)){
                          konfidencialajam<-append(konfidencialajam, round(devumiem2[1,i]-plus(devumiem2[c(2:23),i]),3))
                          dev_partika<-append(dev_partika, round(plus(devumiem2[c(2,3),i]),3))
                          dev_viegla<-append(dev_viegla, round(plus(devumiem2[c(4,5,6),i]),3))
                          dev_kokapstr<-append(dev_kokapstr, round(plus(devumiem2[c(7),i]),3))
                          dev_polig<-append(dev_polig, round(plus(devumiem2[c(8,9),i]),3))
                          dev_kimiska<-append(dev_kimiska, round(plus(devumiem2[c(10,11,12),i]),3))
                          dev_nemetaliska<-append(dev_nemetaliska, round(plus(devumiem2[c(13),i]),3))
                          dev_metaliska<-append(dev_metaliska, round(plus(devumiem2[c(14,15),i]),3))
                          dev_masinbuve<-append(dev_masinbuve, round(plus(devumiem2[c(16,17,18),i]),3))
                          dev_transportbuve<-append(dev_transportbuve, round(plus(devumiem2[c(19,20),i]),3))
                          dev_cita<-append(dev_cita, round(plus(devumiem2[c(21,22),i]),3))
                          dev_iekartu<-append(dev_iekartu, round(plus(devumiem2[c(23),i]),3))
                       } 
                       devumiem3<-data.frame(devumiem2[1,])
                       devumiem3<-rbind(devumiem3, dev_partika)
                       devumiem3<-rbind(devumiem3, dev_viegla)
                       devumiem3<-rbind(devumiem3, dev_kokapstr)
                       devumiem3<-rbind(devumiem3, dev_polig)
                       devumiem3<-rbind(devumiem3, dev_kimiska)
                       devumiem3<-rbind(devumiem3, dev_nemetaliska)
                       devumiem3<-rbind(devumiem3, dev_metaliska)
                       devumiem3<-rbind(devumiem3, dev_masinbuve)
                       devumiem3<-rbind(devumiem3, dev_transportbuve)
                       devumiem3<-rbind(devumiem3, dev_cita)
                       devumiem3<-rbind(devumiem3, dev_iekartu)
                       devumiem3<-rbind(devumiem3, konfidencialajam)
                       timeline_2013<-seq(as.Date("2013-01-01"), as.Date(paste0(actualYear,"-",actualMonth_NR,"-01")), by="months")
                       
                       output$DevumuGraf <-renderHighchart({
                             highchart(type='stock') %>%
                                 hc_chart(type = "column") %>% 
                                 
                                hc_plotOptions(column = list(stacking = "normal")) %>%
                                hc_xAxis(labels = list(format = '{value:%b %y}'), categories = unique(tmp_df_devumiem$Periods2) ) %>%
                                hc_yAxis( title = list(text = "Ietekme uz Apstrādes rūpniecības izmaiņām pret iepriekšējā gada atbilstošo mēnesi"),
                                          plotLines = list(list(
                                             value = 0,
                                             color = 'grey',
                                             width = 3,
                                             zIndex = 4
                                          ))) %>%
                                hc_tooltip(crosshairs = FALSE, split = FALSE, shared = TRUE) %>%
                             hc_add_series(data=xts(as.numeric(devumiem3[2,2:ncol(devumiem3)]), timeline_2013), name='[C10_12] Pārtikas un dzērienu ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[3,2:ncol(devumiem3)]), timeline_2013), name='[C13_15] Vieglā rūpniecība')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[4,2:ncol(devumiem3)]), timeline_2013), name='[C16] Kokapstrāde')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[5,2:ncol(devumiem3)]), timeline_2013), name='[C17_18] Papīra ražošana un poligrāfija')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[6,2:ncol(devumiem3)]), timeline_2013), name='[C19_22] Ķīmiskā rūpniecība')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[7,2:ncol(devumiem3)]), timeline_2013), name='[C23] Nemetālisko minerālu izstr. ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[8,2:ncol(devumiem3)]), timeline_2013), name='[C24_25] Metālu un metālizstrādājumu ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[9,2:ncol(devumiem3)]), timeline_2013), name='[C26_28] Datoru, elektrisko un citu iekārtu ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[10,2:ncol(devumiem3)]), timeline_2013), name='[C29_30] Automobiļu un citu transportlīdzekļu ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[11,2:ncol(devumiem3)]), timeline_2013), name='[C31_32] Mēbeļu un citu produktu ražošana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[12,2:ncol(devumiem3)]), timeline_2013), name='[C33] Iekārtu remonts un uzstādīšana')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[13,2:ncol(devumiem3)]), timeline_2013), name='Konfidencionālās nozares un statistiskā kļūda')%>%
                             hc_add_series(data=xts(as.numeric(devumiem3[1,2:ncol(devumiem3)]), timeline_2013), name='Apstrādes rūpniecība kopā', type='line', color = 'red',
                                           marker = list(symbol = 'circle'))%>%
                             hc_credits(
                                enabled = TRUE,
                                text = "Centrālā statistikas pārvalde | RUI020m RUI010 Produkcijas apjoma un apgrozījuma indeksi",
                                href = "https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__NOZ__RU__RUI/?tablelist=true"
                             ) %>%
                                 hc_rangeSelector( buttons = list(
                                   list(type = 'all', text = 'Viss'),
                                   #list(type = 'year', count = 10, text = '10G'),
                                   list(type = 'year', count = 5, text = '5G'),
                                   list(type = 'year', count = 2, text = '2G'),
                                   list(type = 'year', count = 1, text = 'Gads'),
                                   list(type = 'month', count = 6, text = '1/2G')
                                ), selected = 1)%>% 
                             hc_legend(enabled = TRUE)
                          })
                       
                       
                 ## ievieto visu UIs ----------
                       insertUI(
                          selector = '#show_more_detail',
                          ui = div( id = 'conents_for_more_detail',
                                    ## 1.4 Treemap on Apstrādes rūpniecības attīstība --------------------
                                    h2(paste0('Papildus informācija par apstrādes rūpniecības attīstību'), style = "font-family: 'Arial"),
                                    tags$a(href = 'https://stat.gov.lv/lv/statistikas-temas/noz/rupnieciba', "Sadaļa par rūpniecību Oficiālās statistikas portālā", target = "_blank", style = "font-family: 'Arial"),
                                    fluidRow( highchartOutput('KeyExTM')  ) %>% withSpinner(type=4) ,
                                    
                                    
                                    ## 1.5 Grafiks, kas parāda apgrozījuma indeksu attīstību, kā arī naudas apmērā apgrozījuma attīstību -------------
                                    h2(paste0('Rūpniecības nozaru apgrozījuma attīstības tendences'), style = "font-family: 'Arial"),
                                    p("Nospied uz nozares nosaukuma, lai attēlotu tās attīstības tendenci"),
                                    fluidRow( h3("Apstrādes rūpniecības apgrozījuma attīstība", align = 'center'),
                                              column( width = 6, h4("Sezonāli izlīdzināti indeksi"), highchartOutput('KeyApgrLine')  ),
                                              column( width = 6, h4("Apgrozījuma tendences, tūkst. EUR"), highchartOutput('KeyApgrPercent')  ) ),
                                    h2(paste0('Rūpniecības nozaru konjunktūras attīstības tendences'), style = "font-family: 'Arial"),
                                    fluidRow( h3(" ", align = 'center'),
                                              column( width = 6, h4("Ekonomikas sentimenta un rūpniecības konfidences rādītāji"), highchartOutput('SentimentLine') ),
                                              column( width = 6, h4("Rūpniecības konjunktūras rādītāji"), highchartOutput('KonjunkturLine')  ) ),
            
                                    
                                    ## 1.6.1 Nozaru devumi -----------------
                                    h2(paste0('Nozaru devumi apstrādes rūpniecības attīstībā'), style = "font-family: 'Arial"),
                                    tags$p( "Grafiks attēlo nozaru izmaiņas pret iepriekšējā gada atbilstošo mēnesi, pareizinātas ar attiecīgās nozares svaru apstrādes rūpniecības pievienotajā vērtībā attiecīgajā pārskata gadā."),
                                    fluidRow(column(12, div(style="height:60vh", highchartOutput("DevumuGraf", height = "60vh")))   ),
                                    
                                    )
                       )
                       
                       ## hide load more message ---
                       ## --- show loading message ---
                       shinyjs::hide( id = "load_more_message" )
                    })
      
      
      ## III. Nozaru analīzes rīks ------------------------
      observeEvent(input$btn_build_country_report,
                   {
                      tmp_execution <- FALSE
                      
                      if(is.null(input$select_country)) {
                         showModal(modalDialog(
                            title = "Brīdinājums",
                            tags$b("Lūdzu izvēlies vienu vai vairākas nozares vai arī vienu līdzīgo nozaru grupu!"),
                            size = 's'
                         ))
                      }
                      
                      ## III.0 Nozaru vai nozaru grupu izvēles pamatierobežojumi ------------------------
                      ## One can either select one or multiple countries, OR only one country group
                      if( any(input$select_country %in% list_nozares[['Līdzīgo nozaru grupa']]) &
                          length(input$select_country)>1 ){
                         showModal(modalDialog(
                            title = "Brīdinājums",
                            tags$b("Tu vari izvēlēties tikai VIENU ierakstu no līdzīgo nozaru grupas!"),
                            size = 's'
                         ))
                      }
                      
                      ##  a country group selected
                      if( any(input$select_country %in% list_nozares[['Līdzīgo nozaru grupa']]) & length(input$select_country)==1 ){
                         tmp_selected_countries <- input$select_country
                         tmp_execution <- TRUE
                         tmp_single_country <- FALSE
                         output$SelectedMarketMultiple <-
                            renderText({paste0("Izvēlēta līdzīgo nozaru grupa: ",input$select_country)})
                      }
                      
                      # multiple countries selected
                      if( !any(input$select_country %in% list_nozares[['Līdzīgo nozaru grupa']]) & length(input$select_country)>1 ){
                         tmp_selected_countries <- input$select_country
                         tmp_execution <- TRUE
                         tmp_single_country <- FALSE
                         output$SelectedMarketMultiple <-
                            renderText({paste0("Izvēlētas ", length(tmp_selected_countries) , " nozares. ")})
                      }
                      
                      ## only one country selected!
                      if(!any(input$select_country %in% list_nozares[['Līdzīgo nozaru grupa']]) &  length(input$select_country)==1 ) {
                         tmp_selected_countries <- input$select_country
                         tmp_execution <- TRUE
                         tmp_single_country <- TRUE
                         output$SelectedMarketSingle <-
                            renderText({paste0('Izvēlēta viena nozare: ',tmp_selected_countries)})
                      }

                      ### work on next only when the inputs are correct!!!
                      if( tmp_execution ){
                         ## hide howto ----
                         shinyjs::hide(id = 'country_howto')
                         ## show wait message ----
                         shinyjs::show( id = 'wait_message_country_intel' )
                         ## disable a button -----
                         shinyjs::disable("btn_build_country_report")
                         ## disable a country selection button -----
                         shinyjs::disable("select_country")
                         
                         ## !!!!!!!!!!!!!!!! insert UI country name --------------------
                         insertUI(
                            selector = "#country_name",
                            ui = div(
                               id = 'country_name_single_or_multiple',
                               conditionalPanel( "input.select_country.length == 1 &&
                                                 input.select_country.valueOf() != 'Līdzīgo nozaru grupa' &&
                                                 input.select_country.valueOf() != 'Apstrādes rūpniecība' && " ,
                                                 fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedMarketSingle"))), align = "center" ), style = "color:darkblue" ) )
                               ),
                               
                               conditionalPanel( "input.select_country.length > 1 || 
                                              input.select_country.valueOf() == 'Līdzīgo nozaru grupa' || 
                                              input.select_country.valueOf() == 'Apstrādes rūpniecība'|| ",
                                               fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedMarketMultiple"))), align = "center" ), style = "color:darkblue" ) )

                               )
                            )
                         )
                         #shinyjs::hide( id = 'wait_message' )
                         ## III.1 Basic country info table ---------------------------------
                         # ### define the select country
                         print("------------------  Basic country tables -------------------------")
                        
                         ## hide wait message ----
                         shinyjs::hide( id = 'wait_message_country_intel' )

                     
                      }
                   }
                  )
      
      
      ## IV. Appendix .......  Reset buttions -------------------------
      ## 1. reset btn for Commodity intelligence Exports --------------------
      observeEvent( input$btn_reset_ci_ex,
                    {
                       ### try remove UI -- when pre-defined HS
                       removeUI( selector = '#body_ex_line_value_percent')
                       removeUI( selector = '#body_ex_growth_tab')
                       removeUI( selector = '#body_ci_markets_ex_selector')
                       removeUI( selector = '#body_ci_markets_ex_map')
                       removeUI( selector = '#body_ci_markets_ex_top')
                       removeUI( selector = '#body_ci_markets_ex_growth')
                       removeUI( selector = '#body_appendix_hs_ex')
                       removeUI( selector = '#body_ci_markets_ex_fail_msg')
                       removeUI( selector = '#body_ci_markets_ex_global_facts')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_map')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_sankey')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_summary_tab')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_msg')
                       shinyjs::hide( id = "body_ci_market_loading_message" )
                       
                       ### remove UI -- when self_defined HS
                       removeUI( selector = '#body_ex_line_value_percent_self_defined')
                       removeUI( selector = '#body_ex_growth_tab_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_selector_self_defined')
                       removeUI( selector = '#body_selected_ex_line_value_percent_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_map_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_top_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_growth_self_defined')
                       removeUI( selector = '#body_appendix_hs_ex_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_fail_msg_self_define')
                       removeUI( selector = '#body_ci_markets_ex_global_facts_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_map_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_sankey_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_summary_tab_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_msg_self_define')
                       shinyjs::hide( id = "body_ci_market_loading_message_self_define" )
                       
                       ### clear all outputs
                       output$HS_ex <- renderDataTable(NULL)
                       output$HS_pre_ex <- renderDataTable(NULL)
                       output$CIExportValueLine <- renderHighchart(highchart())
                       output$CIExportPercentLine <- renderHighchart(highchart())
                       output$MapEXMarket <- renderHighchart(highchart())
                       output$GrowthTabSelectedEx <- renderDataTable(NULL)
                       output$SelectedExMarketLine <- renderHighchart(highchart())
                       output$SelectedExMarketLinePercent <- renderHighchart(highchart())
                       output$SelectedExMarketGrowthTab <- renderDataTable(NULL)
                       
                       ## hide all ids
                       #shinyjs::hide(selector = '#body_ex')
                       #shinyjs::hide(selector = '#body_value_ex')
                       #shinyjs::hide(selector = '#body_percent_ex')
                       #shinyjs::hide(selector = '#body_growth_ex')
                       #shinyjs::hide(selector = '#body_ci_markets_ex')
                       #shinyjs::hide(selector = '#body_appendix_hs_ex')
                       shinyjs::show(id = 'ci_howto_ex')
                       shinyjs::reset('sidebar_ci_exports')
                       ## disable the buttone ---
                       shinyjs::enable("btn_build_commodity_report_ex")
                       shinyjs::enable("select_comodity_ex")
                       shinyjs::enable("file_comodity_ex")
                       shinyjs::enable("rbtn_prebuilt_diy_ex")
                       
                     }
                    )
      
      ## 2. reset btn for Commodity intelligence Imports --------
      observeEvent( input$btn_reset_ci_im,
                    {
                       ### try remove UI -- when pre-defined HS
                       removeUI( selector = '#body_im_line_value_percent')
                       removeUI( selector = '#body_im_growth_tab')
                       removeUI( selector = '#body_ci_markets_im_selector')
                       removeUI( selector = '#body_ci_markets_im_map')
                       removeUI( selector = '#body_ci_markets_im_top')
                       removeUI( selector = '#body_ci_markets_im_growth')
                       removeUI( selector = '#body_appendix_hs_im')
                       
                       ### remove UI -- when self_defined HS
                       removeUI( selector = '#body_im_line_value_percent_self_defined')
                       removeUI( selector = '#body_im_growth_tab_self_defined')
                       removeUI( selector = '#body_ci_markets_im_selector_self_defined')
                       removeUI( selector = '#body_selected_im_line_value_percent_self_defined')
                       removeUI( selector = '#body_ci_markets_im_map_self_defined')
                       removeUI( selector = '#body_ci_markets_im_top_self_defined')
                       removeUI( selector = '#body_ci_markets_im_growth_self_defined')
                       removeUI( selector = '#body_appendix_hs_im_self_defined')
                       
                       ### clear all outputs
                       output$HS_im <- renderDataTable(NULL)
                       output$HS_pre_im <- renderDataTable(NULL)
                       output$CIImportValueLine <- renderHighchart(highchart())
                       output$CIImportPercentLine <- renderHighchart(highchart())
                       output$MapIMMarket <- renderHighchart(highchart())
                       output$GrowthTabSelectedIm <- renderDataTable(NULL)
                       output$SelectedImMarketLine <- renderHighchart(highchart())
                       output$SelectedImMarketLinePercent <- renderHighchart(highchart())
                       output$SelectedImMarketGrowthTab <- renderDataTable(NULL)
                       
                       ## hide all ids
                       # shinyjs::hide(selector = '#body_im')
                       # shinyjs::hide(selector = '#body_value_im')
                       # shinyjs::hide(selector = '#body_percent_im')
                       # shinyjs::hide(selector = '#body_growth_im')
                       # shinyjs::hide(selector = '#body_ci_markets_im')
                       # shinyjs::hide(selector = '#body_appendix_hs_im')
                       shinyjs::show(id = 'ci_howto_im')
                       shinyjs::reset('sidebar_ci_imports') 
                       ## enable the buttone ---
                       shinyjs::enable("btn_build_commodity_report_im")
                       shinyjs::enable("select_comodity_im")
                       shinyjs::enable("file_comodity_im")
                       shinyjs::enable("rbtn_prebuilt_diy_im")
                       
                     }
                    )
      
      ## 3. reset btn for Country intelligence ------------
      observeEvent( input$btn_reset_cr,
                    {
                       ## remove UIs 
                       removeUI( selector = "#country_name_single_or_multiple" )
                       removeUI( selector = "#country_info_table_map" )
                       removeUI( selector = "#country_trade_summary_all_items" )
                       removeUI( selector = "#country_trade_summary_appendix" )
                       
                       ## clear all output
                       output$CountryTable <- renderDataTable(NULL)
                       output$MapSelectedCountry <- renderHighchart(highchart())
                       output$CountryTradeTableTotal <- renderDataTable(NULL)
                       output$CountryTwowayTradeGraphTotal <- renderHighchart(highchart())
                       output$CountryTradeBalanceGraphTotal <- renderHighchart(highchart())
                       output$CountryExportsGraphTotal <- renderHighchart(highchart())
                       output$CountryExportsGraphTotalPercent <- renderHighchart(highchart())
                       output$CountryImportsGraphTotal <- renderHighchart(highchart())
                       output$CountryImportsGraphTotalPercent <- renderHighchart(highchart())
                       output$KeyExCountryTotalTreeMap <- renderHighchart(highchart())
                       output$KeyImCountryTotalTreeMap <- renderHighchart(highchart())
                       output$KeyExCountryTotalLine <- renderHighchart(highchart())
                       output$KeyExCountryTotalLinePercent <- renderHighchart(highchart())
                       output$KeyImCountryTotalLine <- renderHighchart(highchart())
                       output$CountrySummaryAllExports <- renderDataTable(NULL)
                       output$CountrySummaryAllImports <- renderDataTable(NULL)
                       output$CountrySummaryAllTwowayBalance  <- renderDataTable(NULL)

                       
                       output$SelectedMarketSingle <- renderText(NULL)
                       output$SelectedMarketMultiple <-renderText(NULL)
                          
                       ## hide all ids
                       shinyjs::show(id = 'country_howto')
                       #shinyjs::hide(id = 'country_basic_info')
                       #shinyjs::hide(id = 'country_trade_summary')
                       #shinyjs::hide(id = 'country_trade_summary_individual')
                       #shinyjs::hide(id = "country_trade_summary_appendix")
                       #shinyjs::hide(id = "country_single_name")
                       #shinyjs::hide(id = "country_multiple_name")
                       # shinyjs::hide(id = 'country_trade_single')
                       reset('sidebar_cr')
                       ## ensable a button
                       shinyjs::enable("btn_build_country_report")
                       shinyjs::enable("select_country")
                    }
                  )
      
      # withProgress(message = 'Finishing in about 10s', value = 1, {
      #    # Increment the progress bar, and update the detail text.
      #    incProgress( 1, detail = NULL)
      #    Sys.sleep(3)
      #    
      # })
      
      ## 4. Monthly update ------------------------
      output$MonthlyUpdate <- 
         renderUI({
            tags$iframe(
               src = SNZ_link,
               seamless = "seamless",
               frameborder = 0,
               height="800px", width="100%")
         })
   }