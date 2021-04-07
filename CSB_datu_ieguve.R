library(pxweb)

### Dati no rūpniecības ikmēneša tabulas (kalendārie)
pxq <- pxweb_query(list("NACE_MIG"=c("*"),
                        "ContentsCode"=c("RUI020m4"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI020m", pxq)
datacal_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(datacal_df)=c('Nozare','Periods','Kalendarie')

### Dati no rūpniecības ikmēneša tabulas (sezonālie)
pxq <- pxweb_query(list("NACE_MIG"=c("*"),
                        "ContentsCode"=c("RUI020m3"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI020m", pxq)
datasez_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(datasez_df)=c('Nozare','Periods','Sezonalie')

### Pēdējā mēneša dati no Tabula "No gada sākuma"
pxq <- pxweb_query(list("NACE"=c("*"),
                        "ContentsCode"=c("RUI030m"),
                        "TIME"=c(datasez_df$Periods[nrow(datasez_df)]) ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI030m", pxq)
datagada_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(datagada_df)=c('Nozare','Periods','No gada sakuma')

### Visi dati tabulā "No gada sākuma"
pxq <- pxweb_query(list("NACE"=c("*"),
                        "ContentsCode"=c("RUI030m"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI030m", pxq)
datagada_all_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(datagada_all_df)=c('Nozare','Periods','No gada sakuma')

### Sezonālie otrajā variantā (laikam bija indekss)
pxq <- pxweb_query(list("NACE_MIG"=c("*"),
                        "ContentsCode"=c("RUI020m"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI020m", pxq)
sezonalie_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(sezonalie_df)=c('Nozare','Periods','Sezonalie')

### Svaru tabula
pxq <- pxweb_query(list("NACE"=c("*"),
                        "ContentsCode"=c("RUI010"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI010", pxq)
svari_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(svari_df)=c('Nozare','Periods','Svari')

### Kalendārie indeksi
pxq <- pxweb_query(list("NACE_MIG"=c("*"),
                        "ContentsCode"=c("RUI020m1"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI020m", pxq)
kalendarie_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(kalendarie_df)=c('Nozare','Periods','Kalendarie')

### Ceturkšņa apgrozījuma indeksi
pxq <- pxweb_query(list("SALES"=c("TOVT", "TOVD", "TOVE"),
                        "NACE"=c("*"),
                        "ContentsCode"=c("RUI040c"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUI040c", pxq)
sez_apgr_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(sez_apgr_df)=c('Virziens','Nozare','Periods','Sezonalie')

### Naudas tabula
pxq <- pxweb_query(list("NACE"=c("*"),
                        "ContentsCode"=c("RUA030c1",
                                         "RUA030c2",
                                         "RUA030c3"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/NOZ/RU/RUI/RUA030c", pxq)
apgr_nauda_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(apgr_nauda_df)=c('Nozare','Periods','Apgrozijums_nauda', 'Vietejais', 'Eksports')
apgr_nauda_df<-cbind(apgr_nauda_df, 'Vietejais_nauda'=round(apgr_nauda_df$Apgrozijums_nauda*apgr_nauda_df$Vietejais/100,1))
apgr_nauda_df<-cbind(apgr_nauda_df, 'Eksports_nauda'=round(apgr_nauda_df$Apgrozijums_nauda*apgr_nauda_df$Eksports/100,1))

### Sentimenta indekss
pxq <- pxweb_query(list("ContentsCode"=c("KRE010m"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/VEK/KR/KRE/KRE010m", pxq)
sentimenta_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(sentimenta_df)=c('Periods','Sentiments')
sentimenta_df<-cbind(sentimenta_df, Sentimenta_izm<-sentimenta_df$Sentiments-100)
names(sentimenta_df)=c('Periods','Sentiments', 'Sentimenta_izm')

### Konfidences rādītājs
pxq <- pxweb_query(list("VAL"=c("SA"),
                        "ContentsCode"=c("KRE020m"),
                        "INDICATOR"=c("CI_IND"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/VEK/KR/KRE/KRE020m", pxq)
konfidences_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(konfidences_df)=c('Variants','Rādītājs','Periods','Konfidence')

### Konjunktūras rādītāji
pxq <- pxweb_query(list("ENT_SIZE"=c("EMP_TOTAL"),
                        "ContentsCode"=c("KRR030m"),
                        "INDICATOR"=c("CI_IND","PEN3","EEXN3","CBS","BSEXN3","EVEN3","EVEN3_EU","EVEN3_CIS"),
                        "TIME"=c("*") ))
datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/VEK/KR/KRR/KRR030m", pxq)
konjunkturas_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
names(konjunkturas_df)=c('Grupa','Rādītājs','Periods','Konjunktura')

#LIELAIS  SBS MASIVS
### Dati no SBS
if (FALSE){
  pxq <- pxweb_query(list("NACE"=c("B","B05","B051","B052","B06","B061","B062",
                                   "B07","B071","B072","B0721","B0729","B08","B081","B0811",
                                   "B0812","B089","B0891","B0892","B0893","B0899","B09","B091","B099",
                                   "C","C10","C101","C1011","C1012","C1013","C102","C103",
                                   "C1031","C1032","C1039","C104","C1041","C1042","C105","C1051","C1052","C106",
                                   "C1061","C1062","C107","C1071","C1072","C1073","C108","C1081","C1082","C1083",
                                   "C1084","C1085","C1086","C1089","C109","C1091","C1092","C11","C1101",
                                   "C1102","C1103","C1104","C1105","C1106","C1107","C12","C13",
                                   "C131","C132","C133","C139","C1391","C1392","C1393",
                                   "C1394","C1395","C1396","C1399","C14","C141","C1411","C1412","C1413","C1414",
                                   "C1419","C142","C143","C1431","C1439","C15","C151","C1511","C1512",
                                   "C152","C16","C161","C162","C1621","C1622","C1623","C1624",
                                   "C1629","C17","C171","C1711","C1712","C172","C1721","C1722","C1723","C1724",
                                   "C1729","C18","C181","C1811","C1812","C1813","C1814","C182","C19",
                                   "C191","C192","C20","C201","C2011","C2012","C2013","C2014",
                                   "C2015","C2016","C2017","C202","C203","C204","C2041","C2042",
                                   "C205","C2051","C2052","C2053","C2059","C206","C21","C211",
                                   "C212","C22","C221","C2211","C2219","C222","C2221","C2222","C2223",
                                   "C2229","C23","C231","C2311","C2312","C2313","C2314","C2319","C232",
                                   "C233","C2331","C2332","C234","C2341","C2342","C2343","C2344","C2349","C235",
                                   "C2351","C2352","C236","C2361","C2362","C2363","C2364","C2365","C2369","C237",
                                   "C239","C2391","C2399","C24","C241","C242","C243",
                                   "C2431","C2432","C2433","C2434","C244","C2441","C2442","C2443","C2444","C2445",
                                   "C2446","C245","C2451","C2452","C2453","C2454","C25","C251","C2511","C2512",
                                   "C252","C2521","C2529","C253","C254","C255","C256",
                                   "C2561","C2562","C257","C2571","C2572","C2573","C259","C2591","C2592","C2593",
                                   "C2594","C2599","C26","C261","C2611","C2612","C262","C263",
                                   "C264","C265","C2651","C2652","C266","C267","C268",
                                   "C27","C271","C2711","C2712","C272","C273","C2731","C2732",
                                   "C2733","C274","C275","C2751","C2752","C279","C28","C281",
                                   "C2811","C2812","C2813","C2814","C2815","C282","C2821","C2822","C2823","C2824",
                                   "C2825","C2829","C283","C284","C2841","C2849","C289","C2891","C2892",
                                   "C2893","C2894","C2895","C2896","C2899","C29","C291","C292",
                                   "C293","C2931","C2932","C30","C301","C3011","C3012","C302","C303",
                                   "C304","C309","C3091","C3092","C3099","C31","C3101",
                                   "C3102","C3103","C3109","C32","C321","C3211","C3212","C3213","C322",
                                   "C323","C324","C325","C329","C3291","C3299","C33",
                                   "C331","C3311","C3312","C3313","C3314","C3315","C3316","C3317","C3319","C332",
                                   "D","D351","D3511","D3512","D3513","D3514","D352","D3521",
                                   "D3522","D3523","D353"),
                          "ContentsCode"=c("UFR010"),
                          "INDICATOR"=c("*"),
                          "TIME"=c("*") ))
  datac<-pxweb_get("https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/ENT/UF/UFR/UFR010", pxq)
  SBS_df <- as.data.frame(datac, column.name.type = "text", variable.value.type = "code")
  names(SBS_df)=c('NACE','Rādītājs','Periods','Vērtība')
}

actualYear <- substr(datacal_df$Periods[nrow(datacal_df)],1,4)
actualMonth_NR <- substr(datacal_df$Periods[nrow(datacal_df)],6,7)

save(datacal_df, datasez_df, datagada_df, datagada_all_df, sezonalie_df, svari_df, kalendarie_df, sez_apgr_df,
     apgr_nauda_df, sentimenta_df, konfidences_df, konjunkturas_df, actualYear, actualMonth_NR,
     #SBS_df,
     file="C:/Users/artur/OneDrive/Dokumenti/R/BCD_Dashboard/CSP_data.rdata")
