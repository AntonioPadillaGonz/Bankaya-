
install.packages("tidyverse")
install.packages("dplyr")
install.packages("openxlsx")

?read_csv

library(tidyverse)
library(dplyr)

main <- read.csv("C:/Users/HP/Desktop/modelo/main_dataset.csv")
main1 <- main %>% 
  mutate(
    
    
#CONVERSION DE VARIABLES PARA SU USO (ARCHIVO INTERNO)
    
    ultima_appfec  = as.Date(last_app_date), 
    primerappfec   = as.Date(first_app_date),
      diffecha     =as.numeric(ultima_appfec)-as.numeric (primerappfec),
    bnplastfec     = as.Date(last_bnpl_app_date),
    bnplfirstfec   = as.Date(first_bnpl_app_date),
      bnpldif      = as.numeric(bnplastfec) - as.numeric(bnplfirstfec), 
    inquiries3m    = as.numeric(n_inquiries_l3m),
    inquiries6m    = as.numeric(n_inquiries_l6m),
      inquidif     = as.numeric(inquiries6m) - as.numeric(inquiries3m),
      totalinqui   = as.numeric(inquidif) + as.numeric(inquiries3m),
    preaplbnpl     = as.numeric(n_bnpl_apps),
    aplappbnpl     = as.numeric(n_bnpl_approved_apps),
      totalbnpl    = as.numeric(aplappbnpl) - as.numeric (preaplbnpl), 
    maxtardio77    = as.numeric(max_days_late),
    mas34tardio    = as.numeric(target),  
    oriaplica      = as.numeric(account_to_application_days), 
    sfprevias      = as.numeric(n_sf_apps), 
    
    
#CREACION DE RANGOS DE LAS VARIABLES A USAR    
          rangobnpldif = case_when ( 
                                    bnpldif < 0 ~         "<0",
                                    bnpldif == 0 ~        "=0",
                                    is.na(bnpldif) ~      "NoAplica",
                                    TRUE ~ NA_character_),
    
       rangototalbnpl = case_when ( 
                                   totalbnpl ==0  ~            "Bueno",
                                   between(totalbnpl,-5,-1)~   "<=-5",
                                   between(totalbnpl,-10,-6)~  "<=-10",
                                   between(totalbnpl,-20,-11)~ "<=-20",
                                   totalbnpl >0  ~             "No",
                                   is.na(totalbnpl) ~          "SinInfo",
                                   TRUE ~ NA_character_),
     rangomaxtardio77 = case_when (
                                   maxtardio77 <= 0 ~          "<=0",
                                   between(maxtardio77,1,5)~   "<=5",
                                   between(maxtardio77,6,10)~  "<=10",
                                   between(maxtardio77,11,20)~ "<=20",
                                   between(maxtardio77,21,30)~ "<=30",
                                   maxtardio77 >= 30 ~         ">30",
                                   TRUE ~ NA_character_),
    
    rangomas34tardio = case_when  (
                                   mas34tardio == 0 ~          "No",
                                   mas34tardio == 1 ~          "Si34",
                                   TRUE ~ NA_character_),
    
    rangooriaplica  = case_when   (
                                   oriaplica == 0 ~            "0",
                                   between(oriaplica,1,15)~    "<=15", 
                                   between(oriaplica,16,30)~   "<=30",
                                   between(oriaplica,31,60)~   "<=60",
                                   between(oriaplica,61,90)~   "<=90",
                                   between(oriaplica,91,180)~  "<=180",
                                   between(oriaplica,181,360)~ "<=360",
                                   between(oriaplica,361,720)~ "<=720",
                                   oriaplica > 720 ~           ">720",
                                   TRUE ~ NA_character_),
    
    rangosfprevias  = case_when   (
                                   is.na(sfprevias) ~ "SinInfo",
                                   sfprevias == 0 ~ "0",
                                   between(sfprevias, 1, 5)   ~ "<=5", 
                                   between(sfprevias, 6, 10)  ~ "<=10",
                                   between(sfprevias, 11, 15) ~ "<=15",
                                   between(sfprevias, 16, 25) ~ "<=25",
                                   between(sfprevias, 26, 35) ~ "<=35",
                                   sfprevias > 35 ~ ">35"),
    
    
    
#ASIGANCION DE PESOS DE ACUERDO A LA CASUISTICA POR VARIABLE    
    
   tardiofinal     = case_when   (
                                  rangomaxtardio77 == "<=0" & rangomas34tardio==  "No"    ~ 500,   
                                  rangomaxtardio77 == "<=0" & rangomas34tardio==  "Si34"  ~ -100,
                                  rangomaxtardio77 == "<=5" & rangomas34tardio==  "No"    ~ 200,
                                  rangomaxtardio77 == "<=5" & rangomas34tardio==  "Si34"  ~ -150,
                                  rangomaxtardio77 == "<=10" & rangomas34tardio== "No"    ~ 100,
                                  rangomaxtardio77 == "<=10" & rangomas34tardio== "Si34"  ~ -200,
                                  rangomaxtardio77 == "<=20" & rangomas34tardio== "No"    ~ 50,
                                  rangomaxtardio77 == "<=20" & rangomas34tardio== "Si34"  ~ -250,
                                  rangomaxtardio77 == "<=30" & rangomas34tardio== "No"    ~ 25,
                                  rangomaxtardio77 == "<=30" & rangomas34tardio== "Si34"  ~ -300,
                                  rangomaxtardio77 == ">30" & rangomas34tardio==  "No"    ~ 10,
                                  rangomaxtardio77 == ">30" & rangomas34tardio==  "Si34"  ~ -400),  
   
   
   
   aplicacionori    = case_when   (
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "0"       ~ 50,   
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=15"    ~ 100,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=30"    ~ 150,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=60"    ~ 200,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=90"    ~ 250,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=180"   ~ 250,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=360"   ~ 250,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  "<=720"   ~ 250,
                                  rangototalbnpl == "Bueno"  & rangooriaplica==  ">720"    ~ 250,
                                  
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "0"       ~ 100,   
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=15"    ~ 150,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=30"    ~ 200,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=60"    ~ 250,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=90"    ~ 300,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=180"   ~ 300,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=360"   ~ 300,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  "<=720"   ~ 300,
                                  rangototalbnpl == "<=-5"  & rangooriaplica==  ">720"    ~ 300,
                                  
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "0"       ~ 150,   
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=15"    ~ 250,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=30"    ~ 300,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=60"    ~ 350,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=90"    ~ 400,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=180"   ~ 400,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=360"   ~ 400,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  "<=720"   ~ 400,
                                  rangototalbnpl == "<=-10"  & rangooriaplica==  ">720"    ~ 400,
                                  
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "0"       ~ 250,   
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=15"    ~ 350,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=30"    ~ 400,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=60"    ~ 450,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=90"    ~ 500,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=180"   ~ 500,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=360"   ~ 500,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  "<=720"   ~ 500,
                                  rangototalbnpl == "<=-20"  & rangooriaplica==  ">720"    ~ 500,
                                  
                                  rangototalbnpl == "No"  & rangooriaplica==  "0"       ~ 0,   
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=15"    ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=30"    ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=60"    ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=90"    ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=180"   ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=360"   ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  "<=720"   ~ 0,
                                  rangototalbnpl == "No"  & rangooriaplica==  ">720"    ~ 0,)
   
                                  )  

prueba <- main1 %>% group_by(sfprevias, rangosfprevias) %>% 
  summarise(Cuentas = n(),)

prueba2 <- main1 %>% 
  group_by(n_bnpl_approved_apps) %>% 
  summarise(Cuentas = n(),)       


??read_csv
credit <- read.csv("C:/Users/HP/Desktop/modelo/credit_reports.csv")
credit1 <- credit %>% 
  
  
  
  mutate( 
    

#CONVERSION DE VARIABLES PARA SU USO (BURO)        
    
    creditomaximo   = as.numeric(MAX_CREDIT),
    limitecredito   = as.numeric(CREDIT_LIMIT),
    fechapeoratraso = as.Date(WORST_DELAY_DATE),
    diadereporte    = as.Date(REPORT_DATE),
    pagostotales    = as.numeric(TOTAL_PAYMENTS),
    pagosatrasados  = as.numeric(DELAYED_PAYMENTS),
    peoratraso      = as.numeric(WORST_DELAY),
    Pagosreportados = as.numeric(TOTAL_REPORTED_PAYMENTS),
    
    
    
#ELABORACION DE RANGOS     
         
      rangocredito = case_when ( 
                                creditomaximo ==0  ~                  "SinCredito",
                                between(creditomaximo,1,5000)~        "<=5k",
                                between(creditomaximo,5001,10000)~    "<=10k",
                                between(creditomaximo,10001,25000)~   "<=25k",
                                between(creditomaximo,25001,50000)~   "<=50k",
                                between(creditomaximo,50001,75000)~   "<=75k",
                                between(creditomaximo,75001,100000)~  "<=100k",
                                creditomaximo > 100000 ~              ">100k",
                                is.na(creditomaximo) ~                "SinCredito",
                                TRUE ~ NA_character_),
    
    
    rangolimitcre = case_when ( 
                               limitecredito ==0  ~                  "SinCredito",
                               between(limitecredito,1,5000)~        "<=5k",
                               between(limitecredito,5001,10000)~    "<=10k",
                               between(limitecredito,10001,25000)~   "<=25k",
                               between(limitecredito,25001,50000)~   "<=50k",
                               between(limitecredito,50001,75000)~   "<=75k",
                               between(limitecredito,75001,100000)~  "<=100k",
                               limitecredito > 100001 ~              ">100k",
                               is.na(limitecredito) ~                "SinCredito",
                               TRUE ~ NA_character_),
                                 
    rangopagosto = case_when ( 
                              pagostotales ==0  ~                  "SinPago",
                              between(pagostotales,1,10)~          "<=10",
                              between(pagostotales,11,20)~         "<=20",
                              between(pagostotales,21,30)~         "<=30",
                              between(pagostotales,31,40)~         "<=40",
                              between(pagostotales,41,50)~         "<=50",
                              between(pagostotales,51,100)~        "<=100",
                              between(pagostotales,101,250)~       "<=250",
                              between(pagostotales,250,500)~       "<=500",
                              pagostotales > 501 ~                 ">500",
                              is.na(pagostotales) ~                "SinPago",
                              TRUE ~ NA_character_),
    
    peridopago   = case_when ( 
                              PAYMENT_FREQUENCY =="ANUAL"                                 ~ 365,
                              PAYMENT_FREQUENCY =="BIMESTRAL"                             ~ 60,
                              PAYMENT_FREQUENCY =="CATORCENAL"                            ~ 14,
                              PAYMENT_FREQUENCY =="DEDUCCION DEL SALARIO"                 ~ 15,
                              PAYMENT_FREQUENCY =="MENSUAL"                               ~ 30,
                              PAYMENT_FREQUENCY =="PAGO MINIMO PARA CUENTAS REVOLVENTES"  ~ 30,
                              PAYMENT_FREQUENCY =="QUINCENAL"                             ~ 15,
                              PAYMENT_FREQUENCY =="SEMANAL"                               ~ 7,
                              PAYMENT_FREQUENCY =="SEMESTRAL"                             ~ 180,
                              PAYMENT_FREQUENCY =="TRIMESTRAL"                            ~ 90,
                              PAYMENT_FREQUENCY =="UNA SOLA EXHIBICION"                   ~ 30,
                              is.na(PAYMENT_FREQUENCY) ~ NA_real_,
                              TRUE ~ NA_real_),

pagocorriente   = case_when ( 
                            CURRENT_PAYMENT =="V"                  ~ 999,
                            CURRENT_PAYMENT =="--"                 ~ 0,
                            CURRENT_PAYMENT ==""                   ~ 0,
                            is.na(as.numeric(CURRENT_PAYMENT))     ~ NA_real_,
                            TRUE   ~ as.numeric (CURRENT_PAYMENT)), 

    
    pagoatrar  = round(pagosatrasados/peridopago,digits=0),
   atrasopeor  = case_when ( 
                            round(peoratraso / peridopago) == ""   ~ 0,
                            round(is.na(as.numeric(peoratraso / peridopago))) | 
                            round(is.nan(as.numeric(peoratraso / peridopago))) ~ 0,
                            TRUE   ~ round(as.numeric(peoratraso / peridopago))), 

    atvspagcor = round(pagocorriente-atrasopeor,digits=0),

   rangoatrcor = case_when ( 
                            atvspagcor <0  ~                  "Muy_Malo",
                            atvspagcor ==0 ~                  "Regular",
                            between(atvspagcor,1,3) ~         "Malo",
                            atvspagcor ==4 ~                  "Bueno",
                            between(atvspagcor,5,6)~          "Muy_Bueno",
                            atvspagcor >=7 ~                  "Excelente",
                            is.na(atvspagcor) ~                "Revisar",
                             TRUE ~ NA_character_),

#ASIGANCION DE PESOS POR CASUISTICA


   weight      = case_when ( 
                            rangoatrcor == "Excelente"   ~       3,
                            rangoatrcor == "Muy_Bueno"   ~       2,
                            rangoatrcor == "Bueno"       ~       1,
                            rangoatrcor == "Regular"     ~      -4,
                            rangoatrcor == "Malo"        ~      -5,
                            rangoatrcor == "Muy_Malo"    ~      -6,
                            ),


carrythatweight= case_when (
                            weight==3 & rangolimitcre== "SinCredito" ~ 0,
                            weight==3 & rangolimitcre== "<=5k"       ~ 10,  
                            weight==3 & rangolimitcre== "<=10k"      ~ 15,
                            weight==3 & rangolimitcre== "<=25k"      ~ 30,
                            weight==3 & rangolimitcre== "<=50k"      ~ 55,
                            weight==3 & rangolimitcre== "<=75k"      ~ 80,
                            weight==3 & rangolimitcre== "<=100k"     ~ 150,
                            weight==3 & rangolimitcre== ">100k"      ~ 200,
                            
                            weight==2 & rangolimitcre== "SinCredito" ~ 0,
                            weight==2 & rangolimitcre== "<=5k"       ~ 5,  
                            weight==2 & rangolimitcre== "<=10k"      ~ 10,
                            weight==2 & rangolimitcre== "<=25k"      ~ 25,
                            weight==2 & rangolimitcre== "<=50k"      ~ 50,
                            weight==2 & rangolimitcre== "<=75k"      ~ 75,
                            weight==2 & rangolimitcre== "<=100k"     ~ 100,
                            weight==2 & rangolimitcre== ">100k"      ~ 150,
                            
                      
                            weight==1 & rangolimitcre== "SinCredito" ~ 0,
                            weight==1 & rangolimitcre== "<=5k"       ~ 0, 
                            weight==1 & rangolimitcre== "<=10k"      ~ 5,
                            weight==1 & rangolimitcre== "<=25k"      ~ 20,
                            weight==1 & rangolimitcre== "<=50k"      ~ 45,
                            weight==1 & rangolimitcre== "<=75k"      ~ 70,
                            weight==1 & rangolimitcre== "<=100k"     ~ 90,
                            weight==1 & rangolimitcre== ">100k"      ~ 100,          
                            
                            weight==-4 & rangolimitcre== "SinCredito" ~ -5,
                            weight==-4 & rangolimitcre== "<=5k"       ~ -5, 
                            weight==-4 & rangolimitcre== "<=10k"      ~ -10,
                            weight==-4 & rangolimitcre== "<=25k"      ~ -40,
                            weight==-4 & rangolimitcre== "<=50k"      ~ -90,
                            weight==-4 & rangolimitcre== "<=75k"      ~ -140,
                            weight==-4 & rangolimitcre== "<=100k"     ~ -180,
                            weight==-4 & rangolimitcre== ">100k"      ~ -200,
                            
                            weight==-5 & rangolimitcre== "SinCredito" ~ -5,
                            weight==-5 & rangolimitcre== "<=5k"       ~ -10, 
                            weight==-5 & rangolimitcre== "<=10k"      ~ -20,
                            weight==-5 & rangolimitcre== "<=25k"      ~ -50,
                            weight==-5 & rangolimitcre== "<=50k"      ~ -100,
                            weight==-5 & rangolimitcre== "<=75k"      ~ -150,
                            weight==-5 & rangolimitcre== "<=100k"     ~ -200,
                            weight==-5 & rangolimitcre== ">100k"      ~ -300,
                              
                              
                            weight==-6 & rangolimitcre== "SinCredito" ~ -10,
                            weight==-6 & rangolimitcre== "<=5k"       ~ -20,
                            weight==-6 & rangolimitcre== "<=10k"      ~ -30,
                            weight==-6 & rangolimitcre== "<=25k"      ~ -60,
                            weight==-6 & rangolimitcre== "<=50k"      ~ -110,
                            weight==-6 & rangolimitcre== "<=75k"      ~ -160,
                            weight==-6 & rangolimitcre== "<=100k"     ~ -300,
                            weight==-6 & rangolimitcre== ">100k"      ~ -400,
                            is.na(weight) ~ NA_real_,
                            TRUE ~ NA_real_),)


  


  weightXcliente <- credit1 %>%
    group_by(customer_id) %>%
    summarise(Weightfinal=sum(carrythatweight,na.rm=TRUE))
  
  
  
  prueba4 <-credit1 %>% 
    group_by(weightXcliente) %>% 
    summarise(customer_id = n(),)  
  
  
  #ELABORACION DE ARCHIVO FINAL CON EL USO DE INFO INTERTNA Y BURO
  
  VF <- left_join(main1,weightXcliente,by="customer_id") %>%
  mutate( 
    
         aplicacionori1= ifelse(is.na(aplicacionori),0, aplicacionori),
         Weightfinal1  = ifelse(is.na(Weightfinal)  ,0, Weightfinal  ),
          
          
          Primera = tardiofinal + aplicacionori1-Weightfinal1,
         
         
   scinicial = case_when(
                          Primera <=0                 ~ "<=0",
                          between(Primera,1,200)      ~ "<=200",
                          between(Primera,201,450)    ~ "<=450",
                          between(Primera,451,650)    ~ "<=650",
                          between(Primera,651,850)    ~ "<=850",
                          Primera > 850               ~ ">850"), 
   
   Sc1      = case_when(
     
                        scinicial == "<=0" & rangosfprevias== "SinInfo"  ~ 1,
                        scinicial == "<=0" & rangosfprevias== "0"        ~ 1,
                        scinicial == "<=0" & rangosfprevias== "<=5"      ~ 1,
                        scinicial == "<=0" & rangosfprevias== "<=10"     ~ 1,
                        scinicial == "<=0" & rangosfprevias== "<=15"     ~ 1,
                        scinicial == "<=0" & rangosfprevias== "<=25"     ~ 1,
                        scinicial == "<=0" & rangosfprevias== "<=35"     ~ 1,
                        scinicial == "<=0" & rangosfprevias== ">35"      ~ 1,
                        
                        scinicial == "<=200" & rangosfprevias== "SinInfo"  ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "0"        ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "<=5"      ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "<=10"     ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "<=15"     ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "<=25"     ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== "<=35"     ~1.5 ,
                        scinicial == "<=200" & rangosfprevias== ">35"      ~1.5 ,
                        
                        scinicial == "<=450" & rangosfprevias== "SinInfo"  ~2 ,
                        scinicial == "<=450" & rangosfprevias== "0"        ~2 ,
                        scinicial == "<=450" & rangosfprevias== "<=5"      ~2 ,
                        scinicial == "<=450" & rangosfprevias== "<=10"     ~2 ,
                        scinicial == "<=450" & rangosfprevias== "<=15"     ~2 ,
                        scinicial == "<=450" & rangosfprevias== "<=25"     ~2 ,
                        scinicial == "<=450" & rangosfprevias== "<=35"     ~2 ,
                        scinicial == "<=450" & rangosfprevias== ">35"      ~2 ,
                        
                        scinicial == "<=650" & rangosfprevias== "SinInfo"  ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "0"        ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "<=5"      ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "<=10"     ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "<=15"     ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "<=25"     ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== "<=35"     ~2.5 ,
                        scinicial == "<=650" & rangosfprevias== ">35"      ~2.5 ,
                        
                        scinicial == "<=850" & rangosfprevias== "SinInfo"  ~3 ,
                        scinicial == "<=850" & rangosfprevias== "0"        ~3 ,
                        scinicial == "<=850" & rangosfprevias== "<=5"      ~3 ,
                        scinicial == "<=850" & rangosfprevias== "<=10"     ~3 ,
                        scinicial == "<=850" & rangosfprevias== "<=15"     ~3 ,
                        scinicial == "<=850" & rangosfprevias== "<=25"     ~3 ,
                        scinicial == "<=850" & rangosfprevias== "<=35"     ~3 ,
                        scinicial == "<=850" & rangosfprevias== ">35"      ~3 ,
                        
                        scinicial == ">850" & rangosfprevias== "SinInfo"  ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "0"        ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "<=5"      ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "<=10"     ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "<=15"     ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "<=25"     ~3.5 ,
                        scinicial == ">850" & rangosfprevias== "<=35"     ~3.5 ,
                        scinicial == ">850" & rangosfprevias== ">35"      ~3.5 ),
   
   
   SC2= Sc1*Primera,
   
   
   #COOLUMNA FINAL PARA EL OTORGAMIENTO DEL FINANCIAMIENTO
   
   OHDTOP  = case_when(
                       SC2 <=0                 ~ "<=0",   #Denegados
                       between(SC2,1,200)      ~ "<=200", #Aprobado bajo revision de otras variables (segmentos) 
                       between(SC2,201,450)    ~ "<=450", #Aprobados
                       between(SC2,451,650)    ~ "<=650", #Aprobados
                       between(SC2,651,850)    ~ "<=850", #Aprobados
                       SC2 > 850               ~ ">850") #Aprobados 
  )
        
        
        
  
  
  prueba3 <-VF %>% 
    group_by(OHDTOP) %>% 
    summarise(customer_id = n(),) 
  
  
  prueba5 <-VF %>% 
    group_by(Weightfinal) %>% 
    summarise(customer_id = n(),)   
  
  
  write.csv(VF, "C:/Users/HP/Desktop/modelo/VF.csv", row.names = FALSE)
  write.csv(main1, "C:/Users/HP/Desktop/modelo/main1.csv", row.names = FALSE)
  write.csv(prueba5, "C:/Users/HP/Desktop/modelo/cuadro1.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  

  #prueba1 <- credit1 %>% 
  #group_by(carrythatweight) %>% 
  #summarise(Cuentas = n(),)       

  
  #atvspagcor <- credit1 %>% 
  #group_by(customer_id, rangodesdup) %>% 
  #summarise(Cuentas = n(),)
    
    


  #deduplicated_main1 <- unique(main1)
  #VF <- left_join(main1,atvspagcor,by="customer_id")

         
         
           
 







    
#write.csv(main1, "C:/Users/HP/Desktop/modelo/main1.csv", row.names = FALSE)
    






  #negativos = if_else(totalinqui<0, "negativos","positivos") %>%#
  #filter(negativos == "negativos") %>%#
  #summarise(count_negativos = n())#
#print(main1$count_negativos)#
    
     


    #negativos = if_else(diffecha<0, "negativos","positivos")) %>%#
    #filter(negativos == "negativos") %>%#
    #summarise(count_negativos = n())#
    #print(main1$count_negativos)#

    #negativosbnpl = if_else(bnpldif < 0, "negativos", "positivos")) %>%#
    #filter(negativosbnpl == "negativos") %>%#
    #summarise(count_negativos = n())#
    #print(main1$count_negativos)#

    

 


