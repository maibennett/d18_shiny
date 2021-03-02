library(dplyr)
library(tidyr)
library(RColorBrewer)
library(firasans)
library("shades")
library("colorspace")
library(htmltools)
library("ggplot2")
library("ggmap")
library(wesanderson)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(hrbrthemes)
library(stringr)
library(emo)
library(leaflet)
library(rgdal) 
library(geojson)
library(htmltools)

hrbrthemes::update_geom_font_defaults(family=font_fsm)

URLd18 = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/d_export.csv"
URLd18_listas_cores = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_core2017_candidatos.csv"
URLd18_listas_diputados = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_diputados2017_candidatos.csv"
URLd18_listas_concejales = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_concejales2016_candidatos.csv"

URLd18_efectos = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/efectos.csv"

d = read.csv(URLd18)
d_listas_cores = read.csv(URLd18_listas_cores)
d_listas_diputados = read.csv(URLd18_listas_diputados)
d_listas_concejales = read.csv(URLd18_listas_concejales)
d_efectos = read.csv(URLd18_efectos)

d = left_join(d,d_efectos,by="COMUNA")

url <- 'https://raw.githubusercontent.com/pachamaltese/chilemapas/master/data_geojson/comunas/r07.geojson'
comunas_r07 <- rgdal::readOGR(url)

comunas_d18 = as.data.frame(cbind(c("Cauquenes","Chanco","Colbun","Linares","Longavi","Parral",
                      "Pelluhue","Retiro","San Javier","Villa Alegre","Yerbas Buenas"),
                    c("07201","07202","07402","07401","07403","07404",
                      "07203","07405","07406","07407","07408")))
names(comunas_d18) = c("COMUNA","codigo_comuna")

comunas_r07 <- merge(comunas_r07,comunas_d18,by="codigo_comuna") 

comunas_d18 = comunas_r07[!is.na(comunas_r07$COMUNA),]

# Define server logic required to draw a histogram
server = function(input, output, session) {

    
    ##########################################################################
    updateSelectInput(session, "eleccion", choices=c("CORES 2017","Diputados 2017","Concejales 2016"), 
                      selected="CORES 2017")
    
    updateSelectInput(session, "partido", choices=c("Todos (No derecha)","PS","DC","PPD","PRSD"), 
                      selected="Todos (No derecha)")
    
    updateSelectInput(session, "metric2", choices=c("Num. Votos","Porcentaje"), 
                      selected="Num. Votos")
    
    d_eleccion_cores = reactive({
        
        d2 = d_listas_cores
            
        d2 = d2[!(d2$Tendencia %in% c("Derecha1","Derecha2","Derecha3")),]
                
        d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
            summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                
        d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
        d2_sum = d2 %>% group_by(COMUNA) %>% 
            summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
        d2_sum$metric = d2_sum$n
                
        if(input$metric2 == "Porcentaje"){
            d2_sum$metric = d2_sum$perc*100
        }
                
        d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
        d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
        d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
            
        if(input$partido=="PS"){
            d2 = d2[d2$Partido %in% c("PARTIDO SOCIALISTA DE CHILE","SOCIALISTA DE CHILE"),]
            
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
            
            d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
            
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
            
            d2_sum$metric = d2_sum$n
            
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
            
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
            
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
            
        }
        
        if(input$partido=="DC"){
            d2 = d2[d2$Partido %in% c("PARTIDO DEMOCRATA CRISTIANO","DEMOCRATA CRISTIANO"),]
            
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
            
            d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
            
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
            
            d2_sum$metric = d2_sum$n
            
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
            
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
            
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
    
        }
        
        if(input$partido=="PPD"){
            d2 = d2[d2$Partido %in% c("PARTIDO POR LA DEMOCRACIA","POR LA DEMOCRACIA"),]
            
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
            
            d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
            
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
            
            d2_sum$metric = d2_sum$n
            
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
            
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
            
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
            
        }
        
        if(input$partido=="PRSD"){
            d2 = d2[d2$Partido %in% c("PARTIDO RADICAL SOCIALDEMOCRATA","RADICAL SOCIALDEMOCRATA",
                                        "INDEPENDIENTE RADICAL SOCIALDEMOCRATA"),]
            
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
            
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
            
            d2_sum$metric = d2_sum$n
            
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
            
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
            
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
            
        }
    
    
        if(input$eleccion == "Diputados 2017"){
            d2 = d_listas_diputados
            
            if(input$partido=="Todos (No Derecha)"){
                d2 = d2[!(d2$Tendencia %in% c("Derecha1","Derecha2","Derecha3")),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="PS"){
                d2 = d2[d2$Partido %in% c("PARTIDO SOCIALISTA DE CHILE","SOCIALISTA DE CHILE"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="DC"){
                d2 = d2[d2$Partido %in% c("PARTIDO DEMOCRATA CRISTIANO","DEMOCRATA CRISTIANO"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PPD"){
                d2 = d2[d2$Partido %in% c("PARTIDO POR LA DEMOCRACIA","POR LA DEMOCRACIA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PRSD"){
                d2 = d2[d2$Partido %in% c("PARTIDO RADICAL SOCIALDEMOCRATA","RADICAL SOCIALDEMOCRATA",
                                            "INDEPENDIENTE RADICAL SOCIALDEMOCRATA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
        }
        if(input$eleccion == "Concejales 2016"){
            d2 = d_listas_concejales
            
            if(input$partido=="Todos (No Derecha)"){
                d2 = d2[!(d2$Tendencia %in% c("Derecha1","Derecha2","Derecha3")),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="PS"){
                d2 = d2[d2$Partido %in% c("PARTIDO SOCIALISTA DE CHILE","SOCIALISTA DE CHILE"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="DC"){
                d2 = d2[d2$Partido %in% c("PARTIDO DEMOCRATA CRISTIANO","DEMOCRATA CRISTIANO"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PPD"){
                d2 = d2[d2$Partido %in% c("PARTIDO POR LA DEMOCRACIA","POR LA DEMOCRACIA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
               d2_sum
            }
            
            if(input$partido=="PRSD"){
                d2 = d2[d2$Partido %in% c("PARTIDO RADICAL SOCIALDEMOCRATA","RADICAL SOCIALDEMOCRATA",
                                            "INDEPENDIENTE RADICAL SOCIALDEMOCRATA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
        }
        
        labs <- lapply(seq(nrow(d2_sum)), function(i) {
            paste0( '<p>', d2_sum[i, "Candidato"], '<p></p>', 
                    d2_sum[i, "Partido"], '</p><p>', 
                    "N Votos: ",d2_sum[i, "n_cand"],'</p><p>', 
                    "% Votos: ",round(d2_sum[i, "perc_cand"]*100,0), '</p>' ) 
        })
        
        d2_sum
    
    })
    
    
    d_eleccion_diputados = reactive({
        
    
            d2 = d_listas_diputados
            
            d2 = d2[!(d2$Tendencia %in% c("Derecha1","Derecha2","Derecha3")),]
                
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                
            d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
            d2_sum$metric = d2_sum$n
                
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
                
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
            
            
            if(input$partido=="PS"){
                d2 = d2[d2$Partido %in% c("PARTIDO SOCIALISTA DE CHILE","SOCIALISTA DE CHILE"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="DC"){
                d2 = d2[d2$Partido %in% c("PARTIDO DEMOCRATA CRISTIANO","DEMOCRATA CRISTIANO"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PPD"){
                d2 = d2[d2$Partido %in% c("PARTIDO POR LA DEMOCRACIA","POR LA DEMOCRACIA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PRSD"){
                d2 = d2[d2$Partido %in% c("PARTIDO RADICAL SOCIALDEMOCRATA","RADICAL SOCIALDEMOCRATA",
                                          "INDEPENDIENTE RADICAL SOCIALDEMOCRATA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
        
            }
        
        labs <- lapply(seq(nrow(d2_sum)), function(i) {
            paste0( '<p>', d2_sum[i, "Candidato"], '<p></p>', 
                    d2_sum[i, "Partido"], '</p><p>', 
                    "N Votos: ",d2_sum[i, "n_cand"],'</p><p>', 
                    "% Votos: ",round(d2_sum[i, "perc_cand"]*100,0), '</p>' ) 
        })
        
        d2_sum
        
    })
    
    
    d_eleccion_concejales = reactive({
        
    
            d2 = d_listas_concejales
            
            d2 = d2[!(d2$Tendencia %in% c("Derecha1","Derecha2","Derecha3")),]
                
            d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                
            d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
            d2_sum = d2 %>% group_by(COMUNA) %>% 
                summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
            d2_sum$metric = d2_sum$n
                
            if(input$metric2 == "Porcentaje"){
                d2_sum$metric = d2_sum$perc*100
            }
                
            d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
            d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
            d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
            
            if(input$partido=="PS"){
                d2 = d2[d2$Partido %in% c("PARTIDO SOCIALISTA DE CHILE","SOCIALISTA DE CHILE"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
                
            }
            
            if(input$partido=="DC"){
                d2 = d2[d2$Partido %in% c("PARTIDO DEMOCRATA CRISTIANO","DEMOCRATA CRISTIANO"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PPD"){
                d2 = d2[d2$Partido %in% c("PARTIDO POR LA DEMOCRACIA","POR LA DEMOCRACIA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_candidato$perc_cand = round(d2_candidato$perc_cand,2)
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
                
                d2_sum
            }
            
            if(input$partido=="PRSD"){
                d2 = d2[d2$Partido %in% c("PARTIDO RADICAL SOCIALDEMOCRATA","RADICAL SOCIALDEMOCRATA",
                                          "INDEPENDIENTE RADICAL SOCIALDEMOCRATA"),]
                
                d2_candidato = d2 %>% group_by(COMUNA, Pacto, Partido, Candidato) %>% 
                    summarise(n_cand = sum(n_votos), perc_cand = sum(perc_votos_comuna))
                
                d2_sum = d2 %>% group_by(COMUNA) %>% 
                    summarise(n = sum(n_votos), perc = sum(perc_votos_comuna))
                
                d2_sum$metric = d2_sum$n
                
                if(input$metric2 == "Porcentaje"){
                    d2_sum$metric = d2_sum$perc*100
                }
                
                d2_candidato = d2_candidato %>% arrange(COMUNA, desc(n_cand))
                d2_candidato = d2_candidato[!duplicated(d2_candidato$COMUNA),]
                
                d2_sum = left_join(d2_sum, d2_candidato, by="COMUNA")
            
            }
        
        labs <- lapply(seq(nrow(d2_sum)), function(i) {
            paste0( '<p>', d2_sum[i, "Candidato"], '<p></p>', 
                    d2_sum[i, "Partido"], '</p><p>', 
                    "N Votos: ",d2_sum[i, "n_cand"],'</p><p>', 
                    "% Votos: ",round(d2_sum[i, "perc_cand"]*100,0), '</p>' ) 
        })
        
        d2_sum
        
    })
    
    renderMap_comuna = function() {
        renderLeaflet({
            
            d2_sum = d_eleccion_cores()
            
            if(input$eleccion=="Diputados 2017"){
                d2_sum = d_eleccion_diputados()
            }
            
            if(input$eleccion=="Concejales 2016"){
                d2_sum = d_eleccion_concejales()
            }
            
            labs <- lapply(seq(nrow(d2_sum)), function(i) {
                paste0( '<p>', d2_sum[i, "Candidato"], '<p></p>', 
                        d2_sum[i, "Partido"], '</p><p>',
                        d2_sum[i, "Pacto"], '</p><p>',
                        "N Votos: ",d2_sum[i, "n_cand"],'</p><p>', 
                        "Perc. Votos: ",round(d2_sum[i, "perc_cand"]*100,0), '%</p>' ) 
            })
            
            comunas_d18 = merge(comunas_d18,d2_sum,by="COMUNA")
            
            if(input$metric2=="Porcentaje"){
                pal <- colorNumeric("viridis", domain = c(0,100))
            }
            
            if(input$metric2=="Num. Votos"){
                pal <- colorNumeric("viridis", domain = c(0,max(d2_sum$n)))
            }
            
            #is_mobile_device <- isTRUE(input$isMobile)
            
            zoom_v = 8
            #if(is_mobile_device==TRUE){zoom_v = 3}
            
            base_map <- leaflet() %>%
                setView(lng = -71.867, lat = -35.81, zoom = zoom_v) %>%
                addProviderTiles("CartoDB.Positron")
            #addPolygons(data = states, fillColor = "#ffffff",
            #            color = "#ffffff",
            #            fillOpacity = 0,
            #            weight = 2) %>%
            map <- base_map %>% addPolygons(data = comunas_d18, fillColor = ~pal(metric), 
                                            color = "#b2aeae", # you need to use hex colors
                                            fillOpacity = 0.8, 
                                            weight = 1, 
                                            smoothFactor = 0.2) %>%
                addPolygons(data = comunas_d18, fillColor = NULL, 
                            color = "#b2aeae", # you need to use hex colors
                            fillOpacity = 0, 
                            weight = 0, 
                            smoothFactor = 0.2,
                            popup = lapply(labs, htmltools::HTML),
                            popupOptions = popupOptions(maxWidth = 300, closeOnClick = TRUE))
            
           map <-  map %>% addPolylines(data = comunas_d18, color = "white", opacity = 1, weight = 1.2) 
           
           if(input$metric2=="Num. Votos"){
               map <- map %>% addLegend("bottomleft", pal = pal,values = quantile(comunas_d18$metric,probs = seq(0,1,0.25), na.rm=TRUE),
                          title = input$metric2,
                          opacity = 1, bins=3, na.label = "No info")
           }
           
           if(input$metric2=="Porcentaje"){
               map <- map %>% addLegend("bottomleft", pal = pal,values = seq(0,100,25),
                                        title = input$metric2,
                                        opacity = 1, bins=3, na.label = "No info")
           }
           
           map
            
        })
        
    }
    
    output$map_comuna = renderMap_comuna()

}
