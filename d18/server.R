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
 
    updateSelectInput(session, "listasderecha", choices=c(1,2), 
                      selected=1)
    updateSelectInput(session, "listasconcerta", choices=c(1,2), 
                      selected=1)
    updateSelectInput(session, "listasizq", choices=c(0,1,2), 
                      selected=1)
    updateSelectInput(session, "listasindep", choices=c(0,1), 
                      selected=0)
    
    
    updateSliderInput(session, "EfectoPandemia", value = 0.5, min = 0, max = 1,
                      step = 0.25)
    updateSelectInput(session, "EfectoPlebiscito", choices=c("Si","No"), 
                      selected="Si")
    
    updateSelectInput(session, "metric", choices=c("Porcentaje","N Votos"), 
                      selected="Porcentaje")
    
    d_participacion = reactive({
        #d$p = d$participacion2020_proy2017
        d$p = d$p2020
        d$votes2020 = NA
        
        #if(input$EfectoPlebiscito=="Si"){
        if(input$EfectoPlebiscito=="No"){
            #d$p = d$p*(1+d$EfectoPlebiscito)
            d$p = d$p*(1-d$EfectoPlebiscito)
        }
        
        #d$p = d$p*(1+d$EfectoCOVID*input$EfectoPandemia)
        d$p = d$p*(1-d$EfectoCOVID*(1-input$EfectoPandemia))
        
        d_part = d %>%
            mutate(votes2020 = p*n2020)
        
        d_part = d_part %>% group_by(COMUNA) %>%
            summarise(n = sum(n2020), vote = sum(votes2020))
        
        d_part$p = d_part$vote/d_part$n
        
        d_part
    })
    
    
    d_votes_cores = reactive({
        
            d_listas = d_listas_cores
            
            d_listas$lista_select = NA
            
            if(input$listasderecha==1){
                d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1","Derecha2")] = "Derecha" 
            }
            
            if(input$listasderecha==2){
                d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1")] = "Derecha 1"
                d_listas$lista_select[d_listas$Tendencia %in% c("Derecha2")] = "Derecha 2"
            }
            
            
            if(input$listasconcerta==1){
                d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1","Concertacion2","Concertacion3")] = "Ex-Concertacion" 
            }
            
            if(input$listasconcerta==2){
                d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1")] = "Ex-Concertacion 1"
                d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion2","Concertacion3")] = "Ex-Concertacion 2"
            }
            
            if(input$listasizq==0){
                if(input$listasconcerta==1){
                    d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion"
                }
                
                if(input$listasconcerta==2){
                    d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion 1"
                }
            }
            
            if(input$listasizq==1){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Izquierda"
            }
            
            if(input$listasizq==2){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1")] = "Izquierda 1"
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda2")] = "Izquierda 2"
            }
            
            if(input$listasindep==1){
                d_listas$lista_select[!is.na(d_listas$Independientes)] = "Independientes"
            }
            
            #d_listas$lista_select = factor(d_listas$lista_select)
            
            d_votes = d_listas %>% group_by(COMUNA, lista_select) %>%
                summarise(n_votos2017 = sum(n_votos), perc_votos2017 = sum(perc_votos_comuna))
            
            d_votes

    })
    
    
    d_votes_diputados = reactive({
        
        d_listas = d_listas_diputados
        
        d_listas$lista_select = NA
        
        if(input$listasderecha==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1","Derecha2")] = "Derecha" 
        }
        
        if(input$listasderecha==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1","Derecha2")] = "Derecha" 
        }
        
        
        if(input$listasconcerta==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1","Concertacion2","Concertacion3","Concertacion4")] = "Ex-Concertacion" 
        }
        
        if(input$listasconcerta==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion2")] = "Ex-Concertacion 1"
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1","Concertacion3","Concertacion4")] = "Ex-Concertacion 2"
        }
        
        if(input$listasizq==0){
            if(input$listasconcerta==1){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion"
            }
            
            if(input$listasconcerta==2){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion 1"
            }
        }
        
        if(input$listasizq==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Izquierda"
        }
        
        if(input$listasizq==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1")] = "Izquierda 1"
            d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda2")] = "Izquierda 2"
        }
        
        if(input$listasindep==1){
            d_listas$lista_select[!is.na(d_listas$Independientes)] = "Independientes"
        }
        
        #d_listas$lista_select = factor(d_listas$lista_select)
        
        d_votes = d_listas %>% group_by(COMUNA, lista_select) %>%
            summarise(n_votos2017 = sum(n_votos), perc_votos2017 = sum(perc_votos_comuna))
        
        d_votes
        
    })
    
    
    d_votes_concejales = reactive({
        
        d_listas = d_listas_diputados
        
        d_listas$lista_select = NA
        
        if(input$listasderecha==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1","Derecha2")] = "Derecha" 
        }
        
        if(input$listasderecha==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Derecha1","Derecha2")] = "Derecha 1"
            d_listas$lista_select[d_listas$Tendencia %in% c("Derecha3")] = "Derecha 2"
        }
        
        
        if(input$listasconcerta==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1","Concertacion2","Concertacion3","Concertacion4")] = "Ex-Concertacion" 
        }
        
        if(input$listasconcerta==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion2")] = "Ex-Concertacion 1"
            d_listas$lista_select[d_listas$Tendencia %in% c("Concertacion1","Concertacion3","Concertacion4")] = "Ex-Concertacion 2"
        }
        
        if(input$listasizq==0){
            if(input$listasconcerta==1){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion"
            }
            
            if(input$listasconcerta==2){
                d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion 1"
            }
        }
        
        if(input$listasizq==1){
            d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Izquierda"
        }
        
        if(input$listasizq==2){
            d_listas$lista_select[d_listas$Tendencia %in% c("Izquierda1","Izquierda2")] = "Izquierda"
        }
        
        if(input$listasindep==0){
            d_listas$lista_select[d_listas$Tendencia=="Independiente"] = "Independientes"
        }
        
        if(input$listasindep==1){
            d_listas$lista_select[!is.na(d_listas$Independientes)] = "Independientes"
        }
        
        #d_listas$lista_select = factor(d_listas$lista_select)
        
        d_votes = d_listas %>% group_by(COMUNA, lista_select) %>%
            summarise(n_votos2017 = sum(n_votos), perc_votos2017 = sum(perc_votos_comuna))
        
        d_votes
        
    })
        

    d_all_cores = reactive(left_join(d_votes_cores(),d_participacion(),by="COMUNA"))
    
    d_all_diputados = reactive(left_join(d_votes_diputados(),d_participacion(),by="COMUNA"))
    
    d_all_concejales = reactive(left_join(d_votes_concejales(),d_participacion(),by="COMUNA"))
 
    
    renderBarChart_cores = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            
            data = d_all_cores()
            
            data$votes2020 = data$vote*data$perc_votos2017
            
            if(input$metric=="Porcentaje"){
                data$metric = round(data$perc_votos2017*100,1)
            }
            
            if(input$metric=="N Votos"){
                data$metric = round(data$votes2020,0)
            }
                
            if(length(unique(data$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            
                
            plt = data %>%
                plot_ly(x = ~COMUNA, y = ~metric, color = ~lista_select, type = 'bar',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Resultados Votacion por Lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Votacion 2021', 
                                    showgrid = FALSE, zeroline = TRUE),
                       barmode = 'stack',
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend,
                                     orientation = 'h',
                                     yanchor = 'top'),
                       margin = m)
            
            
            plt
            
        })
    }
    
    
    renderElectedChart_cores = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            data2 = d_all_cores()
            
            data2$votes2020 = data2$vote*data2$perc_votos2017
            
            n_cupos = 4
            
            # Estimar D'Hondt para cada lista:
            
            alloc = 0
            
            d_all_listas = data2 %>% group_by(lista_select) %>%
                summarise(total_votes = sum(votes2020))
            
            d_all_listas$lista_select = as.character(d_all_listas$lista_select)
            
            cupos = rep(NA,n_cupos)
            
            d_all_listas$cupos_aux = 1
            
            for(i in 1:n_cupos){
                
                d_all_listas$q = d_all_listas$total_votes/d_all_listas$cupos_aux
                
                cupos[i] = d_all_listas$lista_select[d_all_listas$q==
                                                         max(d_all_listas$q)]
                alloc = alloc + 1
                
                d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] = 
                    d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] + 1
            }
            
            d_all_listas$cupos = d_all_listas$cupos_aux - 1
            
            if(length(unique(data2$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data2$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data2$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            plt = d_all_listas %>%
                plot_ly(type = 'scatter', mode = 'markers',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Numero de cupos por lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Numero de cupos', 
                                    showgrid = FALSE, zeroline = TRUE,
                                    tick0 = 0, dtick = 1),
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend),
                       margin = m) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 1, showlegend = FALSE,
                          marker = list(size=15, color = 'white',line = list(width = 2, alpha=1))) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 0.5, showlegend = FALSE,
                          marker = list(size=11, line = list(width = 2, alpha=1)),
                          hoverinfo = "text",text=~paste0(lista_select,": \n",
                                                          cupos," cupos\n",
                                                          round(total_votes,0)," votos"))
            
            
            plt
            
        })
    }
    
    output$Votes2020_cores = renderBarChart_cores()
    output$Cupos2020_cores = renderElectedChart_cores()
    
    
    
    renderBarChart_diputados = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            data = d_all_diputados()
            
            data$votes2020 = data$vote*data$perc_votos2017
            
            if(input$metric=="Porcentaje"){
                data$metric = round(data$perc_votos2017*100,1)
            }
            
            if(input$metric=="N Votos"){
                data$metric = round(data$votes2020,0)
            }
            
            if(length(unique(data$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            
            
            plt = data %>%
                plot_ly(x = ~COMUNA, y = ~metric, color = ~lista_select, type = 'bar',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Resultados Votacion por Lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Votacion 2021', 
                                    showgrid = FALSE, zeroline = TRUE),
                       barmode = 'stack',
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend,
                                     orientation = 'h',
                                     yanchor = 'top'),
                       margin = m)
            
            
            plt
            
        })
    }
    
    
    renderElectedChart_diputados = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            data2 = d_all_diputados()
            
            data2$votes2020 = data2$vote*data2$perc_votos2017
            
            n_cupos = 4
            
            # Estimar D'Hondt para cada lista:
            
            alloc = 0
            
            d_all_listas = data2 %>% group_by(lista_select) %>%
                summarise(total_votes = sum(votes2020))
            
            d_all_listas$lista_select = as.character(d_all_listas$lista_select)
            
            cupos = rep(NA,n_cupos)
            
            d_all_listas$cupos_aux = 1
            
            for(i in 1:n_cupos){
                
                d_all_listas$q = d_all_listas$total_votes/d_all_listas$cupos_aux
                
                cupos[i] = d_all_listas$lista_select[d_all_listas$q==
                                                         max(d_all_listas$q)]
                alloc = alloc + 1
                
                d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] = 
                    d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] + 1
            }
            
            d_all_listas$cupos = d_all_listas$cupos_aux - 1
            
            if(length(unique(data2$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data2$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data2$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            plt = d_all_listas %>%
                plot_ly(type = 'scatter', mode = 'markers',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Numero de cupos por lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Numero de cupos', 
                                    showgrid = FALSE, zeroline = TRUE,
                                    tick0 = 0, dtick = 1),
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend),
                       margin = m) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 1, showlegend = FALSE,
                          marker = list(size=15, color = 'white',line = list(width = 2, alpha=1))) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 0.5, showlegend = FALSE,
                          marker = list(size=11, line = list(width = 2, alpha=1)),
                          hoverinfo = "text",text=~paste0(lista_select,": \n",
                                                          cupos," cupos\n",
                                                          round(total_votes,0)," votos"))
            
            
            plt
            
        })
    }
    
    output$Votes2020_diputados = renderBarChart_diputados()
    output$Cupos2020_diputados = renderElectedChart_diputados()
    
    
    renderBarChart_concejales = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            data = d_all_concejales()
            
            data$votes2020 = data$vote*data$perc_votos2017
            
            if(input$metric=="Porcentaje"){
                data$metric = round(data$perc_votos2017*100,1)
            }
            
            if(input$metric=="N Votos"){
                data$metric = round(data$votes2020,0)
            }
            
            if(length(unique(data$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            
            
            plt = data %>%
                plot_ly(x = ~COMUNA, y = ~metric, color = ~lista_select, type = 'bar',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Resultados Votacion por Lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Votacion 2021', 
                                    showgrid = FALSE, zeroline = TRUE),
                       barmode = 'stack',
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend,
                                     orientation = 'h',
                                     yanchor = 'top'),
                       margin = m)
            
            
            plt
            
        })
    }
    
    
    renderElectedChart_concejales = function() {
        renderPlotly({
            
            is_mobile_device <- isTRUE(input$isMobile)
            
            if(is_mobile_device==TRUE){
                ## Parameters
                y_legend = -0.5
                
            }
            
            if(is_mobile_device==FALSE){
                ## Parameters
                y_legend = -0.1
            }
            
            m <- list(
                l = 50,
                r = 50,
                b = 100,
                t = 90,
                pad = 4
            )
            
            data2 = d_all_concejales()
            
            data2$votes2020 = data2$vote*data2$perc_votos2017
            
            n_cupos = 4
            
            # Estimar D'Hondt para cada lista:
            
            alloc = 0
            
            d_all_listas = data2 %>% group_by(lista_select) %>%
                summarise(total_votes = sum(votes2020))
            
            d_all_listas$lista_select = as.character(d_all_listas$lista_select)
            
            cupos = rep(NA,n_cupos)
            
            d_all_listas$cupos_aux = 1
            
            for(i in 1:n_cupos){
                
                d_all_listas$q = d_all_listas$total_votes/d_all_listas$cupos_aux
                
                cupos[i] = d_all_listas$lista_select[d_all_listas$q==
                                                         max(d_all_listas$q)]
                alloc = alloc + 1
                
                d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] = 
                    d_all_listas$cupos_aux[d_all_listas$lista_select %in% cupos[i]] + 1
            }
            
            d_all_listas$cupos = d_all_listas$cupos_aux - 1
            
            if(length(unique(data2$lista_select))==3){
                col_pal = c("#900DA4",
                            "#F89441","#FCCE25")
            }
            
            if(length(unique(data2$lista_select))==2){
                col_pal = c("#900DA4","#FCCE25")
            }
            if(length(unique(data2$lista_select))>3){
                col_pal = c("#0D0887","#5601A4","#900DA4",
                            "#BF3984","#F89441","#FCCE25")
            }
            
            plt = d_all_listas %>%
                plot_ly(type = 'scatter', mode = 'markers',
                        colors = col_pal)  %>% 
                layout(title = list(text = "Numero de cupos por lista\n (Simulacion 2021)",y=1.2),
                       xaxis = list(title = ""),
                       yaxis = list(side = 'left', title = 'Numero de cupos', 
                                    showgrid = FALSE, zeroline = TRUE,
                                    tick0 = 0, dtick = 1),
                       #margin=m1,
                       plot_bgcolor="rgba(0, 0, 0, 0)",
                       paper_bgcolor="rgba(0, 0, 0, 0)",
                       legend = list(orientation = 'h',
                                     xanchor = 'center',
                                     x=0.5,
                                     bgcolor=alpha("#000000FF",0),
                                     bordercolor=alpha("#000000FF",0),
                                     y = y_legend),
                       margin = m) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 1, showlegend = FALSE,
                          marker = list(size=15, color = 'white',line = list(width = 2, alpha=1))) %>%
                add_trace(x = ~lista_select, y = ~cupos, 
                          color = ~lista_select, opacity = 0.5, showlegend = FALSE,
                          marker = list(size=11, line = list(width = 2, alpha=1)),
                          hoverinfo = "text",text=~paste0(lista_select,": \n",
                                                          cupos," cupos\n",
                                                          round(total_votes,0)," votos"))
            
            
            plt
            
        })
    }
    
    output$Votes2020_concejales = renderBarChart_concejales()
    output$Cupos2020_concejales = renderElectedChart_concejales()
    
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
