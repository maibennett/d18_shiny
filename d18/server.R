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

## ENGLISH VERSION

hrbrthemes::update_geom_font_defaults(family=font_fsm)

URLd18 = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/d_export.csv"
URLd18_listas = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/resultados_core2017.csv"
URLd18_efectos = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/efectos.csv"

d = read.csv(URLd18)
d_listas = read.csv(URLd18_listas)
d_efectos = read.csv(URLd18_efectos)

d = left_join(d,d_efectos,by="COMUNA")

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
        d$p = d$participacion2020_proy2017
        d$votes2020 = NA
        
        if(input$EfectoPlebiscito=="Si"){
            d$p = d$p*(1+d$EfectoPlebiscito)
        }
        
        d$p = d$p*(1+d$EfectoCOVID*input$EfectoPandemia)
        
        d_part = d %>%
            mutate(votes2020 = p*n2020)
        
        d_part = d_part %>% group_by(COMUNA) %>%
            summarise(n = sum(n2020), vote = sum(votes2020))
        
        d_part$p = d_part$vote/d_part$n
        
        d_part
    })
    
    
    d_votes = reactive({
        
        d_listas$lista_select = NA
        
        if(input$listasderecha==1){
            d_listas$lista_select[d_listas$Lista %in% c("Derecha1","Derecha2")] = "Derecha" 
        }
        
        if(input$listasderecha==2){
            d_listas$lista_select[d_listas$Lista %in% c("Derecha1")] = "Derecha 1"
            d_listas$lista_select[d_listas$Lista %in% c("Derecha2")] = "Derecha 2"
        }
        
        
        if(input$listasconcerta==1){
            d_listas$lista_select[d_listas$Lista %in% c("Concertacion1","Concertacion2")] = "Ex-Concertacion" 
        }
        
        if(input$listasconcerta==2){
            d_listas$lista_select[d_listas$Lista %in% c("Concertacion1")] = "Ex-Concertacion 1"
            d_listas$lista_select[d_listas$Lista %in% c("Concertacion2")] = "Ex-Concertacion 2"
        }
        
        if(input$listasizq==0){
            if(input$listasconcerta==1){
                d_listas$lista_select[d_listas$Lista %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion"
            }
            
            if(input$listasconcerta==2){
                d_listas$lista_select[d_listas$Lista %in% c("Izquierda1","Izquierda2")] = "Ex-Concertacion 1"
            }
        }
        
        if(input$listasizq==1){
            d_listas$lista_select[d_listas$Lista %in% c("Izquierda1","Izquierda2")] = "Izquierda"
        }
        
        if(input$listasizq==2){
            d_listas$lista_select[d_listas$Lista %in% c("Izquierda1")] = "Izquierda 1"
            d_listas$lista_select[d_listas$Lista %in% c("Izquierda2")] = "Izquierda 2"
        }
        
        if(input$listasindep==1){
            d_listas$lista_select[d_listas$Indepdentientes==1] = "Independientes"
        }
        
        #d_listas$lista_select = factor(d_listas$lista_select)
        
        d_votes = d_listas %>% group_by(COMUNA, lista_select) %>%
            summarise(n_votos2017 = sum(n_votos), perc_votos2017 = sum(perc_votos))
        
        d_votes
    })
        

    d_all = reactive(left_join(d_votes(),d_participacion(),by="COMUNA"))
 
    renderBarChart = function() {
        renderPlotly({
            
            data = d_all()
            
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
                layout(title = list(text = "Resultados Votacion por Lista (Simulacion 2021)",y=1.2),
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
                                     bordercolor=alpha("#000000FF",0)))
            
            
            plt
            
        })
    }
    
    
    renderElectedChart = function() {
        renderPlotly({
            
            data2 = d_all()
            
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
                layout(title = list(text = "Numero de cupos por lista (Simulacion 2021)",y=1.2),
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
                                     bordercolor=alpha("#000000FF",0))) %>%
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
    
    output$Votes2020 = renderBarChart()
    output$Cupos2020 = renderElectedChart()

}
