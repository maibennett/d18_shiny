library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    ### For Style and Format
    
    tags$head(
        HTML('<meta property="og:title" content="Distrito 18 en Numeros">
    <meta property="og:image" content = "https://raw.githubusercontent.com/maibennett/d18/main/images/chart.png">
    <meta property="og:description" content="Visualizacion y Simulaciones para el Distrito 18">')),
    
    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/maibennett/d18/main/images/chart.png")),
    
    tags$head(HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">')),
    
    tags$head(
        
        tags$style("
      .myFooter{height:80px;}
      .mySpace{height:10px;
                margin-top: 20px}
      .mySummary{height:70px;
                margin-bottom:20px;}
      .myBarChart{min-height:400px;}
      .myLineChart{height:600px;}
      p.big {line-height: 1.8;
              color: #6D6F73}
      p.medium {line-height: 1.3;
                color: #6D6F73}
      ul.big {line-height: 1.3;
            color: #6D6F73}
      ul.big2 {line-height: 1.5;
            color: #6D6F73}
      p.note {
               color: #6D6F73;
               font-size: 12px;
               font-family: 'Roboto Condensed', sans-serif;
               font-weight: 100;
               margin-bottom: 10px
      }
    .center {
      display: block;
      margin-left: auto;
      margin-right: auto;
      width: 50%;
    }"
        ),
        
        tags$style("#spanish{
                  font-size: 22px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 400;
                  }"),
        
        tags$style("#tab{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab1{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab2{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab3{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab4{color: #6D6F73;
                  font-size: 25px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 500;
                  }"),
        
        tags$style("#note{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#note2{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#note3{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update1{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update2{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update3{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update4{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update5{color: #6D6F73;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 600;
                  }"),
        
        tags$style("#TotalCases{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        tags$style("#NewCases{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewDeaths{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalDeaths{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalRecovered{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewTests{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalHosp{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewICU{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style(HTML("

  
                .box.box-solid.box-primary {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-info {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-success {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-primary>.box-header {
                    color:#ffffff;
                    background:#ee204d;
                    text-align: center;

                }
  
                .box.box-solid.box-primary>.box-body{
                    color:#ffffff;
                    background:#fe7d84;
                    border: 5px solid #ee204d
                }
                
                .box.box-solid.box-info>.box-header {
                    color:#ffffff;
                    background:#4198b6;
                    text-align: center;

                }
                .box.box-solid.box-info>.box-body{
                    color:#ffffff;
                    background:#6ab2ca;
                    border: 5px solid #4198b6
                }
                
                .box.box-solid.box-success>.box-header {
                    color:#ffffff;
                    background:#bcd25b;
                    text-align: center;

                }
                .box.box-solid.box-success>.box-body{
                    color:#ffffff;
                    background:#c9df67;
                    border: 5px solid #bcd25b
                }

                ")),
        
        tags$style(HTML("
      #separator {
          border-top: 5px dashed #6D6F73;
      }
    ")),
        
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@700&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@300&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@500&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@100&display=swap');
      
      @import url('https://fonts.googleapis.com/css?family=Fira+Code:700&display=swap');
      
      h1 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 700;
      };
      
      #total{color: #6D6F73;
                    font-size: 20px;
                    font-family: 'Roboto Condensed', sans-serif;
        font-weight: 700;
                    };
              
      h3 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
      };
      
      h4 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 400;
      };
      
      h5 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;
      };
      
      h6 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 100;
      };
      
      ol {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
      };
      
      ul {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
      }
      
    "))
    ),
    ### Actual setup starts here:
    
    useShinydashboard(),
    
    titlePanel(h1("D18 en numeros"),
               windowTitle = "D18 en numeros"),
    
    navbarPage(h4("Menu",
                  style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),id="panels",
               
               tabPanel(h4("Simulaciones",
                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),value="residence",
                        fluidRow(column(12,textOutput('total'),
                                        tags$style("#total{color: #6D6F73;
                  font-size: 50px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"))),
                        fluidRow(column(12, textOutput("update")),
                                 tags$style('margin-bottom: 10px;')),
                        fluidRow(column(12,
                                        h4(HTML("<p class='big'>Simulaciones para la eleccion de <b>Convencion Constitucional en el Distrito 18</b>:<br>
                          <ul class='big'><li> Escenarios segun participacion, numero de listas, y contigencias.</li>
                          <li> [Proximamente: Ajustes por paridad]</li></ul><br></p>")
                                        ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
                        fluidRow(column(12,
                                        h3("Numeros de Listas:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        fluidRow(
                            column(
                                width=3, 
                                selectizeInput(
                                    "listasderecha", label=h5("Derecha:",
                                                        style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                            ),
                            column(
                                width=3, 
                                selectizeInput(
                                    "listasconcerta", label=h5("Ex-Concertacion:",
                                                     style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                            ),
                            column(
                                width=3, 
                                selectizeInput(
                                    "listasizq", label=h5("Izquierda:",
                                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                            ),
                            column(
                                width=3, 
                                selectizeInput(
                                    "listasindep", label=h5("Independientes",
                                                              style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                            )
                        ),
                        fluidRow(column(12,
                                        h3("Parametros de participacion:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        fluidRow(
                          column(
                            width=4, 
                            sliderInput(
                              "EfectoPandemia", label=h5("Efecto Pandemia:",
                                                        style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 0.5, min=0, max=1, step = 0.25,width="90%")
                          ),
                          column(
                            width=4, 
                            selectizeInput(
                              "EfectoPlebiscito", label=h5("Efecto Plebiscito:",
                                                          style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                          ),
                          column(
                            width=4, 
                            selectizeInput(
                              "metric", label=h5("% o num de votos?",
                                                    style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=NULL, width="90%")
                          )
                        ),
                        fluidRow(column(12,plotlyOutput("Votes2020",width="100%"))
                        ),
                        fluidRow(column(12, HTML(paste0("<p class='note'>","Nota: Participacion basada en padron 2020, participacion 2017-2020, y votacion para COREs 2017","</p>")))),
                        
                        fluidRow(column(12,"")
                        ),
                        
                        fluidRow(column(12,plotlyOutput("Cupos2020",width="100%"))
                        ),
                        fluidRow(column(12, HTML(paste0("<p class='note'>","Nota: Participacion basada en padron 2020, participacion 2017-2020, y votacion para COREs 2017","</p>"))))
               )
    ) 
))
