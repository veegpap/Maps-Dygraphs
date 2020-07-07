
library(shiny)
library(dygraphs)
library(leaflet)

  
  navbarPage("My Phd", collapsible = TRUE, 
             
             
             
             tabPanel("Map", 
                      column(2, 
                             
                             tags$head(
                               tags$style(HTML(" .optgroup-header {
                                 color: darkred !important;
                                 font-weight:bold !important;
                                 font-size: 14px !important;
                              }
                                      "))),
                             
                             selectInput("paramo", "Select a parameter",
                                         choices = c("theta","ksi","kapa","lamda")),
                             selectInput("time", "Select year",
                                         list(Calculations=c(1960,1970,1980,1990,2000,2010),
                                              Forecast=c(2015,2020,2025,2030,2035,2040,2044,2045)) 
                                         #choices = c(1960:2045)
                                         ),
                             radioButtons("mode", label = "Select map mode", inline = F, choices = list("palette","mean"), 
                                          selected = "palette")
                             ),
                      column(10,
                             column(6,
                                    fluidRow(align = "left", h4("MALES"), leafletOutput("malemap",height=500)),
                                    fluidRow(align = "center", plotOutput("malehist",height=300))
                                    #fluidRow(align = "center",h4("Statistics"), tableOutput("malestat"))
                                    ),
                             column(6,
                                    fluidRow(h4("FEMALES"), leafletOutput("femalemap",height=500)),
                                    fluidRow(align = "center", plotOutput("femalehist",height=300))
                                    #fluidRow(align = "center",h4("Statistics"), tableOutput("femalestat"))
                             ))
                      ),
             
             tabPanel("Graph", 
                      fluidRow(align = "center",
                        selectInput("country", "Select a country from the list", 
                                           choices = c("Denmark", "Finland", "France", "Island", "Italy","Netherlands", "Norway", "Sweden", "Switzerland",
                                                       "Austria","Belgium", "Czechia", "Ireland","Estonia","Latvia","Lithuania","Germany", "Greece","Poland",
                                                       "Portugal","Spain","United Kingdom"),
                                           selected = "Denmark", multiple = FALSE,
                                           selectize = T, width = "200px", size = NULL)),
                      fluidRow(
                        column(6, dygraphOutput("theta_graf", height="300px")),
                        column(6, dygraphOutput("ksi_graf", height="300px"))
                      ),
                      fluidRow(
                        column(6, dygraphOutput("kapa_graf", height="300px")),
                        column(6, dygraphOutput("lamda_graf", height="300px"))
                      ))
             
             
             
             
             
  )
  


