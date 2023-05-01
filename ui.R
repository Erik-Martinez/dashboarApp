library(shinydashboard)
library(tidyverse)
library(lubridate)
library(data.table) # funcion fread() para cargar los datos pesados
library(DT)#tablas mas interactivas
library(gridExtra)#combinar graficas
library(shinyWidgets) #opciones extra UI
library(shinyjs) # simbolos de carga
library(shinycssloaders)#simbolos de carga 2
library(plotly)#graficos interactivos
library(forecast)#graficos de series temporales
library(seasonal)#descomposicion de series temporales
#grafica maps
library(geojsonio)# cargar datos mapa
library(htmltools)  # Used for constructing map labels using HTML
library(leaflet)    # The map-making package
library(sp) #usar formato large spatialPolygonsData


# carga valores datos

source("global.R")


#--------------------------------------

#UI

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "APP"
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("Análisis EPA",tabName = "menu1" ,
                         menuSubItem('Información', tabName = 'menu_info_epa'),
                         menuSubItem('Tipos de contrato', tabName = 'menu_contra_epa'),
                         menuSubItem('Sector de ocupación', tabName = 'menu_sector_epa'),
                         menuSubItem('Horas', tabName = 'menu_horas'),
                         menuSubItem('Trabajo publico', tabName = 'menu_publico'),
                         menuSubItem("Autónomos", tabName = "menu_autonomo")
                ),
                
                menuItem("Afiliados a la seguridad social",tabName = "menu2" ,
                         menuSubItem('Información', tabName = 'menu_info_afi'),
                         menuSubItem('por sexo y edad', tabName = 'menu_afi_socio'),
                         menuSubItem('por sector de actividad', tabName = 'menu_afi_sector'),
                         menuSubItem('Serie temporal', tabName = 'menu_serie'),
                         menuSubItem('cotizaciones', tabName = 'menu23')
                ),
                
                menuItem("Análisis Tasa de Paro",tabName = "menu3" ,
                         menuSubItem('Por', tabName = 'menu21'),
                         menuSubItem('Sub Menu 2', tabName = 'menu22'),
                         menuSubItem('Sub Menu 3', tabName = 'menu23')
                )
    ),
    # Agregar botón en el footer
    tags$div(class = "sidebar-footer",
             materialSwitch(
               inputId = "galicia",
               label = "Datos solo de Galicia", 
               right = TRUE
             )
             
    )
  ),
  
  dashboardBody(
    useShinyjs(), #carga de librería dentro de la sección
    tabItems(
      tabItem("menu_info_epa",h1("Pagina 1 en construccion")),
      
  #----------------------------------------------------------#   
      
    #gráfica tipos contratos EPA (id=0)
  
      tabItem("menu_contra_epa",h1("Tipos de contrato"),
              fluidRow(width=12,
                       box( width = 2,
                            title = "Inputs", status = "warning", background = "blue",
                            switchInput("incluir_na", label = "Incluir otros(NA)", 
                                        labelWidth = "80px", onLabel = "Si",
                                        offLabel = "No", onStatus = "success", 
                                        offStatus = "danger", value= 1),
                            selectInput("select_year", h4("Año"), choices = year_list),
                            selectInput("select_trim", h4("Trimestre"), choices=trim_list),
                            selectInput("select_prov", h4("Provincia"), choices = prov_list),
                            selectInput("select_vari", h4("Dividido por:"), 
                                        choices = list_vari)),
                       box(width=10, solidHeader = T, collapsible = F, heightFill = TRUE,
                           shinycssloaders::withSpinner(plotlyOutput("plot_contra", 
                                                                   height = 500)))),
              
                fluidRow(column(width=2), column(width=10,
                                                box(width=10,
                                                    title=div("Datos"),
                                                    dataTableOutput("tabla_contra"))))
              
      ),
      
  #---------------------------------------------------------------#  
  
    #grafica sector de ocupación (id=1)
  
      tabItem("menu_sector_epa",h1("Sector de ocupación"),
              fluidRow(width=12,
                       box( width = 3,
                            title = "Inputs", status = "warning", background = "blue",
                            radioGroupButtons("ocu_act",label = h4("Sistema de clasificaión"),
                                              choices = c("Ocupación (CNO)", "Actividad (CNAE)"),
                                              justified = TRUE, status = "primary"),
                            selectInput("select_year1", h4("Año"), choices = year_list),
                            selectInput("select_trim1", h4("Trimestre"), choices=trim_list),
                            selectInput("select_prov1", h4("Provincia"), choices = prov_list),
                            selectInput("select_vari1", h4("Dividido por:"), 
                                        choices = list_vari)),
                       box(width=9, solidHeader = T, collapsible = F, heightFill =T,
                           shinycssloaders::withSpinner(uiOutput("plots_ocupa")))),
              
              fluidRow(column(width=2), column(width=10,
                                               box(width=10,
                                                   title=div("Datos"),
                                                   dataTableOutput("tabla_ocupa"))))
              
             
              
              
              
              
              ),
  
  #---------------------------------------------------------------#  
  
    #gráficas horas promedios y mapas (id=2)    
  
      tabItem("menu_horas",h1("Estadísticas de horas de trabajo"),
              fluidRow(width=12,
                       box( width = 3,
                            title = "Inputs", status = "warning", background = "blue",
                            selectInput("select_year2", h4("Año"), choices = year_listMod),
                            selectInput("select_trim2", h4("Trimestre"), choices=trim_list),
                            radioGroupButtons("med_mediana",label = "Horas de trabajo semanal",
                              choices = c("Media", "Mediana"),justified = TRUE,
                              status = "primary"),
                            radioGroupButtons("tipo_hora",label = "Tipo horas de trabajo semanal",
                                              choices = c("De contrato", "Reales"),justified = TRUE,
                                              status = "primary")
                       ),
                       box(width=7, solidHeader = T, collapsible = F, heightFill =T,
                           shinycssloaders::withSpinner(leafletOutput("plot_mapHoras"))))
              
      ),
  
  #---------------------------------------------------------------#  
  
    #infobox trabajo publico (id=3)
      
      tabItem("menu_publico",h1("Estadísticas de trabajadores del sector público"),
              fluidRow(width=12,
                       box(width = 3,
                            title = "Inputs", status = "warning", background = "blue",
                            selectInput("select_year3", h4("Año"), choices = year_list),
                            selectInput("select_trim3", h4("Trimestre"), choices=trim_list),
                            selectInput("select_prov3", h4("Provincia"), choices = prov_list),
                            radioGroupButtons( inputId = "select_sexo", label = h4("Sexo"),
                                               choices = c("Ambos", "Hombre", "Mujer"),
                                               justified = TRUE)),
                       box(width = 4, title = "% de trabajores por tipo administración",
                           solidHeader = T, collapsible = F, heightFill =T,
                           shinycssloaders::withSpinner(plotlyOutput("plot_publi1"))),
                       
                       box(width = 4, title = "% de trabajadores por nivel de estudio",
                           solidHeader = T, collapsible = F, heightFill =T,
                           shinycssloaders::withSpinner(plotlyOutput("plot_publi2")))
                       
              
      ),
      fluidRow(width=12,
               valueBoxOutput("info_trab"),
               valueBoxOutput("info_tiempo"),
               valueBoxOutput("info_horas")
               ),
      
      ),
  
  tabItem("menu_autonomo",h1("Estadísticas de trabajadores autonomos")
          ),
  
  #---------------------------------------------------------------#
  #                     Análisis de afiliados                     #
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  
    # afiliados por variables socio-demográficas (id=a1)
      tabItem("menu_afi_socio",h1("Datos de afiliados a la seguridad social por edad y sexo"),
              fluidRow(#width=12,
                       box(width = 12,
                           title = "Inputs", status = "warning", background = "blue",
                           column(width=2,pickerInput("select_prov_af1", h4("Provincia"), choices = prov_list_mod,
                                       multiple=TRUE, selected = prov_list_mod,options = list(`actions-box` = TRUE))),
                           column(width=3,radioGroupButtons( inputId = "select_sexo_af1", label = h4("Sexo"),
                                              choices = c("Ambos", "Hombre", "Mujer"),
                                              justified = TRUE)),
                           column(width=3,dateRangeInput(inputId = "rango_fechas", 
                                                         label = h4("Selecciona un rango de fechas:"),
                                                         min=as.Date("2009-01-01"),
                                                         max=as.Date("2023-03-31"),
                                                         start = as.Date("2012-01-01"),
                                                         end = as.Date("2023-03-31"),
                                                         format = "yyyy-mm"
                                                         ))
                           )
                       ),
              fluidRow(box(width=12, solidHeader = T, collapsible = F, heightFill =T,
                           shinycssloaders::withSpinner(plotlyOutput("plot_af1")))),
              
              fluidRow(column(width=2), column(width=10,
                                               box(width=10,
                                                   title=div("Datos"),
                                                   dataTableOutput("tabla_af1"))))
              ),
      
  tabItem("menu_afi_sector",h1("Datos de afiliados a la seguridad social por sector de actividad")),
      
  tabItem("menu_serie",h1("Series temporales por provincia y sector de actividad"),
          fluidRow(width=12,
                   box(width = 3,
                       title = "Inputs", status = "warning", background = "blue",
                       selectInput("select_prov_af3", h4("Provincia"), choices = prov_list_mod),
                       radioGroupButtons( inputId = "select_cnae_af3", label = h4("Seleciona:"),
                                          choices = c("Clasificacion 1","Clasificacion 2"),
                                          justified = TRUE),
                       selectInput("select_acti_af3", h4("Sector de actividad (CNAE)"), 
                                   choices = list_act_ampliado),
                       radioGroupButtons( inputId = "select_regimen_af3", label = h4("Regimen"),
                                          choices = c("Total","General", "Autonomos"),
                                          justified = TRUE)),
                   box(width = 9,
                       solidHeader = T, collapsible = F, heightFill =T,
                       shinycssloaders::withSpinner(plotlyOutput("plot_af3_obs")),
                       shinycssloaders::withSpinner(plotlyOutput("plot_af3_tre")),
                       shinycssloaders::withSpinner(plotlyOutput("plot_af3_error")),
                 
                       ),
                   ),
          fluidRow(width=12,
                   column(width=3),
                   column(width=9,
                          tabBox(title="Estacionalidad", side = "right", width=12,
                                 tabPanel("Mensual",
                                          shinycssloaders::withSpinner(plotlyOutput("plot_af3_est"))),
                                 tabPanel("Anual",      
                                          shinycssloaders::withSpinner(plotlyOutput("plot_af3_est_ano"))))
                   )
          )
  )

    
    
      
    )
  )
)


