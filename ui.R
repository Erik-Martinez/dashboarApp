library(shinydashboard)
library(leaflet) #maps
library(shinyjs) # simbolos de carga
library(shinycssloaders)#simbolos de carga 2

# valores datos
year_list <-list("Serie temporal-Temporalidad"=77, "2005"=2005, "2006"=2006, "2007"=2007, 
                 "2008"=2008, "2009"=2009, "2010"=2010, "2011"=2011, "2012"=2012,
                 "2013"=2013, "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                 "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021, "2022"=2022)
trim_list <- list("Anual"=77,"T1"=1,"T2"=2,"T3"=3,"T4"=4)
prov_list <- list("Nacional"=77, "Araba/Álava"=1, "Albacete"=2, 
                  "Alicante/Alacant"=3, "Almería"=4,"Ávila"=5, "Badajoz"=6, 
                  "Balears, Illes"=7, "Barcelona"=8, "Burgos"=9, "Cáceres"=10, 
                  "Cádiz"=11, "Castellón/Castelló"=12, "Ciudad Real"=13, 
                  "Córdoba"=14, "Coruña, A"=15, "Cuenca"=16, "Girona"=17, 
                  "Granada"=18, "Guadalajara"=19, "Gipuzkoa"=20, "Huelva"=21, 
                  "Huesca"=22, "Jaén"=23, "León"=24, "Lleida"=25, "Rioja, La"=26, 
                  "Lugo"=27, "Madrid"=28, "Málaga"=29, "Murcia"=30, "Navarra"=31, 
                  "Ourense"=32, "Asturias"=33, "Palencia"=34, "Palmas, Las"=35, 
                  "Pontevedra"=36, "Salamanca"=37, "Santa Cruz de Tenerife"=38, 
                  "Cantabria"=39, "Segovia"=40, "Sevilla"=41, "Soria"=42, 
                  "Tarragona"=43, "Teruel"=44, "Toledo"=45,"Valencia/València"=46,
                  "Valladolid"=47, "Bizkaia"=48, "Zamora"=49, "Zaragoza"=50, 
                  "Ceuta"=51, "Melilla"=52)
list_vari <- list("---"=999,"Grupos de Edad(5 años)"="EDAD5", "Sexo"="SEXO1", "Nacionalidad"="NAC1", 
                  "Nivel de estudios"="NFORMA")


#--------------------------------------

#UI

ui <- dashboardPage(
  dashboardHeader(
    title = "Shiny"
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("Análisis EPA",tabName = "menu1" ,
                         menuSubItem('Información', tabName = 'menu_info_epa'),
                         menuSubItem('Tipos de contrato', tabName = 'menu_contra_epa'),
                         menuSubItem('Sub Menu 3', tabName = 'menu13')
                ),
                
                menuItem("menu2_",tabName = "menu2" ,
                         menuSubItem('Sub Menu 1', tabName = 'menu21'),
                         menuSubItem('Sub Menu 2', tabName = 'menu22'),
                         menuSubItem('Sub Menu 3', tabName = 'menu23')
                )
    )
  ),
  
  dashboardBody(
    useShinyjs(), #carga de librería dentro de la sección
    tabItems(
      tabItem("menu_info_epa",h1("Pagina 1 en construccion")),
      tabItem("menu_contra_epa",h1("Tipos de contrato"),
              fluidRow(box( width = 2,
                            title = "Inputs", status = "warning",
                            checkboxInput("incluir_na", "Incluir NA", 1),
                            selectInput("select_year", h3("Año"), choices = year_list),
                            selectInput("select_trim", h3("Trimestre"), choices=trim_list),
                            selectInput("select_prov", h3("Provincia"), choices = prov_list),
                            selectInput("select_vari", h3("Dividido por:"), 
                                        choices = list_vari)),
                       box(width=10, title="Proporción según tipo de contrato", 
                           solidHeader = T, collapsible = F,
                           shinycssloaders::withSpinner(plotOutput("plot_contra", 
                                                                   height = 500)), 
                           heightFill = TRUE), width=12),
              
                fluidRow(column(width=2), column(width=10,
                                                box(width=10,
                                                    title=div("Datos"),
                                                    dataTableOutput("tabla_contra"))))
              
      ),
      tabItem("menu13",h1("Pagina 3 en construccion")),
      
      tabItem("menu21",h1("Pagina 1 en construccion")),
      tabItem("menu22",h1("Pagina 2 en construccion")),
      tabItem("menu23",h1("Pagina 3 en construccion"))
      
      
    )
  )
)

