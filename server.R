library(shinydashboard)
library(tidyverse)
library(data.table) # funcion fread() para cargar los datos pesados
library(gridExtra)#combinar graficas
library(shinyWidgets) #opciones extra UI
library(shinyjs) # simbolos de carga
library(shinycssloaders)#simbolos de carga 2
library(plotly)#graficos interactivos
#grafica maps
library(geojsonio)# cargar datos mapa
library(htmltools)  # Used for constructing map labels using HTML
library(leaflet)    # The map-making package


server <- function(input, output) {
  
  #valores datos
  cate_ocu <- c("Ocupaciones militares. Fuerzas armadas",
                "Directores y gerentes. Dirección de las empresas y de las  Administraciones Públicas",
                "Técnicos y Profesionales científicos e intelectuales",
                "Técnicos y Profesionales de apoyo",
                "Empleados contables, administrativos y otros empleados de oficina. Empleados de tipo administrativo",
                "Trabajadores de servicios de restauración, personales, protección y vendedores de comercio",
                "Trabajadores cualificados en el sector agrícola, ganadero, forestal y pesquero. 
                Trabajadores cualificados en la agricultura y en la pesca",
                "Artesanos y trabajadores cualificados de las industrias manufactureras y la construcción. 
                Artesanos y trabajadores cualificados de las industrias manufactureras, la construcción, 
                y la minería, excepto operadores de instalaciones y maquinaria" ,
                "Operadores de instalaciones y maquinaria, y montadores",
                "Ocupaciones elementales. Trabajadores no cualificados")
  cate_situ <- c("Agricultura, ganadería, silvicultura y pesca",
                 "Industria de la alimentación, textil, cuero, madera y papel",
                 "Industrias extractivas, refino de petróleo, industria química, 
                 farmacéutica, industria del caucho y materias plásticas, 
                 suministro energía eléctrica, gas, vapor y aire acondicionado, 
                 suministro de agua, gestión de residuos. Metalurgia",
                 "Construcción de maquinaria, equipo eléctrico y material de transporte. 
                 Instalación y reparación industrial","Construcción",
                 "Comercio al por mayor y al por menor y sus instalaciones y reparaciones. 
                 Reparación de automóviles, hostelería",
                 "Transporte y almacenamiento. Información y comunicaciones",
                 "Intermediación financiera, seguros, actividades inmobiliarias, 
                 servicios profesionales, científicos, administrativos y otros",
                 "Administración Pública, educación y actividades sanitarias",
                 "Otros servicios.")
  age <- c("0 a 4 años", "5 a 9 años", "10 a 15 años", "16 a 19 años", 
           "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años", 
           "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años", 
           "60 a 64 años", "65 o más años")
  estudios <-c("Analfabetos", "Educación primaria incompleta", "Educación primaria", 
              "Primera etapa de educación secundaria", 
              "Segunda etapa de educación secundaria. Orientación general", 
               "Segunda etapa de educación secundaria. Orientación profesional (incluye educación postsecundaria no superior)", 
                 "Educación superior")
  provincia <- c("Araba", "Albacete", "Alacant", "Almería", "Ávila", "Badajoz", 
                    "Illes Balears", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castelló", 
                    "Ciudad Real", "Córdoba", "A Coruña", "Cuenca", "Girona", "Granada", 
                    "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", 
                    "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", 
                    "Ourense", "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca", 
                    "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria", 
                    "Tarragona", "Teruel", "Toledo", "València", "Valladolid", "Bizkaia", 
                    "Zamora", "Zaragoza", "Ceuta", "Melilla")

  #carga de datos
  
  data <-read.csv("muestra_epa.csv") #cambiar
  data1 <- data %>% 
    mutate(PROV = factor(PROV, labels = provincia)) %>% 
    mutate(EDAD5 = factor(EDAD5, labels = age)) %>% 
    mutate(SEXO1 = factor(SEXO1, labels=c("1.Hombre", "2.Mujer"))) %>% 
    mutate(NAC1 = factor(NAC1, labels=c("1.Española", "2.Española y doble nacionalidad", "3.Extranjera"))) %>% 
    mutate(NFORMA = factor(NFORMA)) %>% 
    mutate(OCUP1 = factor(OCUP1,labels=cate_ocu)) %>%
    mutate(ACT1 = factor(ACT1, labels = cate_situ)) %>% 
    mutate(HORASP = ifelse(HORASP != 9999,
                           as.numeric(str_sub(HORASP, 1, -3)) + as.numeric(str_sub(HORASP, -2)) / 60,
                           NA)) %>% 
    mutate(HORASH=ifelse(HORASH != 9999,
                  as.numeric(str_sub(HORASH, 1, -3)) + as.numeric(str_sub(HORASH, -2)) / 60,
                  NA)) %>% 
    filter(AOI==3 | AOI==4)%>%
    select(year,trim,comu,PROV,EDAD5,SEXO1,DUCON1,NFORMA, NAC1, OCUP1, ACT1, HORASP, HORASH)
  
  # Carga los datos mapa
    # Obtención de los datos :https://public.opendatasoft.com/explore/dataset/provincias-espanolas/export/?sort=provincia&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6InByb3ZpbmNpYXMtZXNwYW5vbGFzIiwib3B0aW9ucyI6eyJzb3J0IjoicHJvdmluY2lhIn19LCJjaGFydHMiOlt7ImFsaWduTW9udGgiOnRydWUsInR5cGUiOiJjb2x1bW4iLCJmdW5jIjoiQ09VTlQiLCJzY2llbnRpZmljRGlzcGxheSI6dHJ1ZSwiY29sb3IiOiIjRkY1MTVBIn1dLCJ4QXhpcyI6ImNjYWEiLCJtYXhwb2ludHMiOjUwLCJzb3J0IjoiIn1dLCJ0aW1lc2NhbGUiOiIiLCJkaXNwbGF5TGVnZW5kIjp0cnVlLCJhbGlnbk1vbnRoIjp0cnVlfQ%3D%3D&location=6,41.20346,-4.14185&basemap=jawg.light
  provin <- geojson_read("provincias-espanolas.geojson", what = "sp")
 
#----------------------------------------------------------#   
  
  #grafica tipos contratos EPA

  
  data_graf <- reactive({
    
    Sys.sleep(1)
    
    #conf. NA
    data_NA <- if(input$incluir_na==1){
      data1 %>% mutate_at("DUCON1", ~replace(., is.na(.), 9)) %>%
        mutate(DUCON1=factor(DUCON1, labels=c("1.Indefinido", "6.Temporal", "9.Otros")))}
    else {
      data1 %>% filter(!is.na(DUCON1)) %>%
        mutate(DUCON1=factor(DUCON1, labels=c("1.Indefinido", "6.Temporal")))}
    
    
    
    #filtrar
    data_NA %>% 
      filter(if (input$select_year!=77){year==input$select_year}else{TRUE},
             if(input$select_trim!=77){trim == input$select_trim } else {TRUE},
             if(input$select_prov!=77){PROV == input$select_prov} else {TRUE}) %>% 
      mutate(vari_div = if(input$select_vari=="EDAD5"){EDAD5} 
             else if(input$select_vari=="SEXO1"){SEXO1}
             else if(input$select_vari=="NAC1"){NAC1}
             else if(input$select_vari=="NFORMA"){NFORMA}else{factor("", levels = "")}) %>%
      group_by(year, vari_div, DUCON1) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      filter(if (input$select_year==77){DUCON1=="6.Temporal"}else{TRUE}) %>% 
      select(year, vari_div, DUCON1, muestra, freq)
    
  })
  
  output$plot_contra <-renderPlotly({
    if(input$select_year!=77){
      ggplot(data_graf(), aes(x=vari_div, y=freq, fill=DUCON1))+
        geom_col(position = "dodge", colour="black")+
        #geom_text(aes(label=freq), vjust=-3, position= position_dodge(.9))+
        scale_y_continuous(labels = scales::percent)+
        ylim(0,1.1)+
        ggtitle("Proporción de población activa según tipo de contrato")}
    else {
      ggplot(data_graf(), aes(x=year, y= freq, color=vari_div))+
        geom_line(linewidth =0.5)+
        geom_point(size=1)+
        geom_text(aes(label=freq), vjust=-3, position= position_dodge(.9), size=4)+
        scale_y_continuous(labels = scales::percent)+
        theme(axis.text.x = element_text(hjust = 1))+
        ggtitle("Serie temporal de proporción de población activa con contrato temporal")
    }
    
    
  })
  
  output$tabla_contra <- renderDataTable({
    data_graf()
  })
  
#---------------------------------------------------------------#  
  
  #grafica sector de ocupación
  
    
  data_graf1 <- reactive({
    
    Sys.sleep(1)
    
    data_NA1 <- data1 %>%  
      mutate(sector =if(input$ocu_act == "Ocupación (CNO)"){OCUP1}else{ACT1}) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,NFORMA, NAC1, sector)

    #filtrar
    data_NA1 %>% 
      filter(if (input$select_year1!=77){year==input$select_year1}else{TRUE},
             if(input$select_trim1!=77){trim == input$select_trim1 } else {TRUE},
             if(input$select_prov1!=77){PROV == input$select_prov1} else {TRUE}) %>% 
      mutate(vari_div = if(input$select_vari1=="EDAD5"){EDAD5} 
             else if(input$select_vari1=="SEXO1"){SEXO1}
             else if(input$select_vari1=="NAC1"){NAC1}
             else if(input$select_vari1=="NFORMA"){NFORMA}else{factor("", levels = "")}) %>%
      group_by(year, vari_div, sector) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      #filter(if (input$select_year==77){DUCON1=="6.Temporal"}else{TRUE}) %>% 
      select(year, vari_div, sector, muestra, freq)
    
  })
  
  output$tabla_ocupa <- renderDataTable({
    data_graf1()
  })
  
  # Función para crear múltiples gráficos en una caja
  create_plot_box <- function(data) {
    
    plots <- list()
    
    if (input$select_year1 != 77) {
      eti <- levels(data_graf1()$vari_div)
      
      if(length(eti)>13){eti <- eti[4:length(eti)]}
      
      
      
      for (i in 1:length(eti)) {
        p <- plot_ly(data_graf1() %>% filter(vari_div==eti[i]), type = "pie", 
                     values = ~freq, labels = ~sector,
                     marker = list(colors = c("red", "blue", "green", "orange", 
                                              "purple"), opacity=0.8)) %>% 
          layout(showlegend = F, title=as.character(eti[i]))
        
        plots[[i]] <- p
      }
    }
    else{
      p <- plot_ly(data_graf1(), x = ~factor(year), y = ~freq, 
                   color = ~sector, text = ~paste(sector, ": ", scales::percent(freq)),
                   type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F, title = "Proporción de sector",
               yaxis = list(title = "Proporción", tickformat = ".0%"), 
               xaxis = list(title = "Año"))
      plots[[1]] <- p
    }
    
    
    box <- fluidRow(
      column(width=12, style = "padding: 0px;", 
             lapply(plots, function(p) {
               div(style = paste0("width: ", 100, "%; padding: 10px; display: inline-block;"),
                   p)
             })
      )
    )
    
    return(box)
  }
  
  output$plots_ocupa <- renderUI({
    box <- create_plot_box(data_graf1())
    box
  })
  

#---------------------------------------------------------------#  
  
  #gráficas horas promedios y mapas
  
  
  data_graf2 <- reactive({
    
    Sys.sleep(1)
    
    #datos 
    data_NA1 <- data1 %>% 
      mutate(x=if(input$tipo_hora=="De contrato"){x=HORASP}else{HORASH}) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,HORASP, HORASH, x, comu)
    
    #filtrar
    data_NA1 <- data_NA1 %>% 
      filter(year==input$select_year2) %>% 
      filter(if(input$select_trim2!=77){trim == input$select_trim2} else {TRUE}) %>%
      filter(!is.na(x)) %>% 
      group_by(comu, PROV) %>%
      summarise(prom=ifelse(input$med_mediana == "Media", round(mean(x),2),
                            round(median(x),2))) %>%
      mutate(provincia=PROV) %>% 
      select(comu, provincia, prom)
    
    # unimos datos
    provin_horas <- merge(provin@data, data_NA1, by = "provincia", all.x = TRUE)
    
    # recuperar el formato especial
    provin_merged <- merge(provin, provin_horas, by = "provincia")
    provin_merged
 
  })
  

  
  # Crea el mapa interactivo
  output$plot_mapHoras <- renderLeaflet({
    
    colores <- colorFactor(palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "yellow",
                                       "#9a352c", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                       "#aec7e8", "#ffbb78", "#ff0000", "#ff9896", "#c5b0d5",
                                       "#c7c7c7", "#dbdb8d", "#9edae5", "#f7b6d2", "#bcbd22"), 
                           domain = unique(data_graf2()@data$comu))
    
    #creacion de las etiquetas en formato html
    labels <- sprintf(
      "<strong>%s</strong><br/>%g horas / semana",
      data_graf2()@data$provincia, data_graf2()@data$prom
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(data_graf2()) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      addPolygons(fillColor = ~colores(comu), color = "#000000", weight = 1,
                  smoothFactor = 0.5, opacity = 1, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "yellow", weight = 2,
                                                      bringToFront = TRUE),
                  label =  labels)#labels

    
  })


  #---------------------------------------------------------------#  
  
  #infobox trabajo publico
  
  data_graf3 <- reactive({
    
    Sys.sleep(1)
    
    #datos 
    data_NA1 <- data1 %>% 
      mutate(x=if(input$tipo_hora=="De contrato"){x=HORASP}else{HORASH}) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,HORASP, HORASH, x, comu)
  })
 
  
}