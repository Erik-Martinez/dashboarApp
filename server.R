
server <- function(input, output, session) {
  
  #carga valores datos
  
  source("global.R")
  

  #carga de datos EPA
  
  data1 <-read.csv("data/epa_data.csv") #cambiar
  data1 <- data1 %>% 
    mutate(PROV = factor(PROV, labels = provincia)) %>% 
    mutate(EDAD5 = factor(EDAD5, labels = age)) %>% 
    mutate(SEXO1 = factor(SEXO1, labels=c("Hombre", "Mujer"))) %>% 
    mutate(NAC1 = factor(NAC1, labels=c("1.Española", "2.Española y doble nacionalidad", "3.Extranjera"))) %>% 
    mutate(NFORMA = factor(NFORMA)) %>% 
    mutate(SP = factor(SP, labels = admi)) %>% 
    mutate(HORASP = ifelse(HORASP != 9999,
                           as.numeric(str_sub(HORASP, 1, -3)) + as.numeric(str_sub(HORASP, -2)) / 60,
                           NA)) %>% 
    mutate(HORASH=ifelse(HORASH != 9999,
                  as.numeric(str_sub(HORASH, 1, -3)) + as.numeric(str_sub(HORASH, -2)) / 60,
                  NA)) %>% 
    #filter(AOI==3 | AOI==4)%>%
    mutate(OCUP1 = factor(OCUP1,labels=cate_ocu)) %>%
    mutate(ACT1 = factor(ACT1, labels = cate_act)) %>% 
    mutate(SITU = factor(SITU, labels = cate_situ)) %>% 
    select(year,trim,comu,PROV,EDAD5,SEXO1,DUCON1,NFORMA, NAC1, OCUP1, ACT1, 
           HORASP, HORASH, SP, SITU)
  
  # Carga los datos mapa
    # Obtención de los datos :https://public.opendatasoft.com/explore/dataset/provincias-espanolas/export/?sort=provincia&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6InByb3ZpbmNpYXMtZXNwYW5vbGFzIiwib3B0aW9ucyI6eyJzb3J0IjoicHJvdmluY2lhIn19LCJjaGFydHMiOlt7ImFsaWduTW9udGgiOnRydWUsInR5cGUiOiJjb2x1bW4iLCJmdW5jIjoiQ09VTlQiLCJzY2llbnRpZmljRGlzcGxheSI6dHJ1ZSwiY29sb3IiOiIjRkY1MTVBIn1dLCJ4QXhpcyI6ImNjYWEiLCJtYXhwb2ludHMiOjUwLCJzb3J0IjoiIn1dLCJ0aW1lc2NhbGUiOiIiLCJkaXNwbGF5TGVnZW5kIjp0cnVlLCJhbGlnbk1vbnRoIjp0cnVlfQ%3D%3D&location=6,41.20346,-4.14185&basemap=jawg.light
  provin <- geojson_read("provincias-espanolas.geojson", what = "sp")
  
  
  #carga de datos afiliados divididos por datos sociodemograficos
  afi_socio <- read.csv("data/AfiliadosMedios_12_23_Socio_Mod.csv")
  
  afi_socio1 <- afi_socio %>% 
    mutate(PROV=factor(PROV, labels = c(provincia, "Nacional"))) %>% 
    mutate(EDAD5=factor(EDAD5, labels = c(age[4:length(age)], "Total"))) %>% 
    mutate(SEXO=factor(SEXO, labels = c("Hombre", "Mujer", "Total")))%>% 
    mutate(date = ym(periodo))
  
  #cargar datos afiliados por sector
  
  afi_sector <- read.csv("data/AfiliadosMedios_12_23_sector_Mod(2).csv")
  
  afi_sector1 <- afi_sector %>% 
    mutate(PROV=factor(PROV, labels = c(provincia, "Nacional"))) %>%
    mutate(regimen=factor(regimen, labels=c("General", "Autonomos", "Total"))) %>%
    mutate(actividad=factor(actividad, labels = cate_act_ampliado )) %>% 
    mutate(act_pib=factor(act_pib, labels=cate_act_PIB)) %>% 
    mutate(date = ym(periodo))
  
  #carga datos de cotizaciones
  
  cotizaciones <- read.csv("data/Cotizantes y base_18_23_Mod.csv")
  
  cotizantes1 <- cotizaciones %>% 
    mutate(PROV=factor(PROV, labels = c(provincia, "Nacional"))) %>% 
    mutate(SEXO=factor(SEXO, labels = c("Hombre", "Mujer", "Total"))) %>% 
    mutate(regis=factor(regis, labels = c("Cotizantes", "Bases Medias")))
    
    
    

#----------------------------------------------------------#   
  
  #cambios en UI por Galicia

  
  observeEvent(input$galicia, {
    if(input$galicia == TRUE){ 
      updateSelectInput(session, "select_prov", "Provincia",prov_gali)
      updateSelectInput(session, "select_prov1", "Provincia",prov_gali)
      updateSelectInput(session, "select_prov3", "Provincia",prov_gali)
      updateSelectInput(session, "select_prov4", "Provincia",prov_gali)
      updatePickerInput(session, "select_prov_af1", "Provincia",prov_gali)
      updateSelectInput(session, "select_prov_af3", "Provincia",prov_gali)}
    else{restaurar_selectores(session)}
  })
  
  #cambio en UI por sistema de clasificacion CNAE
  
  observeEvent(input$select_cnae_af3,{
    if(input$select_cnae_af3=="Clasificacion 2"){ 
      updateSelectInput(session, "select_acti_af3", "Sector de actividad (CNAE)",
                        cate_act_PIB)}
    else{updateSelectInput(session, "select_acti_af3", "Sector de actividad (CNAE)",
                           list_act_ampliado)}
  })
  
#----------------------------------------------------------# 
  
  #función para generar gráficas a descargar
  generar_archivo <- function(grafico) {
    # Genera el archivo de descarga
    output$descarga_grafico <- downloadHandler(
      filename = function() {
        paste0("grafico_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Genera el archivo PNG
        png(file)
        print(grafico)
        dev.off()
      }
    )
  }
  
 
#----------------------------------------------------------#   
  
  #grafica tipos contratos EPA (id=0)

  
  data_graf <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #conf. NA
    data_NA <- if(input$incluir_na==1){
      data_NA %>% mutate_at("DUCON1", ~replace(., is.na(.), 9)) %>%
        mutate(DUCON1=factor(DUCON1, labels=c("1.Indefinido", "6.Temporal", "9.Otros")))}
    else {
      data_NA %>% filter(!is.na(DUCON1)) %>%
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
  
  #grafica sector de ocupación (id=1)
  
    
  data_graf1 <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    data_NA <- data_NA %>%  
      mutate(sector =if(input$ocu_act == "Ocupación (CNO)"){OCUP1}else{ACT1}) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,NFORMA, NAC1, sector)

    #filtrar
    data_NA %>% 
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
      #mutate(color=colorFactor(palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
       #                              "yellow","#9a352c", "#e377c2", "#7f7f7f", 
        #                             "#bcbd22", "#17becf"), 
         #                domain = unique(data_graf1()$sector))) %>% 
      select(year, vari_div, sector, muestra, freq)
    
  })
  
  output$tabla_ocupa <- renderDataTable({
    data_graf1()
  })
  
 
  
  # Función para crear múltiples gráficos en una caja
  create_plot_box <- function(data) {
    
    plots <- list()
    
    eti <- levels(data_graf1()$vari_div)
    
    colores <- colorFactor(palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                       "#c5b0d5","#9a352c", "#e377c2", "#c7c7c7", 
                                       "#bcbd22", "#17becf"), 
                           domain = unique(data_graf1()$sector))
    
    
    if(length(eti)>13){eti <- eti[4:length(eti)]}
    
    if (input$select_year1 != 77) {
      
      for (i in 1:length(eti)) {
        p <- plot_ly(data_graf1() %>% filter(vari_div==eti[i]), type = "pie", 
                     values = ~freq, labels = ~sector,
                     marker = list(colors = ~colores(sector),opacity=0.8)) %>% 
          layout(showlegend = F, title=as.character(eti[i]))
        
        plots[[i]] <- p
      }
    }
    else{
      if(input$select_vari1==999){
        p <- plot_ly(data_graf1(), x = ~factor(year), y = ~freq, 
                     color = ~sector, text = ~paste(sector, ": ", scales::percent(freq)),
                     type = "bar", marker = list(opacity = 0.8)) %>%
          layout(showlegend = F, title = "Proporción de sector",
                 yaxis = list(title = "Proporción", tickformat = ".0%"), 
                 xaxis = list(title = "Año"))
        plots[[1]] <- p
      }
      else{
        for (i in 1:length(eti)) {
          p <- plot_ly(data_graf1() %>% filter(vari_div==eti[i]), type = "bar",
                       x = ~factor(year), y = ~freq, 
                       color = ~sector, text = ~paste(sector, ": ", scales::percent(freq)),
                       values = ~freq, labels = ~sector,
                       marker = list(colors = ~colores(sector),opacity=0.8)) %>% 
            layout(showlegend = F, title=as.character(eti[i]))
          
          plots[[i]] <- p
        }
      }
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
  
  #gráficas horas promedios y mapas (id=2)
  
  
  data_graf2 <- reactive({
    
    Sys.sleep(1)
    
    #datos 
    data_NA <- data1 %>% 
      mutate(x=if(input$tipo_hora=="De contrato"){x=HORASP}else{HORASH}) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,HORASP, HORASH, x, comu)
    
    #filtrar
    data_NA <- data_NA %>% 
      filter(year==input$select_year2) %>% 
      filter(if(input$select_trim2!=77){trim == input$select_trim2} else {TRUE}) %>%
      filter(!is.na(x)) %>% 
      group_by(comu, PROV) %>%
      summarise(prom=ifelse(input$med_mediana == "Media", round(mean(x),2),
                            round(median(x),2))) %>%
      mutate(provincia=PROV) %>% 
      select(comu, provincia, prom)
    
    # unimos datos
    provin_horas <- merge(provin@data, data_NA, by = "provincia", all.x = TRUE)
    
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
  
  #infobox trabajo publico (id=3)
  
  data_graf3 <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Asalariado sector público") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1)
    
    data_NA %>%
      filter(if (input$select_year3!=77){year==input$select_year3}else{TRUE},
             if(input$select_trim3!=77){trim == input$select_trim3 } else {TRUE},
             if(input$select_prov3!=77){PROV == input$select_prov3} else {TRUE}) %>% 
      filter(if(input$select_sexo=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>% 
      group_by(year, SP) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      select(year, SP, muestra, freq)
      
      
  })
  
  data_graf3_est <- reactive({
    
    Sys.sleep(1)
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Asalariado sector público") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1, NFORMA)
    
    data_NA %>%
      filter(if (input$select_year3!=77){year==input$select_year3}else{TRUE},
             if(input$select_trim3!=77){trim == input$select_trim3 } else {TRUE},
             if(input$select_prov3!=77){PROV == input$select_prov3} else {TRUE}) %>% 
      filter(if(input$select_sexo=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>% 
      group_by(year, NFORMA) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      select(year, NFORMA, muestra, freq)
    
    
  })
  
  data_graf3_info <- reactive({
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Asalariado sector público") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1, NFORMA)
  })
  
  
  
  output$plot_publi1 <- renderPlotly({
    
    if(input$select_year3!=77){
      plot_ly(data_graf3() , type = "pie", 
              values = ~freq, labels = ~SP,
              marker = list(colors = c("red", "blue", "green", "orange", 
                                       "purple"), opacity=0.8)) %>% 
        layout(showlegend = F)
    }
    else{
      plot_ly(data_graf3(), x = ~factor(year), y = ~freq, 
              color = ~SP, text = ~paste(SP, ": ", scales::percent(freq)),
              type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F,
               yaxis = list(title = "Proporción", tickformat = ".0%"), 
               xaxis = list(title = "Año"))
    }
    
    
  })
  
  output$plot_publi2 <- renderPlotly({
    
    if(input$select_year3!=77){
      plot_ly(data_graf3_est() , type = "pie", 
              values = ~freq, labels = ~NFORMA,
              marker = list(colors = c("red", "blue", "green", "orange", 
                                       "purple"), opacity=0.8)) %>% 
        layout(showlegend = F)
    }
    else{
      plot_ly(data_graf3_est(), x = ~factor(year), y = ~freq, 
              color = ~NFORMA, text = ~paste(NFORMA, ": ", scales::percent(freq)),
              type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F,
               yaxis = list(title = "Proporción", tickformat = ".0%"), 
               xaxis = list(title = "Año"))
    }
    
    
  })
 
  #output$tabla_prueba <- renderDataTable({data_graf3()})
  
  
  data_graf3_box <- reactive({
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    data_NA1 <- data_NA %>% 
      mutate_at("DUCON1", ~replace(., is.na(.), 9)) %>%
      mutate(DUCON1=factor(DUCON1, labels=c("1.Indefinido", "6.Temporal", "9.Otros"))) %>% 
      filter(year==input$select_year3,
             if(input$select_trim3!=77){trim == input$select_trim3 } else {TRUE},
             if(input$select_prov3!=77){PROV == input$select_prov3} else {TRUE}) %>% 
      filter(if(input$select_sexo=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>%
      select(year,trim,EDAD5,SEXO1, DUCON1, HORASP, SITU)
    
    
    trab <- data_NA1 %>%
      group_by(year, SITU) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      filter(SITU=="Asalariado sector público") %>%
      select(freq)
    
    tiempo <- data_NA1 %>% 
      group_by(year, SITU, DUCON1) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      filter(SITU=="Asalariado sector público") %>%
      filter(DUCON1=="6.Temporal") %>% 
      select(freq)
    
    horas <- data_NA1 %>% 
      filter(!is.na(HORASP)) %>% 
      group_by(year, SITU) %>%
      summarise(prom= round(mean(HORASP),2)) %>%
      filter(SITU=="Asalariado sector público") %>%
      select(prom)
    
    info_boxs <- list(trab = trab$freq[1]*100, tiempo = tiempo$freq[1]*100, 
                     horas = horas$prom[1])
    info_boxs
    
  })
  
  output$info_trab <- renderValueBox({
    valueBox(paste0(data_graf3_box()$trab, "%"), "Trabajadores en el sector publico",
      icon = icon("briefcase", lib = "glyphicon"))
  })
  
  output$info_tiempo <- renderValueBox({
    valueBox(paste0(data_graf3_box()$tiempo, "%"), "Trabajadores con contrato temporal",
             icon = icon("send", lib = "glyphicon"))
  })
  
  output$info_horas <- renderValueBox({
    valueBox(paste0(data_graf3_box()$horas, "h"), "Horas semanales medias por contrato",
             icon = icon("hourglass", lib = "glyphicon"))
  })
  
  
  #---------------------------------------------------------------#  
  
  #autonomos (id=4)
  data_graf4 <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Trabajador independiente o empresario sin asalariados") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1)
    
    data_NA %>%
      filter(if (input$select_year4!=77){year==input$select_year4}else{TRUE},
             if(input$select_trim4!=77){trim == input$select_trim4 } else {TRUE},
             if(input$select_prov4!=77){PROV == input$select_prov4} else {TRUE}) %>% 
      filter(if(input$select_sexo4=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo4=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>% 
      group_by(year, ACT1) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      select(year, ACT1, muestra, freq)
    
    
  })
  
  data_graf4_est <- reactive({
    
    Sys.sleep(1)
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Trabajador independiente o empresario sin asalariados") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1, NFORMA)
    
    data_NA %>%
      filter(if (input$select_year4!=77){year==input$select_year4}else{TRUE},
             if(input$select_trim4!=77){trim == input$select_trim4 } else {TRUE},
             if(input$select_prov4!=77){PROV == input$select_prov4} else {TRUE}) %>% 
      filter(if(input$select_sexo4=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo4=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>% 
      group_by(year, NFORMA) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      select(year, NFORMA, muestra, freq)
    
    
  })
  
  data_graf4_info <- reactive({
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    #datos 
    data_NA <- data_NA %>% 
      filter(SITU=="Trabajador independiente o empresario sin asalariados") %>% 
      select(year,trim,PROV,EDAD5,SEXO1,ACT1, SP, DUCON1, NFORMA)
  })
  
  
  
  output$plot_publi_aut1 <- renderPlotly({
    
    if(input$select_year4!=77){
      plot_ly(data_graf4() , type = "pie", 
              values = ~freq, labels = ~ACT1,
              marker = list(colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "yellow",
                                       "#9a352c", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"), 
                            opacity=0.8)) %>% 
        layout(showlegend = F)
    }
    else{
      plot_ly(data_graf4(), x = ~factor(year), y = ~freq, 
              color = ~ACT1, text = ~paste(ACT1, ": ", scales::percent(freq)),
              type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F,
               yaxis = list(title = "Proporción", tickformat = ".0%"), 
               xaxis = list(title = "Año"))
    }
    
    
  })
  
  output$plot_publi_aut2 <- renderPlotly({
    
    if(input$select_year4!=77){
      plot_ly(data_graf4_est() , type = "pie", 
              values = ~freq, labels = ~NFORMA,
              marker = list(colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "yellow",
                                       "#9a352c", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"), 
                            opacity=0.8)) %>% 
        layout(showlegend = F)
    }
    else{
      plot_ly(data_graf4_est(), x = ~factor(year), y = ~freq, 
              color = ~NFORMA, text = ~paste(NFORMA, ": ", scales::percent(freq)),
              type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F,
               yaxis = list(title = "Proporción", tickformat = ".0%"), 
               xaxis = list(title = "Año"))
    }
    
    
  })
  
  #output$tabla_prueba <- renderDataTable({data_graf4()})
  
  
  data_graf4_box <- reactive({
    #galicia
    if(input$galicia==TRUE){data_NA <- data1 %>% filter(comu==12)}else{data_NA<-data1}
    
    data_NA1 <- data_NA %>% 
      mutate_at("DUCON1", ~replace(., is.na(.), 9)) %>%
      mutate(DUCON1=factor(DUCON1, labels=c("1.Indefinido", "6.Temporal", "9.Otros"))) %>% 
      filter(year==input$select_year4,
             if(input$select_trim4!=77){trim == input$select_trim4 } else {TRUE},
             if(input$select_prov4!=77){PROV == input$select_prov4} else {TRUE}) %>% 
      filter(if(input$select_sexo=="Hombre"){SEXO1=="Hombre"}
             else if(input$select_sexo=="Mujer"){SEXO1=="Mujer"}else{TRUE}) %>%
      select(year,trim,EDAD5,SEXO1, DUCON1, HORASH, SITU)
    
    
    trab <- data_NA1 %>%
      group_by(year, SITU) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),4))%>%
      filter(SITU=="Trabajador independiente o empresario sin asalariados") %>%
      select(freq)
    
    horas <- data_NA1 %>% 
      filter(!is.na(HORASH)) %>% 
      group_by(year, SITU) %>%
      summarise(prom= round(mean(HORASH),2)) %>%
      filter(SITU=="Trabajador independiente o empresario sin asalariados") %>%
      select(prom)
    
    info_boxs <- list(trab = trab$freq[1]*100,horas = horas$prom[1])
    info_boxs
    
  })
  
  output$info_trab_aut <- renderValueBox({
    valueBox(paste0(data_graf4_box()$trab, "%"), "Trabajadores autonomos",
             icon = icon("briefcase", lib = "glyphicon"))
  })
  
  
  output$info_horas_aut <- renderValueBox({
    valueBox(paste0(data_graf4_box()$horas, "h"), "Horas semanales medias de trabajo",
             icon = icon("hourglass", lib = "glyphicon"))
  })
  
  
  

  #---------------------------------------------------------------#
  #                     Análisis de afiliados                     #
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  
  # afiliados por variables socio-demográficas (id=af1)
  
  data_graf_af1 <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){afi_socio_NA <- afi_socio1 %>% filter(comu==12)}else{afi_socio_NA <- afi_socio1}
    
    afi_socio_NA %>% 
      filter(PROV %in% input$select_prov_af1) %>% 
      filter(if(input$select_sexo_af1=="Hombre"){SEXO=="Hombre"} 
             else if(input$select_sexo_af1=="Mujer"){SEXO=="Mujer"}else{SEXO=="Total"}) %>%
      filter(date >= input$rango_fechas[1] & date <= input$rango_fechas[2]) %>% 
      filter(EDAD5==input$select_age_af1) %>% 
      select(date, PROV, afi_med)
  })
  
  output$tabla_af1 <- renderDataTable({
    data_graf_af1()
  })
  
  output$plot_af1 <- renderPlotly({
    ggplot(data_graf_af1(), aes(x=date, y= afi_med, color=PROV))+
      geom_line(linewidth =0.5)+
      geom_point(size=0.5)+
      #geom_text(aes(label=afi_med), vjust=-3, position= position_dodge(.9), size=1)+
      theme(axis.text.x = element_text(hjust = 1),legend.position = "none")
  })
  

  
  # afiliados por variables de sector (id=af2)
  
  data_graf_af2 <- reactive({
    
    Sys.sleep(1)
    
    #galicia
    if(input$galicia==TRUE){
      afi_sector_NA <- afi_sector1 %>% 
        filter(comu==12)}else{afi_sector_NA<-afi_sector1}
    
    afi_sector_NA %>% 
      filter(PROV %in% input$select_prov_af2) %>% 
      filter(actividad == input$select_act_af2)%>%
      filter(date >= input$rango_fechas_af2[1] & date <= input$rango_fechas_af2[2]) %>% 
      filter(regimen==input$select_reg_af2) %>% 
      select(date, PROV, afi_med)
  })
  
  output$tabla_af2 <- renderDataTable({
    data_graf_af2()
  })
  
  output$plot_af2 <- renderPlotly({
    ggplot(data_graf_af2(), aes(x=date, y= afi_med, color=PROV))+
      geom_line(linewidth =0.5)+
      geom_point(size=0.5)+
      #geom_text(aes(label=afi_med), vjust=-3, position= position_dodge(.9), size=1)+
      theme(axis.text.x = element_text(hjust = 1),legend.position = "none")
  })
  
  
  
  # series temporales de afiliados (id=af3)
  
  data_graf_af3 <- reactive({
    
    if(input$galicia==TRUE){
      afi_sector_NA <- afi_sector1 %>% 
        filter(comu==12)}else{afi_sector_NA<-afi_sector1}
     
    
    if(input$select_prov_af3=="Galicia"){dat <-
      if(input$select_cnae_af3=="Clasificacion 1"){
        afi_sector_NA %>% 
          group_by(periodo,regimen,actividad) %>% 
          summarise(afi_med=sum(afi_med)) %>% 
          mutate(afi_med=as.numeric(afi_med)) %>% 
          filter(regimen==input$select_regimen_af3) %>% 
          filter(actividad == input$select_acti_af3)%>%
          as.data.frame() %>%
          select(afi_med)  
      }else{
        afi_sector_NA %>% 
          group_by(periodo,regimen,act_pib) %>% 
          summarise(afi_med=sum(afi_med)) %>% 
          mutate(afi_med=as.numeric(afi_med)) %>% 
          filter(regimen==input$select_regimen_af3) %>% 
          filter(act_pib == input$select_acti_af3)%>%
          as.data.frame() %>%
          select(afi_med)  
      }
    }else{dat <-
      if(input$select_cnae_af3=="Clasificacion 1"){
      afi_sector_NA %>% 
        filter(regimen==input$select_regimen_af3) %>% 
        filter(PROV==input$select_prov_af3) %>% 
        filter(actividad==input$select_acti_af3)%>%
        select(afi_med)
     }else{
        afi_sector_NA %>% 
          filter(regimen==input$select_regimen_af3) %>% 
          filter(PROV==input$select_prov_af3) %>% 
          filter(act_pib==input$select_acti_af3)%>%
          group_by(date)%>%
          summarise(afi_med=sum(afi_med))%>%
          select(afi_med)
           
        }}
  
     ts_data<- ts(dat, start=c(2009,1), frequency=12) 
     data <- ts_data %>% seas() 
       #ts_data %>% stl(s.window = "periodic")
       
     list(ts_data=ts_data, data=data)
  })
  
   

  
  
  output$plot_af3_obs <- renderPlotly({
    data <- data_graf_af3()$ts_data 
    
    forecast::autoplot(data, main="Serie Temporal Original") + 
      ggtitle("Serie Temporal Original") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Valor")
  })
  
  
  output$plot_af3_est <- renderPlotly({
    data <- data_graf_af3()$ts_data
    
    forecast::ggseasonplot(data, main="Estacionalidad por año") +
    ggtitle("Estacionalidad por año") +
    theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot_af3_est_ano <- renderPlotly({
    data <- data_graf_af3()$data %>% seasonal()
    
    forecast::autoplot(data, main="Estacionalidad") + 
      ggtitle("Estacionalidad") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Valor")
  })
  
  output$plot_af3_tre <- renderPlotly({
    #data <- data_graf_af3()$decomp
    data <- data_graf_af3()$data %>% trendcycle()
    
    forecast::autoplot(data, main="Tendencia") +
      ggtitle("Tendencia") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Valor")
  })
  
  output$plot_af3_error <- renderPlotly({
    #data <- data_graf_af3()$decomp
    data <- data_graf_af3()$data %>% remainder()
    
    forecast::  autoplot(data, main="Error") +
      ggtitle("Error") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Valor")
  })
  



  
  
  
  
  
  
}