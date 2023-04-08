library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(gridExtra)#combinar graficas
library(shinyWidgets) #opciones extra UI
library(leaflet) #maps
library(shinyjs) # simbolos de carga
library(shinycssloaders)#simbolos de carga 2
library(plotly)#graficos interactivos


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
  age <- c("0 a 4 años", "5 a 9 años", "10 a 15 años", "16 a 19 años", 
           "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años", 
           "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años", 
           "60 a 64 años", "65 o más años")
  estudios <-c("Analfabetos", "Educación primaria incompleta", "Educación primaria", 
              "Primera etapa de educación secundaria", 
              "Segunda etapa de educación secundaria. Orientación general", 
               "Segunda etapa de educación secundaria. Orientación profesional (incluye educación postsecundaria no superior)", 
                 "Educación superior")
  
  
  #carga de datos
  
  data <-read.csv("muestra_epa.csv") #cambiar
  data1 <- data %>% 
    mutate(EDAD5 = factor(EDAD5, labels = age)) %>% 
    mutate(SEXO1 = factor(SEXO1, labels=c("1.Hombre", "2.Mujer"))) %>% 
    mutate(NAC1 = factor(NAC1, labels=c("1.Española", "2.Española y doble nacionalidad", "3.Extranjera"))) %>% 
    mutate(NFORMA = factor(NFORMA)) %>% 
    mutate(OCUP1 = factor(OCUP1,labels=cate_ocu)) %>%
    filter(AOI==3 | AOI==4)%>%
    select(year,trim,PROV,EDAD5,SEXO1,DUCON1,NFORMA, NAC1, OCUP1)
 
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
        geom_line(size=0.5)+
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
      #mutate(OCUP1=factor(OCUP1,labels=cate_ocu)) %>% 
      select(year,trim,PROV,EDAD5,SEXO1,NFORMA, NAC1, OCUP1)

    #filtrar
    data_NA1 %>% 
      filter(if (input$select_year1!=77){year==input$select_year1}else{TRUE},
             if(input$select_trim1!=77){trim == input$select_trim1 } else {TRUE},
             if(input$select_prov1!=77){PROV == input$select_prov1} else {TRUE}) %>% 
      mutate(vari_div = if(input$select_vari1=="EDAD5"){EDAD5} 
             else if(input$select_vari1=="SEXO1"){SEXO1}
             else if(input$select_vari1=="NAC1"){NAC1}
             else if(input$select_vari1=="NFORMA"){NFORMA}else{factor("", levels = "")}) %>%
      group_by(year, vari_div, OCUP1) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      #filter(if (input$select_year==77){DUCON1=="6.Temporal"}else{TRUE}) %>% 
      select(year, vari_div, OCUP1, muestra, freq)
    
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
                     values = ~freq, labels = ~OCUP1,
                     marker = list(colors = c("red", "blue", "green", "orange", 
                                              "purple"), opacity=0.8)) %>% 
          layout(showlegend = F, title=as.character(eti[i]))
        
        plots[[i]] <- p
      }
    }
    else{
      p <- plot_ly(data_graf1(), x = ~factor(year), y = ~freq, 
                   color = ~OCUP1, text = ~paste(OCUP1, ": ", scales::percent(freq)),
                   type = "bar", marker = list(opacity = 0.8)) %>%
        layout(showlegend = F, title = "Proporción de sector de ocupación",
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
  



  
 
  
}