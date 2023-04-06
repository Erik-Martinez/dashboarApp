library(shinydashboard)
library(tidyverse)
library(leaflet) #maps
library(shinyjs) # simbolos de carga
library(shinycssloaders)#simbolos de carga 2


server <- function(input, output) {
  
  #grafica tipos contratos EPA
  data <-read.csv("muestra_epa.csv") #cambiar
  data1 <- data %>% 
    filter(AOI==3 | AOI==4)%>%
    select(year,trim,PROV,EDAD5,SEXO1,DUCON1,NFORMA, NAC1)
  
  
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
      mutate(vari_div = if(input$select_vari=="EDAD5"){factor(EDAD5)} 
             else if(input$select_vari=="SEXO1"){factor(SEXO1, labels=c("1.Hombre", "2.Mujer"))}
             else if(input$select_vari=="NAC1"){factor(NAC1, labels=c("1.Española", "2.Española y doble nacionalidad", "3.Extranjera"))}
             else if(input$select_vari=="NFORMA"){NFORMA}else{factor("", levels = "")}) %>%
      group_by(year, vari_div, DUCON1) %>%
      summarise(n=n()) %>%
      mutate(muestra=n) %>% 
      mutate(freq=round(n/sum(n),3))%>%
      filter(if (input$select_year==77){DUCON1=="6.Temporal"}else{TRUE}) %>% 
      select(year, vari_div, DUCON1, muestra, freq)
    
  })
  
  output$plot_contra <-renderPlot({
    if(input$select_year!=77){
      ggplot(data_graf(), aes(x=vari_div, y=freq, fill=DUCON1))+
        geom_col(position = "dodge", colour="black")+
        geom_text(aes(label=freq), vjust=-0.2, position= position_dodge(.9))+
        scale_y_continuous(labels = scales::percent)+
        ylim(0,1.1)}
    else {
      ggplot(data_graf(), aes(x=year, y= freq, color=vari_div))+
        geom_line(size=1)+
        geom_point(size=2)+
        geom_text(aes(label=freq), vjust=-0.2, position= position_dodge(.9), size=4)+
        scale_y_continuous(labels = scales::percent)+
        theme(axis.text.x = element_text(hjust = 1))
    }
    
    
  })
  
  output$tabla_contra <- renderDataTable({
    data_graf()
  })
  
}