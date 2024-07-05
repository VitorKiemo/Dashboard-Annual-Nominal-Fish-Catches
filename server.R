library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
    
  })
  
  dados_filtrados <- reactive({
    dados <- dados2 %>% 
      filter(area %in% input$area,
             Ano %in% input$ano,
             country %in% input$country)
  })
  
  output$mais_pescados <- renderTable({
    tab <- dados_filtrados() %>%
      group_by(species) %>%
      summarise(Total = sum(Valores)) %>%
      arrange(desc(Total)) %>%
      as.data.frame() %>%
      mutate(Total = prettyNum(Total, big.mark = ".", decimal.mark = ",")) %>%
      head(10)
  })
  
  output$top_paises <- renderTable({
    tab <- dados_filtrados() %>%
      group_by(country) %>%
      summarise(Total = sum(Valores)) %>%
      arrange(desc(Total)) %>%
      as.data.frame() %>%
      mutate(Total = prettyNum(Total, big.mark = ".", decimal.mark = ",")) %>%
      head(10)
  })
  
  output$pais_ano <- renderPlot({
    p <- dados2 %>%
      group_by(country, Ano) %>%
      summarise(Total = sum(Valores)) %>%
      filter(country %in% top_paises) %>%
      ggplot(aes(x = Ano, y = Total)) +
      geom_line(aes(group = country, colour = country)) +
      lims(y = c(0, NA)) +
      theme_linedraw()
    p
  })
  
  output$especie_ano <- renderPlot({
    p <- dados2 %>%
      group_by(species, Ano) %>%
      summarise(Total = sum(Valores)) %>%
      filter(species %in% top5_especies) %>%
      ggplot(aes(x = Ano, y = Total)) +
      geom_line(aes(group = species, colour = species)) +
      lims(y = c(0, NA)) + theme(axis.text = element_text(size = 10),
                                 axis.title = element_text(size = 14, face ="bold")) +
      theme_linedraw()
    p
  })
  
  output$total_ano <- renderPlot({
    p <- dados2 %>% 
      group_by(Ano) %>% 
      summarise(Total = sum(Valores)) %>%
      ggplot(aes(x = Ano, y = Total, group = 1))+
      geom_line()+theme(axis.text=element_text(size=12),
                        axis.title=element_text(size=14,face="bold"))+
      scale_y_continuous(labels = scales::number)+
      geom_point()+
      theme_linedraw()
    p
  })
  
  output$total_species <- renderPlot({
    p <- dados_filtrados() %>% 
      group_by(species) %>% 
      summarise(Total = sum(Valores)) %>%
      filter(species %in% top20_especies) %>% 
      ggplot(aes(x = species, y = Total, fill = species))+
      geom_bar(stat = "Identity")+
      scale_fill_hue(c = 40)+
      theme_classic()+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.position = "none")
    p
  })
  
  output$total_paises <- renderPlot({
    p <- dados_filtrados() %>% 
      group_by(country) %>% 
      summarise(Total = sum(Valores)) %>%
      filter(country %in% top20_paises) %>% 
      ggplot(aes(x = country, y = Total, fill = country))+
      geom_bar(stat = "Identity")+
      scale_fill_hue(c = 40)+
      theme_classic()+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.position = "none")
    p
  })
  
}
