library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(shinyWidgets)

# Define UI for application that draws a histogram
div_placeholder <- function(content = "PLACEHOLDER",
                            color = "#d2d2d2",
                            height = "3em") {
  css <- c(
    sprintf("background-color: %s;", color),
    "padding: 5px;",
    "margin: 5px;",
    "border-radius: 5px;",
    sprintf("height: %s;", height)
  )
  div(content, style = paste0(css, collapse = " "))
}


fluidPage(

    # Application title
    titlePanel("Annual Nominal Fish Catches"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          width =2,
          pickerInput("area",
                      "Selecione a área:",
                      multiple = T,
                      choices = unique(dados2$area),
                      options = list('actions-box' = T)),
          pickerInput("ano",
                      "Selecione o ano:",
                      multiple = T,
                      choices = unique(dados2$Ano),
                      options = list('actions-box' = T)),
          pickerInput("país",
                      "Selecione o país:",
                      multiple = T,
                      choices = unique(dados2$country),
                      options = list('actions-box' = T))
       ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
              column(width = 11,
                     tabsetPanel(
                       tabPanel("País x Ano", plotOutput("pais_ano")),
                       tabPanel("Espécie x Ano", plotOutput("especie_ano")),
                       tabPanel("Total x Ano", plotOutput("total_ano")))
                     ),
              column(width = 1,
                     tableOutput("mais_pescados"))
              ),
            
            fluidRow(
              column(width = 11,
                     tabsetPanel(
                       tabPanel("Total Espécies", plotOutput("total_species")),
                       tabPanel("Total Países", plotOutput("total_paises")))
              ),
              column(width = 1,
                     tableOutput("top_paises"))
                     )
            )
        )
    )
