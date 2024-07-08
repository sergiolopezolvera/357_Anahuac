library(shiny)
library(tidyverse)

resultados <- read.csv("data/SEDEMA.csv") %>%
  pivot_longer(cols = c(Pidio, Contraparte.pidio, Obtuvo, Contraparte.obtuvo),
               names_to = "Criterio", values_to = "Resultado")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(" Análisis del juego Tiburones y charales"),
  p("Análisis de resultados de estudiantes del Docotrado en Ciencias Sociales de", strong(" CIESAS-Golfo"), " en julio de 2024."),
  h3("Análisis individual"),
  
  p("Elige", strong(" una persona"), " y el", strong(" conjunto de criterios"), " que te interesen para analizar sus resultados."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alumno",
                  "Elige una persona:",
                  choices = c("Misael_Garcia",
                              "Levi_Villanueva",
                              "Osmar_Ayala",
                              "Adriana_Reyes",
                              "Juan_Contreras",
                              "Rudy_Alamilla",
                              "Guadalupe_Alarcón",
                              "Brenda_Soto",
                              "Rosa_Nochebuena",
                              "Carlos_Pimentel",
                              "Gloria_Solis",
                              "Ivan_Mezquita"),
                  multiple = FALSE,
                  selected = "Misael_Garcia"),
      
      selectInput("criterios",
                  "Elige uno o más criterios:",
                  choices = c("Pidio",
                              "Obtuvo",
                              "Contraparte.pidio",
                              "Contraparte.obtuvo"),
                  multiple = TRUE,
                  selected = c("Pidio",
                               "Obtuvo",
                               "Contraparte.pidio",
                               "Contraparte.obtuvo"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("individualPlot")
    )
  ),
  
  h3("Análisis comparado"),
  
  p("Elige un", strong(" conjunto de personas"), " y", strong(" un criterio"), "que te interesen para analizar sus resultados."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alumnos",
                  "Elige una o más personas:",
                  choices = c("Misael_Garcia",
                              "Levi_Villanueva",
                              "Osmar_Ayala",
                              "Adriana_Reyes",
                              "Juan_Contreras",
                              "Rudy_Alamilla",
                              "Guadalupe_Alarcón",
                              "Brenda_Soto",
                              "Rosa_Nochebuena",
                              "Carlos_Pimentel",
                              "Gloria_Solis",
                              "Ivan_Mezquita"),
                  multiple = TRUE,
                  selected = "Misael_Garcia"),
      
      selectInput("criterio",
                  "Elige un criterio:",
                  choices = c("Pidio",
                              "Obtuvo",
                              "Contraparte.pidio",
                              "Contraparte.obtuvo"),
                  multiple = FALSE,
                  selected = c("Pidio",
                               "Obtuvo"))
    ),
    
    mainPanel(
      plotOutput("comparativoPlot")
    )
  ),
  
  h3("Resumen de rankings"),
  p("Analiza quién pidió más, contra quién pidieron más, quién obtuvo más y contra quien obtuvieron más."),
  p("¿Existen relaciones claras entre las variables?"),
  
  fluidRow(
    column(3,
           h4("¿Quién pidió más?"),
           tableOutput("pidiomas")
    ),
    column(3,
           h4("¿Contra quién pidieron más?"),
           tableOutput("pidieronmas")
    ),
    column(3,
           h4("¿Quién obtuvo más?"),
           tableOutput("obtuvomas")
    ),
    column(3,
           h4("¿Contra quién obtuvieron más?"),
           tableOutput("obtuvieronmas")
    )
  ),
  
  fluidRow(
           column(3,
                  h4("¿Quién cooperó más veces?"),
                  tableOutput("colaboromas")
           ),
           column(3,
                  h4("¿Quién se sacrificó más veces?"),
                  tableOutput("sacrificomas")
           ),
           column(3,
                  h4("¿Quién intentó aprovecharse más veces?"),
                  tableOutput("aprovechomas")
           )),
  
  strong("Web app desarrollada por Sergio López Olvera"),
  
  p("Puedes consultar el código de esta app en", a(href = "https://github.com/sergiolopezolvera/357_Anahuac/blob/main/app.R", "mi GitHub."))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$individualPlot <- renderPlot({
    # draw plot
    resultados %>%
      filter(Alumno == input$alumno & Criterio %in% input$criterios) %>%
      ggplot(aes(Ronda, Resultado, color = Criterio)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(limits = c(1,18), breaks = c(seq(1,18))) +
      scale_y_continuous(breaks = c(0,3,5,7))
  })
  
  output$comparativoPlot <- renderPlot({
    # draw plot
    resultados %>%
      filter(Alumno %in% input$alumnos & Criterio == input$criterio) %>%
      ggplot(aes(Ronda, Resultado, color = Alumno)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(limits = c(1,18), breaks = c(seq(1,18))) +
      scale_y_continuous(breaks = c(0,3,5,7))
  })
  
  output$pidiomas <- renderTable(resultados %>%
                                   filter(Criterio == "Pidio") %>%
                                   group_by(Alumno) %>%
                                   summarise(Promedio = mean(Resultado)) %>%
                                   arrange(desc(Promedio)))
  
  output$pidieronmas <- renderTable(resultados %>%
                                      filter(Criterio == "Contraparte.pidio") %>%
                                      group_by(Alumno) %>%
                                      summarise(Promedio = mean(Resultado)) %>%
                                      arrange(desc(Promedio)))
  
  output$obtuvomas <- renderTable(resultados %>%
                                    filter(Criterio == "Obtuvo") %>%
                                    group_by(Alumno) %>%
                                    summarise(Promedio = mean(Resultado)) %>%
                                    arrange(desc(Promedio)))
  
  output$obtuvieronmas <- renderTable(resultados %>%
                                        filter(Criterio == "Contraparte.obtuvo") %>%
                                        group_by(Alumno) %>%
                                        summarise(Promedio = mean(Resultado)) %>%
                                        arrange(desc(Promedio)))
  
  output$colaboromas <- renderTable(resultados %>%
                                        filter(Criterio == "Pidio" & Resultado == 5) %>%
                                        group_by(Alumno) %>%
                                        summarise(Veces = n()) %>%
                                        arrange(desc(Veces)))
  
  output$sacrificomas <- renderTable(resultados %>%
                                       filter(Criterio == "Pidio" & Resultado == 3) %>%
                                       group_by(Alumno) %>%
                                       summarise(Veces = n()) %>%
                                       arrange(desc(Veces)))
  
  output$aprovechomas <- renderTable(resultados %>%
                                       filter(Criterio == "Pidio" & Resultado == 7) %>%
                                       group_by(Alumno) %>%
                                       summarise(Veces = n()) %>%
                                       arrange(desc(Veces)))
}

# Run the application 
shinyApp(ui = ui, server = server)
