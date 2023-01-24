library(shiny)
library(tidyverse)

resultados <- read.csv("data/357_AlumnosAnahuac.csv") %>%
  pivot_longer(cols = c(Pidio, Contraparte.pidio, Obtuvo, Contraparte.obtuvo),
               names_to = "Criterio", values_to = "Resultado")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Tiburones y charales"),
  p("Análisis de resultados de los alumnos de la asignatura ", em("Derecho Ambiental"), " de la", strong(" Universidad Anáhuac campus Xalapa"), " en 2023."),
  h3("Análisis individual"),
  
  p("Elige", strong(" una alumna o un alumno"), " y el", strong(" conjunto de criterios"), " que te interesen para analizar sus resultados."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alumno",
                  "Elige una alumna o un alumno:",
                  choices = c("Alejandra",
                              "Alfonso",
                              "Alicia",
                              "Allan",
                              "Carmen Dayana",
                              "Clemente",
                              "Gerardo",
                              "José María",
                              "Karla",
                              "Leslie",
                              "Neila"),
                  multiple = FALSE),
      
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
  
  p("Elige un", strong(" conjunto de alumnas o alumnos"), " y", strong(" un criterio"), "que te interesen para analizar sus resultados."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alumnos",
                  "Elige una o más alumnas o alumnos:",
                  choices = c("Alejandra",
                              "Alfonso",
                              "Alicia",
                              "Allan",
                              "Carmen Dayana",
                              "Clemente",
                              "Gerardo",
                              "José María",
                              "Karla",
                              "Leslie",
                              "Neila"),
                  multiple = TRUE,
                  selected = "Alejandra"),
      
      selectInput("criterio",
                  "Elige un:",
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
  strong("Web app desarrollada por Sergio López Olvera")
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
      scale_x_continuous(limits = c(1,20), breaks = c(seq(1,20))) +
      scale_y_continuous(breaks = c(0,3,5,7))
  })
  
  output$comparativoPlot <- renderPlot({
    # draw plot
    resultados %>%
      filter(Alumno %in% input$alumnos & Criterio == input$criterio) %>%
      ggplot(aes(Ronda, Resultado, color = Alumno)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(limits = c(1,20), breaks = c(seq(1,20))) +
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
}

# Run the application 
shinyApp(ui = ui, server = server)