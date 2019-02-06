library(tidyverse)
library(shiny)
library(shinyWidgets)

## define width of sliders 'globally'
width <- 220

## Transcript of Records

bpo2017 <- c("Allgemeine", "Biologische", "Differentielle",
             "Entwicklung & Pädagogische", 
             "Expra", "Klinische",
             "Diagnostik", "Methodenlehre", "Soziale",
             
             "QM I", "QM II")

# "General Studies", 
# "Wahlpflicht I", "Wahlpflicht II", "Bachelorarbeit")

cp_2017 <- c(18, 6, 6, 15, 6, 15, 12, 12, 15, 6, 9)

df <- data.frame(modul = bpo2017, 
                 cp    = cp_2017)

choices <- c(1.0, 1.3, 1.7,
             2.0, 2.3, 2.7,
             3.0, 3.3, 3.7, 4.0)

# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  
  # Application title
  titlePanel("Notenrechner für den Bachelor Psychologie in Bremen"),
  p(strong("!! Alle Angaben ohne Gewähr !!")),
  
  # check for Prüfungsordnung & graded general studies
  fluidRow(
    sidebarLayout(
      #column(3,
             sidebarPanel(
               p(strong("6 CP General Studies")),
               checkboxInput("gs_note", "benotet"),
               
               actionButton("calc", "Note berechnen", icon = icon("calculator")),
               
               verbatimTextOutput("note"),
               verbatimTextOutput("note_zpa"),
               verbatimTextOutput("zpa_ohne_ba")
      #       )
      ),
      
      # column(9,
             # Ask for grades
             mainPanel(
               # p("als Dezimaltrenner bitte einen Punkt verwenden"),
               
               h3("Pflichtbereich"),
               
               fluidRow(
                 column(4,
                        # sliderInput("allg", "Allgemeine", min = 1.0, max = 4.0, value = 4.0, 
                        #             step = 1/3, round = -1, width = 250, ticks = FALSE),
                        
                        sliderTextInput("allg", "Allgemeine", choices = choices, width = width),
                        
                        sliderTextInput("biol", "Biologische", choices, width = width),
                        sliderTextInput("diff", "Differentielle", choices, width = width)
                 ),
                 
                 column(4,
                        sliderTextInput("entw", "Entwicklung & Pädagogische", choices, width = width),
                        sliderTextInput("expr", "Expra", choices, width = width),
                        sliderTextInput("klin", "Klinische", choices, width = width)
                 ),
                 
                 column(4,
                        sliderTextInput("diag", "Diagnostik", choices, width = width),
                        sliderTextInput("meth", "Methodenlehre", choices, width = width),
                        sliderTextInput("sozi", "Soziale und A&O", choices, width = width)
                 )
               ),
               
               h3("General Studies"),
               
               p(strong("Hinweis:"), 
                 "derzeit werden die freien 6 General Studies CP bei der Berechnung", strong("nicht"), "berücksichtigt"),
               
               fluidRow(
                 # column(4, numericInput("ast", "AST", 4, 1.0, 4.0, width = 250)),
                 column(4, sliderTextInput("qm1", "Statistik I", choices, width = width)),
                 column(4, sliderTextInput("qm2", "Statistik II", choices, width = width)),
                 
                 conditionalPanel("input.gs_note == true",
                                  column(4, sliderTextInput("gs", "6 freie CP", choices, width = width)))
               ),
               
               fluidRow(
                 h3("Wahlpflichtmodule & Bachelorarbeit"),
                 column(4, sliderTextInput("wpm1", "WPM 1", choices, width = width)),
                 column(4, sliderTextInput("wpm2", "WPM 2", choices, width = width)),
                 column(4, sliderTextInput("ba", "Bachelorarbeit", choices, width = width))
               )
             )
      # )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # observeEvent(input$gs_note, {
  #   gs_note <- input$gs_note
  #   
  #   if (gs_note == TRUE) {
  #     insertUI(selector = "#gs_ui",
  #              ui = column(4, numericInput("gs", "6 freie CP", 
  #                                          4, 1.0, 4.0, width = 250)))
  #   } else {
  #     removeUI(selector = "#gs_ui")
  #   }
  #   # removeUI(selector = "#gs_ui"))
  # })
  
  observeEvent(input$calc, {
    n1  <- input$allg
    n2  <- input$biol
    n3  <- input$diff
    n4  <- input$entw
    n5  <- input$expr
    n6  <- input$klin
    n7  <- input$diag
    n8  <- input$meth
    n9  <- input$sozi
    
    # n10 <- input$ast
    n11 <- input$qm1
    n12 <- input$qm2
    
    n13 <- input$wpm1
    n14 <- input$wpm2
    
    n15 <- input$ba
    
    note <- c(n1, n2, n3, n4, n5, 
              n6, n7, n8, n9, # n10, 
              n11, n12)
    
    result <- df %>% 
      mutate(
        note = note,
        zw1  = note * cp,
        zw2  = zw1 / sum(cp)
      )
    
    weight <- c(.6, .2, .2)
    
    pflichtbereich <- sum(result$zw2)
    wahlbereich    <- mean(c(n13, n14))
    bachelorarbeit <- n15
    
    mean_w <- weighted.mean(c(pflichtbereich,
                              wahlbereich,
                              bachelorarbeit),
                            w = weight)
    
    output$note <- renderText(paste("Note laut PO:", mean_w))
    
    # note auf transcript / laut zpa:
    zpa_2017 <- c(bpo2017, "Wahlpflicht I", "Wahlpflicht II", "Bachelorarbeit")
    zpa_cp   <- c(cp_2017, 15, 15, 12)
    zpa_note <- c(note, n13, n14, n15)
    
    zpa_result <- data.frame(
      modul = zpa_2017,
      cp    = zpa_cp,
      note  = zpa_note
    ) %>% 
      mutate(
        zw1  = note * cp,
        zw2  = zw1 / sum(cp)
      )
    
    zpa_note <- round(sum(zpa_result$zw1) / sum(zpa_result$cp), 2)
    zpa_ohne_ba <- round(sum(zpa_result$zw1[1:14]) / sum(zpa_result$cp[1:14]), 2)
    
    output$note_zpa <- renderText(paste("Note laut ZPA:", zpa_note))
    output$zpa_ohne_ba <- renderText(paste("ohne die BA:", zpa_ohne_ba))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

