library(dplyr)
library(shiny)
library(shinyWidgets)

## define width of sliders and slider-choices and weights 'globally'
width <- 220

choices <- c(1.0, 1.3, 1.7,
             2.0, 2.3, 2.7,
             3.0, 3.3, 3.7, 4.0)

weight <- c(.6, .2, .2)


# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  
  # Application title
  titlePanel("Notenrechner für den Bachelor Psychologie in Bremen"),
  
  hr(),
  
  fluidRow(
    div(align = "center",
        p(strong("!! Alle Angaben ohne Gewähr !!")),
        column(4, h3("Note laut PO:"), h3(textOutput("note"))),
        column(4, h3("Note laut ToR:"), h3(textOutput("note_zpa"))),
        column(4, h3("Note laut ToR ohne die BA:"), h3(textOutput("zpa_ohne_ba")))
    )
  ),
  
  hr(),
  
  # check for Prüfungsordnung & graded general studies
  fluidRow(
    
    sidebarLayout(
      #column(3,
             sidebarPanel(
               p(strong("freie General Studies CP")),
               checkboxInput("gs_check", "benotet"),
               
               conditionalPanel("input.gs_check == true",
                                numericInput("gs_anzahl", "Wieviele davon?", 6, 1, 6, 1, 120)),
               
               actionButton("calc", "Note berechnen", icon = icon("calculator"))
      ),
      
      # column(9,
             # Ask for grades
             mainPanel(
               
               h3("Pflichtbereich"),
               
               fluidRow(
                 column(4,
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
               
               fluidRow(
                 column(4, sliderTextInput("qm1", "Statistik I", choices, width = width)),
                 column(4, sliderTextInput("qm2", "Statistik II", choices, width = width)),
                 
                 conditionalPanel("input.gs_check == true",
                                  column(4, sliderTextInput("gs_note", "freie GS CP", choices, width = width)))
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
    df <- readRDS("bpo2017.rds")
    
    df$note <- c(input$allg, input$biol, input$diff, input$entw,
                 input$expr, input$klin, input$diag, input$meth,
                 input$sozi, input$qm1, input$qm2)
    
    print(df)
    
    if (input$gs_check) {
      gs <- data.frame(
        modul = "General Studies",
        cp    = input$gs_anzahl,
        note  = input$gs_note
      ) 
      
      df <- rbind(df, gs)
    }
    
    result <- df %>% 
      mutate(
        zw1  = note * cp,
        zw2  = zw1 / sum(cp)
      )
    
    pflichtbereich <- sum(result$zw2)
    wahlbereich    <- mean(c(input$wpm1, input$wpm2))
    bachelorarbeit <- input$ba
    
    mean_w <- weighted.mean(c(pflichtbereich,
                              wahlbereich,
                              bachelorarbeit),
                            w = weight) %>% round(2)
    
    # output$note <- renderText(paste("Note laut PO:", mean_w))
    output$note <- renderText(mean_w)
    
    # note auf transcript / laut zpa:
    # zpa_2017 <- c(bpo2017, "Wahlpflicht I", "Wahlpflicht II", "Bachelorarbeit")
    # zpa_cp   <- c(cp_2017, 15, 15, 12)
    # zpa_note <- c(note, n13, n14, n15)
    # 
    # zpa_result <- data.frame(
    #   modul = zpa_2017,
    #   cp    = zpa_cp,
    #   note  = zpa_note
    # ) %>% 
    #   mutate(
    #     zw1  = note * cp,
    #     zw2  = zw1 / sum(cp)
    #   )
    # 
    # zpa_note <- round(sum(zpa_result$zw1) / sum(zpa_result$cp), 2)
    # zpa_ohne_ba <- round(sum(zpa_result$zw1[1:14]) / sum(zpa_result$cp[1:14]), 2)
    # 
    # # output$note_zpa <- renderText(paste("Note laut ZPA:", zpa_note))
    # # output$zpa_ohne_ba <- renderText(paste("ohne die BA:", zpa_ohne_ba))
    zpa <- data.frame(
      modul = c("Wahlpflicht I", "Wahlpflicht II", "Bachelorarbeit"),
      cp    = c(15, 15, 12),
      note  = c(input$wpm1, input$wpm2, input$ba)
    ) %>% 
      mutate(
        zw1  = note * cp,
        zw2  = zw1 / sum(cp)
      )
    
    zpa_result <- rbind(result, zpa)
    
    
    zpa_note    <- round(sum(zpa_result$zw1) / sum(zpa_result$cp), 2)
    zpa_ohne_ba <- round(sum(zpa_result$zw1[-length(zpa_result$zw1)]) / sum(zpa_result$cp[-length(zpa_result$zw1)]), 2)
    
    output$note_zpa <- renderText(zpa_note)
    output$zpa_ohne_ba <- renderText(zpa_ohne_ba)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

