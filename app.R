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
  h1("Notenrechner für den Bachelor Psychologie in Bremen", align = "center"),
  
  hr(),
  
  fluidRow(
    div(align = "center",
        p(strong("!! Alle Angaben ohne Gewähr !!")),
        column(4, h3("Note laut PO:"), h3(textOutput("note")),
               p("(was auf dem Zeugnis stehen wird)")),
        column(4, h3("Note laut ZPA:"), h3(textOutput("note_zpa")),
               p("(was auf dem Transcript of Records steht)")),
        column(4, h3("Note ohne die BA:"), h3(textOutput("zpa_ohne_ba")),
               p("(womit sich meist beworben wird)"))
    )
  ),
  
  hr(),
  
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
    
    output$note <- renderText(mean_w)

    
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

