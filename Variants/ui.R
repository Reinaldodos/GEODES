


library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Quand deux épidémies se croisent"),
        
        shiny::wellPanel(
            shiny::textInput(
                inputId = "Init_total",
                label = "Nb de cas journaliers à J0: ", 
                value = 20000),
            
            plotOutput(outputId = "Graphe",
                       width = "100%"),
            
            fluidRow(
                column(
                    3,
                    sliderInput(
                        inputId = "R_sauvage",
                        label = "R0 du variant classique",
                        min = 0,
                        max = 2,
                        value = .95,
                        step = .05,
                        round = F
                    ),
                    sliderInput(
                        inputId = "Surinfection",
                        label = "Taux de surinfection des nouveaux variants (%)",
                        min = 0,
                        max = 100,
                        value = 50,
                        step = 10,
                        round = T
                    )
                ),
                column(
                    4,
                    offset = 1,
                    sliderInput(
                        inputId = "Ratio_variant",
                        label = "Proportion de nouveaux variants (%)",
                        min = 0,
                        max = 100,
                        value = 6,
                        step = 1,
                        round = T
                    ),
                    sliderInput(
                        inputId = "Fenetre",
                        label = "Fenêtre prévisionnelle (jours)",
                        min = 0,
                        max = 150,
                        value = 14,
                        step = 7,
                        round = F
                    )
                ),
                column(
                    4,
                    materialSwitch(
                        inputId = "Cas_total",
                        label = "Nombre de cas totaux",
                        right = TRUE
                    ),
                    materialSwitch(
                        inputId = "Cas_sauvage",
                        label = "Nombre de cas du variant classique",
                        right = TRUE,
                        status = "primary"
                    ),
                    materialSwitch(
                        inputId = "Cas_variant",
                        label = "Nombre de cas des nouveaux variants",
                        right = TRUE,
                        status = "danger"
                    ),
                    materialSwitch(
                        inputId = "Log_scale",
                        label = "Echelle logarithmique",
                        right = TRUE
                    )
                )
            )
        )
    )
)
            