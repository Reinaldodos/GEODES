


library(shiny)

TEST <- function(R_sauvage, Ratio_variant, Surinfection, Init_total) {
    Tau = 3.96
    R_variant = R_sauvage * Surinfection
    
    K_sauvage = log(R_sauvage)/Tau
    K_variant = log(R_variant)/Tau
    
    Init_variant = Ratio_variant * Init_total / 100
    Init_sauvage = Init_total - Init_variant
    
    Courbe_sauvage = function(N) {
        Init_sauvage * exp(K_sauvage) ** N
    }
    Courbe_variant = function(N) {
        Init_variant * exp(K_variant) ** N
    }
    Courbe_totale = function(N) {
        Courbe_sauvage(N) + Courbe_variant(N)
    }
    
    return(
        list(
            "Variant" = Courbe_variant,
            "Classique" = Courbe_sauvage,
            "Total" = Courbe_totale
        )
    )
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    GRAPHE = reactive({
        Outils =
            TEST(
                R_sauvage = input$R_sauvage,
                Ratio_variant = input$Ratio_variant,
                Surinfection = 1 + input$Surinfection / 100,
                Init_total = as.integer(input$Init_total)
            )
        require(ggplot2)
        PLOTT =
            plot =
            ggplot(
                data = data.frame(Nb_jours = 0:input$Fenetre),
                mapping = aes(x = Nb_jours)
            ) +
            ylim(0, NA) +
            geom_hline(yintercept = 5000,
                       colour = "green",
                       linetype = "twodash") +
            ylab(label = "Nombre de cas journaliers") +
            xlab(label = "Nombre de jours prévisionnels") +
            theme_minimal()
        
        if (input$Cas_total) {
            PLOTT =
                PLOTT +
                stat_function(fun = Outils$Total, colour = "black")
        }
        
        if (input$Cas_variant) {
            PLOTT =
                PLOTT +
                stat_function(fun = Outils$Variant,
                              colour = "red")
        }
        
        if (input$Cas_sauvage) {
            PLOTT =
                PLOTT +
                stat_function(fun = Outils$Classique,
                              colour = "blue")
        }
        
        
        
        return(PLOTT)
        
    })
    
    
    output$Graphe <- renderPlot({
        GRAPHE()
    })
    
})
