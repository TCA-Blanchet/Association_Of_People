library(shiny)
library(leaflet)
library(bslib)

# Define UI for application
fluidPage(
  theme = bs_theme(
    bg = "white",        # couleur de fond
    fg = "blue",         # couleur du texte
    primary = "#0d6efd", # couleur principale
    secondary = "#6c757d", # couleur secondaire
    base_font = font_google("Roboto")
  ),
  
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    tabPanel("Accueil",
             titlePanel(icon("home"), "Accueil"),
             
             passwordInput("password", "Password:"),
             actionButton("go", "Go"),
             verbatimTextOutput("value"), 
             
             navlistPanel(
               "Liste",
               tabPanel("Catégorie produits"),
               tabPanel("Nom de la région")
             ),
             
             imageOutput("aop_image")
    ), # fermeture tabPanel Accueil
    
    tabPanel("Carte AOP",
             titlePanel("Carte Interactive des AOP"),
             
             selectInput("variable_produit", "Catégorie produits:",
                         c("Boissons" = "cyl",
                           "Produits carnés" = "am",
                           "Produits laitiers" = "gear", 
                           "Autres" = "autre")),
             tableOutput("data_AOP"),
             
             selectInput("variable_region", "Nom de la région:",
                         c("Auvergne rhone alpes" = "cyl",
                           "Iles de France" = "am",
                           "Occitanie" = "Occitanie")),
             tableOutput("data_département"),
             
             fluidRow(
               column(width = 4),
               column(width = 2, offset = 3)
             ),
             fluidRow(column(width = 12)),
             
             checkboxGroupInput("icons", "Choose icons:",
                                choiceNames = list(
                                  icon("calendar"), 
                                  icon("bed"),
                                  icon("cog"), 
                                  icon("bug")
                                ),
                                choiceValues = list(
                                  "calendar", "bed", "cog", "bug"
                                )
             )
    ), # fermeture tabPanel Carte AOP
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             
             checkboxGroupInput("variable_about", "Variables to show:",
                                c("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear")),
             
             fluidRow(
               column(width = 4),
               column(width = 2, offset = 3)
             ),
             fluidRow(column(width = 12)),
             
             checkboxGroupInput("variable_about2", "Variables to show:",
                                c("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear"))
             
    ) # fermeture tabPanel About
  ) # fermeture tabsetPanel
) # fermeture fluidPage