library(shiny)
library(leaflet)
library(bslib)

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#333333",        # Texte gris foncé pour le confort visuel
    primary = "#D1477D",   # Bordeaux Clair / Framboise profond
    secondary = "#F8F9FA", # Fond gris très clair pour les contrastes
    base_font = font_google("Inter"), 
    heading_font = font_google("Playfair Display")
  ),
  
  titlePanel(h1("Carte des AOP Françaises", 
                style = "font-weight: 500; color: #9A2A56; margin-bottom: 25px;")), 
  
  tabsetPanel(
    tabPanel("Accueil",
             titlePanel(tagList(icon("home"), "Accueil")),
             br(),
             fluidRow(
               column(width = 4,
                      wellPanel(
                        passwordInput("password", "Accès sécurisé :"),
                        actionButton("go", "Entrer", class = "btn-primary")
                      )
               )
             ),
             br(),
             navlistPanel(
               "Exploration",
               tabPanel("Catégorie produits"),
               tabPanel("Nom de la région")
             )
    ),
    
    tabPanel("About",
             # --- SECTION PRÉSENTATION BORDEAUX CLAIR ---
             fluidRow(
               column(width = 12,
                      wellPanel(
                        h2("Titre du projet : [Saisir le Titre Ici]", 
                           style = "color: #D1477D; font-weight: 600;"),
                        hr(style = "border-top: 2px solid #D1477D; width: 50px; margin-left: 0;"),
                        
                        p("NOTRE MISSION", 
                          style = "color: #9A2A56; letter-spacing: 2px; font-size: 0.8em; font-weight: bold;"),
                        
                        p("Bienvenue dans notre application. Ce projet vise à cartographier 
                          les Appellations d'Origine Protégées (AOP) françaises pour permettre 
                          une meilleure visualisation géographique des terroirs.", 
                          style = "text-align: justify; color: #444; font-size: 1.1em;"),
                        
                        # Style : fond très légèrement teinté, bordure gauche franche
                        style = "background-color: #FFF9FB; border: none; border-left: 5px solid #D1477D; border-radius: 4px;"
                      )
               )
             ),
             
             titlePanel(h3("L'Équipe", style="color: #9A2A56; margin-top: 30px;")),
             
             fluidRow(
               column(width = 3, wellPanel(h4("Julien"), p("Bio ici"), style = "border-top: 3px solid #D1477D; background: white;")),
               column(width = 3, wellPanel(h4("Kevine"), p("Bio ici"), style = "border-top: 3px solid #D1477D; background: white;")),
               column(width = 3, wellPanel(h4("Tibault"), p("Bio ici"), style = "border-top: 3px solid #D1477D; background: white;")),
               column(width = 3, wellPanel(h4("Glory"), p("Bio ici"), style = "border-top: 3px solid #D1477D; background: white;"))
             )
    )
  )
)

server <- function(input, output, session) {
  # Logique serveur
}

shinyApp(ui, server)