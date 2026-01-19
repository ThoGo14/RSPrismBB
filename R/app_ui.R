#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    
    golem_add_external_resources(),

        dashboardPage(
      # ------------------- Header -------------------------------------------------------------------------#
      dashboardHeader(
        title = "RSPrism",
        tags$li(
          class = "dropdown",
          style = "padding: 15px; color: white;",
          textOutput("version_number", inline = TRUE)
        ),
        tags$li(
          class = "dropdown",
          style = "padding: 8px;",
          selectInput(
            "selected_language",
            label = NULL,
            choices = c("Deutsch" = "de", "English" = "en"),
            selected = "de",
            width = "120px"
          )
        )
      ),

      # ------------------- Sidebar -------------------------------------------------------------------------#
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          # Tab for data input
          menuItem(
            text = "Dateneingabe",  
            expandedName = "dataTabExpand",
            icon = icon("arrow-up-from-bracket"),
            menuItem(
              text = "Datentabelle",
              tabName = "data",
              icon = icon("table")
            ),
            # Data upload
            fileInput(
              "DataTable",
              "Datei hochladen",
              multiple = FALSE,
              accept = c(".xlsx"),
              placeholder = "Max. 100MB"
            ),
            div(style = "margin-top: -30px"),
            checkboxInput(
              "InpDat",
              label = "Array-Daten?",
              value = FALSE
            ),
            numericInput("colcut", label = "Spalte vor numerischen Daten", value = 0, min = 0),
            startExpanded = TRUE
          ),

          # Tab für Plots
          menuItem(
            text = "Plots",
            expandedName = "PlotsExpand",
            icon = icon("image"),
            menuItem(
              text = "Plots erstellen",
              tabName = "Plots_erstellen",
              icon = icon("chart-column")
            ),
            selectInput("dotid", label = "Punktfarbe nach Gruppe?", choices = NULL, selectize = TRUE),
            selectInput("groupnameis", label = "Gruppenspalte?", choices = NULL, selectize = TRUE),
            selectInput("DataY", label = "Y-Achsen Variable", choices = NULL, selectize = TRUE),
            textInput("LabelY", label = "Y-Achsen Beschriftung", value = "Y-Achse"),
            textInput("LabelX", label = "X-Achsen Beschriftung", value = NULL),
            checkboxInput("TitelKursiv", label = "Titel kursiv?", value = FALSE),
            checkboxInput("LegendenTitel", label = "Legendentitel ausblenden?", value = FALSE),
            checkboxInput("BoxColor", label = "Boxplot einfärben?", value = FALSE),
            checkboxInput("InvertPoint", label = "Punktfarbe invertieren?", value = FALSE),
            sliderInput("PointSize", label = "Punktgröße", min = 1, max = 10, step = 0.5, value = 2),
            fluidRow(
              style = "margin-left: 0px; margin-right: 0px;",
              column(6, numericInput("yMin", "Y-Achse Min", value = NA, min = 0)),
              column(6, numericInput("yMax", "Y-Achse Max", value = NA, min = 0))
            ),
            
            # Download options
            tags$hr(style = "margin-top: 20px; margin-bottom: 10px;"),
            h3(textOutput("download_options_header", inline = TRUE), style = "margin-left: 15px; margin-right: 5px;"),
            textInput("Filename", label = "Bilddateiname", placeholder = "Dateiname"),
            numericInput("ImageWidth", label = "Bildbreite (cm)", value = 20),
            numericInput("ImageHeight", label = "Bildhöhe (cm)", value = 20),
            numericInput("ImageDPI", label = "Bildauflösung (DPI)", value = 200),
            radioButtons(
              "ImageFiletype",
              label = "Bildformat",
              choices = c("png (in cm)" = "png", "pdf (in cm)" = "pdf", "svg (in cm)" = "svg"),
            ),
            downloadButton("downloadPlot", label = "Plot herunterladen", style = "display: block; margin-left: 50px; margin-right: 50px;")
          ),
          
          # Session Info
          menuItem(
            text = "Session Info",
            tabName = "SessionInfo",
            icon = icon("circle-info")
          )
        ),
        
        # Changelog Button
        actionButton("show_changelog", "Changelog", icon = icon("circle-info"))
      ),

      # ------------------- Body ----------------------------------------------------------------------------#
      dashboardBody(
        tabItems(
          # For Tables
          tabItem(
            tabName = "data",
            box(
              title = textOutput("data_table_title_out", inline = TRUE),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              textOutput("data_table_info_1_out"),
              br(),
              textOutput("data_table_info_2_out"),
              br(),
              textOutput("data_table_info_3_out"),
              br(),
              textOutput("data_table_info_4_out"),
              br(),
              textOutput("data_table_info_5_out")
            ),
            DT::DTOutput("data")
          ),
          
          # For plots
          tabItem(
            tabName = "Plots_erstellen",
            tabsetPanel(
              id = "plot_tabs",
              type = "tabs",
              tabPanel("Boxplot", plotOutput("BoxplotsWithDots", height = "auto", width = "auto")),
              tabPanel("Barplot", plotOutput("BarplotsWithDots", height = "auto", width = "auto"))
            ),
            
            tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
            
            box(
              title = textOutput("info_box_title_out", inline = TRUE),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              textOutput("info_box_text_1_out"),
              textOutput("info_box_text_2_out"),
              br(),
              textOutput("info_box_text_3_out")
            ),
            
            box(
              title = textOutput("group_selection_title_out", inline = TRUE),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              
              fluidRow(
                column(
                  width = 12,
                  uiOutput("group_selection_checkbox_ui")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("group_order_ui")
                )
              )
            ),
            box(
              title = textOutput("color_palette_title_out", inline = TRUE),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              uiOutput("ColorPickerUI")
            )
          ),
          
          # For Session Info
          tabItem(
            tabName = "SessionInfo",
            verbatimTextOutput("sessionInfo")
          )
        ),
        
        # Enable shinyjs for dynamic label updates
        useShinyjs()
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RSPrismBB"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  )
}
