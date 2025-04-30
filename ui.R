dashboardPage(
  # ------------------- Header -------------------------------------------------------------------------#
  dashboardHeader(title = "Bar- und Boxplot",
                  tags$li(
                    class = "dropdown",
                    style = "padding: 15px; color: white;",
                    aktuelle_version  # Zeigt die neueste Version aus der Datei an
                  )),

  # ------------------- Sidebar -------------------------------------------------------------------------#
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", # Tab-Panel ID hinzufügen, aktiver tab in input$tabs
      # Tab für Daten eingabe
      menuItem(
        text = "Data Input",
        expandedName = "dataTabExpand",
        icon = icon("arrow-up-from-bracket"),
        menuItem(
          text = "Data Table",
          tabName = "data",
          icon = icon("table")
        ),
        # Datei upload
        fileInput(
          "DataTable",
          "Upload a data file:",
          multiple = FALSE,
          accept = c(".xlsx"),
          placeholder = "Max. 100MB"
        ),
        # Da der Abstadn viel zu groß ist, wird er hiermit reduziert
        div(style = "margin-top: -30px"),
        # array daten oder nicht
        checkboxInput(
          "InpDat",
          label = "Sind die Daten eines Parameters (zbsp. Gene) pro Reihe bzw. Zeile angegeben (zbsp. Array data)?",
          value = FALSE
        ),

        # Ab wann sind in der Tabelle Nummerische Daten (für konvertierung)
        numericInput("colcut", label = "Spaltennummer bevor nummerische Werte kommen:", value = 0, min = 0),
        # Am anfang direkt ausgeklappt
        startExpanded = TRUE
      ),

      # Tab für Boxplot
      menuItem(
        text = "Plots",
        expandedName = "PlotsExpand",
        icon = icon("image"),
        menuItem(
          text = "Plots erstellen",
          tabName = "Plots_erstellen",
          icon = icon("chart-column")
        ),
        # Gruppe für Punktfarbe
        selectInput("dotid", label = "Was definiert die Punktfarbe?", choices = NULL, selectize = TRUE),
        # Spaltenname für Gruppierung
        selectInput("groupnameis", label = "Welche Spalte soll als Gruppe verwendet werden?", choices = NULL, selectize = TRUE),
        # Plotvariable
        selectInput("DataY", label = "Was soll auf der Y-Achse geplottet werden?", choices = NULL, selectize = TRUE),
        textInput("LabelY", label = "Beschriftung Y-Achse", value = "y-Axis"),
        textInput("LabelX", label = "Beschriftung X-Achse", value = NULL),
        checkboxInput("TitelKursiv", label = "Titel kursiv", value = FALSE),
        column(6, numericInput("yMin", "Y-Achse Min", value = NA, min = 0)),
        column(6, numericInput("yMax", "Y-Achse Max", value = NA, min = 0)),
        sliderInput("PointSize", label = "Punktgröße", min = 1, max = 10, step = 0.5, value = 2),
        colourInput("Colorpicker", "Colorpicker für Hex-Codes", value = "red"),
        
        # Zwischenüberschrift
        div(
          h3("Download Optionen", style = "display: block; margin-left: 15px; margin-right: 5px;"),
          # Dateiname
          textInput("Filename", label = "Bild Dateiname ", placeholder = "Dateiname"),
          # Bildbreite-, höhe und -auflösung
          numericInput("ImageWidth", label = "Bildbreite", value = 20),
          numericInput("ImageHeight", label = "Bildhöhe", value = 20),
          numericInput("ImageDPI", label = "Bildauflösung (dots per inch)", value = 200),
          # Bild speichern als png oder PDF
          radioButtons(
            "ImageFiletype",
            label = "Bildformat auswählen",
            choices = c("png (in cm)" = "png", "pdf (in cm)" = "pdf")
          ),
          # Downloadbutton, style definiert die position
          downloadButton("downloadPlot", style = "display: block; margin-left: 50px; margin-right: 50px;")
          )
      ),
      menuItem(
        text = "Session Info",
        tabName = "SessionInfo",
        icon = icon("circle-info")
      )
    ),
    actionButton("show_changelog", "Changelog anzeigen", icon = icon("circle-info"))
  ),

  # ------------------- Body ----------------------------------------------------------------------------#
  dashboardBody( # tabName in sidebar and body needs to match in name (but not in order)
    tabItems(
      # tab für die Tabelle
      tabItem(
        tabName = "data",
        verbatimTextOutput("TextTableOutput"),
        DTOutput("data")
      ),
      tabItem(
        tabName = "Plots_erstellen",  # dein neuer gemeinsamer Tab
        tabsetPanel(
          id = "plot_tabs",  # wichtig für server-seitige Logik
          type = "tabs",
          tabPanel("Boxplot", plotOutput("BoxplotsWithDots")),
          tabPanel("Barplot", plotOutput("BarplotsWithDots"))
        ),
        # ab hier alles gemeinsam für beide
        verbatimTextOutput("TextColorTableBoxplot"),
        fluidRow(
          column(
            width = 12,
            checkboxGroupButtons(
              inputId = "selected_groups",
              label = "Wähle die Gruppen, die im Plot erscheinen sollen:",
              choices = character(0),
              selected = character(0),
              status = "primary",
              direction = "horizontal",
              checkIcon = list(yes = icon("check"))
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            orderInput(
              "group_order",
              label = "Wähle die Reihenfolge der Gruppen:",
              items = NULL,
              class = "btn-group",
              width = "100%"
            )
          )
        ),
        DTOutput("SelectionGroup")
      ),
      # tab für die session info
      tabItem(
        tabName = "SessionInfo",
        verbatimTextOutput("sessionInfo")
      )
    ),

    # CSS-Datei für Style
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    )
  )
)
