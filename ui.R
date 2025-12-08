dashboardPage(
  # ------------------- Header -------------------------------------------------------------------------#
  dashboardHeader(
    title = "Bar- und Boxplot",
    tags$li(
      class = "dropdown",
      style = "padding: 15px; color: white;",
      aktuelle_version # Zeigt die neueste Version aus der Datei an
    )
  ),

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
        # Da der Abstand viel zu groß ist, wird er hiermit reduziert
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
        checkboxInput("LegendenTitel", label = "Legenden Titel ausblenden", value = FALSE),
        checkboxInput("BoxColor", label = "Boxplot Farbe", value = FALSE),
        checkboxInput("InvertPoint", label = "Punktfarbe invertieren", value = FALSE),
        sliderInput("PointSize", label = "Punktgröße", min = 1, max = 10, step = 0.5, value = 2),
        fluidRow(
          style = "margin-left: 0px; margin-right: 0px;",
          column(6, numericInput("yMin", "Y-Achse Min", value = NA, min = 0)),
          column(6, numericInput("yMax", "Y-Achse Max", value = NA, min = 0))
        ),
        
        # Zwischenüberschrift
        tags$hr(style = "margin-top: 20px; margin-bottom: 10px;"),
        h3("Download Optionen", style = "margin-left: 15px; margin-right: 5px;"),
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
          choices = c("png (in cm)" = "png", "pdf (in cm)" = "pdf", "svg (in cm)" = "svg"),
        ),
        # Downloadbutton, style definiert die position
        downloadButton("downloadPlot", style = "display: block; margin-left: 50px; margin-right: 50px;")
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
        # verbatimTextOutput("TextTableOutput"),
        box(
          title = "Daten Tabelle",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          "Die Daten können als Excel-Datei (.xlsx) hochgeladen werden. Die Datei darf maximal 100MB groß sein.",
          br(),
          "Stelle sicher, dass die Daten korrekt formatiert sind, bevor du sie hochlädst. Numerische Werte sollten als solche erkannt werden.",
          br(),
          "Wenn die Daten eines Parameters (z.B. Gene) pro Reihe bzw. Zeile angegeben sind (z.B. Array-Daten), aktiviere die entsprechende Option im Seitenmenü.",
          br(),
          "Gib die Spaltennummer an, bevor numerische Werte in der Tabelle beginnen, um eine korrekte Konvertierung sicherzustellen.",
          br(),
          "Nach dem Hochladen werden die Daten verarbeitet und die ersten 10 Zeilen und 10 Spalten der Tabelle angezeigt."
        ),
        DTOutput("data")
      ),
      tabItem(
        tabName = "Plots_erstellen", # dein neuer gemeinsamer Tab
        tabsetPanel(
          id = "plot_tabs", # wichtig für server-seitige Logik
          type = "tabs",
          tabPanel("Boxplot", plotOutput("BoxplotsWithDots", height = "auto", width = "auto")),
          tabPanel("Barplot", plotOutput("BarplotsWithDots", height = "auto", width = "auto"))
        ),
        # ab hier alles gemeinsam für beide

        # Abstand zwischen Plot und Optionen
        tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
        
        # Informationstext zu den Plot Optionen
        box(
          title = "Information",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          "Die Bildgröße kann in den Download-Optionen angepasst werden. Die Bildvorschau entspricht der tatsächlichen Auflösung des Bildes.",
          "Wenn der Text abgeschnitten ist, muss entweder der Text oder die Bildgröße angepasst werden.",
          br(),
          "Wenn Punktfarbe und Gruppe identisch sind, wird können die Boxplots farbig dargestellt werden."
        ),
        
        box(
          title = "Gruppen auswählen und Reihenfolge festlegen",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          
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
          )
        ),
        box(
          title = "Farbpalette anpassen",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          uiOutput("ColorPickerUI")
        )
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
