dashboardPage(
  # ------------------- Header -------------------------------------------------------------------------#
  dashboardHeader(title = "Bar- und Boxplot"),
  
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
        ),
        # Da der Abstadn viel zu groß ist, wird er hiermit reduziert
        div(style = "margin-top: -30px"),
        # array daten oder nicht
        checkboxInput(
          "InpDat", 
          label = "Sind die Daten eines Parameters (zbsp. Gene) pro Reihe angegeben (zbsp. Array data)?",
          value = FALSE),
        
        # Ab wann sind in der Tabelle Nummerische Daten (für konvertierung)
        textInput("colcut", label = "Spaltennummer bevor nummerische Werte kommen:", value = 0),
        # Am anfang direkt ausgeklappt
        startExpanded = TRUE
      ),
      
      # Tab für Boxplot
      menuItem(
        text = "Plots",
        expandedName = "PlotsExpand",
        icon = icon("image"),
        menuItem(
          text = "Boxplots mit Punkten",
          tabName = "BoxplotsWithDots",
          icon = icon("boxes-stacked")
        ),
        menuItem(
          text = "Barplot mit Punkten",
          tabName = "BarplotsWithDots",
          icon = icon("chart-simple")
        ),
        # Gruppe für Punktfarbe
        selectInput("dotid", label = "Was definiert die Punktfarbe?", choices = NULL),
        # Spaltenname für Gruppierung
        selectInput("groupnameis", label = "Welche Spalte soll als Gruppe verwendet werden?", choices = NULL),
        # Plotvariable
        selectInput("DataY", label = "Was soll auf geplottet werden?", choices = NULL),
        
        textInput("LabelY", label = "Beschriftung Y-Achse", value = "y-Axis"),
        textInput("LabelX", label = "Beschriftung X_Achse", value = NULL),
        sliderInput("PointSize", label = "Punktgröße", min = 1, max = 10, step = 0.5, value = 2),
        
        # Wähle die Gruppen aus, die im Plot erscheinen sollen
        checkboxGroupInput(
          inputId = "selected_groups",
          label = "Wähle die Gruppen, die im Plot erscheinen sollen:",
          choices = NULL,
          # wird dynamisch basierend auf den Daten gesetzt
          selected = NULL # standardmäßig keine Gruppen ausgewählt
        ),
        
        # Slider für die Reihenfolge der Gruppen, text muss um 15px eingerückt werden
        orderInput(
          "group_order",
          label = tags$span(style = "margin-left: 15px", "Ordne die Gruppen:"),
          items = NULL
        ),
        
        # Zwischenüberschrift
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
          choices = c("png (in cm)" = "png", "pdf (in inch)" = "pdf")
        ),
        # Downloadbutton, style definiert die position
        downloadButton("downloadPlot", style = "display: block; margin-left: 50px; margin-right: 50px;")
        
      )
      
      
    )
  ),
  
  # ------------------- Body ----------------------------------------------------------------------------#
  dashboardBody(# tabName in sidebar and body needs to match in name (but not in order)
    
    tabItems(
      # tab für die Tabelle
      tabItem(tabName = "data", DTOutput("data")),
      # tab für die boxplots
      tabItem(
        tabName = "BoxplotsWithDots",
        plotOutput("BoxplotsWithDots"),
        verbatimTextOutput("TextColorTableBoxplot"),
        DTOutput("SelectionGroupBoxplot")
      ),
      tabItem(
        tabName = "BarplotsWithDots",
        plotOutput("BarplotsWithDots"),
        verbatimTextOutput("TextColorTableBarplot"),
        DTOutput("SelectionGroupBarplot")
      )
    ),
  
    # CSS-Datei für Style
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    )
    
  )
)