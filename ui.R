dashboardPage(
  # ------------------- Header -------------------------------------------------------------------------#
  dashboardHeader(
    title = textOutput("app_title_header", inline = TRUE),
    tags$li(
      class = "dropdown",
      style = "padding: 15px; color: white;",
      aktuelle_version # Zeigt die neueste Version aus der Datei an
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
      id = "tabs", # Tab-Panel ID hinzufügen, aktiver tab in input$tabs
      # Tab für Daten eingabe
      menuItem(
        text = translator$tl("data_input"),
        expandedName = "dataTabExpand",
        icon = icon("arrow-up-from-bracket"),
        menuItem(
          text = translator$tl("data_table"),
          tabName = "data",
          icon = icon("table")
        ),
        # Datei upload
        fileInput(
          "DataTable",
          translator$tl("upload_file"),
          multiple = FALSE,
          accept = c(".xlsx"),
          placeholder = translator$tl("max_size")
        ),
        # Da der Abstand viel zu groß ist, wird er hiermit reduziert
        div(style = "margin-top: -30px"),
        # array daten oder nicht
        checkboxInput(
          "InpDat",
          label = translator$tl("array_data_question"),
          value = FALSE
        ),

        # Ab wann sind in der Tabelle Nummerische Daten (für konvertierung)
        numericInput("colcut", label = translator$tl("column_before_numeric"), value = 0, min = 0),
        # Am anfang direkt ausgeklappt
        startExpanded = TRUE
      ),

      # Tab für Boxplot
      menuItem(
        text = translator$tl("plots"),
        expandedName = "PlotsExpand",
        icon = icon("image"),
        menuItem(
          text = translator$tl("plots_create"),
          tabName = "Plots_erstellen",
          icon = icon("chart-column")
        ),
        # Gruppe für Punktfarbe
        selectInput("dotid", label = translator$tl("dot_color_question"), choices = NULL, selectize = TRUE),
        # Spaltenname für Gruppierung
        selectInput("groupnameis", label = translator$tl("group_column_question"), choices = NULL, selectize = TRUE),
        # Plotvariable
        selectInput("DataY", label = translator$tl("y_axis_variable"), choices = NULL, selectize = TRUE),
        textInput("LabelY", label = translator$tl("y_axis_label"), value = translator$tl("y_axis")),
        textInput("LabelX", label = translator$tl("x_axis_label"), value = NULL),
        checkboxInput("TitelKursiv", label = translator$tl("title_italic"), value = FALSE),
        checkboxInput("LegendenTitel", label = translator$tl("legend_title_hide"), value = FALSE),
        checkboxInput("BoxColor", label = translator$tl("boxplot_color"), value = FALSE),
        checkboxInput("InvertPoint", label = translator$tl("invert_point_color"), value = FALSE),
        sliderInput("PointSize", label = translator$tl("point_size"), min = 1, max = 10, step = 0.5, value = 2),
        fluidRow(
          style = "margin-left: 0px; margin-right: 0px;",
          column(6, numericInput("yMin", translator$tl("y_axis_min"), value = NA, min = 0)),
          column(6, numericInput("yMax", translator$tl("y_axis_max"), value = NA, min = 0))
        ),
        
        # Zwischenüberschrift
        tags$hr(style = "margin-top: 20px; margin-bottom: 10px;"),
        h3(textOutput("download_options_header", inline = TRUE), style = "margin-left: 15px; margin-right: 5px;"),
        # Dateiname
        textInput("Filename", label = translator$tl("image_filename"), placeholder = translator$tl("dateiname")),
        # Bildbreite-, höhe und -auflösung
        numericInput("ImageWidth", label = translator$tl("image_width"), value = 20),
        numericInput("ImageHeight", label = translator$tl("image_height"), value = 20),
        numericInput("ImageDPI", label = translator$tl("image_dpi"), value = 200),
        # Bild speichern als png oder PDF
        radioButtons(
          "ImageFiletype",
          label = translator$tl("image_format"),
          choices = c("png (in cm)" = "png", "pdf (in cm)" = "pdf", "svg (in cm)" = "svg"),
        ),
        # Downloadbutton, style definiert die position
        downloadButton("downloadPlot", label = translator$tl("download_button"), style = "display: block; margin-left: 50px; margin-right: 50px;")
      ),
      menuItem(
        text = translator$tl("session_info"),
        tabName = "SessionInfo",
        icon = icon("circle-info")
      )
    ),
    actionButton("show_changelog", translator$tl("changelog_button"), icon = icon("circle-info"))
  ),

  # ------------------- Body ----------------------------------------------------------------------------#
  dashboardBody( # tabName in sidebar and body needs to match in name (but not in order)
    tabItems(
      # tab für die Tabelle
      tabItem(
        tabName = "data",
        # verbatimTextOutput("TextTableOutput"),
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
        DTOutput("data")
      ),
      tabItem(
        tabName = "Plots_erstellen", # dein neuer gemeinsamer Tab
        tabsetPanel(
          id = "plot_tabs", # wichtig für server-seitige Logik
          type = "tabs",
          tabPanel(translator$tl("tab_boxplot"), plotOutput("BoxplotsWithDots", height = "auto", width = "auto")),
          tabPanel(translator$tl("tab_barplot"), plotOutput("BarplotsWithDots", height = "auto", width = "auto"))
        ),
        # ab hier alles gemeinsam für beide

        # Abstand zwischen Plot und Optionen
        tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
        
        # Informationstext zu den Plot Optionen
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
      # tab für die session info
      tabItem(
        tabName = "SessionInfo",
        verbatimTextOutput("sessionInfo")
      )
    ),

    # CSS-Datei für Style
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Enable shinyjs for dynamic label updates
    useShinyjs()
  )
)
