server <- function(input, output, session) {
  # ---------------------------------------- Data Input -----------------------------------------------


  FunctionColcutNA <- function(df) {
    TempDF <- df

    TempDF[1, ] <- sapply(TempDF[1, ], \(x) ifelse(is.na(x), "NA", x))
    colnames(TempDF) <- make.unique(as.character(TempDF[1, ])) # Erste Zeile als Spaltennamen
    TempDF <- TempDF[-1, ] # Entferne die erste Zeile aus den Daten

    # Konvertiere sicherheitshalber zu einem DataFrame, falls das noch nicht geschehen ist
    TempDF <- as.data.frame(TempDF)

    req(input$colcut)
    colcut <- input$colcut %>% abs()


    # Konvertiere die relevanten Spalten zu numerischen Werten
    if (colcut + 1 == ncol(TempDF)) {
      TempDF[, (colcut + 1):ncol(TempDF)] <-
        lapply(TempDF[, (colcut + 1):ncol(TempDF)], as.numeric) %>% unlist()
    } else if (colcut >= ncol(TempDF)) {
      TempDF <- TempDF
    } else {
      TempDF[, (colcut + 1):ncol(TempDF)] <-
        lapply(TempDF[, (colcut + 1):ncol(TempDF)], as.numeric)
    }
    return(TempDF)
  }

  # Verwende eine reaktive Funktion, um die hochgeladene Datei einzulesen
  DataTable <- reactive({
    req(
      input$DataTable,
      input$colcut
    ) # Sicherstellen, dass die Datei existiert
    TempDF <- read.xlsx(input$DataTable$datapath, colNames = FALSE) # Datei lesen

    if (input$InpDat) {
      # Transponiere und setze die erste Zeile als Spaltennamen
      TempDF <- t(TempDF)
    }

    TempDF <- FunctionColcutNA(TempDF)

    return(TempDF)
  })

  # Update des selectInput basierend auf den Spaltennamen von DataTable
  observe({
    req(DataTable()) # Warten, bis die Datei geladen ist
    req(input$colcut)

    colcut <- input$colcut %>% abs()
    if (colcut > ncol(DataTable())) {
      colcut <- ncol(DataTable())
    }

    All_colname <- colnames(DataTable())
    Colname_before_colcut <- All_colname[1:colcut]
    Colname_after_colcut <- All_colname[(colcut + 1):length(All_colname)]

    # Aktualisiere die Auswahl für dotid
    updateSelectizeInput(session, "dotid", choices = Colname_before_colcut, server = TRUE)
    # Aktualisiere die Auswahl für DataX
    updateSelectizeInput(session, "groupnameis", choices = Colname_before_colcut, server = TRUE)
    # Aktualisiere die Auswahl für DataY
    updateSelectizeInput(session, "DataY", choices = Colname_after_colcut, server = TRUE)
  })


  # Render die Tabelle nur, wenn DataTable verfügbar ist
  output$data <- renderDT({
    req(DataTable()) # Warten, bis die Datei geladen ist

    max_row <- ifelse(nrow(DataTable()) > 10, 10, nrow(DataTable()))
    max_col <- ifelse(ncol(DataTable()) > 10, 10, ncol(DataTable()))

    ## round numeric data
    TempDF <- data.table(DataTable()[1:max_row, 1:max_col]) %>%
      mutate_if(is.numeric, ~ round(., 4))

    datatable(TempDF, options = list(server = TRUE, pageLength = 25, scrollX = TRUE, dom = "t"))
  })



  # ---------------------------------------- Update Input -----------------------------------------------
  observe({
    req(DataTable())

    # Aktualisiere die Auswahl für selected_groups
    updateCheckboxGroupInput(
      session,
      inputId = "selected_groups",
      choices = unique(DataTable()[[input$groupnameis]]),
      selected = unique(DataTable()[[input$groupnameis]])
    )
  })

  observeEvent(input$selected_groups, {
    req(DataTable())
    # Filtere die Gruppen basierend auf den ausgewählten Gruppen
    selected_items <- unique(DataTable()[[input$groupnameis]])[unique(DataTable()[[input$groupnameis]]) %in% input$selected_groups]

    # Aktualisiere die Reihenfolge für group_order basierend auf den ausgewählten Gruppen
    updateOrderInput(session,
      inputId = "group_order",
      items = selected_items,
      item_class = "info"
    )
  })


  # ---------------------------------------- Table for color Boxplot --------------------------------------------
  # Erstelle eine reaktive Tabelle für die Gruppen
  SelectionGroupBoxplot <- reactiveVal()
  
  # Beobachte Änderungen an der Tabelle
  observe({
    req(input$SelectionGroupBoxplot_cell_edit) # Sicherstellen, dass die Tabelle bearbeitet wurde
    # select new and old input
    new_data <- input$SelectionGroupBoxplot_cell_edit
    old_data <- SelectionGroupBoxplot()
    
    # Ändere die Farbe in der Tabelle
    old_data[new_data$row, "Color"] <- new_data$value
    SelectionGroupBoxplot(old_data)
  })
  
  # Render die Tabelle
  output$SelectionGroupBoxplot <- renderDT(
    {
      req(DataTable())
      
      # Wie viele Farben benötigt
      colourcount <- length(unique(DataTable()[[input$dotid]]))
      # Tabelle für default farben
      TempDF <- data.frame(
        "GroupID" = sort(unique(DataTable()[[input$dotid]])),
        "Color" = getPalette(colourcount)
      )
      SelectionGroupBoxplot(TempDF) # Setze die reaktive Tabelle
      return(TempDF)
    },
    # Erste Spalte mit Gruppennamen darf nicht bearbeitet werden
    editable = list(target = "row", disable = list(columns = c(0))),
    # keine Zeilenauswahl möglich
    rownames = FALSE,
    selection = "none",
    # zeige nur die Tabelle an
    options = list(dom = "t", pageLength = -1)
  )
  
  # ---------------------------------------- Table for color Barplot --------------------------------------------
  # Erstelle eine reaktive Tabelle für die Gruppen
  SelectionGroupBarplot <- reactiveVal()
  
  # Beobachte Änderungen an der Tabelle
  observe({
    req(input$SelectionGroupBarplot_cell_edit) # Sicherstellen, dass die Tabelle bearbeitet wurde
    # select new and old input
    new_data <- input$SelectionGroupBarplot_cell_edit
    old_data <- SelectionGroupBarplot()
    
    # Ändere die Farbe in der Tabelle
    old_data[new_data$row, "Color"] <- new_data$value
    SelectionGroupBarplot(old_data)
  })
  
  # Render die Tabelle
  output$SelectionGroupBarplot <- renderDT(
    {
      req(DataTable())
      
      # Wie viele Farben benötigt
      colourcount <- length(unique(DataTable()[[input$dotid]]))
      # Tabelle für default farben
      TempDF <- data.frame(
        "GroupID" = unique(DataTable()[[input$dotid]]),
        "Color" = getPalette(colourcount)
      )
      SelectionGroupBarplot(TempDF) # Setze die reaktive Tabelle
      return(TempDF)
    },
    # Erste Spalte mit Gruppennamen darf nicht bearbeitet werden
    editable = list(target = "row", disable = list(columns = c(0))),
    # keine Zeilenauswahl möglich
    rownames = FALSE,
    selection = "none",
    # zeige nur die Tabelle an
    options = list(dom = "t")
  )
  
  # ---------------------------------------- BoxplotsWithDots-----------------------------------------------
  # Render den Plot
  output$BoxplotsWithDots <- renderPlot({
    req(SelectionGroupBoxplot()) # Sicherstellen, dass die Tabelle vorhanden ist
    p <- BoxplotInput()
    plot(p)
  })
  
  # plot erstellen
  BoxplotInput <- reactive({
    req(
      input$selected_groups,
      input$group_order,
      SelectionGroupBoxplot()
    ) # Sicherstellen, dass die Tabelle vorhanden ist
    
    # daten Filtern, welche im Sidepanel gewählt werden
    filtered_data <- DataTable() %>%
      filter(.data[[input$groupnameis]] %in% input$selected_groups)
    
    # sortiere die Daten wie im Sidepanel + Faktorisiere
    ordered_data <- filtered_data %>%
      mutate(!!input$groupnameis := factor(.data[[input$groupnameis]], levels = input$group_order))
    
    # Setze y_min and y_max mit defaults
    y_min <- ifelse(is.numeric(input$yMin) & !is.na(input$yMin), input$yMin, NA)
    y_max <- ifelse(is.numeric(input$yMax) & !is.na(input$yMax), input$yMax, NA)
    
    # Schauen ob y_min auch kleiner ist ist y_max, ansonsten tauschen
    if (!is.na(y_min) & !is.na(y_max) & y_min > y_max) {
      temp <- y_min
      y_min <- y_max
      y_max <- temp
    }
    
    
    # erste plot mit ggplot
    p <- ordered_data %>%
      ggplot(aes(x = .data[[input$groupnameis]], y = .data[[input$DataY]])) +
      geom_boxplot(outlier.color = NA) +
      geom_beeswarm(
        aes(colour = as.factor(.data[[input$dotid]])),
        shape = 21,
        fill = "black",
        cex = 1.2,
        size = input$PointSize,
        stroke = input$PointSize / 3
      ) +
      theme_classic() +
      labs(
        title = input$DataY,
        y = input$LabelY,
        x = input$LabelX
      ) +
      scale_colour_manual(
        name = input$dotid,
        values = setNames(SelectionGroupBoxplot()[["Color"]], SelectionGroupBoxplot()[["GroupID"]]) 
      ) +
      {
        if (is.numeric(y_min) | is.numeric(y_max)) {
          ylim(c(y_min, y_max))
        }
      } +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5, face = ifelse(input$TitelKursiv, "italic", "plain"))
      )
    return(p)
  })
  # ---------------------------------------- BarplotsWithDots ----------------------------------------------
  # Render den Plot
  output$BarplotsWithDots <- renderPlot({
    req(SelectionGroupBarplot()) # Sicherstellen, dass die Tabelle vorhanden ist
    p <- BarplotInput()
    plot(p)
  })
  
  # plot erstellen
  BarplotInput <- reactive({
    req(
      input$selected_groups,
      input$group_order,
      SelectionGroupBarplot()
    ) # Sicherstellen, dass die Tabelle vorhanden ist
    
    # daten Filtern, welche im Sidepanel gewählt werden
    filtered_data <- DataTable() %>%
      filter(.data[[input$groupnameis]] %in% input$selected_groups)
    
    # sortiere die Daten wie im Sidepanel + Faktorisiere
    ordered_data <- filtered_data %>%
      mutate(!!input$groupnameis := factor(.data[[input$groupnameis]], levels = input$group_order))
    
    Statistics <- ordered_data %>%
      group_by(.data[[input$groupnameis]]) %>%
      summarise(
        mean = mean(.data[[input$DataY]], na.rm = TRUE),
        sd = sd_n(.data[[input$DataY]]),
        median = median(.data[[input$DataY]], na.rm = TRUE),
        min = min(.data[[input$DataY]], na.rm = TRUE),
        max = max(.data[[input$DataY]], na.rm = TRUE),
        range = max - min,
      ) %>%
      as.data.frame() %>%
      mutate(!!input$groupnameis := factor(.data[[input$groupnameis]], levels = input$group_order))
    
    # Setze y_min and y_max mit defaults
    y_min <- ifelse(is.numeric(input$yMin) & !is.na(input$yMin), input$yMin, NA)
    y_max <- ifelse(is.numeric(input$yMax) & !is.na(input$yMax), input$yMax, NA)
    
    # Schauen ob y_min auch kleiner ist ist y_max, ansonsten tauschen
    if (!is.na(y_min) & !is.na(y_max) & y_min > y_max) {
      temp <- y_min
      y_min <- y_max
      y_max <- temp
    }
    
    # erste plot mit ggplot
    p <- Statistics %>%
      ggplot() +
      geom_errorbar(
        aes(
          ymin = mean - (mean * 0.1),
          ymax = mean + sd,
          x = .data[[input$groupnameis]]
        ),
        width = 0.5,
        colour = "grey"
      ) +
      geom_bar(
        aes(
          x = .data[[input$groupnameis]],
          y = mean
        ),
        stat = "identity"
      ) +
      geom_beeswarm(
        data = ordered_data,
        aes(
          x = .data[[input$groupnameis]],
          y = .data[[input$DataY]],
          colour = as.factor(.data[[input$dotid]])
        ),
        shape = 21,
        fill = "black",
        cex = 1.2,
        size = input$PointSize,
        stroke = input$PointSize / 3
      ) +
      theme_classic() +
      labs(
        title = input$DataY,
        y = input$LabelY,
        x = input$LabelX
      ) +
      scale_colour_manual(
        name = input$dotid,
        values = setNames(SelectionGroupBarplot()[["Color"]], SelectionGroupBarplot()[["GroupID"]])
      ) +
      {
        if (is.numeric(y_min) | is.numeric(y_max)) {
          ylim(c(y_min, y_max))
        }
      } +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5, face = ifelse(input$TitelKursiv, "italic", "plain"))
      )
    return(p)
  })
  
  # ---------------------------------------- Erklärung und Text -----------------------------------------------------
  TextColorTable <-
    paste0(
      "INFO\n",
      "Die nachfolgende Tabelle wird für die Farben im Plot verwendet. Es sind 9 Farben vordefiniert ",
      "(RColorBrewer --> Set1).\n",
      "Wenn eine andere Farbe gewünscht ist: Doppelklick auf die Zelle ",
      "und eine neue Farbe als Name (alles klein geschrieben)\n",
      "oder als HEX-Code eintragen --> mit 'STRG + ENTER' bestätigen.\n",
      "Wenn der Fehler '[object Object]' angezeigt wird, ist vermutlich die Farbe falsch definiert."
    )

  # Text als erklärung
  output$TextColorTableBoxplot <- renderText({
    req(DataTable())
    TextColorTable
  })

  # Text als erklärung
  output$TextColorTableBarplot <- renderText({
    req(DataTable())
    TextColorTable
  })

  TextTableOutput <-
    paste0(
      "VORSCHAU:\n",
      "Die ersten 10 Zeilen und 10 Spalten der Input-Tabelle werden angezeigt."
    )

  output$TextTableOutput <- renderText({
    req(DataTable())
    TextTableOutput
  })

  output$sessionInfo <- renderPrint({
    sessionInfo() # Gibt die Session-Info direkt als Ausgabe zurück
  })
  # ---------------------------------------- Download -----------------------------------------------------
  # Für Download Button
  output$downloadPlot <- downloadHandler(
    # Dateiname erstellen
    filename = function() {
      # Stelle sicher, dass ein Dateiname eingegeben wurde
      if (is.null(input$Filename) || input$Filename == "") {
        return(paste0("plot.", input$ImageFiletype))
      }
      paste0(input$Filename, ".", input$ImageFiletype)
    },
    # plot speichern
    content = function(file) {
      # Überprüfe den aktiven Tab
      selected_plot <- if (input$tabs == "BoxplotsWithDots") {
        BoxplotInput() # Für Boxplots
      } else if (input$tabs == "BarplotsWithDots") {
        BarplotInput() # Für Barplots
      }

      if (input$ImageFiletype == "png") {
        # Speichern als PNG
        ggsave(
          filename = file,
          plot = selected_plot,
          device = "png",
          width = as.numeric(input$ImageWidth),
          height = as.numeric(input$ImageHeight),
          units = "cm",
          dpi = input$ImageDPI
        )
      } else if (input$ImageFiletype == "pdf") {
        # Speichern als PDF
        ggsave(
          filename = file,
          plot = selected_plot,
          device = "pdf",
          width = as.numeric(input$ImageWidth),
          height = as.numeric(input$ImageHeight),
          units = "cm",
          dpi = input$ImageDPI
        )
      }
    }
  )

  # ---------------------------------------- Changelog -----------------------------------------------------
  
  observeEvent(input$show_changelog, {
    changelog_path <- "changelog.txt"
    
    if (!file.exists(changelog_path)) {
      changelog_html <- "<b>Kein Changelog gefunden.</b>"
    } else {
      changelog_content <- readLines(changelog_path, warn = FALSE)
      
      if (length(changelog_content) == 0) {
        changelog_html <- "<b>Changelog ist leer.</b>"
      } else {
        changelog_html <- paste(changelog_content, collapse = "<br>")
      }
    }
    
    showModal(modalDialog(
      title = strong("Updateverlauf"),  # Titel direkt fett machen,
      HTML(changelog_html),
      easyClose = TRUE,
      footer = modalButton("Schließen")
    ))
  })
  
  
  # ensure that it will stop the websocket server started by shiny::runApp() and the underlying R process when the
  # browser window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
}
