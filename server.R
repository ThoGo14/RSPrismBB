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
    
    # Falls Datei leer ist, gibt es kein Absturz
    validate(
      need(nrow(TempDF) > 1, "Die Datei ist leer oder fehlerhaft.")
    )
    
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
    updateCheckboxGroupButtons(
      session,
      inputId = "selected_groups",
      choices = unique(DataTable()[[input$groupnameis]]),
      selected = unique(DataTable()[[input$groupnameis]]),
      status = "primary",        # für Farbe (z.B. "primary" = blau)
      checkIcon = list(
        yes = icon("check")      # für sichtbaren Haken
      )
    )
  })

  observeEvent(input$selected_groups, {
    req(DataTable())
    # Filtere die Gruppen basierend auf den ausgewählten Gruppen
    selected_items <- DataTable() %>% 
      filter(.data[[input$groupnameis]] %in% input$selected_groups) %>%
      pull(.data[[input$groupnameis]]) %>%
      unique()
      
      # unique(DataTable()[[input$groupnameis]])[unique(DataTable()[[input$groupnameis]]) %in% input$selected_groups]

    # Aktualisiere die Reihenfolge für group_order basierend auf den ausgewählten Gruppen
    updateOrderInput(session,
      inputId = "group_order",
      items = selected_items,
      item_class = "info"
    )
  })
  
  
  # ---------------------------------------- Color Picker for Groups --------------------------------------------
  # Erstelle eine reaktive Tabelle für die Gruppen
  SelectionGroup <- reactiveVal()
  
  # Speichere die Farbzuordnungen persistent
  ColorStorage <- reactiveVal(data.frame(GroupID = character(), Color = character(), stringsAsFactors = FALSE))
  
  # Dynamisch Color Picker generieren basierend auf group_order
  output$ColorPickerUI <- renderUI({
    req(DataTable(), input$selected_groups, input$group_order, input$dotid)
    
    # Hole gespeicherte Farben oder erstelle Defaults
    # WICHTIG: isolate() verhindert Re-Rendering wenn sich Farben ändern
    stored_colors <- isolate(ColorStorage())
    all_groups <- sort(unique(DataTable()[[input$dotid]]))
    
    # Prüfe ob die gespeicherten Farben zu den aktuellen Gruppen passen
    stored_groups <- stored_colors$GroupID
    groups_match <- length(stored_groups) == length(all_groups) && all(all_groups %in% stored_groups)
    
    # Wenn noch keine Farben gespeichert sind ODER die Gruppen nicht passen, erstelle neue Defaults
    if (nrow(stored_colors) == 0 || !groups_match) {
      colourcount <- length(all_groups)
      default_colors <- getPalette(colourcount)
      stored_colors <- data.frame(
        GroupID = all_groups,
        Color = default_colors,
        stringsAsFactors = FALSE
      )
      isolate(ColorStorage(stored_colors))
    }
    
    # Bestimme welche Gruppen angezeigt werden sollen
    if (input$dotid == input$groupnameis) {
      # Verwende die Reihenfolge von group_order (nur ausgewählte Gruppen)
      groups <- input$group_order
    } else {
      # Zeige alle unique Werte von dotid (sortiert)
      groups <- all_groups
    }
    
    # Erstelle Color Picker nur für die ausgewählten Gruppen in der richtigen Reihenfolge
    color_pickers <- map(seq_along(groups), function(i) {
      group <- groups[i]
      # Finde die gespeicherte Farbe für diese Gruppe
      color_idx <- which(stored_colors$GroupID == group)
      current_color <- if (length(color_idx) > 0) stored_colors$Color[color_idx] else "#FF0000"
      
      column(
        width = 3,
        colourInput(
          inputId = paste0("color_", make.names(group)),
          label = as.character(group),
          value = current_color,
          showColour = "both",
          allowedCols = NULL
        )
      )
    })
    # Zeige die Color Picker in einem Grid an
    fluidRow(color_pickers)
  })
  
  # Sammle alle ausgewählten Farben (für alle Gruppen, nicht nur selected)
  observe({
    req(DataTable())
    
    all_groups <- sort(unique(DataTable()[[input$dotid]]))
    stored_colors <- ColorStorage()
    
    # Warte bis alle Inputs vorhanden sind
    all_inputs_exist <- all(sapply(all_groups, function(g) {
      !is.null(input[[paste0("color_", make.names(g))]])
    }))
    
    if (all_inputs_exist) {
      # Aktualisiere die Farben für alle Gruppen
      colors <- sapply(all_groups, function(g) {
        color_input <- input[[paste0("color_", make.names(g))]]
        if (!is.null(color_input)) {
          return(color_input)
        } else {
          # Falls kein Input vorhanden, behalte die gespeicherte Farbe
          idx <- which(stored_colors$GroupID == g)
          if (length(idx) > 0) return(stored_colors$Color[idx])
          return("#FF0000")
        }
      })
      
      TempDF <- data.frame(
        "GroupID" = all_groups,
        "Color" = colors,
        stringsAsFactors = FALSE
      )
      
      ColorStorage(TempDF)
      SelectionGroup(TempDF)
    }
  })
  
  # ---------------------------------------- BoxplotsWithDots-----------------------------------------------
  # Render den Plot
  output$BoxplotsWithDots <- renderPlot({
    req(SelectionGroup(), input$selected_groups, input$group_order)
    # Stelle sicher, dass SelectionGroup Daten hat
    validate(
      need(nrow(SelectionGroup()) > 0, "ColorPicker wird geladen")
    )
    p <- BoxplotInput()
    plot(p)
  },
  width = function() {
    w <- input$ImageWidth
    if (!is.null(w) && !is.na(w)) {
      w_num <- as.numeric(w)
      if (!is.na(w_num) && w_num > 0) {
        return(w_num * 37.795)
      }
    }
    return(800)
  },
  height = function() {
    h <- input$ImageHeight
    if (!is.null(h) && !is.na(h)) {
      h_num <- as.numeric(h)
      if (!is.na(h_num) && h_num > 0) {
        return(h_num * 37.795)
      }
    }
    return(600)
  },
  res = 96
  )
  
  # plot erstellen
  BoxplotInput <- reactive({
    req(
      input$selected_groups,
      input$group_order,
      SelectionGroup()
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
      {
        if (input$BoxColor && input$dotid == input$groupnameis) {
          geom_boxplot(aes(fill = as.factor(.data[[input$dotid]])), alpha = 0.3, outlier.shape = NA)
        } else {
          geom_boxplot(outlier.shape = NA)
        }
      } +
      {
        if (input$InvertPoint) {
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
          )
        } else {
          geom_beeswarm(
            data = ordered_data,
            aes(
              x = .data[[input$groupnameis]],
              y = .data[[input$DataY]],
              fill = as.factor(.data[[input$dotid]])
            ),
            shape = 21,
            color = "black",
            cex = 1.2,
            size = input$PointSize,
            stroke = input$PointSize / 3
          )
        }
      } +
      theme_classic() +
      labs(
        title = input$DataY,
        y = input$LabelY,
        x = input$LabelX
      ) +
      {
        if (input$InvertPoint && input$BoxColor && input$dotid == input$groupnameis) {
          # Beide aktiviert
          list(
            scale_colour_manual(
              name = input$dotid,
              values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]]),
            ),
            scale_fill_manual(
              name = input$dotid,
              values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
            )
          )
        } else if (input$InvertPoint) {
          # Nur InvertPoint
          scale_colour_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        } else {
          # Normal
          scale_fill_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        }
      } +
      {
        if (is.numeric(y_min) | is.numeric(y_max)) {
          ylim(c(y_min, y_max))
        }
      } +
      theme(
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#000001"),
        legend.text = element_text(size = 16),
        legend.title = if (!input$LegendenTitel) {
          element_text(size = 16)
        } else {
          element_blank()
        },
        plot.title = element_text(size = 16, hjust = 0.5, face = ifelse(input$TitelKursiv, "italic", "plain"))
      )
    return(p)
  })
  # ---------------------------------------- BarplotsWithDots ----------------------------------------------
  # Render den Plot
  output$BarplotsWithDots <- renderPlot({
    req(SelectionGroup(), input$selected_groups, input$group_order)
    # Stelle sicher, dass SelectionGroup Daten hat
    validate(
      need(nrow(SelectionGroup()) > 0, "ColorPicker wird geladen")
    )
    p <- BarplotInput()
    plot(p)
  },
  width = function() {
    w <- input$ImageWidth
    if (!is.null(w) && !is.na(w)) {
      w_num <- as.numeric(w)
      if (!is.na(w_num) && w_num > 0) {
        return(w_num * 37.795)
      }
    }
    return(800)
  },
  height = function() {
    h <- input$ImageHeight
    if (!is.null(h) && !is.na(h)) {
      h_num <- as.numeric(h)
      if (!is.na(h_num) && h_num > 0) {
        return(h_num * 37.795)
      }
    }
    return(600)
  },
  res = 96
  )
  
  # plot erstellen
  BarplotInput <- reactive({
    req(
      input$selected_groups,
      input$group_order,
      # SelectionGroupBarplot(),
      SelectionGroup()
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
      {
        if (input$InvertPoint) {
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
          )
        } else {
          geom_beeswarm(
            data = ordered_data,
            aes(
              x = .data[[input$groupnameis]],
              y = .data[[input$DataY]],
              fill = as.factor(.data[[input$dotid]])
            ),
            shape = 21,
            color = "black",
            cex = 1.2,
            size = input$PointSize,
            stroke = input$PointSize / 3
          )
        }
      } +
      theme_classic() +
      labs(
        title = input$DataY,
        y = input$LabelY,
        x = input$LabelX
      ) +
      {
        if (input$InvertPoint) {
          # Nur InvertPoint
          scale_colour_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        } else {
          # Normal
          scale_fill_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        }
      } +
      {
        if (is.numeric(y_min) | is.numeric(y_max)) {
          ylim(c(y_min, y_max))
        }
      } +
      theme(
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#000001"),
        legend.text = element_text(size = 16),
        legend.title = if (!input$LegendenTitel) {
          element_text(size = 16)
        } else {
          element_blank()
        },
        plot.title = element_text(size = 16, hjust = 0.5, face = ifelse(input$TitelKursiv, "italic", "plain"))
      )
    return(p)
  })
  
  # ---------------------------------------- Erklärung und Text -----------------------------------------------------

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
      selected_plot <- if (input$plot_tabs == "Boxplot") {
        BoxplotInput()
      } else if (input$plot_tabs == "Barplot") {
        BarplotInput()
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
          device = cairo_pdf,
          width = as.numeric(input$ImageWidth),
          height = as.numeric(input$ImageHeight),
          units = "cm",
          dpi = input$ImageDPI
        )
      } else if (input$ImageFiletype == "svg") {
        # Speichern als SVG
        
        TempFile <- tempfile(fileext = ".svg")
        
        ## try saving with ggsave otherwise use svg()
        tryCatch( 
          {
            ggsave(
              filename = TempFile,
              plot = selected_plot,
              device = "svg",
              width = as.numeric(input$ImageWidth),
              height = as.numeric(input$ImageHeight),
              units = "cm",
              dpi = input$ImageDPI
            )
          },
          error = function(e) {
            svg(
              filename = TempFile,
              width = as.numeric(input$ImageWidth) / 2.54,
              height = as.numeric(input$ImageHeight) / 2.54
            )
            print(selected_plot)
            dev.off()
            showNotification("SVG wurde mit der base svg-Funktion gespeichert.", type = "warning")
          }
        )
        
        x <- readLines(TempFile)
        x <- str_remove(x, " textLength='[0-9.]+px'")
        x <- str_remove(x, " lengthAdjust='[a-zA-Z]+'")
        writeLines(x, con = file)
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
