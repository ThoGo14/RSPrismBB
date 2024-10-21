server <- function(input, output, session) {
  # Verwende eine reaktive Funktion, um die hochgeladene Datei einzulesen
  DataTable <- reactive({
    req(input$DataTable) # Sicherstellen, dass die Datei existiert
    TempDF <- read.xlsx(input$DataTable$datapath, colNames = FALSE) # Datei lesen
    
    if (input$InpDat) {
      # Transponiere und setze die erste Zeile als Spaltennamen
      TempDF <- TempDF %>% t()
    }
    
    colnames(TempDF) <- make.names(TempDF[1, ], unique = TRUE)  # Erste Zeile als Spaltennamen
    TempDF <- TempDF[-1, ]           # Entferne die erste Zeile aus den Daten
    
    # Konvertiere sicherheitshalber zu einem DataFrame, falls das noch nicht geschehen ist
    TempDF <- as.data.frame(TempDF)
    
    colcut <- as.numeric(input$colcut)
    
    # Konvertiere die relevanten Spalten zu numerischen Werten
    if (colcut + 1 == ncol(TempDF)) {
      TempDF[, (colcut + 1):ncol(TempDF)] <-
        lapply(TempDF[, (colcut + 1):ncol(TempDF)], as.numeric) %>% unlist()
    } else {
      TempDF[, (colcut + 1):ncol(TempDF)] <-
        lapply(TempDF[, (colcut + 1):ncol(TempDF)], as.numeric)
    }
    
    
    return(TempDF)
  })
  
  # Update des selectInput basierend auf den Spaltennamen von DataTable
  
  observe({
    req(DataTable())
    updateSelectInput(session, "dotid", choices = colnames(DataTable()[c(1:input$colcut)]))
  })
  
  observe({
    req(DataTable())
    updateSelectInput(session, "groupnameis", choices = colnames(DataTable()[c(1:input$colcut)]))
  })
  
  observe({
    req(DataTable())
    updateSelectInput(session, "DataY", choices = colnames(DataTable()[-c(1:input$colcut)]))
  })
  
  
  # Render die Tabelle nur, wenn DataTable verfügbar ist
  output$data <- renderDT({
    req(DataTable()) # Warten, bis die Datei geladen ist
    ## round numeric data
    TempDF <- data.table(DataTable()) %>%
      mutate_if(is.numeric, ~ round(., 4))
  })
  
  # ---------------------------------------- PlotsWithDots-----------------------------------------------
  
  # Erstelle eine reaktive Tabelle für die Gruppen
  SelectionGroup <- reactiveVal()
  
  # Beobachte Änderungen an der Tabelle
  observe({
    req(input$SelectionGroup_cell_edit) # Sicherstellen, dass die Tabelle bearbeitet wurde
    # select new and old input
    new_data <- input$SelectionGroup_cell_edit
    old_data <- SelectionGroup()
    
    # Ändere die Farbe in der Tabelle
    old_data[new_data$row, "Color"] <- new_data$value
    SelectionGroup(old_data)
  })
  
  # Render die Tabelle
  output$SelectionGroup <- renderDT({
    req(DataTable())
    colourcount <- length(unique(DataTable()[[input$dotid]]))
    
    TempDF <- data.frame("GroupID" = unique(DataTable()[[input$dotid]]),
                         "Color" = getPalette(colourcount))
    SelectionGroup(TempDF) # Setze die reaktive Tabelle
    return(TempDF)
  },
  editable = list(
    target = "row",
    disable = list(columns = c(0))), # Erste Spalte mit Gruppennamen darf nicht bearbeitet werden
  rownames = FALSE,
  selection = "none", # keine Zeilenauswahl möglich
  options = list(dom = 't') # zeige nur die Tabelle an
  )
  
  # Render den Plot
  output$PlotsWithDots <- renderPlot({
    req(SelectionGroup())  # Sicherstellen, dass die Tabelle vorhanden ist
    p <- plotInput()
    plot(p)
  })
  
  
  plotInput <- reactive({
    req(SelectionGroup())  # Sicherstellen, dass die Tabelle vorhanden ist
    
    p <- DataTable() %>%
      ggplot(aes(x = .data[[input$groupnameis]], y = .data[[input$DataY]])) +
      geom_boxplot(outlier.color = NA) +
      geom_beeswarm(
        aes(colour = as.factor(.data[[input$dotid]])),
        shape = 21,
        fill = "black",
        cex = 1.2,
        size = 2
      ) +
      theme_classic() +
      labs(title = input$DataY,
           y = "y-AXIS",
           x = NULL) +
      scale_colour_manual(
        name = input$groupnameis,
        values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
      ) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5)
      )
    return(p)
  })
  
  output$TextColorTable <- renderText(
    paste0("INFO\n",
           "Die nachfolgende Tabelle wird für die Farben im Plot verwendet. Es sind 9 Farben vordefiniert ",
           "(RColorBrewer --> Set1).\n", 
           "Wenn eine andere Farbe gewünscht ist: Doppelklick auf die Zelle ",
           "und eine neue Farbe als Name (alles klein geschrieben)\n", 
           "oder als HEX-Code eintragen --> mit 'STRG + ENTER' bestätigen.\n",
           "Wenn der Fehler '[object Object]' angezeigt wird, ist vermutlich die Farbe falsch definiert.")
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$Filename, ".png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = plotInput(),
        device = "png",
        width = 25,
        height = 20,
        units = "cm",
        dpi = 200
      )
    }
  )
}