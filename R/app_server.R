#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @import ggplot2
#' @import dplyr
#' @importFrom shinyjs runjs
#' @importFrom shinyWidgets checkboxGroupButtons updateCheckboxGroupButtons
#' @importFrom shinyjqui orderInput
#' @importFrom colourpicker colourInput
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom data.table data.table :=
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom purrr map
#' @importFrom stringr str_remove
#' @importFrom stats median setNames
#' @importFrom utils sessionInfo
#' @noRd
app_server <- function(input, output, session) {
  # Source the original server logic
  # Initialize translator
  translator <- initialize_translator()
  
  # ---------------------------------------- Language Switching -----------------------------------------------
  
  # Reactive translator that updates when language changes
  i18n <- reactive({
    req(input$selected_language)
    translator$set_translation_language(input$selected_language)
    translator
  })
  
  # Helper function to translate
  tl <- function(key) {
    translations_json <- jsonlite::fromJSON(
      system.file("translations.json", package = "RSPrismBB")
    )
    translations_json[[input$selected_language %||% "de"]][[key]] %||% key
  }
  
  # Render dynamic text outputs for UI elements
  output$download_options_header <- renderText({ tl("download_options") })
  output$data_table_title_out <- renderText({ tl("data_table_title") })
  output$data_table_info_1_out <- renderText({ tl("data_table_info_1") })
  output$data_table_info_2_out <- renderText({ tl("data_table_info_2") })
  output$data_table_info_3_out <- renderText({ tl("data_table_info_3") })
  output$data_table_info_4_out <- renderText({ tl("data_table_info_4") })
  output$data_table_info_5_out <- renderText({ tl("data_table_info_5") })
  output$info_box_title_out <- renderText({ tl("info_box_title") })
  output$info_box_text_1_out <- renderText({ tl("info_box_text_1") })
  output$info_box_text_2_out <- renderText({ tl("info_box_text_2") })
  output$info_box_text_3_out <- renderText({ tl("info_box_text_3") })
  output$group_selection_title_out <- renderText({ tl("group_selection_title") })
  output$color_palette_title_out <- renderText({ tl("color_palette_title") })
  
  # Render version number (language-dependent)
  output$version_number <- renderText({
    changelog_path <- if(input$selected_language == "de") {
      system.file("changelog_de.txt", package = "RSPrismBB")
    } else if (input$selected_language == "en") {
      system.file("changelog_en.txt", package = "RSPrismBB")
    } else {
      system.file("changelog_de.txt", package = "RSPrismBB")
    }
    
    if (file.exists(changelog_path)) {
      changelog_content <- readLines(changelog_path, warn = FALSE)
      if (length(changelog_content) > 0) {
        version <- changelog_content[1]
        version <- sub(" - .*", "", version)
        version <- sub("<b>", "", version)
        version <- sub("</b>", "", version)
        return(version)
      }
    }
    return("Version 1.6.0")
  })
  
  # Dynamic UI for group selection checkbox
  output$group_selection_checkbox_ui <- renderUI({
    req(DataTable())
    shinyWidgets::checkboxGroupButtons(
      inputId = "selected_groups",
      label = tl("group_selection_label"),
      choices = unique(DataTable()[[input$groupnameis]]),
      selected = unique(DataTable()[[input$groupnameis]]),
      status = "primary",
      direction = "horizontal",
      checkIcon = list(yes = icon("check"))
    )
  })
  
  # Dynamic UI for group order
  output$group_order_ui <- renderUI({
    req(DataTable(), input$selected_groups)
    selected_items <- DataTable() %>% 
      filter(.data[[input$groupnameis]] %in% input$selected_groups) %>%
      pull(.data[[input$groupnameis]]) %>%
      unique()
    
    orderInput(
      "group_order",
      label = tl("group_order_label"),
      items = selected_items,
      class = "btn-group",
      width = "100%"
    )
  })
  
  # Update sidebar labels
  observeEvent(input$selected_language, {
    # Update Main header - multiline
    title_parts <- strsplit(tl("app_title"), ": ")[[1]]
    if (length(title_parts) == 2) {
      header_html <- sprintf(
        '<div style="text-align: center; line-height: 1.2; padding: 5px 0;"><div>%s</div><div style="font-size: 12px;">%s</div></div>',
        title_parts[1], title_parts[2]
      )
    } else {
      header_html <- tl("app_title")
    }
    shinyjs::runjs(sprintf("$('.main-header .logo').html('%s');", header_html))

    # Update Sidebar Menu Items 
    shinyjs::runjs(sprintf("$('a[data-value=\"data\"]').find('span').last().text('%s');", tl("data_table")))
    shinyjs::runjs(sprintf("$('a[data-value=\"Plots_erstellen\"]').find('span').last().text('%s');", tl("plots_create")))
    shinyjs::runjs(sprintf("$('a[data-value=\"SessionInfo\"]').find('span').last().text('%s');", tl("session_info")))
    
    # Update expandable menu headers
    shinyjs::runjs(sprintf("$('.treeview:has(a[data-value=\"data\"]) > a > span:first').text('%s');", tl("data_input")))
    shinyjs::runjs(sprintf("$('.treeview:has(a[data-value=\"Plots_erstellen\"]) > a > span:first').text('%s');", tl("plots")))

    # Update fileInput label
    shinyjs::runjs(sprintf("$('#DataTable-label').text('%s');", tl("upload_file")))

    # Update selectInput labels
    updateSelectInput(session, "dotid", label = tl("dot_color_question"))
    updateSelectInput(session, "groupnameis", label = tl("group_column_question"))
    updateSelectInput(session, "DataY", label = tl("y_axis_variable"))
    
    # Update textInput labels 
    updateTextInput(session, "LabelY", label = tl("y_axis_label"))
    updateTextInput(session, "LabelX", label = tl("x_axis_label"))
    updateTextInput(session, "Filename", label = tl("image_filename"))
    
    # Update checkboxInput labels
    updateCheckboxInput(session, "InpDat", label = tl("array_data_question"))
    updateCheckboxInput(session, "TitelKursiv", label = tl("title_italic"))
    updateCheckboxInput(session, "LegendenTitel", label = tl("legend_title_hide"))
    updateCheckboxInput(session, "BoxColor", label = tl("boxplot_color"))
    updateCheckboxInput(session, "InvertPoint", label = tl("invert_point_color"))

    # Update numericInput labels
    updateNumericInput(session, "colcut", label = tl("column_before_numeric"))
    updateNumericInput(session, "yMin", label = tl("y_axis_min"))
    updateNumericInput(session, "yMax", label = tl("y_axis_max"))
    updateNumericInput(session, "ImageWidth", label = tl("image_width"))
    updateNumericInput(session, "ImageHeight", label = tl("image_height"))
    updateNumericInput(session, "ImageDPI", label = tl("image_dpi"))
    
    # Update sliderInput label
    updateSliderInput(session, "PointSize", label = tl("point_size"))
    
    # Update radioButtons label
    updateRadioButtons(session, "ImageFiletype", label = tl("image_format"))
    
    # Update downloadButton label
    shinyjs::runjs(sprintf("$('#downloadPlot').text('%s');", tl("download_button")))
    
    # Update actionButton with icon
    shinyjs::runjs(sprintf("
      $('#show_changelog').html('<i class=\"fa fa-circle-info\" role=\"presentation\" aria-label=\"circle-info icon\"></i> %s');
    ", tl("changelog_button")))
  })
  
  # ---------------------------------------- Data Input -----------------------------------------------
  
  # Function to handle colcut and NA values
  FunctionColcutNA <- function(df) {
    TempDF <- df

    TempDF[1, ] <- sapply(TempDF[1, ], \(x) ifelse(is.na(x), "NA", x))
    colnames(TempDF) <- make.unique(as.character(TempDF[1, ]))
    TempDF <- TempDF[-1, ]
    TempDF <- as.data.frame(TempDF)

    req(input$colcut)
    colcut <- input$colcut %>% abs()

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

  # Reactive expression to read and process the uploaded data
  DataTable <- reactive({
    req(input$DataTable, input$colcut)
    TempDF <- openxlsx::read.xlsx(input$DataTable$datapath, colNames = FALSE)

    shiny::validate(
      need(!is.null(TempDF), tl("file_empty_error"))
    )
    shiny::validate(
      need(ncol(TempDF) >= 1, tl("file_empty_error"))
    )
        
    if (input$InpDat) {
      TempDF <- t(TempDF)
    }

    TempDF <- FunctionColcutNA(TempDF)
    return(TempDF)
  })

  observe({
    req(DataTable(), input$colcut)
    
    colcut <- input$colcut %>% abs()
    if (colcut > ncol(DataTable())) {
      colcut <- ncol(DataTable())
    }

    All_colname <- colnames(DataTable())
    Colname_before_colcut <- All_colname[1:colcut]
    Colname_after_colcut <- All_colname[(colcut + 1):length(All_colname)]

    updateSelectizeInput(session, "dotid", choices = Colname_before_colcut, server = TRUE)
    updateSelectizeInput(session, "groupnameis", choices = Colname_before_colcut, server = TRUE)
    updateSelectizeInput(session, "DataY", choices = Colname_after_colcut, server = TRUE)
  })

  output$data <- DT::renderDT({
    max_row <- ifelse(nrow(DataTable()) > 10, 10, nrow(DataTable()))
    max_col <- ifelse(ncol(DataTable()) > 10, 10, ncol(DataTable()))

    TempDF <- data.table::data.table(DataTable()[1:max_row, 1:max_col]) %>%
      mutate_if(is.numeric, ~ round(., 4))

    DT::datatable(TempDF, options = list(server = TRUE, pageLength = 25, scrollX = TRUE, dom = "t"))
  })

  # ---------------------------------------- Color Picker for Groups --------------------------------------------
  SelectionGroup <- reactiveVal()
  ColorStorage <- reactiveVal(data.frame(GroupID = character(), Color = character(), stringsAsFactors = FALSE))
  
  output$ColorPickerUI <- renderUI({
    req(DataTable(), input$selected_groups, input$group_order, input$dotid)
    
    stored_colors <- isolate(ColorStorage())
    all_groups <- sort(unique(DataTable()[[input$dotid]]))
    
    stored_groups <- stored_colors$GroupID
    groups_match <- length(stored_groups) == length(all_groups) && all(all_groups %in% stored_groups)
    
    if (nrow(stored_colors) == 0 || !groups_match) {
      colourcount <- length(all_groups)
      getPalette <- get_color_palette()
      default_colors <- getPalette(colourcount)
      stored_colors <- data.frame(
        GroupID = all_groups,
        Color = default_colors,
        stringsAsFactors = FALSE
      )
      isolate(ColorStorage(stored_colors))
    }
    
    if (input$dotid == input$groupnameis) {
      groups <- input$group_order
    } else {
      groups <- all_groups
    }
    
    color_pickers <- purrr::map(seq_along(groups), function(i) {
      group <- groups[i]
      color_idx <- which(stored_colors$GroupID == group)
      current_color <- if (length(color_idx) > 0) stored_colors$Color[color_idx] else "#FF0000"
      
      column(
        width = 3,
        colourpicker::colourInput(
          inputId = paste0("color_", make.names(group)),
          label = as.character(group),
          value = current_color,
          showColour = "both",
          allowedCols = NULL
        )
      )
    })
    fluidRow(color_pickers)
  })
  
  observe({
    req(DataTable())
    
    all_groups <- sort(unique(DataTable()[[input$dotid]]))
    stored_colors <- ColorStorage()
    
    all_inputs_exist <- all(sapply(all_groups, function(g) {
      !is.null(input[[paste0("color_", make.names(g))]])
    }))
    
    if (all_inputs_exist) {
      colors <- sapply(all_groups, function(g) {
        color_input <- input[[paste0("color_", make.names(g))]]
        if (!is.null(color_input)) {
          return(color_input)
        } else {
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
  output$BoxplotsWithDots <- renderPlot({
    req(SelectionGroup(), input$selected_groups, input$group_order)
    shiny::validate(
      need(nrow(SelectionGroup()) > 0, tl("colorpicker_loading"))
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
  
  BoxplotInput <- reactive({
    req(input$selected_groups, input$group_order, SelectionGroup())
    
    filtered_data <- DataTable() %>%
      filter(.data[[input$groupnameis]] %in% input$selected_groups)
    
    ordered_data <- filtered_data %>%
      mutate(!!input$groupnameis := factor(.data[[input$groupnameis]], levels = input$group_order))
    
    y_min <- ifelse(is.numeric(input$yMin) & !is.na(input$yMin), input$yMin, NA)
    y_max <- ifelse(is.numeric(input$yMax) & !is.na(input$yMax), input$yMax, NA)
    
    if (!is.na(y_min) & !is.na(y_max) & y_min > y_max) {
      temp <- y_min
      y_min <- y_max
      y_max <- temp
    }
    
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
          ggbeeswarm::geom_beeswarm(
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
          ggbeeswarm::geom_beeswarm(
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
          scale_colour_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        } else {
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
  output$BarplotsWithDots <- renderPlot({
    req(SelectionGroup(), input$selected_groups, input$group_order)
    shiny::validate(
      need(nrow(SelectionGroup()) > 0, tl("colorpicker_loading"))
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
  
  BarplotInput <- reactive({
    req(input$selected_groups, input$group_order, SelectionGroup())
    
    filtered_data <- DataTable() %>%
      filter(.data[[input$groupnameis]] %in% input$selected_groups)
    
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
    
    y_min <- ifelse(is.numeric(input$yMin) & !is.na(input$yMin), input$yMin, NA)
    y_max <- ifelse(is.numeric(input$yMax) & !is.na(input$yMax), input$yMax, NA)
    
    if (!is.na(y_min) & !is.na(y_max) & y_min > y_max) {
      temp <- y_min
      y_min <- y_max
      y_max <- temp
    }
    
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
          ggbeeswarm::geom_beeswarm(
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
          ggbeeswarm::geom_beeswarm(
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
          scale_colour_manual(
            name = input$dotid,
            values = setNames(SelectionGroup()[["Color"]], SelectionGroup()[["GroupID"]])
          )
        } else {
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
  
  # ---------------------------------------- Session Info -----------------------------------------------------
  output$sessionInfo <- renderPrint({
    sessionInfo()
  })
  
  # ---------------------------------------- Download -----------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (is.null(input$Filename) || input$Filename == "") {
        return(paste0("plot.", input$ImageFiletype))
      }
      paste0(input$Filename, ".", input$ImageFiletype)
    },
    content = function(file) {
      selected_plot <- if (input$plot_tabs == "Boxplot") {
        BoxplotInput()
      } else if (input$plot_tabs == "Barplot") {
        BarplotInput()
      }
      
      if (input$ImageFiletype == "png") {
        ggplot2::ggsave(
          filename = file,
          plot = selected_plot,
          device = "png",
          width = as.numeric(input$ImageWidth),
          height = as.numeric(input$ImageHeight),
          units = "cm",
          dpi = input$ImageDPI
        )
      } else if (input$ImageFiletype == "pdf") {
        ggplot2::ggsave(
          filename = file,
          plot = selected_plot,
          device = grDevices::cairo_pdf,
          width = as.numeric(input$ImageWidth),
          height = as.numeric(input$ImageHeight),
          units = "cm",
          dpi = input$ImageDPI
        )
      } else if (input$ImageFiletype == "svg") {
        TempFile <- tempfile(fileext = ".svg")
        
        tryCatch( 
          {
            ggplot2::ggsave(
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
            grDevices::svg(
              filename = TempFile,
              width = as.numeric(input$ImageWidth) / 2.54,
              height = as.numeric(input$ImageHeight) / 2.54
            )
            print(selected_plot)
            grDevices::dev.off()
            showNotification("SVG wurde mit der base svg-Funktion gespeichert.", type = "warning")
          }
        )
        
        x <- readLines(TempFile)
        x <- stringr::str_remove(x, " textLength='[0-9.]+px'")
        x <- stringr::str_remove(x, " lengthAdjust='[a-zA-Z]+'")
        writeLines(x, con = file)
      }
    }
  )

  # ---------------------------------------- Changelog -----------------------------------------------------
  observeEvent(input$show_changelog, {
    changelog_path <- if(input$selected_language == "de") {
      system.file("changelog_de.txt", package = "RSPrismBB")
    } else if (input$selected_language == "en") {
      system.file("changelog_en.txt", package = "RSPrismBB")
    } else {
      system.file("changelog_en.txt", package = "RSPrismBB")
    }
    
    if (!file.exists(changelog_path)) {
      changelog_html <- if(input$selected_language == "de") {
        "<b>Kein Changelog gefunden.</b>"
      } else {
        "<b>No changelog found.</b>"
      }
    } else {
      changelog_content <- readLines(changelog_path, warn = FALSE)
      
      if (length(changelog_content) == 0) {
        changelog_html <- if(input$selected_language == "de") {
          "<b>Changelog ist leer.</b>"
        } else {
          "<b>Changelog is empty.</b>"
        }
      } else {
        changelog_html <- paste(changelog_content, collapse = "<br>")
      }
    }
    
    title_text <- if(input$selected_language == "de") "Updateverlauf" else "Update History"
    button_text <- if(input$selected_language == "de") "Schlie\u00dfen" else "Close"
    
    showModal(modalDialog(
      title = strong(title_text),
      HTML(changelog_html),
      easyClose = TRUE,
      footer = modalButton(button_text)
    ))
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
