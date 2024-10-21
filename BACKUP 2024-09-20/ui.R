dashboardPage(
  # ------------------- Header -------------------------------------------------------------------------#
  dashboardHeader(title = "Volcanoplot SexDiff"),
  
  # ------------------- Sidebar -------------------------------------------------------------------------#
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Data Table", tabName = "data"),
      
      menuItem(text = "Plots with Dots", tabName = "PlotsWithDots"),
      
      menuItem(text = "Barplot", tabName = "Barplot"),
      
      fileInput(
        "DataTable",
        "Upload a data file:",
        multiple = FALSE,
        accept = c(".xlsx")
      ),
      
      checkboxInput("InpDat", label = "Are values of one parameter (e.g. gene) in listed in a row (e.g. Array data)?", value = FALSE),
      
      textInput("colcut", label = "Number of columns before numeric data starts:", value = 0),
      
      selectInput("dotid", label = "What defines the dot-colour?", choices = NULL),
      
      selectInput("groupnameis", label = "What is the grouping column?", choices = NULL),
      
      selectInput("DataY", label = "What do you want on the y axis?", choices = NULL),
      
      # checkboxInput("CheckTitle", label = "Print title for plot", value = FALSE),
      # # Use conditionalPanel to show the text input only when the checkbox is TRUE
      # conditionalPanel(
      #   condition = "input.CheckTitle == true",
      #   # JS condition to check if the checkbox is TRUE
      #   textInput("Title", label = "Enter a title", placeholder = "Title"),
      #   checkboxInput("CheckTitleCenter", label = "Shall the title be centered", value = FALSE)
      # ),
      
      
      textInput("Filename", label = "Enter Filename (for download)", placeholder = "filename"),
      downloadButton("downloadPlot", style = "display: block; margin-left: 50px; margin-right: 50px;")
      
      
    )
  ),
  
  # ------------------- Body ----------------------------------------------------------------------------#
  dashboardBody(# tabName in sidebar and body needs to match in name (but not in order)
    tabItems(
      tabItem(tabName = "data", DTOutput("data")),
      tabItem(
        tabName = "PlotsWithDots",
        plotOutput("PlotsWithDots"),
        verbatimTextOutput("TextColorTable"),
        DTOutput("SelectionGroup")
      )
    ))
)