library(shiny)

# don't think we need this since dropping java-based XLconnect
#options(java.parameters = "-Xss2560k")

shinyUI(fluidPage(
  titlePanel(
    HTML(
      "<h1><strong>Logistic-Abbott Models in Environmental Safety</strong></h1><h4>Version 2.0</h4><h4>
      Running from github repository https://github.com/gcarrghub/LogisticAbbott</h4>"
    ),
    windowTitle = "Environmental Safety Logistic-Abbott Tool"
  ),
  fluidRow(column(
    width = 3,
    wellPanel(
      p("Enter the data you wish to analyze"),
      #progressInit(),
      shiny::fileInput(
        inputId = 'inputFile',
        label = 'Choose File',
        multiple = FALSE,
        placeholder = "Select a file"
      ),
      #uiOutput("sheetUI"),
      htmlOutput("sheetUI"),
      #links with output$sheetUI
      htmlOutput("doseColUI"),
      #links to output$doseColUI for the dialog box to create input$nameDoseCol
      htmlOutput("respColUI"),
      #links to output$respColUI for the dialog box to create input$nameRespCol
      htmlOutput("sizeColUI"),
      #links to output$sizeColUI for the dialog box to create input$nameSizeCol
      #hr(),
      sliderInput("ECx.targets", "Effect Level", 5, 95, 50, step = 5),
      #links to input$ECx.targets
      sliderInput("confidenceCI", "Confidence Level (Logistic only)", 50, 99, 95, step = 1),
      #links to input$confidenceCI
      radioButtons(inputId = "modelType",label = "Model assumption on background",
        choiceNames = c("Assume Zero (STD)", "Estimate it (Abbott)"),
        choiceValues = c("lcx", "abbott"),
        inline = TRUE,
        width = "100%",
        selected = "lcx"
      ),
      checkboxInput("doseTicks", "Axis Ticks at Test Levels", value = FALSE),
      checkboxInput("annotate", "Annotate Plot", value = FALSE),
      #textInput("zeroSub","Values to Substitute for Zero (when applicable)",value=NULL),
      textInput("xlab", "x-label for plot", value = "Concentration"),
      textInput("ylab", "y-label for plot", value = "Response Level"),
      checkboxInput("annot8Plot", "Annotate Plot", value = FALSE),
      checkboxInput("debugPrint", "Print Debug Info (local only)", value = FALSE),
      br(),
      actionButton("updateRes", "Calculate Results")
    )
  ),
  column(
    width = 9,
    tabsetPanel(
      id = "tabs",#type="pills",
      tabPanel(
        "Help/Documentation",
        h3("Firefox or Chrome browsers suggested."),
        tags$ul(
          tags$li("Others may work (e.g., Safari and Brave on Mac) but NOT MS IE in Windows."),
          tags$li(
            "Usually the tool will open in your default browser.  
          If you are not certain of your browser's compatibility, 
          simply copy-paste the full link/address into the address 
          bar of an open Chrome or Firefox window.  
          Your R session provides the same address in a message 
          that should look like 'Listening on http://127.0.0.1:XXXX'."
          ),
          tags$li(
            "It's even OK to have multiple sessions open that point 
          to the same address.  For example, in one window
          you could have the instructions displayed, and in the 
          other select data and options.  Or side-by-side
          analyses of different experiments."
          )),
        br(),
        strong("Purpose of the Tool:  "),
        "Estimate the concentration that results in a given response level, 
        by logistic regression methods.  This analysis 
        is appropriate for experiments in which a continuous response is 
        measured on each individual or experimental unit, such as a body weight, 
        or possibly for groups of individuals, such as counts of algae in a vessel.  
        The response level must decline as toxicity (the exposure level) increases.",
        tags$ul(
          tags$li(
            strong(
              "This tool is appropriate for binary response data, such of mortaility."
            )
          ),
          tags$li(
            "Proper usage is the sole responsibility of the user.  
            No warranty is made or implied, but we appreciate constructive feedback."
          )
        ),
        br(),
        column(
          width = 8,
          strong("Directions:"),
          tags$ul(
            tags$li(
              "Choose the file containing data for analysis, from a single experiment, 
              (see Data Format and Acceptable File Formats below) using the 'Choose File' 
              button in the grey panel at left. Verify data in the 'Data For Analysis' tab."
            ),
            tags$li(
              "Select the desired Effect Level using the slider (effect is response level
              as a percent) so 20 is for the concentration resulting in a 
              20% response level, or 20% of the range when an Abbott background adjustment is made.  
              In cases where the response level of the data does not cover the requested
              effect level, the estimated effect level concentration can be beyond the range of those tested."
            ),
            tags$li(
              "The 'Calculate Results' button in the lefthand panel will start the analysis, 
              after data are selected, as long as some basic criteria are satisfied.  Once completed,
              Both PDF and Excel versions of output can be downloaded at the bottom of results."
            ),
            tags$li(
              strong("Compute time: "),"Results will usually be almost instantaneous when the
              response profile fits well with a logistic model."
            ),
            tags$li(
              strong("Results: "),"The user is fully responsible for addressing validity of results.  
              In particular, pay close attention to any cases where the ECx or confidence limits take values 
              beyond the range of concentrations tested."
            ),
            tags$li(
              strong("Exiting: "),"Closing the browser window(s) does not stop the R process.  
              If launched inside Rstudio, click the red 'stop sign' button, in the basic R 
              gui press 'Esc'.  Quitting/killing/exiting from R/Rstudio has the same effect."
            )
          ),
          br(),
          strong("Data & File Formats:"),
          tags$ul(
            tags$li(
              strong("Layout: "),
              "!!!-IMPORTANT-!!! Data must be arranged in a clean, column-wise rectangular 
              layout with the column labels in the first row, and no extra space/columns 
              on the left margin, no extra columns between data columns, etc.  It should 
              look like the example here.  No annotation of data values should be used 
              (eg, asterisks); everything should be purely numeric except for the column labels.  
              Even formatting of values can be problematic, so as a general rule-of-thumb keep 
              the data file minimal, and clean."
            ),
            tags$li(
              strong("Variables: "),
              "The input data MUST contain at least three columns, one for the concentrations ('doses') 
              one for the count of responses ('responses') and one for the group sizes ('sizes').  When columns of
              data are given in this order, the tool will assign them in the correct order.  Otherwise, if
              names don't match you will have to provide the mapping in selection boxes."
            ),
            tags$li(
              strong("MSExcel (both .xls & .xlsx extensions):"), "Data for each experiment to be analyzed 
              should be in separate worksheets and follow 'Layout' principles above.  Start the data in 
              row 1 column A. When it contains multiple worksheets, the user will be prompted to select 
              one that should contain valid data.  The github repository contains an XLSX workbook within 
              which several valid example datasets are provided, one in each worksheet.  By offering this 
              xlsx capability we also need to prevent errors by filtering out sheets that do not look
              like data.  If the desired worksheet is not in the list, then we were unable to detect at 
              least two columns of numbers that could be used in this type of model so it was removed; 
              check the data versus our guidance above."
            ),
            tags$li(
              strong("Comma separated Values (.csv): "),"Follow same principles.  
              In this plain text file format, values in a row are separated by commas."
            ),
            tags$li(
              strong("Text (.txt): "),"Follow same principles.  
              In this plain text file format, values in a row are separated by tabs."
            )
          ),
          br(),
          strong("Contacts:"),
          tags$ul(
            tags$li("Primary: Gregory Carr (carr.gj@pg.com)"),
            tags$li("Secondary: Joel Chaney (chaney.jg@pg.com)")
          ),
          img(src = "PG-DMS.jpg",
              #as a part of the webpage display, by default this file is looked for in www, but
              #we want to reserve that folder for only temporary files
              height = 100)
        ),
        column(width = 4,
               strong("Sample Data:"),
               tableOutput("exampDat"))
      ),
      tabPanel(
        "Data For Analysis",
        p("Data read in for Analysis"),
        div(textOutput("badData"), style = "color:red"),
        fluidRow(
          column(
            width = 3,
            tableOutput("DataTab")),
          column(
            width=9,
            plotOutput("baseplot"),          
            strong("Directions:"),
            tags$ul(
              tags$li(
                "Choose the file containing data for analysis, from a single experiment, 
              (see Data Format and Acceptable File Formats below) using the 'Choose File' 
              button in the grey panel at left. Verify data in the 'Data For Analysis' tab."
              ),
              tags$li(
                "Select the desired Effect Level using the slider (effect is response level
              as a percent) so 20 is for the concentration resulting in a 
              20% response level, or 20% of the range when an Abbott background adjustment is made.  
              In cases where the response level of the data does not cover the requested
              effect level, the estimated effect level concentration can be beyond the range of those tested."
              ),
              tags$li(
                "The 'Calculate Results' button in the lefthand panel will start the analysis, 
              after data are selected, as long as some basic criteria are satisfied.  Once completed,
              Both PDF and Excel versions of output can be downloaded at the bottom of results."
              ),
              tags$li(
                strong("Compute time: "),"Results will usually be almost instantaneous when the
              response profile fits well with a logistic model."
              ),
              tags$li(
                strong("Results: "),"The user is fully responsible for addressing validity of results.  
              In particular, pay close attention to any cases where the ECx or confidence limits take values 
              beyond the range of concentrations tested."
              ),
              tags$li(
                strong("Exiting: "),"Closing the browser window(s) does not stop the R process.  
              If launched inside Rstudio, click the red 'stop sign' button, in the basic R 
              gui press 'Esc'.  Quitting/killing/exiting from R/Rstudio has the same effect."
              )
            ))
        )),
      tabPanel(
        "Results",
        uiOutput("messages"),
        tableOutput("resultsTable"),
        tableOutput("resultsTableSK"),
        plotOutput("plot"),
        br(),
        downloadButton('downloadResults', 'Download Excel File'),
        downloadButton('downloadPlot', 'Download PDF')
      )
    )
  ))
))
