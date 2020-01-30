
library(shiny)
library(shinyIncubator)

options(java.parameters = "-Xss2560k")

shinyUI(pageWithSidebar(
  titlePanel(HTML("<h1><strong>Environmental Safety Logistic-Abbott Analysis</strong></h1><h4>Version 1.0</h4>"), 
             windowTitle = "Environmental Safety Logistic-Abbott Analysis"),
  
  sidebarPanel(
    p("Enter the data you wish to analyze"),
	fileInput('inputFile', 'Choose File', multiple=FALSE),
	progressInit(),
	htmlOutput("sheetUI"),
	htmlOutput("doseColUI"),
	htmlOutput("respColUI"),
	htmlOutput("sizeColUI"),
	sliderInput("ECx.targets","Effect Level",5,95,50,step=5),
	sliderInput("confidenceCI","Confidence Level",50,99,95,step=1),
	checkboxInput("bgAdjust","Background Adjust",value=FALSE),
	#checkboxInput("doTrend","Do Trend Analysis",value=FALSE),
	#checkboxInput("showSTD","Plot Approximate Standard CI",value=FALSE),
	textInput("xlab","x-label for plot",value="Concentration"),
	textInput("ylab","y-label for plot",value="Probability"),
	checkboxInput("bndCurves","Plot Bounds for Dose-Response Curve",value=FALSE),
	checkboxInput("annotate","Annotate Plot",value=FALSE),
	br(),
	actionButton("updateRes","Calculate Results")
  ),
  
  mainPanel(
    tabsetPanel(id="tabManager",
	  tabPanel("Help/Documentation",
        HTML(
		  "<h4>For best results, use Firefox or Chrome browsers.</h4>
		  <h5>Purpose of the Tool</h5>
		  <p>Fit a logistic regression model to survival data, or other count data, and calculate confidence intervals on the concentration that causes a user-specified
		     effect level, such as a 10% increase in mortality.  The experiment must be one in which a pre-specified number of individuals is exposed, per group, and
			 then observed to count the number with a response of interest, such as mortality.</p>
		  <h5>Directions :</h5>
		  <p><li>Choose the file containing data for analysis, from a single experiment, (see Data Format and Acceptable File Formats below) using the 
		     'Choose File' button in the grey panel.  If an appropriate file was chosen, the data will appear on the Data For Analysis Tab without an error statement
		     (click on Data For Analysis link above to change tabs).</li>
		  <li>Select the desired Effect Level and Confidence Level using the sliders.</li>
		  <li>Check the 'Background Adjust' box if adjustment for control group mortality is desired.  Control data will only be used if this box is selected, and will 
		      only be shown in the 'Data For Analysis' view if the box is selected.</li>
		  <li>The remaining inputs in the grey panel affect the plot in the Results tab.  Select the Results tab to display the results.</li></p>
		  <p>*Analysis Results may take a couple seconds to display on the Results tab
		  *The Profile Likelihood Confidence Interval is preferred to the Standard Confidence Interval.</p>
		  <h5>Data Format </h5>
		  <p>!!!-IMPORTANT-!!! Data must be arranged in a clean, column-wise rectangular layout with the column labels in the first row,
             and no extra space/columns on the left margin, no extra columns between data columns, etc.  No annotation of data values
             should be used (eg, asterisks); everything should be purely numeric except for the column labels. </p>
		  <p> Minimally, the input data should contain exactly three columns, by default one labeled 'doses' (on the measured scale-not log),
              one labeled 'responses' (the counts of events of interest) , and one labeled 'sizes' (the group size).  If all these labels are
              not matched, the user will be asked to identify the columns for each of these three purposes. </p>			 
		  <p>The experiment must be one in which a given number of individuals is exposed, per group,
		    and then observed to count the number with a response of interest, such as mortality</p>
		  <h5>Acceptable File Formats & Data Directions :</h5>
		  <p><li>MSExcel Files (both .xls & .xlsx extensions) : If using an Excel file, a single worksheet in that file must be dedicated to the one experiment to analyze 
		    (no other cells should be filled).  Start the data in row 1 column A.  When an Excel file is used as input, the sheet names within the Excel workbook
			will appear in the grey panel.  If there is more than one sheet in the file, you will be prompted to choose the sheet containing the data.</li>
		  <li>CSV file : Follow same principles.  In this file format, values in a row are separated by commas.</li>
		  <li>Text File : Follow same principles.  In this file format, values in a row are separated by tabs.</li>
		  <br>
		  <li>CSV and TXT files can be created in MSExcel, or other text editing software.</li>
		  <h5>Example Data :</h5>
		  "
		),
		tableOutput("exampDat"),
		HTML(
		  "
		  <h5>Contacts :</h5>
		  <p>Primary : Gregory Carr (carr.gj)
		  <br>
		    Secondary : Joel Chaney (chaney.jg)</p>
		  "
		), value="helpTab"
	  ),
      tabPanel("Data For Analysis",
	    textOutput("dataReadText"),
		div(textOutput("badDataFlag"), style = "color:red"),
		br(),
		tableOutput("DataTab"),
		textOutput("dose0Flag"),
		value="dataTab"
	  ),
      tabPanel("Results",
	    #HTML(textOutput("zCATrendNote")),
		div(textOutput("zCAWarn"), style = "color:red"),
	    textOutput("noGoodData"),
	    textOutput("zCATrendNote"),
		br(),
	    tableOutput("zCARes"),
	    tableOutput("resTab"),
		plotOutput("plotRes"),
		downloadButton('downloadResults', 'Download Excel File'),
		downloadButton('downloadPlot', 'Download PDF'),
		value="resultsTab"
	  )
      #tabPanel("Results",tableOutput("resTab"))
	)
  )
  
))
