#The following is a general Shiny App used to display statistics, charts, and graphs based on sanitized user input data.

# COPY FROM EXAMPLE SCRIPT
library(dplyr)
  
# input_csv <- read.csv("C:/Users/Lian Plass/Downloads/_TEMP/JAX-CHNA/CHNA_Analysis/basic_dataset.csv", stringsAsFactors=FALSE)

var_obs_1<-input_csv[,1]
var_obs_2<-input_csv[,2]
var_obs_3<-input_csv[,3]
var_obs_4<-input_csv[,4]
var_obs_5<-input_csv[,5]
var_obs_6<-input_csv[,6]
var_obs_7<-input_csv[,7]
var_obs_8<-input_csv[,8]
var_obs_9<-input_csv[,9]
var_obs_10<-input_csv[,10]

all_var_obs<-c("var_obs_1","var_obs_2","var_obs_3","var_obs_4","var_obs_5","var_obs_6","var_obs_7","var_obs_8","var_obs_9","var_obs_10")

descriptives_table<-(0)

input_mean<-c(0)
input_median<-c(0)
input_mode<-c(0)
input_range<-c(0)
input_SD<-c(0)
input_variance<-c(0)
input_count<-c(0)

#MEAN
mean_calc = function(var_observations){
  input_mean<<-mean(var_observations)
}

#MEDIAN
median_calc = function(var_observations){
  input_median<<-median(var_observations)
}

#MODE
mode_calc = function(var_observations){
  uniquevar <-unique(var_observations)
  input_mode<<-uniquevar[which.max(tabulate(match(var_observations, uniquevar)))]
}

#RANGE
range_calc = function(var_observations){
  input_range <<-max(var_observations)-min(var_observations)
}

#STANDARD DEVIATION
SD_calc = function(var_observations){
  input_SD<<-sd(var_observations)
}

#VARIANCE
vari_calc = function(var_observations){
  input_variance<<-var(var_observations)
}

#COUNT (NOT WORD FREQUENCY ANALYSIS)
count_calc = function(var_observations){
  input_count<<-head(table(var_observations))
}

#CALCULATES AND FORMATS MEAN, MEDIAN, MODE, RANGE, SD, AND VARIATION FOR INPUT TABLE

table1 = function(input_csv){
  str(input_csv)
  input_csv<-select_if(input_csv, is.numeric)
  for (i in 1:ncol(input_csv)){
    clean_input_csv<- na.omit(input_csv[,i])
    if (i==1){
      descriptives_table<-data.frame("Dataset"=colnames(input_csv)[i],
                                     "Mean"=mean_calc(clean_input_csv),
                                     "Median"=median_calc(clean_input_csv),
                                     "Mode"=mode_calc(clean_input_csv),
                                     "Range"=range_calc(i),
                                     "Standard Deviation"=SD_calc(clean_input_csv),
                                     "Variance"=vari_calc(clean_input_csv)
                                     )
    } else{
      temporary_dataframe<-data.frame("Dataset"=colnames(input_csv)[i],
                                       "Mean"=mean_calc(clean_input_csv),
                                       "Median"=median_calc(clean_input_csv),
                                       "Mode"=mode_calc(clean_input_csv),
                                       "Range"=range_calc(i),
                                       "Standard Deviation"=SD_calc(clean_input_csv),
                                       "Variance"=vari_calc(clean_input_csv)
                                      )
      descriptives_table <- rbind(descriptives_table,temporary_dataframe)
    }
  }
  descriptives_table
}

element1 = function(input_csv){
  str(input_csv)
  input_csv<-select_if(input_csv,is.character)
  for (i in input_csv){
    print(count_calc(i))
  }
}


# TEST CODE HERE
# typeof(input_csv[,5])
# select_if(input_csv,is.character)
# element1(input_csv)

# SHINY APP BEGINS HERE
library(shiny)

ui<- fluidPage(
  titlePanel("Quick Survey Analytics"),
  tags$p("So you just conducted a community survey--AWESOME"),
  tags$p("Now...what to do with all that sweet, sweet data?  Use (and adapt) this tool to get some quick analytics...without the code."),
  sidebarLayout(
    #This is the left sidebar containing operation checkboxes
    sidebarPanel(
      fileInput(
        inputId = "input_csv",
        label = "Upload your sanitized CSV here:",
        accept = c("text/csv", "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      tags$hr(),
      checkboxInput(
        inputId = "header",
        label = "Header",
        value = TRUE
      ),
    ),
#This is the main panel containing the descriptive statistic plots and the word cloud analysis
    mainPanel(
      tags$h3("Basic Descriptive Statistics from Dataset:"),
      tags$p("Descriptive statistics (e.g., mean, median, and mode) will populate here once you load in your file. Once you have loaded in your data.  Click on the tabs above to navigate to Plots, Word Frequency Analysis, and more."),
      tableOutput("descriptive_statistics"),
      tags$h3("Counts (Categorical Variables Only)"),
      tags$p("Here are some descriptive statistics for the categorical (non-numeric) variables you entered.  Do NOT run this function on short response answers that have not been categorized."),
      verbatimTextOutput("descriptive_counts"),
      plotOutput("descriptive_statistics_plots"),
      plotOutput("word_cloud_plots")
    )
  )
)

server<- function(input, output){
  output$descriptive_statistics<-renderTable({
    inFile <-input$input_csv
    if(is.null(inFile))
      return(NULL)
    table1_input<-read.csv(inFile$datapath, header = input$header)
    table1(table1_input) 
    

    # descriptives_table

    #observe({
    #output$value<- ADD SELECTION IN A SPECIFIC ORDER IN A LIST
    #output$descriptive_statistics_plots
    #output$word_cloud_plots
  })
  
  output$descriptive_counts<-renderText({
    inFile<-input$input_csv
    if(is.null(inFile))
      return(NULL)
    element1_input<-read.csv(inFile$datapath, header = input$header,stringsAsFactors=FALSE)
    #FIGURE OUT HOW TO GET THIS OUTPUT TO POPULATE
    element1(element1_input)
  }
    
  )
  }

shinyApp(ui=ui, server=server)

# END OF SHINY APP
