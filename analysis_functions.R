#The following is a general Shiny App used to display statistics, charts, and graphs based on sanitized user input data.

# COPY FROM EXAMPLE SCRIPT
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(topicmodels)
library(tidytext)

input_csv <- read.csv("basic_dataset.csv", stringsAsFactors=FALSE)
#input_csv<-c(" ")

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
count_calc = function(Counts){
  input_count<<-head(sort(table(Counts), decreasing=TRUE))
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
  input_csv<-select_if(input_csv,is.character)
  for (i in input_csv){
    print(count_calc(i))
  }
}

#PEARSONS CHI SQUARED
table2 = function(input_csv){
  data.frame( 
    names(select_if(input_csv,is.numeric)),
    cor(select_if(input_csv,is.numeric))
  )
}


#PLOTS
#NUMERIC VARIABLES

#plot_var_selection<-input_csv$potato

all_plots<-function(plot_var_selection){
  if(is.numeric(plot_var_selection)==FALSE){
    plot_var_selection<-data.frame(table(plot_var_selection))
    names(plot_var_selection)<-c("Categories","Frequencies")
    plot_var_selection$Categories<-paste0(plot_var_selection$Categories, " (", round((plot_var_selection$Frequencies/sum(plot_var_selection$Frequencies))*100, digits=2),"%)")
    
    ggplot(data=plot_var_selection, aes(x="", y=Frequencies, fill=Categories))+
      geom_bar(stat="identity",width=1,color="white") +
      coord_polar("y", start=0)+
      theme_void()
  }
}
all_plots(plot_var_selection)

#CATEGORICAL VARIABLES

#WORD FREQUENCY ANALYSES

#TEXT SANITIZATION (TERM DOCUMENT MATRIX)
free_response_selection<-input_csv$chard
text_sanitization1<-function(free_response_selection){
  if(is.character(free_response_selection)==TRUE){
    text<-free_response_selection
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    dtm <<- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    sanitized_text <<- data.frame(word = names(words),freq=words)
  }}

text_sanitization1(free_response_selection)

#TEXT SANITIZATION (DOCUMENT TERM MATRIX)
text_sanitization2<-function(free_response_selection){
  if(is.character(free_response_selection)==TRUE){
    text<-free_response_selection
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    dtm_input <<- DocumentTermMatrix(docs)
  }}

text_sanitization2(free_response_selection)

#WORD CLOUD
set.seed(1)
wordcloud(words = sanitized_text$word, 
          freq = sanitized_text$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"),
          scale=c(3.5,0.5)
          )

#IDENTIFY TOPICS USING LDA
sanitized_text_lda<-LDA(dtm_input, k=5, control=list(seed=2))
sanitized_text_lda

#TOP TERMS BY TOPIC
sanitized_text_prob<-tidy(sanitized_text_lda, matrix="beta")
sanitized_text_prob

sanitized_text_top<- sanitized_text_prob %>%
  group_by(topic) %>%
  slice_max(beta, n=3) %>%
  ungroup() %>%
  arrange(topic, -beta)

sanitized_text_top %>%
  mutate(term=reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic)))+
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales="free")+
  scale_y_reordered()
  
        
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
      navbarPage(title="Results",
                 tabPanel("Basic Insights",
                          tags$h3("Basic Descriptive Statistics from Dataset"),
                          tags$p("Descriptive statistics (e.g., mean, median, and mode) will populate here once you load in your file. Once you have loaded in your data.  Click on the tabs above to navigate to Plots, Word Frequency Analysis, and more."),
                          tableOutput("descriptive_statistics"),
                          tags$h3("Counts (Categorical Variables Only)"),
                          tags$p("Here are some descriptive statistics for the categorical (non-numeric) variables you entered.  Do NOT run this function on short response answers that have not been categorized."),
                          verbatimTextOutput("descriptive_counts"),
                 ),
                 tabPanel("Advanced",
                          tags$h3("Plots"),
                          tags$p("Select a variable from the dropdown to see a plot of the data."),
                          uiOutput("toCol"),
                          plotOutput("descriptive_statistics_plots"),
                          tags$h3("Correlation"),
                          tags$p("The following is the table output for Pearson's Chi Squared test of independence.  It is useful for determining whether variables are correlated. The closer values in this table are to 1, the more heavily correlated they are."),
                          tags$p("This table analyzed all NUMERIC data you entered.  If you wish to analyze non-numeric data, please follow the guide in the csv sanitization Excel Workbook available in the left side panel."),
                          #ADD PEARSON'S CHI TABLE OUTPUT HERE
                          tableOutput("pearsons_table")
                 ),
                 tabPanel("Word Frequency Analysis")
      )
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
  })
  
  output$descriptive_counts<-renderPrint({
    inFile<-input$input_csv
    if(is.null(inFile))
      return(NULL)
    element1_input<-read.csv(inFile$datapath, header = input$header,stringsAsFactors=FALSE)
    element1(element1_input)
  })
  
  output$toCOl <-renderUI({
      inFile <-input$input_csv
      if(is.null(inFile))
        return(NULL) else {
          plot_input<-read.csv(inFile$datapath, header = input$header)
          items<-c(names(plot_input))
          selectInput("plot_prelim_input","Select Variable:", c("a","b","c"))
        }
  })
  
  output$descriptive_statistics_plots<-renderPlot ({
    inFile<-input$input_csv
    if(is.null(inFile))
      return(NULL)
  plot_var_selection<-read.csv(inFile$datapath, header = input$header,stringsAsFactors=FALSE)
  plot_var_selection<-plot_var_selection[,input$plot_prelim_input]
  all_plots(plot_var_selection)
  })
  
  output$pearsons_table<-renderTable({
    inFile <-input$input_csv
    if(is.null(inFile))
      return(NULL)
    table2_input<-read.csv(inFile$datapath, header = input$header)
    table2(table2_input) 
  })    
  
}

shinyApp(ui=ui, server=server)

# END OF SHINY APP
