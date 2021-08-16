###

#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("ggiraph")
#install.packages("ggiraphExtra")
#install.packages("plyr")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

#library(ggiraph)
#library(ggiraphExtra)
#library(plyr)

dashboardPage(
  dashboardHeader(title = "Statistics For Data Analytics",titleWidth =300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction", tabName = "tabIntroduction", icon = icon("dashboard")),
      menuItem("GLM", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Simple LR", tabName = "tabSlr"),
               menuSubItem("Multiple LR", tabName = "tabLr"),
               menuSubItem("Logistic Regression", tabName = "tabLog")
      ),
      
      menuItem("Test for Mean", tabName = "tabHypothesis", icon = icon("th")),
      menuItem("Discrete Probabilitiy", icon = icon("th"), startExpanded = TRUE,
               menuSubItem("Binomial", tabName = "tabBinomial"),
               menuSubItem("Poisson", tabName = "tabPoisson")
      ),
      menuItem("Descriptive Statistics", tabName = "tabDescriptive", icon = icon("th"))
    )),
  dashboardBody(
    tabItems(
      # Tab Introduction
      tabItem(tabName = "tabIntroduction",
              fluidRow(
                box(
                  title = "Introduction", status = "primary",width=12,solidHeader = TRUE,
                  "The project is done as part of CA1 evaluation for the course Statistics For Data Analytics.",
                  tags$br(),
                  "Various statistical techniques have been applied on data sets and interesting results have been
                  observed. Various models of probability model, hypothesis testing and linear regression have been showcased.",tags$br(),
                  "Dashboard framework has been used to design the UI and it is reponsive."
                  #. Discrete probability models like Binomial and Poisson model have been used.Hypothesis testing has been done using test of mean using two tailed,lower tailed and upper tailed method.",tags$br(),"Three models of regression model have been done for dynamic datasets.Simple Linear Regression, Multiple Linear Reression and Logistic Linear Regression have been implemented. Various plots have been used to analyze the performance. Descriptve statistcal insights for any dataset can be seen from the 'Descriptive Statistics' Section"    
                  
                )
              ),
              fluidRow(
                
                box(
                  title = "Probability Model", status = "warning",width=6,solidHeader = TRUE,
                  "Discrete probability model like binomial and poisson have been implemented. Various inputs from
                  users are dynamically accepted and models are made. Graphs are plotted for insights.
                  "
                ),
                
                box(
                  title = "Descriptive Statistics", status = "info",width=6,solidHeader = TRUE,
                  "Any csv file can be uploaded in the module and various statistical inference can be observed.
                  "
                ),
              ),fluidRow(
                box(
                  title = "Hypothesis Testing", status = "danger",width=6,solidHeader = TRUE,
                  "Hypothesis testing using test for mean covering lower tailed, upper tailed and two tailed test have been implemented.
                  Users can provide data and the critical value along with whether the hypothesis is accepted or rejected is shown.
                  Probability distribution graph is also rendered.
                  "
                ),
                
                box(
                  title = "Regression Model", status = "success",width=6,solidHeader = TRUE,
                  "Regression model like simple linear regression, multiple linear regression and logistic regression have been implemented. Users can upload files and view the model performance.
                  Diagnostic plots, plot along LSRL, Confusion Matrix plot and cutoff based on accuracy have been generated.
                  "
                )
                
              )
              
              
              #tab ends        
      ),
      
      # Tab Probability
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      # Tab GLM
      #tab simple lr
      tabItem(tabName = "tabSlr",
              fluidRow(
                box(width=12,
                    box( solidHeader = TRUE, width = 3,fileInput("fileFile", "Choose CSV File",
                                                                 multiple = TRUE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"))
                    ),
                    box( solidHeader = TRUE,width=3,
                         selectInput(inputId="selectPredictorSlr", label = ("Select Predictor"), 
                                     choices = list()
                         )
                    ),
                    box(solidHeader = TRUE,width=3,
                        selectInput(inputId="selectResponseSlr", label = ("Select Response"), 
                                    choices = list()
                        )
                    ),
                    box(solidHeader = TRUE,width=3,tags$br(),
                        actionButton(inputId = "buttonGo", 
                                     label = "Apply Simple LR",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              fluidRow(
                box(width=12,DT::dataTableOutput("slmData"),title = "View Data Set (Click + to expand)",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                )
              ),
              fluidRow(
                box(width=12,verbatimTextOutput("slmSummary"),title = "Summary",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                ),
                box(width=6,plotOutput("slmGraph"),title = "Plot Along LSRL ",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                    
                ),
                box(width=6,plotOutput("slmGraph2"),title = "Diagnostic Plots",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                    
                )
              )#end fr
              
      ),
      #tab multi linear regression
      tabItem(tabName = "tabLr",
              fluidRow(
                box(width=12,
                    box( solidHeader = TRUE, width = 3,fileInput("fileFileLr", "Choose CSV File",
                                                                 multiple = TRUE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"))
                    ),
                    box( solidHeader = TRUE,width=3,
                         selectInput(inputId="selectPredictorLr", label = ("Select Predictors"),multiple = TRUE,
                                     choices = list()
                         )
                    ),
                    box(solidHeader = TRUE,width=3,
                        selectInput(inputId="selectResponseLr", label = ("Select Response"), 
                                    choices = list()
                        )
                    ),
                    box(solidHeader = TRUE,width=3,tags$br(),
                        actionButton(inputId = "buttonGoLr", 
                                     label = "Apply Multiple LR",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              fluidRow(
                box(width=12,DT::dataTableOutput("lrData"),title = "View Data Set (Click + to expand)",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                )
              ),
              fluidRow(
                box(width=12,verbatimTextOutput("lrSummary"),title = "Summary",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                )
                ,box(width=12,plotOutput("lrGraph"),title = "Diagnostic Plot ",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                     
                )
                
              )#end fr
              
      ),#tab logistic regression
      tabItem(tabName = "tabLog",
              fluidRow(
                box(width=12,
                    box( solidHeader = TRUE, width = 3,fileInput("fileFileLog", "Choose CSV File",
                                                                 multiple = TRUE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"))
                    ),
                    box( solidHeader = TRUE,width=3,
                         selectInput(inputId="selectPredictorLog", label = ("Select Predictors"),multiple = TRUE,
                                     choices = list()
                         )
                    ),
                    box(solidHeader = TRUE,width=3,
                        selectInput(inputId="selectResponseLog", label = ("Select Response"), 
                                    choices = list()
                        )
                    ),
                    box(solidHeader = TRUE,width=3,tags$br(),
                        actionButton(inputId = "buttonGoLog", 
                                     label = "Logistic Regression",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              fluidRow(
                box(width=12,DT::dataTableOutput("logData"),title = "View Data Set (Click + to expand)",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                )
              ),
              fluidRow(
                box(width=12,verbatimTextOutput("logSummary"),title = "Summary",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                ),
                box(width=12,plotOutput("logGraph"),title = "Performance Plots",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                    
                )
                
                
              )#end fr
              
      ),
      
        
      # tab descriptive
      tabItem(tabName = "tabDescriptive",
              fluidRow(
                box(width=12,
                    box( solidHeader = TRUE, width = 6,fileInput("fileFileDes", "Choose CSV File",
                                                                 multiple = TRUE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"))
                    ),
                    
                    box(solidHeader = TRUE,width=6,tags$br(),
                        actionButton(inputId = "buttonGoDes", 
                                     label = "View Basic Descriptive Stats",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              fluidRow(
                box(width=12,DT::dataTableOutput("desData"),title = "View Data Set (Click + to expand)",status = 'primary',solidHeader = TRUE,  collapsible = TRUE,collapsed = TRUE
                    
                )
              ),
              fluidRow(
                box(width=12,verbatimTextOutput("desSummary"),title = "Summary",status = 'primary',solidHeader = TRUE,  collapsible = TRUE)
                
                
              )#end fr
              
      ),
      
       
      
      # Tab Hypothesis
      tabItem(tabName = "tabHypothesis",
              fluidRow(
                box(width=12,
                    
                    box( solidHeader = TRUE,width=3,
                         numericInput("textSampleMean", "Sample Mean:",value=0)
                    ),
                    box(solidHeader = TRUE,width=3,
                        numericInput("textPopulationMean", "Population Mean:",value=0),
                    ),
                    box(solidHeader = TRUE,width=3,
                        numericInput("textSD", "Population SD:",value=0),
                    ),
                    box(solidHeader = TRUE,width=3,
                        numericInput("textSize", "Sample Size:",value=0),
                    )
                    
                )
              ),#end fr
              
              fluidRow(
                box(width=12,
                    
                    box(solidHeader = TRUE,width=4,
                        selectInput(inputId="selectTestType", label = ("Select Test"),
                                    choices = list("Two Tailed","Lower Tailed","Upper Tailed")
                        )
                        
                        
                        
                    ),
                    box(solidHeader = TRUE,width=4,
                        
                        sliderInput("sliderSig", "Significane Level:", 0, 0.25, .05)
                        
                        
                    ),
                    box(solidHeader = TRUE,width=4,tags$br(),
                        actionButton(inputId = "buttonGoHypo", 
                                     label = "Apply Hypothesis",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              
              fluidRow(
                box(width=12,verbatimTextOutput("hypoSummary"),title = "Summary",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                    
                ),
                box(width=12,plotOutput("hypoGraph"),title = "Probability Distribution",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                    
                )
                
                
              )#end fr    
      ),
      
      
     
      #Tab probability poisson
      tabItem(tabName = "tabPoisson",
              fluidRow(
                box(width=12,
                    box(solidHeader = TRUE,width=2,
                        numericInput("textLambdaPos", "Lambda:",value=0),
                    ),
                    box(solidHeader = TRUE,width=2,
                        numericInput("textUpperXPos", "Upper Limit X:",value=0),
                    ),
                    box(solidHeader = TRUE,width=2,
                        numericInput("textJPos", "J:",value=0),
                    ),
                    box(solidHeader = TRUE,width=4,
                        sliderInput("sliderPos", "Count Of Stimulated Data:", 1, 800, 80)
                    ),
                    
                    box(solidHeader = TRUE,width=2,tags$br(),
                        actionButton(inputId = "buttonGoPos", 
                                     label = "Apply",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              
              fluidRow(
                box(width=12,plotOutput("posGraph"),title = "Plots",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                )
                
                
              )#end fr
              
      ),
      tabItem(tabName = "tabBinomial",
              fluidRow(
                box(width=12,
                    box(solidHeader = TRUE,width=2,
                        numericInput("textPBin", "Parameter P:",value=0),
                    ),
                    box(solidHeader = TRUE,width=2,
                        numericInput("textNBin", "Parameter N:",value=0),
                    ),
                    box(solidHeader = TRUE,width=2,
                        numericInput("textUpperXBin", "Upper Limit X:",value=0),
                    ),
                    box(solidHeader = TRUE,width=2,
                        numericInput("textJBin", "J:",value=0),
                    ),
                    box(solidHeader = TRUE,width=4,
                        sliderInput("sliderBin", "Count Of Stimulated Data:", 1, 800, 80)
                    ),
                    
                    box(solidHeader = TRUE,width=2,tags$br(),
                        actionButton(inputId = "buttonGoBin", 
                                     label = "Apply",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                )
              ),#end fr
              
              fluidRow(
                box(width=12,plotOutput("binGraph"),title = "Plots",status = 'primary',solidHeader = TRUE,  collapsible = TRUE
                )
                
                
              )#end fr
              
      )
      
    )
  )
)



