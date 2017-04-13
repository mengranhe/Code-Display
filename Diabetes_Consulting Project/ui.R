library(shiny)
library(shinydashboard)
# setwd("~/S17/36726/Diabeetus/726-Project")
source("Shiny.R")

shinyUI(dashboardPage(
  dashboardHeader(title = "Psychological Behavior Diagnosis Graphics", 
                  titleWidth = 500),
  dashboardSidebar(sidebarMenu(
    menuItem("Patient vs. Partner", tabName = "a", icon = icon("th")),
    menuItem("Patient vs. Patient", tabName = "b", icon = icon("th")),
    menuItem("Partner vs. Partner", tabName = "f", icon = icon("th")),
    menuItem("Patient vs. Communal Coping", tabName = "c", icon = icon("th")),
    menuItem("Partner vs. Communal Coping", tabName = "e", icon = icon("th")),
    menuItem("One Variable, Multiple Patients", tabName = "d", icon = icon("th"))
    )),
  dashboardBody(tabItems(
    tabItem(tabName = "a", 
            fluidPage(box(title = "Patient vs. Partner", 
                         id = "tabset1", 
                         selected = "Tab1",
                         tabPanel("Tab1",
                                  helpText("Choose Couple's ID and their mood, Mood rating is average value of similar mood categories, rated from 1 to 5"),
                                  sliderInput("coupleid", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
                                              value = 1, step = 1),
                                  selectInput("dpredictor", label = "Choose patient's mood:", 
                                              choices = list("Happiness" = "isdhappy", "Depression" = "isddepr",
                                                             "Anger" = "isdanger", "Anxiousness" = "isdanx",
                                                             "Supportive" = "isdunsupp", "Avoidant" = "isdavoid",
                                                             "Minimize" = "isdminimize", "Instrumental support" = "isdinstr",
                                                             "Emotional" = "isdemotional"), 
                                              selected = "isdhappy"),
                                  selectInput("ppredictor", label = "Choose partner's mood:", 
                                              choices = list("Happiness" = "isphappy", "Depression" = "ispdepr",
                                                             "Anger" = "ispanger", "Anxiousness" = "ispanx",
                                                             "Supportive" = "ispunsupp", "Avoidant" = "ispavoid",
                                                             "Minimize" = "ispminimize", "Instrumental support" = "ispinstr",
                                                             "Emotional" = "ispemotional"), 
                                              selected = "isphappy")
                                  )
                         ),
                     plotOutput(outputId = "Scatterplot", height = "190px")
                     )
            ),
    tabItem(tabName = "b",
            fluidPage(box(title = "Patient vs. Patient",
                          id = "tabset1", 
                          selected = "Tab1",
                          tabPanel("Tab1",
                                   sliderInput("coupleid2", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
                                               value = 1, step = 1),
                                   selectInput("dpredictor1", label = "Choose patient's mood:", 
                                               choices = list("Happiness" = "isdhappy", "Depression" = "isddepr",
                                                              "Anger" = "isdanger", "Anxiousness" = "isdanx",
                                                              "Supportive" = "isdunsupp", "Avoidant" = "isdavoid",
                                                              "Minimize" = "isdminimize", "Instrumental support" = "isdinstr",
                                                              "Emotional" = "isdemotional"), 
                                               selected = "isdhappy"),
                                   selectInput("dpredictor2", label = "Choose patients's mood:", 
                                               choices = list("Happiness" = "isdhappy", "Depression" = "isddepr",
                                                              "Anger" = "isdanger", "Anxiousness" = "isdanx",
                                                              "Supportive" = "isdunsupp", "Avoidant" = "isdavoid",
                                                              "Minimize" = "isdminimize", "Instrumental support" = "isdinstr",
                                                              "Emotional" = "isdemotional"), 
                                               selected = "isdhappy")
                          )
            ),
            plotOutput(outputId = "Scatterplot2", height = "250px")
            )),
    tabItem(tabName = "f",
            fluidPage(box(title = "Partner vs. Partner",
                          id = "tabset1", 
                          selected = "Tab1",
                          tabPanel("Tab1",
                                   sliderInput("coupleid3", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
                                               value = 1, step = 1),
                                   selectInput("ppredictor1", label = "Choose partner's mood:", 
                                               choices = list("Happiness" = "isphappy", "Depression" = "ispdepr",
                                                              "Anger" = "ispanger", "Anxiousness" = "ispanx",
                                                              "Supportive" = "ispunsupp", "Avoidant" = "ispavoid",
                                                              "Minimize" = "ispminimize", "Instrumental support" = "ispinstr",
                                                              "Emotional" = "ispemotional"), 
                                               selected = "isphappy"),
                                   selectInput("ppredictor2", label = "Choose partners's mood:", 
                                               choices = list("Happiness" = "isphappy", "Depression" = "ispdepr",
                                                              "Anger" = "ispanger", "Anxiousness" = "ispanx",
                                                              "Supportive" = "ispunsupp", "Avoidant" = "ispavoid",
                                                              "Minimize" = "ispminimize", "Instrumental support" = "ispinstr",
                                                              "Emotional" = "ispemotional"), 
                                               selected = "isphappy")
                          )),
                          plotOutput(outputId = "Scatterplot3", height = "250px")
                          )),
    tabItem(tabName = "c",
            fluidPage(box(title = "Patient vs. Communal Coping",
                          id = "tabset1", 
                          selected = "Tab1",
                          tabPanel("Tab1",
                                   sliderInput("coupleid4", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
                                               value = 1, step = 1),
                                   selectInput("dpredictorc", label = "Choose patients's mood:", 
                                               choices = list("Happiness" = "isdhappy", "Depression" = "isddepr",
                                                              "Anger" = "isdanger", "Anxiousness" = "isdanx",
                                                              "Supportive" = "isdunsupp", "Avoidant" = "isdavoid",
                                                              "Minimize" = "isdminimize", "Instrumental support" = "isdinstr",
                                                              "Emotional" = "isdemotional"), 
                                               selected = "isdhappy"),
                                   selectInput("cc0", label = "Choose communal coping variable:", 
                                               choices = list("Patient Problem Handling" = "isdhandler", 
                                                              "Partner Problem Handling" = "isphandler",
                                                              "Patient View of Diabetes" = "isdcc1r", 
                                                              "Partner View of Diabetes" = "ispcc1r",
                                                              "Patient Work Together" = "isdcc2", 
                                                              "Partner Work Together" = "ispcc2"), 
                                               selected = "isdhandler")
                          )),
                      plotOutput(outputId = "Scatterplot4", height = "250px")
                      )),
    tabItem(tabName = "e",
            fluidPage(box(title = "Partner vs. Communal Coping",
                          id = "tabset1", 
                          selected = "Tab1",
                          tabPanel("Tab1",
                                   sliderInput("coupleid5", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
                                               value = 1, step = 1),
                                   selectInput("ppredictorc", label = "Choose patients's mood:", 
                                               choices = list("Happiness" = "isphappy", "Depression" = "ispdepr",
                                                              "Anger" = "ispanger", "Anxiousness" = "ispanx",
                                                              "Supportive" = "ispunsupp", "Avoidant" = "ispavoid",
                                                              "Minimize" = "ispminimize", "Instrumental support" = "ispinstr",
                                                              "Emotional" = "ispemotional"), 
                                               selected = "isphappy"),
                                   selectInput("cc1", label = "Choose communal coping variable:", 
                                               choices = list("Patient Problem Handling" = "isdhandler", 
                                                              "Partner Problem Handling" = "isphandler",
                                                              "Patient View of Diabetes" = "isdcc1r", 
                                                              "Partner View of Diabetes" = "ispcc1r",
                                                              "Patient Work Together" = "isdcc2", 
                                                              "Partner Work Together" = "ispcc2"), 
                                               selected = "isdhandler")
                          )),
                      plotOutput(outputId = "Scatterplot5", height = "250px")
                      )),
    tabItem(tabName = "d",
            fluidPage(box(title = "One Variable",
                          id = "tabset1", 
                          selected = "Tab1",
                          tabPanel("Tab1",
                                   sliderInput("numcouples", "Number of Couples", min = 1, max = 10,
                                               value = 3, step = 1),
                                   selectInput("var1", label = "Choose mood variable:", 
                                               choices = as.list(colnames(dtf[-c(1:7)])), 
                                               selected = "isdhandler"),
                                   actionButton("randButt", "Roll")
                          )),
                      plotOutput(outputId = "Scatterplot6", height = "250px")
                      ))
  )
  )
))

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   # Application title
#   titlePanel("Psychological Behavior Diagnosis Graphics"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Choose Couple's ID and their mood, Mood rating is average value of 
#                similar mood categories, rated from 1 to 5"),
#       sliderInput("coupleid", "Couple's ID", min = min(unique(dtf$coupleid)), max = max(unique(dtf$coupleid)),
#                   value = 1, step = 1),
#       selectInput("dpredictor", label = "Choose patient's mood:", 
#                   choices = list("Happiness" = "isdhappy", "Depression" = "isddepr",
#                                  "Anger" = "isdanger", "Anxiousness" = "isdanx",
#                                  "Supportive" = "isdunsupp", "Avoidant" = "isdavoid",
#                                 "Minimize" = "isdminimize", "Instrumental support" = "isdinstr",
#                                 "Emotional" = "isdemotional"), 
#                   selected = "isdhappy"),
#       selectInput("ppredictor", label = "Choose partner's mood:", 
#                   choices = list("Happiness" = "isphappy", "Depression" = "ispdepr",
#                                  "Anger" = "ispanger", "Anxiousness" = "ispanx",
#                                  "Supportive" = "ispunsupp", "Avoidant" = "ispavoid",
#                                  "Minimize" = "ispminimize", "Instrumental support" = "ispinstr",
#                                  "Emotional" = "ispemotional"), 
#                   selected = "isphappy")
#       ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        h2("Independent Variables Investigation", align = "center"),
#        plotOutput("Scatterplot")
#     )
#   )
# ))
