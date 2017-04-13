library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  options(warn = -1)
  # Need to set working directory to create the data frame.
  require(ggplot2)
  output$Scatterplot <- renderPlot({
    g <- ggplot(dtf[dtf$coupleid == input$coupleid,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$dpredictor))), color = input$dpredictor)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$dpredictor))), color = input$dpredictor))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$ppredictor))), color = input$ppredictor)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$ppredictor))), color = input$ppredictor))
    g <- g  + 
      labs(title = paste0("Patient = ", input$dpredictor, ", Partner = ", input$ppredictor), 
           x = "Time(day)", y = "Rating",
           subtitle = "Patient in red, Partner in blue") + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
  
  output$Scatterplot2 <- renderPlot({
    g <- ggplot(dtf[dtf$coupleid == input$coupleid2,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$dpredictor1))), color = input$dpredictor1)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$dpredictor1))), color = input$dpredictor1))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$dpredictor2))), color = input$dpredictor2)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$dpredictor2))), color = input$dpredictor2))
    g <- g  + 
      labs(title = "Patient vs. Patient", 
           x = "Time", y = "Level") + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
  
  output$Scatterplot3 <- renderPlot({
    g <- ggplot(dtf[dtf$coupleid == input$coupleid3,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$ppredictor1))), color = input$ppredictor1)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$ppredictor1))), color = input$ppredictor1))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$ppredictor2))), color = input$ppredictor2)) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$ppredictor2))), color = input$ppredictor2))
    g <- g  + 
      labs(title = "Partner vs. Partner", 
           x = "Time", y = "Level") + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
  
  output$Scatterplot4 <- renderPlot({
    g <- ggplot(dtf[dtf$coupleid == input$coupleid4,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$dpredictorc))), 
                            shape = eval(parse(text = input$cc0))),
                        size = 3) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$dpredictorc)))))
    g <- g  + 
      labs(title = "Patient vs. Communal Coping", 
           x = "Time", y = input$dpredictorc,
           shape = input$cc0) + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
  
  output$Scatterplot5 <- renderPlot({
    g <- ggplot(dtf[dtf$coupleid == input$coupleid5,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$ppredictorc))), 
                            shape = eval(parse(text = input$cc1))),
                        size = 3) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$ppredictorc)))))
    g <- g  + 
      labs(title = "Patient vs. Communal Coping", 
           x = "Time", y = input$ppredictorc,
           shape = input$cc1) + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
  
  nrand <- eventReactive(input$randButt, {
    input$numcouples
  })
  
  output$Scatterplot6 <- renderPlot({
    nrand()
    couples <- sample(unique(dtf$coupleid), input$numcouples)
    g <- ggplot(dtf[dtf$coupleid %in% couples,], aes(x = as.numeric(time)))
    g <- g + geom_point(aes(y = as.numeric(eval(parse(text = input$var1))), 
                            color = factor(coupleid)),
                        size = 2) + 
      geom_line(aes(y = as.numeric(eval(parse(text = input$var1))), 
                    color = factor(coupleid)), alpha = .5)
    g <- g  + 
      labs(title = "Multiple Couples", 
           x = "Time", y = "Level") + 
      scale_y_continuous(limits = c(1,5)) + 
      scale_x_continuous(breaks = 1:16) +
      theme_bw()
    g
  })
})
