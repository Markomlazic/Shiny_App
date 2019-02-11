#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  mtcars$dispsp <- ifelse(mtcars$disp - 200 > 0, mtcars$disp - 200, 0)
  model1 <- lm(hp ~ disp, data = mtcars)
  model2 <- lm(hp ~ dispsp + disp, data = mtcars)
  
  model1pred <- reactive({
    dispInput <- input$sliderDisp
    predict(model1, newdata = data.frame(disp = dispInput))
  })
    
  model2pred <- reactive({
    dispInput <- input$sliderDisp
    predict(model2, newdata = data.frame(disp = dispInput, dispsp = ifelse(dispInput - 200 > 0, dispInput - 200, 0)))
    
  })
  
  output$plot1 <- renderPlot({
    dispInput <- input$sliderDisp
    
    plot(mtcars$disp, mtcars$hp, xlab = "Displacement", ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(50, 700), ylim = c(50, 350))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        disp = 50:700, dispsp = ifelse(50:700 - 200 > 0, 50:700 - 200, 0)
      ))
      lines(50:700, model2lines, col = "blue", lwd = 2)
    }
    legend(50, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16,
           col = c("red", "blue"), bty = "n", cex = 1.2)
    points(dispInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(dispInput, model2pred(), col = "blue", pch = 16, cex = 2)
  })
  
  output$pred1 <- renderText({
    model1pred()
  })
  
  output$pred2 <- renderText({
    model2pred()
  })
})
  
  