require(mgcv)
require(caret) 
require(kernlab)
require(xlsx)
require(plyr)
require(dplyr)
require(car)
require(MASS)
require(ggplot2)
require(corrgram)
require(MASS)
require(gvlma)
require(randomForest)
require(corrplot)
require(reshape)
require(party)
require(doParallel)


##  reading the data 


load('ep.RData',envir=.GlobalEnv)

shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    paste("You have selected", round((6*(770^0.6666667))/input$SA,2),"as Relative Compactness")})
  output$text2 <- renderText({ 
    paste("You have selected", input$SA,"m2 as Surface Area")})
  output$text3 <- renderText({ 
    paste("You have selected", input$WA,"m2 as Wall Area")})
  output$text4<- renderText({ 
    paste("You have selected", input$RA,"m2 as Roof Area")})
  output$text5 <- renderText({ 
    paste("You have selected", input$OH,"m as Overall Height")})
  output$text6 <- renderText({ 
    paste("You have selected", input$OR,"as Orientation")})
  output$text7 <- renderText({ 
    paste("You have selected", input$GA,"as Glazing Area")})
  output$text8 <- renderText({ 
   paste("You have selected", input$GAD,"as Glazing Area Distribution")})
  output$rmserror <- renderText({ 
  paste("The Root mean square error for heating load is ", round(rmsecombPredH,2),"kW","and for cooling load is",round(rmsecombPredC,2),"kW")})
  
  output$PredH <- renderText({
  
   df <- data.frame((6*(770^0.6666667))/input$SA,input$SA,input$WA,input$RA,input$OH,input$OR,input$GA,input$GAD)
   paste("Heating Load Predicted", round(energyPredH(df),digits=2),"KW")
  })
   output$PredC <- renderText({
   df <- data.frame((6*(770^0.6666667))/input$SA,input$SA,input$WA,input$RA,input$OH,input$OR,input$GA,input$GAD)
   paste("Cooling Load Predicted", round(energyPredC(df),digits=2),"KW")
   
  })  
  
  output$PlotH <- renderPlot({
    p
    })
  
  output$corrp <- renderPlot({
      corrplot(M, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, 
             tl.col = rgb(0, 0, 0))
      mtext("Spearman's Correlation Map", side=3, outer=TRUE, line=-3,cex=1.5) 
        
      
  })
  
  output$rcpic <- renderImage({
    img('RC1.png')
    
  })
  
  
  output$Pairplot <-  renderPlot({pairs(trainTransformedf, panel = panel.smooth,
        cex = 1.5, pch = 20, bg = "light blue",
        diag.panel = panel.hist, cex.labels = 1, font.labels = 2)
  })
 
  
  
  
  output$residp <-  renderPlot({
    par(mar=c(1,1,1,1))
    par(mfrow=c(2,2))
    plot(fitH)
    
                                 
                                 })
  
  
  
  output$table <- renderDataTable({
  
    dataD2
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataD2, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataD2, file)
    }
  )
  
}
)