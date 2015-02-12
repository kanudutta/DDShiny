
 library(shiny)
 
   shinyUI(fluidPage(navbarPage("Load Prediction Dashboard",
    
                                
                                tabPanel("Results",
                                         titlePanel("Interactive Building Heating and Cooling Load Prediction"),
                                         sidebarLayout( 
                                           sidebarPanel(
                                             sliderInput('SA','Surface Area',value = 550, min = 515, max = 805, step = 5,animate = TRUE),
                                             sliderInput('WA','Wall Area',value = 245, min = 245, max = 415, step = 5,animate = TRUE),
                                             sliderInput('RA','Roof Area',value = 120, min = 110.5, max = 220, step = 10,animate = TRUE),
                                             sliderInput('OH','Overall Height',value = 4, min = 3.5, max = 7, step = 0.5,animate = TRUE),
                                             sliderInput('OR','Orientation',value = 3, min = 1, max = 4, step = 1,animate = TRUE),
                                             sliderInput('GA','Glazing Area % of floor area',value = 0.1, min = 0, max = 0.4, step = 0.1,animate = TRUE),
                                             sliderInput('GAD','Glazing Area Distribution',value = 1, min = 0, max = 5, step = 1,animate = TRUE)
                                             
                                           ),
                                           
                                           
                                           mainPanel(
                                             h3('Energy performance estimation of residential buildings using Machine Learning Techniques '),
                                             h5('1) Models were created using Neural Networks,K Nearest Neighbour,Support Vector Regression,Random Forest and Bagged Tree'),
                                             h5('2) Ensemble Stacking used to combine the models '),
                                             h4('Result of Prediction :-'),
                                             h4(textOutput('PredH')),
                                             h4(textOutput('PredC')),
                                             h6(textOutput('rmserror')),
                                             h5('Your selection:-'),
                                             textOutput('text1'),
                                             textOutput('text2'),
                                             textOutput('text3'),
                                             textOutput('text4'),
                                             textOutput('text5'),
                                             textOutput('text6'),
                                             textOutput('text7'),
                                             textOutput('text8'),
                                             h5('3) For detailed and completed analysis run the R code from GitHub '),
                                             h6('Relative compactness is given by RC=6*(V^0.66))/A, The volume of building is assumed constant, therefore we see negative correlation between Area and Height'),
                                             h5('Shiny and R implementation by Kanu Dutta, Email: kandutta@gmail.com')
                                             
                                             
                                             
                                           )
                                         )
                                ),
                                tabPanel("App Guide",
                                         titlePanel(""),
                                         mainPanel(
                                           includeHTML("Guide.html")
                                         )),
                                
                                tabPanel("Plots ",
                                         titlePanel("Plots"),
                                         mainPanel(
                                           tabsetPanel(
                                             
                                             tabPanel("Variable Importance", plotOutput('PlotH')),
                                             tabPanel("Correlation Map", plotOutput('corrp')),
                                             tabPanel("Pairwise Scatter Plot", plotOutput('Pairplot')),
                                             tabPanel("Residual Plot",plotOutput('residp'))
                                                                            
                                             ))),
                                                                            
                                                                            tabPanel("Assumptions",titlePanel("Assumptions and Choices Made"),
                                                                            mainPanel(
                                                                            h5('The data was generated using  Ecotect Simulation, and following assumptions and selections were made:-'),
                                                                            h5('The internal design condition were set as :- 0.6 clo, humidity: 60%,air speed: 0.30 m/s, lighting level: 300 Lux.The internal gains were set to sensible 5 and latent 2 W/m2'),
                                                                            h5('The infiltration rate was set at 0.5 for air change, thermostat range 19-24 deg-C,15-20 hrs operation on weekdays and 10-20 during weekends '),
                                                                            h5('The simulation assumes that the buildings are in Athens,Greece,residential with sven people and sedentary activity 70W'),
                                                                            h5('The newest and most common materials in the building construction industry and lowest U value  ')
                                                                            )),
                                                                            
                                                                            tabPanel("Citation ",
                                                                            titlePanel("Reference material:-"),
                                                                            mainPanel(
                                                                            h4('Reference:-'),
                                                                            h5('Jui-Sheng Chou, Dac-Khuong Bui: Modeling heating and cooling loads by artificial intelligence forenergy-efficient building design'),
                                                                            h5('A. Tsanas, A. Xifara: Accurate quantitative estimation of energy performance of residential buildings using statistical machine learning tools, Energy and Buildings, Vol. 49, pp. 560-567, 2012'),
                                                                            h5('Data Source :The dataset was created by Angeliki Xifara, angxifara@gmail.com, Civil/Structural Engineer, and was processed by Athanasios Tsanas tsanasthanasis@gmail.com, Oxford Centre for Industrial and Applied Mathematics, University of Oxford, UK')
                                                                            )),
                                                                            
                                                                            tabPanel("Building Shape RC",
                                                                            titlePanel("Building Shape measured as Relative Compactness"),
                                                                            mainPanel(
                                                                            h4('Building Shapes for reference:- '),
                                                                            img(src = "RC.PNG",height = 1000, width = 800)
                                                                            )),      
                                                                            
                                                                            tabPanel("Data", fluidRow(downloadButton('downloadData', 'Download'),
                                                                            dataTableOutput(outputId="table")))
                                                                            
                                                                            
                                                                            
                                
   
               

         
   )))