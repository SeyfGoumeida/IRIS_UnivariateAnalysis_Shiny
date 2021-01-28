library(shiny)

ui <- fluidPage(

    titlePanel("TP IRIS {GOUMEIDA AHMED SEYFEDDINE - MLDS}"),
    
    
    
    sidebarLayout(
        
        sidebarPanel(
            fluidRow(
                column(12, 
                       htmlOutput(outputId = "tp")  
                )
            ),
    
            
        ),
        mainPanel(
            fluidRow(
                column(12, 
                   mainPanel(
                       tabsetPanel(
                           id = 'dataset',
                           tabPanel("IRIS", 
                                    DT::dataTableOutput("mytable3")
                                    ),
                           
                           tabPanel("BoxPlot",
                                    plotOutput(outputId = "boxplot"),
                                    plotOutput(outputId = "boxplotSpecies")
                                    ),
                           tabPanel("Hostogram", 
                                    fluidRow(
                                            column(6, 
                                               plotOutput(outputId = "HistogramPW"),
                                               sliderInput("bins",
                                                           "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 5)
                                               ),
                                            column(6, 
                                               plotOutput(outputId = "HistogramSW"),
                                               sliderInput("bins2",
                                                           "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 5)
                                            )
                                    ),
                                    fluidRow(
                                            column(6, 
                                               plotOutput(outputId = "HistogramPL"),
                                               sliderInput("bins3",
                                                           "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 5)
                                                ),
                                            column(6, 
                                               plotOutput(outputId = "HistogramSL"),
                                               sliderInput("bins4",
                                                           "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 5)
                                            )
                                    )
                                
                                ),
                           tabPanel("Summary", 
                                    fluidRow(
                                      column(4, 
                                             htmlOutput(outputId = "Mean")
                                      ),
                                      column(4, 
                                             htmlOutput(outputId = "Median")
                                      ),
                                      column(4, 
                                             htmlOutput(outputId = "first_Quartile")
                                      )
                                    ),
                                    fluidRow(
                                      column(4, 
                                             htmlOutput(outputId = "Max")
                                      ),
                                      column(4, 
                                             htmlOutput(outputId = "Min")
                                      ),
                                      column(4, 
                                             htmlOutput(outputId = "third_Quartile")
                                      )
                                    )
                                    
                           ),
                           tabPanel("Pie", 
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Pie")
                                      )
                                    ),
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Pie2")
                                      )
                                    ),
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Pie3")
                                      )
                                    )
                                    
                           ),
                           tabPanel("Pairs", 
                                    
                                    plotOutput(outputId = "Pairs")
                           ),
                           tabPanel("Nuage", 
                                    fluidRow(
                                      column(6, 
                                             plotOutput(outputId = "Nuage")
                                             
                                      ),
                                      column(6, 
                                             plotOutput(outputId = "Nuage2")
                                             
                                      ),
                                      column(6, 
                                             plotOutput(outputId = "Nuage3")
                                             
                                      ),
                                      column(6, 
                                             plotOutput(outputId = "Nuage4")
                                             
                                      )
                                    )
                           ),
                           tabPanel("About", 
                                    
                                    htmlOutput(outputId = "About")
                           )
                           
                       )
                   )
                  )
            )
        
    ))
)

#------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  
    output$tp<- renderUI({
      HTML(
        paste("<h2>TP1 IRIS</h2>"),
        paste("<h3>Objectif: </h3>"),
        paste("<h4> Creer une shinyapp permettant l’exploration univarie d’un jeu de donnees comportant des variables quantitatives et qualitatives.Votre analyse devra etremise en ligne via votre compte shiny sur shinyapps.io.</h4>" ),
        paste("<ul>
                <li> La liste des jeux de donn´ees possibles est disponible sur mon site google, dans la sectionTeaching ! Datasets mini-projet.</li>
                <li> Vos analyses doivent reprendre les diff´erents graphiques et coefficients pr´esent´es dans les slides de cours (Univariate analysis).</li>
                <li> Il est n´ecessaire de cr´eer un compte sur shinyapps.io pour charger et lancer votre application finale.</li>
                <li> Il est n´ecessaire de cr´eer un compte sur shinyapps.io pour charger et lancer votre application finale.</li>
              </ul>"
              ),
        paste("<h3>---------------------------------------------</h3>"),
        
        paste("<ol>
                <li> Choisir un jeu de donn´ees (parmi ceux propos´es ou un jeu de donn´ees de votre choix).</li>
                <li> Faire l’analyse univari´ee des variables.</li>
                <li> Restituer l’´etude dans une shinyapp qui sera charg´ee dans votre compte shiny</li>
                <li> Pr´evoir un espace d’aide o`u vous donnerez la probl´ematique du jeu de donn´ees et un bref descriptif des donn´ees.</li>
              </ol>"
        )
        )
    })
  
  
  
    # customize the length drop-down menu; display 5 rows per page by default
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(iris, options = list(lengthMenu = c(5,10,20, 30, 50,100,150), pageLength = 10))
    })
    
    #BOXPLOT
    output$boxplot <- renderPlot({
        boxplot(iris$Petal.Width,iris$Sepal.Width,iris$Petal.Length,iris$Sepal.Length,
                at = c(1,2,5,6),
                names = c("Petal.W", "Sepal.W", "Petal.L", "Sepal.L"),
                col = c("orange","red","orange","red"),
                main = "IRIS BOXPLOT",
                xlab = "Mesures(centimeters [CM])",las = 1,
                border = "brown",
                notch = TRUE,
                horizontal = TRUE
        )
    })
    #HISTOGRAM
    
    output$HistogramPW <- renderPlot({
      
 
      
      
        pw = iris$Petal.Width
        bins <- seq(min(pw), max(pw), length.out = input$bins + 1)
        hist(pw,
             breaks = bins,
             col = "orange",
             main = "Number of Flowers by Petal Width",
             xlab = "Petal Width",
             ylab = "Number of Flowers")

    })
    
    output$HistogramSW <- renderPlot({
        sw = iris$Sepal.Width
        bins <- seq(min(sw), max(sw), length.out = input$bins2 + 1)
        hist(sw,
             breaks = bins,
             col = "orange",
             main = "Number of Flowers by Sepal Width",
             xlab = "Sepal Width",
             ylab = "Number of Flowers")
        
    })
    
    output$HistogramPL <- renderPlot({
        pl = iris$Petal.Length
        bins <- seq(min(pl), max(pl), length.out = input$bins3 + 1)
        
        hist(pl,             
             breaks = bins,
             col = "orange",
             main = "Number of Flowers by Petal Length",
             xlab = "Petal Length",
             ylab = "Number of Flowers")
        
    })
    
    output$HistogramSL <- renderPlot({
        sl = iris$Sepal.Length
        bins <- seq(min(sl), max(sl), length.out = input$bins4 + 1)
        
        hist(sl,
             breaks = bins,
             col = "orange",
             main = "Number of Flowers by Sepal Length",
             xlab = "Sepal Length",
             ylab = "Number of Flowers")
        
    })
    
    
    #-------------------------------Summary----------------------------
    #pairs(iris[,1:4])
    #k = describe(iris) 
    k = summary(iris)
    result.meansl <- mean(iris$Sepal.Length)
    result.meanpl <- mean(iris$Petal.Length)
    result.meanpw <- mean(iris$Petal.Width)
    result.meansw <- mean(iris$Sepal.Width)
    
    result.mediansl <- median(iris$Sepal.Length)
    result.medianpl <- median(iris$Petal.Length)
    result.medianpw <- median(iris$Petal.Width)
    result.mediansw <- median(iris$Sepal.Width)
    
    result.maxsl <- max(iris$Sepal.Length)
    result.maxpl <- max(iris$Petal.Length)
    result.maxpw <- max(iris$Petal.Width)
    result.maxsw <- max(iris$Sepal.Width)
    
    result.minsl <- min(iris$Sepal.Length)
    result.minpl <- min(iris$Petal.Length)
    result.minpw <- min(iris$Petal.Width)
    result.minsw <- min(iris$Sepal.Width)
    
    k1 = quantile(iris$Sepal.Length)
    k2 = quantile(iris$Petal.Length)
    k3 = quantile(iris$Petal.Width)
    k4 = quantile(iris$Sepal.Width)
    
    result.first_Quartilesl <- min(k1["25%"])
    result.first_Quartilepl <- min(k2["25%"])
    result.first_Quartilepw <- min(k3["25%"])
    result.first_Quartilesw <- min(k4["25%"])
    
    result.third_Quartilesl <- min(k1["75%"])
    result.third_Quartilepl <- min(k2["75%"])
    result.third_Quartilepw <- min(k3["75%"])
    result.third_Quartilesw <- min(k4["75%"])
    # k = summarize(iris,mean(height,na.rm=TRUE))
    
    
    
    output$Mean <- renderUI({
      HTML(
        paste("<h2>MEAN</h2>"),
        paste("<h4>Sepal Length :",result.mediansl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.mediansw,"</h4>"),
        paste("<h4>Petal Length :",result.medianpl,"</h4>"),
        paste("<h4>Petal Width  :",result.medianpw,"</h4>")
        )
        })
    output$Median <- renderUI({
      HTML(
        paste("<h2>MEDIAN</h2>"),
        paste("<h4>Sepal Length :",result.meansl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.meansw,"</h4>"),
        paste("<h4>Petal Length :",result.meanpl,"</h4>"),
        paste("<h4>Petal Width  :",result.meanpw,"</h4>")
      )
    })
    output$Max<- renderUI({
      HTML(
        paste("<h2>MAX</h2>"),
        paste("<h4>Sepal Length :",result.maxsl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.maxsw,"</h4>"),
        paste("<h4>Petal Length :",result.maxpl,"</h4>"),
        paste("<h4>Petal Width  :",result.maxpw,"</h4>")
      )
    })
    output$Min<- renderUI({
      HTML(
        paste("<h2>MIN</h2>"),
        paste("<h4>Sepal Length :",result.minsl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.minsw,"</h4>"),
        paste("<h4>Petal Length :",result.minpl,"</h4>"),
        paste("<h4>Petal Width  :",result.minpw,"</h4>")
      )
    })
    output$first_Quartile<- renderUI({
      HTML(
        paste("<h2>FIRST QUARTILE</h2>"),
        paste("<h4>Sepal Length :",result.first_Quartilesl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.first_Quartilesw,"</h4>"),
        paste("<h4>Petal Length :",result.first_Quartilepl,"</h4>"),
        paste("<h4>Petal Width  :",result.first_Quartilepw,"</h4>")
      )
    })
    output$third_Quartile<- renderUI({
      HTML(
        paste("<h2>THIRD QUARTILE</h2>"),
        paste("<h4>Sepal Length :",result.third_Quartilesl,"</h4>" ),
        paste("<h4>Sepal Width  :",result.third_Quartilesw,"</h4>"),
        paste("<h4>Petal Length :",result.third_Quartilepl,"</h4>"),
        paste("<h4>Petal Width  :",result.third_Quartilepw,"</h4>")
      )
    })
    #-------------------------Pie--------------------------------------
    output$Pie <- renderPlot({
      pie(table(iris$Species), labels = names(table(iris$Species)), 
          main = "Species", col=c())    
      })
    
    output$Pie2 <- renderPlot({
      pie(table(iris$Sepal.Length), labels = names(table(iris$Sepal.Length)), 
          main = "Sepal.Length", col=c())    
    })
    
    output$Pie3 <- renderPlot({
      pie(table(iris$Petal.Length), labels = names(table(iris$Petal.Length)), 
          main = "Petal.Length", col=c())    
    })
    #----------------------PAIRS--------------------------------------
    output$Pairs <- renderPlot({
      
      pairs(iris[,1:4])
      
    })
    #-----------------------NUAGE---------------------------------------
    library(ggplot2)
    
    output$Nuage <- renderPlot({
      # Basic scatter plot
      ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
      
    })
    output$Nuage2 <- renderPlot({
      # Basic scatter plot
      ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point()
      
    })
    output$Nuage3 <- renderPlot({
      # Basic scatter plot
      ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_point()
      
    })
    output$Nuage4 <- renderPlot({
      # Basic scatter plot
      ggplot(iris, aes(x=Sepal.Width, y=Petal.Width)) + geom_point()
      
    })
    #----------------------ABOUT------------------------------------------
    output$About <- renderUI({
        
        HTML(
            paste("<h1>About</h1>"),
            paste("<h1>Edgar Anderson's Iris Data</h1>"),
            paste("<h2>Description</h2>"),
            paste("<p>This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.</p>"),
            paste("<h2>Usage</h2>"),
            paste("<p>iris , iris3</p>"),
            paste("<h2>Format</h2>"),
            paste("<p>iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
iris3 gives the same data arranged as a 3-dimensional array of size 50 by 4 by 3, as represented by S-PLUS. The first dimension gives the case number within the species subsample, the second the measurements with names Sepal L., Sepal W., Petal L., and Petal W., and the third the species.</p>"),
            paste("<h2>Source</h2>"),
            paste("<p>Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.

The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.</p>"),
            
            paste("<h2>References</h2>"),
            paste("<p>Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole. (has iris3 as iris.)</p>"),
            
            paste("<h2>See Also</h2>"),
            paste("<p>matplot some examples of which use iris.</p>"),
            
            paste("<h2>Examples</h2>")
            
            
            )
      
        
        })
    
}

shinyApp(ui, server)