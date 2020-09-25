
# Load libraries needed
library(shiny)
library(ggplot2)
library(purrr)
library(rootSolve)
source("Rcode.r")
library(plotly)
library(dplyr)

spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Spruce Data Set: Piecewise Regression"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("xk",
                  "Choose first knot:",
                  min = min(d),
                  max = max(d),
                  value = 17.44165,
                  step=0.01),
      sliderInput("xk2",
                  "Choose second knot:",
                  min = min(d),
                  max = max(d),
                  value = 20,
                  step=0.01),
      sliderInput("intervalroot",
                  "choose lower and upper bounds for root interval:",
                  min = min(d),
                  max = max(d),
                  value = c(15,17.55),
                  step=0.01)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regressPlot"),
      plotlyOutput("R2"),
      tableOutput("roots"),
      # table of data
      tableOutput("tab"),
      plotOutput("ideal")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tab <- renderTable(spruce.df)
  
  
  
  output$regressPlot <- renderPlot({
    plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
    sp2.df=within(spruce.df, X<-(BHDiameter-input$xk)*(BHDiameter>input$xk))  
    sp3.df = within(sp2.df, X2<- (BHDiameter-input$xk2)*(BHDiameter>input$xk2))
    lmp=lm(Height ~ BHDiameter + X + X2, data=sp3.df)
    tmp = summary(lmp)
    
    curve(myf2(x,xk=input$xk,xk2 = input$xk2, coef=tmp$coefficients[,"Estimate"] ),
          add=TRUE, 
          lwd=2,
          col="Blue")
    
    points(input$xk,myf(x = input$xk,xk =input$xk,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2) 
    
    
    points(input$xk2,myf2(input$xk2,input$xk,input$xk2,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="orange",cex=2)
    
    text(input$xk,16,
         paste("R sq.=",round(tmp$r.squared,4) ))
    
  }) 
  
  # show linear regression with knots to maximize r^2
  
  output$ideal <- renderPlot({
    plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
    sp2.df=within(spruce.df, X<-(BHDiameter-xy[knot, ]$Var1)*(BHDiameter>xy[knot, ]$Var1))  
    sp3.df = within(sp2.df, X2<- (BHDiameter-xy[knot, ]$Var2)*(BHDiameter>xy[knot, ]$Var2))
    lmp=lm(Height ~ BHDiameter + X + X2, data=sp3.df)
    tmp = summary(lmp)
    curve(myf2(x,xk=xy[knot, ]$Var1,xk2 = xy[knot, ]$Var2, coef=tmp$coefficients[,"Estimate"] ),
          add=TRUE, 
          lwd=2,
          col="Green")
  
  })
  

  output$R2 <- renderPlotly({
    xknots <- seq(min(spruce.df$BHDiameter), max(spruce.df$BHDiameter), length = 300)
    xy = expand.grid(xknots,xknots)
    z <- map2_dbl(xy$Var1, xy$Var2, ~rsq2(.x, .y, spruce.df))
    df <- data.frame(x = xy$Var1, y = xy$Var2, z=z)
    dff <- df %>% filter(z>=0.77)
    plot_ly(data=dff, x= dff$x, y= dff$y, z = dff$z,
            type="mesh3d")
  })
  
  #find knots at maximum z value
  output$roots <- renderTable(
    knot = which.max(z),
    tab = matrix(c("Knot 1", "Knot 2", xy[knot, ]$Var1, xy[knot,]$Var2), nrow = 2, byrow = FALSE),
    tab <- as.table(tab)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)




