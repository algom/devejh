#Shiny App code to generate a BMI chart with categories
#Load necessary libraries
library(shiny)
library(ggplot2)
library(scales)
library(reshape)
library(shinydashboard)

#Start of page ui.R
ui <- dashboardPage(
    
    #Application title.
    dashboardHeader(title=strong("Shiny BMI calculator")
    ),
    #Input of the weight and heigh
    dashboardSidebar(
        numericInput(inputId = "weight", label = "Weight in kg", value = 70, step=0.5),
        numericInput(inputId = "height", label = "Height in meters", value = 1.70, step=0.01),
        radioButtons("radio", label = "Gender",
                     choices = list("Male" = 1, "Female" = 2), 
                     selected = 1),
        numericInput(inputId = "age", label = "Age in years", value = 40, step=1),
        #Submit button so to isolate and only change when pressed
        sidebarMenu(
            menuItem("Press the button when ready"),
            menuItem(submitButton("Submit")),
            menuItem("When ready go to the Results tab"),
            menuItem("Code for App @ GitHub",href="https://github.com/algom/devejh", icon=icon("github"))
        )
    ),
    
    
    
    dashboardBody(
        #Make two tabs, one for info, other for the results  
        tabsetPanel(type = "tabs", id="tabs",
                    #About and Instructions            
                    tabPanel("About", value="panel1",
                             
                             h2(div(strong("Body Mass Index (BMI) shiny calculator", style="color:#104E8B"))),
                             h3("Instructions for using the calculator"),
                             h5("In order to use the calculator and evaluate the", strong("Body Mass Index (BMI)"), "a few parameters are needed,
                                and the following steps must be followed:"),
                             tags$ol(
                                 tags$li("Input the weight in", em("kg")),
                                 tags$li("Input the height in", em("meters")),
                                 tags$li("Then the submit button must be pressed"),
                                 tags$li("The results are presented in the", strong("Results"), "tab")
                             ),
                             h5("The BMI is a quantity value, calculated by dividing the weight (kg) over the square hight (m) of men and women.
                                Using the value obtained as", strong("BMI"), "an individual can be categorized as being:"),
                             tags$ul(
                                 tags$li(strong("Underweight"), "- below 18.5"),
                                 tags$li(strong("Normal weight"), "- between 18.5-25"),
                                 tags$li(strong("Overweight"), " - more than 25 but less than 30"),
                                 tags$li(strong("Obese"), "- more than 30")
                             ),
                             h5("Using the input the App calculates the BMI and reports:"),
                             
                             tags$ol(
                                 tags$li("The entered values"),
                                 tags$li("The calculated BMI"),
                                 tags$li("The category corresponding to the calculated BMI"),
                                 tags$li("Creates a BMI chart divided in the categories, with a point showing the location of the 
                                         calculated", strong("BMI")),
                                 tags$li("It also reports the Resting Metabolic Rate using the Mifflin - St Jeor equation")
                                 ),
                             
                             em("Disclaimer: Although the use of the BMI in different populations may be controversial, it gives an overview and can classify individuals with higher risk for cardiovascular diseases"),
                             hr(),
                             h3("References"),
                             a("http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2065990/", href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2065990/"),
                             br(),
                             a("http://www.ncbi.nlm.nih.gov/pubmed/15883556", href="http://www.ncbi.nlm.nih.gov/pubmed/15883556"),
                             hr(),
                             h5(em("Project for the", strong("Developing Data Products"), "class of the Johns Hopkins University Data Science specialization")),
                             h4("October 2015 by", strong("AG"))
                             ),
                    
                    tabPanel("Results", value="panel2",
                             fluidRow(
                                 #Results of what was entered
                                 valueBoxOutput("we", width=3),
                                 valueBoxOutput("he", width=3)
                                 
                             ),
                             #BMI graph and legend box with info        
                             fluidRow(
                                 box(title="Body Mass Index (BMI) chart with categories", status="primary", solidHeader = TRUE,
                                     plotOutput("chart"), height = 500, width=9),
                                 box(title="Categories", width=3, solidHeader = TRUE,  
                                     h5(strong(div("Obese", style="color:#d73027"))),
                                     h5(strong(div("Overweight", style="color:#f46d43"))),
                                     h5(strong(div("Normal weight", style="color:#006837"))),
                                     h5(strong(div("Underweight", style="color:#1a9850"))),
                                     hr(),
                                     h5("The figure on the left shows a BMI chart, divided by the categories of possible BMI, it also depicts
                                        a point were the calulcated BMI is located and below shows the calculated BMI and the corresponding
                                        category in a color coded way. Moreover, next to the category the calculated Resting Metabolic Rate (RMR)
                                        is shown, using the Mifflin - St Jeor equation." )
                                     )
                                 
                                     ),
                             #Boxes with the calculated BMI and category
                             fluidRow(
                                 valueBoxOutput("pBMI", width=3),
                                 valueBoxOutput("predi", width=6),
                                 valueBoxOutput("rmr", width=3)
                             ),
                             
                             
                             
                             h5(em("Project for the", strong("Developing Data Products"), "class of the Johns Hopkins University Data Science specialization")),
                             h4("October 2015 by", strong("AG"))
                             )
                    )
                             )
    )

####Start of needed functions to calculate the BMI and construct the graph. Note need to be inside of server as should be static
#BMI function
bmip <- function(weight, height) {
    weight/(height^2)
}
#Category of the BMI
predi <- function(weight, height) {
    if (weight/(height^2) < 18.5) {
        "Underweight"
    } else if (weight/(height^2) > 18.5 & weight/(height^2) < 25 ) {
        "Normal weight"
    } else if (weight/(height^2) > 25 & weight/(height^2) < 30) {
        "Overweight"
    } else
        "Obese"
}


#Generation of BMI chart
#Create data frame with weights and heights, a bigger data frame can be made but it takes longer to produce the graph
w <- c(35, 40, 45, 50 ,55,60, 65, 70,75,80,85,90,95,100,105,110,115,120, 125, 130)
h <- c(1.35, 1.40, 1.45, 1.50, 1.55, 1.60, 1.65, 1.70, 1.75, 1.80, 1.85, 1.90, 1.95, 2.00, 2.05, 2.10, 2.15, 2.20, 2.25, 2.30)
#     w <- seq(from=40, to=130, by=0.1)
#     h <- seq(from=1.35, to=2.25, by=0.001)
h2 <- h*h


#Create matrix with the dimensions of the weight and height
x <- matrix(0, length(w), length(h2))
#Create a function that calculates the BMI
sim <- function(w, h2) { 
    w/h2
}
#Fill the matrix with a nested loop to get the BMI of every element of the matrix
for (j in 1:length(w)) {
    for (i in 1:length(h2)) {
        x[i,j] <- sim(w[i], h2[j])
    }
}
#Add the row and column names based on weight and height
rownames(x) <- w
colnames(x) <- h
#Melt the graph for plotting with ggplot
meltx <- melt(x)

#Add categories, depending on values
meltx$cate[meltx$value < 18.5] <- "Underweight"
meltx$cate[meltx$value > 18.5 & meltx$value < 25] <- "Normal_weight"
meltx$cate[meltx$value > 25 & meltx$value < 30] <- "Overweight"
meltx$cate[meltx$value >= 30] <- "Obese"

#Make category as factor
meltx$cate <- as.factor(meltx$cate)

#Original basic graph
#ggplot(meltx, aes(X2, X1)) + geom_tile(aes(fill=value))

#Get the offset of the categories
meltx$rescaleoffset <- meltx$value + 100*(as.numeric(meltx$cate)-1)
#Get range of values
scalerange <- range(meltx$value)
#Range of categories
gradientends <- scalerange + rep(c(0,100,200,300), each=2)
#Gradients of categories
colorends <- c("#1a9850", "#006837", "#f46d43", "#d73027", "#a6d96a", "#66bd63")
#Create the rescale as vector
rescala <- rescale(gradientends)
#Label for the dot
etiq <- "Your BMI"
#Plot
environment<-environment() 
p <- ggplot(meltx, aes_string("X2", "X1"), environment = environment) + 
    geom_tile(aes_string(fill = "rescaleoffset")) + 
    scale_fill_gradientn(colours = colorends, values = rescala) +
    scale_y_continuous(name="Weight (kg)", expand=c(0,0)) +
    scale_x_continuous(name="Height (m)", expand=c(0,0)) +
    theme_bw() +
    theme(legend.position = "none", axis.text=element_text(size=12), axis.title = element_text(size=13), aspect.ratio=1) 

####    
#Start of the server part
#####    
server <- function(input, output, session) {   
    
    #BMI chart
    output$chart <- renderPlot({
        #Plot
        p + geom_point(aes_string(x=input$height, y=input$weight), size=5, alpha=1/100) +
            geom_text(aes_string(x=input$height+0.13, y=input$weight+3, label="etiq"), size=7, alpha=1/100)
    })
    
    #BMI formula for color of box
    bmit <- reactive({ input$weight/(input$height^2)})
    
    #(input$weight/(input$height^2))
    #Function that generates the color and puts it in a vector
    col <- reactive({ if (bmit() < 18.5) {
        colo <- c("lime")
    } else if (bmit() > 18.5 & bmit() < 25) { 
        colo <- c("green")
    } else if (bmit() > 25 & bmit() < 30) {
        colo <- c("orange")
    } else 
        colo <- c("red")
    })
    
    output$we <- renderValueBox({
        valueBox(paste(round(input$weight, digits=2), "kg"),
                 "Weight entered", color = "blue") #icon = icon("area-chart"),
    })
    output$he <- renderValueBox({
        valueBox(paste(round(input$height, digits=2), "m"),
                 "Height entered", color = "blue")
    })
    output$pBMI <- renderValueBox({
        valueBox(round(input$weight/(input$height^2), digits=2), 
                 div(HTML(paste("Your BMI (kg/m", tags$sup(2),")", sep = ""))), color = col()) #Uses the col(), as function!!
    })
    output$predi <- renderValueBox({
        valueBox(predi(input$weight, input$height), 
                 "Category", color = col())
    })
    
    output$rmr <- renderValueBox({
        valueBox(round(if (input$radio == 1) (9.99 * input$weight) + (6.25 * input$height * 100) - (4.92 * input$age) + 5
                 else (9.99 * input$weight) + (6.25 * input$height * 100) - (4.92 * input$age) - 161, digits=2), h6("Resting Metabolic Rate kcal/day"), col= col())
    })
    
}    


shinyApp(ui = ui, server = server)




