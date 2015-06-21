## Project for Developing Data Products
## Step 1
## Load libraries
## Load data

library(shiny)

## Gather basic statewide statistics
## List of counties in state for selectInput
## total number of counties in state, 
## total number of school districts in state,
## total number of students in the state,
## mean number of students per school district in the state
## mean number of students per county

mydata2 <- read.csv("1061_modified_csv.csv", header = TRUE, 
                    colClasses = c("character", "integer", "character", rep("numeric", 8)))
mycountylist <- sort(unique(mydata2$county), decreasing = FALSE)
mycountynumber <- length(mycountylist)
mystatesdcount <- nrow(mydata2)
mystatestudenttotal <- round(sum(mydata2$FTE_students))
mystatesdstudentmean <- round(mean(mydata2$FTE_students))
mycountystudentlist <- data.frame(county = as.character(mydata2$county), 
                                  FTE_students = mydata2$FTE_students)
mycountystudenttotal <- aggregate(FTE_students ~ county, mycountystudentlist, sum)
mycountystudentmean <- round(mean(mycountystudenttotal$FTE_students), digits = 0)

shinyUI(
     pageWithSidebar(
     ## Application Title
        headerPanel("Washington State School District Enrollment, 2012-2013"),
        sidebarPanel(
             h4("Getting Started:"),
             p("Choose a county from the drop-down box below to display a barplot of 
               all school districts for that county with full-time (FTE) student enrollment. 
               The calculations beneath the chart correspond to the selected county."),
             selectInput("county", "Select one county:", "King Co.", choices = mycountylist, selectize = FALSE),
             h3("Facts about WA State"),
             h4("Number of counties"),
             h6(mycountynumber),
             h4("Number of school districts"),
             h6(mystatesdcount),
             h4("Number of students"),
             h6(mystatestudenttotal),
             h4("Mean number of students per county"),
             h6(mycountystudentmean),
             h4("Mean number of students per school district"),
             h6(mystatesdstudentmean),        
             helpText("Data for this application is from the 
                  Washington State Office of Superintendent of Public Instruction, Report 1061."),
             a(href="http://www.k12.wa.us/safs/PUB/LEV/1415/lv.asp", 
               "School District Property Tax Levies, 2014 Collections", target="_blank"),
             helpText("Help documentation in PDF provides more details about the motivation for creating this application 
                      and the sections."),   
             a(href="wastate_k12_enrollment_getting_started.pdf", "Getting Started User Guide", target="_blank"),  
             helpText("The web-based Slidify promotion provides a quick overview."),  
             a(href="http://k12eddata.github.io/ddpp/wak12_enrollment.html", "Washington State K-12 Enrollment", target="_blank")
             
        ),
          mainPanel(
               h3(textOutput("outcounty")),
               plotOutput("newHist"),          
               strong("Number of school districts in the county:"),
               h6(textOutput("countysdcount")),
               strong("Number of students in the county:"),
               h6(textOutput("countystudentcount")),
               strong("Mean number of students per school district:"),
               h6(textOutput("countysdstudentmean")),
               strong("Percent of school districts in WA state:"),
               h6(textOutput("percentsdstate")),
               strong("Percent of students in WA state:"),
               h6(textOutput("percentstudentstate"))          
               )
     )
)