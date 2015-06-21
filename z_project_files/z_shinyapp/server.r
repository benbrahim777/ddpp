## Project for Developing Data Products
## Step 1
## Load libraries
## Load data

library(shiny)
mydata2 <- read.csv("1061_modified_csv.csv", header=TRUE, 
          colClasses = c("character", "integer", "character", rep("numeric", 8)))
old.par <- par(no.readonly=T)

shinyServer(
     function(input, output) {
      
          mycounty <- reactive({input$county})
          
          ## total number of students in the state,
          ## total number of school districts in state,
          ## mean number of students per school district in the state
          mystatestudenttotal <- reactive({round(sum(mydata2$FTE_students))})
          mystatesdcount <- reactive({nrow(mydata2)})
          mystatesdstudentmean <- reactive({round(mean(mydata2$FTE_students))})
          
          ## Calculate county statistics
          ## Subset data by county
          ## Count of school districts in the county
          ## Total number of students in the county
          ## Mean number of students per school district in the county
               
          myquerydata2 <- reactive({subset(mydata2, county==mycounty())})
          myquerydata <- reactive({myquerydata2()[with(myquerydata2(), order(county, -FTE_students, school_district)), ]})
          mycountysdcount <- reactive({nrow(myquerydata())})
          mycountystudenttotal <- reactive({round(sum(myquerydata()$FTE_students), digits = 0)})
          mycountysdstudentmean <- reactive({round(mean(myquerydata()$FTE_students), digits = 0)})
          mypercentstudentsstate <- reactive({round((mycountystudenttotal() / mystatestudenttotal())*100, 
          digits = 0)})
          mypercentsdstate <- reactive({round((mycountysdcount() / mystatesdcount()) * 100, digits = 0)})

          ## Create the barplot for the selected county
          ## School district names are on the x-axis
          ## Add lines for the mean student enrollment for the state and county
          ## Add a legend
          
          output$newHist <- renderPlot({
               par(mar=c(8,5,3,.5))
               barplot(myquerydata()$FTE_students, main="School Districts for the County", 
                       xlab = "", ylab="Number of FTE Students", ylim = c(0, 50000), 
                       cex.axis=0.8, col=terrain.colors(mycountysdcount()), 
                       names.arg=myquerydata()$school_district, cex=0.8, las=2)
               abline(h = mycountysdstudentmean(), col = "black", lty = 1, lwd = 2)
               abline(h = mystatesdstudentmean(), col = "blue", lty = 5, lwd = 2)
               legend("topright", c("Students per county school district", 
                     "Students per Washington state school district"), lty=c(1,1), lwd=c(2.5,2.5), 
                      col = c("black","blue"), title = "Mean # Calculations", cex = 1, 
                      bty = "n", xjust = 1, yjust = 1)     
          
          })
          
          output$outcounty <- renderText({mycounty()})
          output$countysdcount <- renderText({mycountysdcount()})
          output$countystudentcount <- renderText({mycountystudenttotal()})
          output$countysdstudentmean <- renderText({mycountysdstudentmean()})
          output$percentstudentstate <- renderText({mypercentstudentsstate()})
          output$percentsdstate <- renderText({mypercentsdstate()})


}
)
