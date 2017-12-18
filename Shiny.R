#install.packages("shiny")
require(shiny)
require(plotly)
install.packages("scales")
require(scales)
gpal <- scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction=1)(8)

ui <- fluidPage(
  selectInput("Employees", "Employee ID:", temp$Emplid),
  plotlyOutput("plot")
  
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    dt=temp[Emplid == input$Employees,]
    p=ggplot(data=dt, aes(x=Week, y=Sum_hrs)) + geom_line() + scale_x_date(date_breaks = "3 month", date_labels = "%b-%y") +geom_hline(yintercept = dt$Std_Hours, col) +  xlab("") + ylab("Total Hours Clocked per Week")
    ggplotly(p)
  })
  
 

  
}

shinyApp(ui, server)


