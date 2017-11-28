library(shiny)
require(jsonlite)
require(sqldf)
require(forecast)

### AWS Configure, this needs to be run each time R is started.###
aws_access_key_id="##############";
aws_secret_access_key="#################";
default.region="us-east-2"

system(paste("aws configure set aws_access_key_id",aws_access_key_id))
system(paste("aws configure set aws_secret_access_key",aws_secret_access_key))
system(paste("aws configure set default.region",default.region))
##################### AWS Configure END ########################################
startTime = paste(Sys.Date()-90,"T00:00:00.001",sep = "")
endTime = paste(Sys.Date()-1,"T23:59:59.999",sep = "")

filterSpotPriceHistory = function(SpotPriceHistory,zone,instance,os){
  SpotPriceHistory[order(SpotPriceHistory$Timestamp),]
  SpotPriceHistory$Date = as.Date(SpotPriceHistory$Timestamp) 
  
  sqldf(paste("SELECT '",instance,"' AS InstanceType,
              '",zone,"' AS AvailabilityZone,
              '",os,"' AS ProductDescription,
              MAX(SpotPrice) AS SpotPrice, 
              Date, 
              PartOfDay FROM SpotPriceHistory
              WHERE Timestamp >='",startTime,"' AND Timestamp <='",endTime,"' 
              GROUP BY Date,PartOfDay",sep=""))
}


# Define UI for app
ui <- fluidPage(
  
  # App title ----
  titlePanel("Spot The Spot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("instance",
                  "Instance Type",
                  c("Select","m4.4xlarge","r4.8xlarge","r3.4xlarge","r3.xlarge","r3.large","m4.2xlarge",
                    "m4.10xlarge","x1.16xlarge","r3.8xlarge","p2.xlarge","m4.2xlarge"),
                  selected = "Select"),
      selectInput("zone",
                  "Availability Zone",
                  c("Select","us-east-2a","us-east-2b"),
                  selected = "Select"),
      selectInput("os",
                  "Product Description",
                  c("Select","Linux/UNIX","Windows"),
                  selected = "Select"),
      selectInput("predint",
                  "Execution Duration",
                  c("Select","Morning","Afternoon","Evening","Night"),
                  selected = "Select"),
      actionButton("getData",
                   "Get Predicted Price Range")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
     textOutput('nodata'),
     tabsetPanel(type = "tabs",
                 tabPanel("Holt Winters", plotOutput('plot_hw'),
                                          tableOutput('forecasted_hw')),
                 tabPanel("ARIMA", plotOutput('plot_ar'),
                                            tableOutput('forecasted_ar')),
                 tabPanel("S Naive", plotOutput('plot_sn'),
                                            tableOutput('forecasted_sn')),
                 tabPanel("S ARIMA", plotOutput('plot_sar'),
                                            tableOutput('forecasted_sar')),
                 tabPanel("Error Comparison", plotOutput('comparison')))
    )
  )
)

# Define server logic
server <- function(input, output) {
  #input
  
  #AWS Command to get and store data
  checkFilters <- eventReactive(input$getData,
                                {ifelse(input$instance=="Select"||
                                          input$zone=="Select"||
                                          input$os=="Select"||
                                          input$predint=="Select",FALSE,TRUE) 
                                })
  
  
  createData_1 <- reactive({result <- system(paste("aws ec2 describe-spot-price-history",
                                                 ifelse(nchar(input$zone)>0,
                                                        paste("--availability-zone",input$zone),""),
                                                 ifelse(nchar(input$instance)>0,
                                                        paste("--instance-types",input$instance),""),
                                                 ifelse(nchar(input$os)>0,
                                                        paste("--product-descriptions",input$os),""),
                                                 ifelse(nchar(startTime)>0,
                                                        paste("--start-time",startTime),""),
                                                 ifelse(nchar(endTime)>0,
                                                        paste("--end-time",endTime),"")), intern = TRUE)
                          SpotPriceHistory <- fromJSON(result)$SpotPriceHistory})
                          #if(length(SpotPriceHistory) == 0) returnValue("No Data, Cannot Predict!")
                          #Add day of week
  checkData   <-  reactive({ifelse(checkFilters() && (length(createData_1())>0),TRUE,FALSE)})
  
  createData  <-  reactive({if(checkData()){                      
                          #Parts of the Day.
                          # Morning 5 am to 12 pm.
                          # Afternoon 12 pm to 5 pm.
                          # Evening 5 pm to 9 pm.
                          # Night 9 pm to 5 am
                          SpotPriceHistory = createData_1()
                          SpotPriceHistory$Day = weekdays(as.Date(SpotPriceHistory$Timestamp))
                          SpotPriceHistory$PartOfDay = ifelse(5<=as.numeric(substring(SpotPriceHistory$Timestamp,12,13))
                                                              & as.numeric(substring(SpotPriceHistory$Timestamp,12,13)) < 12, 1,#"Morning",
                                                              ifelse(12<=as.numeric(substring(SpotPriceHistory$Timestamp,12,13))
                                                                     & as.numeric(substring(SpotPriceHistory$Timestamp,12,13))<17,2,#"Afternoon",
                                                                     ifelse(17<=as.numeric(substring(SpotPriceHistory$Timestamp,12,13))
                                                                            & as.numeric(substring(SpotPriceHistory$Timestamp,12,13))<21,3,#"Evening",
                                                                            4)))#"Night")))
                          
                          SpotPriceHistory_Filtered = filterSpotPriceHistory(SpotPriceHistory,input$zone,input$instance,
                                                                             input$os)
                          SpotPriceHistory_Filtered$Time = seq.int(nrow(SpotPriceHistory_Filtered))
                          SpotPrice_TS = as.ts(SpotPriceHistory_Filtered[,-1:-3]) #Converts the date to a number
                          
                          ts(SpotPrice_TS[,-2:-4], start=c(SpotPrice_TS[1,2],1), frequency = 4) #Use the date number here to create a time series
                        }})
                          
  hw_model    <-  reactive(HoltWinters(createData()))
  hw_forecast <-  reactive(forecast(hw_model(), h=4, level=95))              
  hw_final_Result <- reactive({HW.pred = hw_forecast()
                              if(input$predint=="Morning") {
                                CI = c("Lower Limit"=HW.pred$lower[1,],"Mean"=HW.pred$mean[1],"Upper Limit"=HW.pred$upper[1,])
                              }
                              else if(input$predint=="Afternoon") {
                                CI = c("Lower Limit"=HW.pred$lower[2,],"Mean"=HW.pred$mean[2],"Upper Limit"=HW.pred$upper[2,])
                              }
                              else if(input$predint=="Evening") {
                                CI = c("Lower Limit"=HW.pred$lower[3,],"Mean"=HW.pred$mean[3],"Upper Limit"=HW.pred$upper[3,])
                              }
                              else if(input$predint=="Night") {
                                CI = c("Lower Limit"=HW.pred$lower[4,],"Mean"=HW.pred$mean[4],"Upper Limit"=HW.pred$upper[4,])
                              }
                            })
  
  ar_model    <-  reactive(auto.arima(createData()))
  ar_forecast <-  reactive(forecast(ar_model(), h=4, level=95))              
  ar_final_Result <- reactive({ar.pred = ar_forecast()
                                if(input$predint=="Morning") {
                                  CI = c("Lower Limit"=ar.pred$lower[1,],"Mean"=ar.pred$mean[1],"Upper Limit"=ar.pred$upper[1,])
                                }
                                else if(input$predint=="Afternoon") {
                                  CI = c("Lower Limit"=ar.pred$lower[2,],"Mean"=ar.pred$mean[2],"Upper Limit"=ar.pred$upper[2,])
                                }
                                else if(input$predint=="Evening") {
                                  CI = c("Lower Limit"=ar.pred$lower[3,],"Mean"=ar.pred$mean[3],"Upper Limit"=ar.pred$upper[3,])
                                }
                                else if(input$predint=="Night") {
                                  CI = c("Lower Limit"=ar.pred$lower[4,],"Mean"=ar.pred$mean[4],"Upper Limit"=ar.pred$upper[4,])
                                }
                              })
  
  sn_model    <-  reactive(snaive(createData()))
  sn_forecast <-  reactive(forecast(sn_model(), h=4))              
  sn_final_Result <- reactive({sn.pred = sn_forecast()
                          if(input$predint=="Morning") {
                            CI = c("Lower Limit"=sn.pred$lower[5],"Mean"=sn.pred$mean[1],"Upper Limit"=sn.pred$upper[5])
                          }
                          else if(input$predint=="Afternoon") {
                            CI = c("Lower Limit"=sn.pred$lower[6],"Mean"=sn.pred$mean[2],"Upper Limit"=sn.pred$upper[6])
                          }
                          else if(input$predint=="Evening") {
                            CI = c("Lower Limit"=sn.pred$lower[7],"Mean"=sn.pred$mean[3],"Upper Limit"=sn.pred$upper[7])
                          }
                          else if(input$predint=="Night") {
                            CI = c("Lower Limit"=sn.pred$lower[8],"Mean"=sn.pred$mean[4],"Upper Limit"=sn.pred$upper[8])
                          }
                          })
  
  sar_model    <-  reactive(arima(createData(),order=c(3,0,0), seasonal=c(0,0,1)))
  sar_forecast <-  reactive(forecast(sar_model(), h=4, level=95))              
  sar_final_Result <- reactive({sar.pred = sar_forecast()
                                if(input$predint=="Morning") {
                                  CI = c("Lower Limit"=sar.pred$lower[1,],"Mean"=sar.pred$mean[1],"Upper Limit"=sar.pred$upper[1,])
                                }
                                else if(input$predint=="Afternoon") {
                                  CI = c("Lower Limit"=sar.pred$lower[2,],"Mean"=sar.pred$mean[2],"Upper Limit"=sar.pred$upper[2,])
                                }
                                else if(input$predint=="Evening") {
                                  CI = c("Lower Limit"=sar.pred$lower[3,],"Mean"=sar.pred$mean[3],"Upper Limit"=sar.pred$upper[3,])
                                }
                                else if(input$predint=="Night") {
                                  CI = c("Lower Limit"=sar.pred$lower[4,],"Mean"=sar.pred$mean[4],"Upper Limit"=sar.pred$upper[4,])
                                }
                              })
  output$nodata <- renderText({if(checkFilters()&!checkData()){"No Data, Cannot Predict!"}})
                                
  output$plot_hw <- renderPlot({if(checkFilters()&checkData()){
                                    hw_plot <- hw_forecast() 
                                    plot(hw_plot)
                                    lines(hw_plot$fitted,col="blue")}})
  output$forecasted_hw <- renderTable({if(checkFilters()) hw_final_Result()},
                                      colnames = FALSE, rownames = TRUE, digits = 3)
  
  output$plot_ar <- renderPlot({if(checkFilters()&checkData()){
                                  ar_plot = ar_forecast() 
                                  plot(ar_plot)
                                  lines(ar_plot$fitted,col="blue")}})
  output$forecasted_ar <- renderTable({ar_final_Result()},
                                      colnames = FALSE, rownames = TRUE, digits = 3)
  
  output$plot_sn <- renderPlot({if(checkFilters()&checkData()){
                                    sn_plot <- sn_forecast()
                                    plot(sn_plot)
                                    lines(sn_plot$fitted,col="blue")}})
  output$forecasted_sn <- renderTable({sn_final_Result()},
                                      colnames = FALSE, rownames = TRUE, digits = 3)
  
  output$plot_sar <- renderPlot({if(checkFilters()&checkData()){
                                    sar_plot <- sar_forecast() 
                                    plot(sar_plot)
                                    lines(sar_plot$fitted,col="blue")}})
  output$forecasted_sar <- renderTable({sar_final_Result()},
                                      colnames = FALSE, rownames = TRUE, digits = 3)
  
  #Error comparison
  output$comparison <- renderPlot(barplot(c(accuracy(hw_forecast())[1,2],accuracy(ar_forecast())[1,2],
                                  accuracy(sn_forecast())[1,2],accuracy(sar_forecast())[1,2]), 
                                  main="Errors of various models", xlab="Models", ylab="Root mean sq errors", 
                                  names.arg=c("HW","ARIMA","SN","SARIMA"),
                                  border="blue"))
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)