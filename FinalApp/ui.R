library(shiny)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel("Green Expectations Sustainability Tool"),
  
  tabsetPanel(
    
    tabPanel("Carbon Calculator",
            h2("Carbon Footprint Calculator"),
            tags$iframe(
              width = "100%", 
              height = "600", 
              style = "border:0;", 
              sandbox = "allow-scripts allow-same-origin allow-forms", 
              src = "https://calculator.carbonfootprint.com/calculator.aspx?c=Full&amp;tab=4&amp;h=48e8b2f8c6839b401c2b1f43c8f92344"
            )
            
    ),
    
    tabPanel("Data Analysis",

        plotOutput("regression_plot"),
        sliderInput("sqft_slider", 
                    "Square Footage:",
                    min = min(0, na.rm = TRUE),
                    max = max(10000, na.rm = TRUE),
                    value = median(2000, na.rm = TRUE),
                    step = 1),
        textOutput("regression_value")

    ),
    
    tabPanel("ChatGPT Consultation",
      textAreaInput("chatgpt_question", "ChatGPT Question", value = "", width = '100%', height = '200px'),
      actionButton("submit2", "Submit ChatGPT Question"),
      textAreaInput("chatgpt_answer", "ChatGPT Response", value = "", width = '100%', height = '200px')
    ),
    
    tabPanel("Glossary",
             h2("Carbon Footprint Calculator"),
             tags$iframe(
               width = "100%", 
               height = "600", 
               style = "border:0;", 
               sandbox = "allow-scripts allow-same-origin allow-forms", 
               src = "https://greenifyai.com/ai/glossary_one_big_text.php"
             )
             
    ),
    
    tabPanel("Survey Questions",
             # Submit Button
             actionButton("submit", "Submit Responses"),
             mainPanel(
               tabsetPanel(
                 # tabPanel("Input Forms",
                 #          textOutput("h126_output"),
                 #          textOutput("h130_output"),
                 #          textOutput("h134_output"),
                 #          textOutput("h137_output"),
                 #          textOutput("h140_output"),
                 #          textOutput("h147_output"),
                 #          textOutput("h151_output"),
                 #          textOutput("h158_output"),
                 #          textOutput("h163_output")
                 # ),
                 # tabPanel("Savings Outputs", 
                 #          textOutput("J91_output"),
                 #          textOutput("J93_output"),
                 #          textOutput("J95_output"),
                 #          textOutput("J104_output"),
                 #          textOutput("J109_output"),
                 #          textOutput("J111_output"),
                 #          textOutput("J113_output"),
                 #          textOutput("J115_output"),
                 #          textOutput("J117_output"),
                 #          textOutput("J119_output"),
                 #          
                 #          textOutput("J120_output"),
                 #          textOutput("J126_output"),
                 #          textOutput("J130_output"),
                 #          textOutput("J134_output"),
                 #          textOutput("J137_output"),
                 #          textOutput("J140_output"),
                 #          textOutput("J143_output"),
                 #          textOutput("J147_output"),
                 #          textOutput("J151_output"),
                 #          textOutput("J158_output"),
                 #          textOutput("J163_output"),
                 #          textOutput("J175_output")
               )#,
               # tabPanel("Emission Percentages",
               #          textOutput("L91_output"),
               #          textOutput("L93_output"),
               #          textOutput("L95_output"),
               #          textOutput("L104_output"),
               #          textOutput("L109_output"),
               #          textOutput("L111_output"),
               #          textOutput("L113_output"),
               #          textOutput("L126_output"),
               #          textOutput("L130_output"),
               #          textOutput("L134_output"),
               #          textOutput("L137_output"),
               #          textOutput("L140_output"),
               #          textOutput("L143_output"),
               #          textOutput("L147_output"),
               #          textOutput("L151_output"),
               #          textOutput("L158_output"),
               #          textOutput("L163_output"),
               #          textOutput("L175_output"),
               #          textOutput("L189_output")
               #)
               #)
             ),
             wellPanel(class="bg-green",
                       style="height: 500px;",
                       # UI function
                       plotOutput("savingsPlot", height = "460px"),
                       # Inside your ui.R
                       plotOutput("carbonFootprintPie"),
             ),
             wellPanel(class="bg-yellow",
                       numericInput("numVehicles", "Number of Vehicles:", min = 0, max = 5, value = 0),
                       uiOutput("vehicleInputs"),  
             ),
             # # Basic information inputs
             # wellPanel(class="bg-yellow",
             #   fluidRow(
             #     column(6, textInput("f5", "How many people live in your home?", value = "1"),
             #            ),
             #     column(6, textInput("f6", "What is your zip code?", value = "49001"),
             #            )
             #   )
             # ),
             # # Heating source and usage inputs
             # wellPanel(class="bg-yellow",
             #   fluidRow(
             #     column(6, radioButtons("f7", "Primary heating source",
             #                            choices = c("Natural gas" = 1, "Electric heat" = 2, "Oil" = 3,
             #                                        "Propane" = 4, "Wood" = 5, "Do not heat" = 6)),
             #            ),
             #     column(6, numericInput("f37", "Natural gas usage (monthly)", value = 50),
             #            )
             #   ),
             #   fluidRow(
             #     column(6, radioButtons("h37", "Billing unit for natural gas",
             #                            choices = c("Dollars" = 1, "Thousand cubic feet" = 2, "Therms" = 3)),
             #            ),
             #     column(6, numericInput("f42", "Electricity usage (monthly)", value = 50),
             #            ),
             #     column(6, radioButtons("h42", "Billing unit for electricity",
             #                            choices = c("Dollars" = 1, "Kilowatt-hours" = 2)),
             #            )
             #   )
             # ),
             # # Green power and other energy sources
             # wellPanel(class="bg-yellow",
             #   fluidRow(
             #     column(6, radioButtons("f45", "Purchase green power?",
             #                            choices = c("Yes" = 1, "No" = 2)),
             #            ),
             #     column(6, numericInput("f49", "Percentage of green power", value = 50),
             #            )
             #   ),
             # fluidRow(
             #   column(6, numericInput("f53", "Fuel oil usage (monthly)", value = 50),
             #          ),
             #   column(6, radioButtons("h53", "Billing unit for fuel oil",
             #                          choices = c("Dollars" = 1, "Gallons" = 2)),
             #          ),
             #   column(6, numericInput("f57", "Propane usage (monthly)", value = 50),
             #          ),
             #   column(6, radioButtons("h57", "Billing unit for propane",
             #                          choices = c("Dollars" = 1, "Gallons" = 2)),
             #          )
             # )
             #),
             fluidRow(
               #class="bg-red",
               div(style = "display: flex; flex-wrap: wrap;", 
                   div(style = "flex: 50%; padding: 5px;", 
                       # Thermostat adjustments and energy-saving actions
                       wellPanel(class="bg-yellow",
                                 # fluidRow(
                                 #   column(6, radioButtons("f130", "Adjust AC thermostat in summer?",
                                 #                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                                 #          ),
                                 #   column(6, numericInput("b131", "Degree(s) Fahrenheit adjustment", value = 50),
                                 #          helpText("Degree(s) to turn up by"))
                                 # ),
                                 fluidRow(
                                   column(6, radioButtons("f134", "Enable computer sleep mode?",
                                                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                                   ),
                                   column(6, radioButtons("f137", "Wash clothes in cold water?",
                                                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                                   )
                                 )
                       )),
                   div(style = "flex: 50%; padding: 5px;", 
                       # Household actions for energy saving
                       wellPanel(class="bg-yellow",
                                 fluidRow(
                                   column(6, numericInput("b139", "Laundry loads per week", value = 50),
                                   ),
                                   column(6, radioButtons("f140", "Use drying rack for laundry?",
                                                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                                   ),
                                   # column(6, numericInput("b143", "Green power usage percentage", value = 0),
                                   #        ),
                                   # column(6, radioButtons("f143", "Decide on green power usage",
                                   #                        choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                                   #        ),
                                   column(6, numericInput("b147", "Bulbs to replace with ENERGY STAR", value = 0),
                                   ),
                                   column(6, radioButtons("f147", "Replace lights with ENERGY STAR bulbs",
                                                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                                   ),
                                   column(6, radioButtons("f151", "Replace fridge with ENERGY STAR model",
                                                          choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                                   ),
                                   # column(6, radioButtons("f158", "Replace furnace with ENERGY STAR model",
                                   #                        choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                                   #        ),
                                   # column(6, radioButtons("f163", "Replace windows with ENERGY STAR models",
                                   #                        choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                                   #        ),
                                   # column(6, radioButtons("f126", "Turn down heating thermostat at night",
                                   #                        choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                                   #        ),
                                   # column(6, numericInput("b127", "Degree(s) Fahrenheit reduction", value = 50),
                                   #        helpText("Degree(s) to turn down by"))
                                 )
                       ))))
    ))
)