library(shiny)

ui <- fluidPage(
  titlePanel("Green Expectations Calculator UI"),
  
  tabsetPanel(
    tabPanel("Survey Questions",
             
       mainPanel(
         textOutput("h126_output"),
         textOutput("h130_output")
       ),
       # The Basics
       fluidRow(
         column(6, textInput("f5", "How many people live in your home?"),
                helpText("Reference: F5")),
         column(6, textInput("f6", "What is your zip code?"),
                helpText("Reference: F6"))
       ),
       
       # Heating Source
       fluidRow(
         column(6, radioButtons("f7", "What is your household's primary heating source?",
                                choices = c("Natural gas" = 1, "Electric heat" = 2, "Oil" = 3,
                                            "Propane" = 4, "Wood" = 5, "Do not heat" = 6)),
                helpText("Enter 1 for natural gas, 2 for electric heat, 3 for oil, 4 for propane, 5 for wood, or 6 if you do not heat your house",
                         "Reference: F7"))
       ),
       
       # Natural Gas Usage
       fluidRow(
         column(6, numericInput("f37", "How much natural gas does your household use per month?", value = NULL),
                helpText("If you enter your monthly consumption in thousand cubic feet, you'll get a more accurate estimate. If you enter dollars, our calculations assume you pay $10.68/thousand cubic feet.",
                         "Reference: F37")),
         column(6, radioButtons("h37", "", choices = c("Dollars" = 1, "Thousand cubic feet" = 2, "Therms" = 3)),
                helpText("$23 is about average in the United States for a household of one person.",
                         "Reference: H37"))
       ),
       
       # Electricity Usage
       fluidRow(
         column(6, numericInput("f42", "How much electricity does your household use per month?", value = NULL),
                helpText("If you enter your average kilowatt-hours, you'll get a more accurate estimate. If you enter dollars, our calculations assume that you pay 11.9 cents/kWh.",
                         "Reference: F42")),
         column(6, radioButtons("h42", "", choices = c("Dollars" = 1, "Kilowatt-hours" = 2)),
                helpText("$44 is about average in the United States for a household of one person.",
                         "Reference: H42"))
       ),
       
       # Green Power Purchase
       fluidRow(
         column(6, radioButtons("f45", "Does your household currently purchase green power?",
                                choices = c("Yes" = 1, "No" = 2)),
                helpText("Green power can often be bought through your local utility or through a green power marketer. For a description of green power visit (http://www.epa.gov/greenpower/whatis/index.htm).",
                         "Reference: F45")),
         column(6, numericInput("f49", "If so, what portion of your household's total purchased electricity use is green power?", value = NULL),
                helpText("Enter 100% if you buy all of your electricity as green power.",
                         "Reference: F49"))
       ),
       # Fuel Oil Usage
       fluidRow(
         column(6, numericInput("f53", "How much fuel oil does your household use per month?", value = NULL),
                helpText("Divide your annual fuel oil consumption (in gallons or dollars) by 12 to obtain a monthly average. If you enter your monthly fuel oil use in gallons, you'll get a more accurate estimate. If you enter dollars, our calculations assume that you pay $4.02/gallon.",
                         "Reference: F53")),
         column(6, radioButtons("h53", "", choices = c("Dollars" = 1, "Gallons" = 2)),
                helpText("$72 is about average in the United States for a household of one person.",
                         "Reference: H53"))
       ),
       
       # Propane Usage
       fluidRow(
         column(6, numericInput("f57", "How much propane does your household use per month?", value = NULL),
                helpText("If you enter your monthly propane use in gallons, you'll get a more accurate estimate. If you enter dollars, our calculations assume that you pay $2.47/gallon.",
                         "Reference: F57")),
         column(6, radioButtons("h57", "", choices = c("Dollars" = 1, "Gallons" = 2)),
                helpText("$37 is about average in the United States for a household of one person.",
                         "Reference: H57"))
       ),
       
       # Thermostat Adjustments
       fluidRow(
         column(6, radioButtons("f130", "Turn up your household's air conditioner thermostat in summer.",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                helpText("Reference: F130")),
         column(6, numericInput("b131", "Degree(s) Fahrenheit", value = NULL),
                helpText("Degree(s) to turn up by", "Reference: B131"))
       ),
       
       # Energy Saving Actions
       fluidRow(
         column(6, radioButtons("f134", "Enable sleep feature on your computer and monitor. Will you take this action?",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                helpText("Reference: F134")),
         column(6, radioButtons("f137", "Wash clothes in cold water instead of hot. Will you take this action?",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                helpText("Reference: F137"))
       ),
       
       # Laundry Actions
       fluidRow(
         column(6, numericInput("b139", "Enter the number of loads you do in a week", value = NULL),
                helpText("Enter the number of loads you do in a week", "Reference: B139")),
         column(6, radioButtons("f140", "Use a clothes line or drying rack for 50% of your laundry, instead of your dryer. Will you take this action?",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                helpText("Reference: F140"))
       ),
       
       # Green Power Usage Action
       fluidRow(
         column(6, numericInput("b143", "Percentage of your current electricity use with Green Power. Will you take this action?", value = 0),
                helpText("Reference: B143")),
         column(6, radioButtons("f143", "Decide on green power usage:",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                helpText("Reference: F143"))
       ),
       
       # Energy-Efficient Lighting Action
       fluidRow(
         column(6, numericInput("b147", "Number of 60-watt incandescent light bulbs to replace with 13-watt energy-efficient bulbs", value = 0),
                helpText("Reference: B147")),
         column(6, radioButtons("f147", "Decide to replace lights with ENERGY STAR bulbs:",
                                choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                helpText("Reference: F147"))
       ),
       
       # ENERGY STAR Refrigerator Replacement Action
       fluidRow(
         column(12, radioButtons("f151", "Replace your refrigerator with an ENERGY STAR model. Will you take this action?",
                                 choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                helpText("Reference: F151"))
       ),
       
       # ENERGY STAR Furnace Replacement Action
       fluidRow(
         column(12, radioButtons("f158", "Replace your furnace with an ENERGY STAR model. Will you take this action?",
                                 choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                helpText("Reference: F158"))
       ),
       
       # ENERGY STAR Windows Replacement Action
       fluidRow(
         column(12, radioButtons("f163", "Replace your windows with ENERGY STAR models. Will you take this action?",
                                 choices = c("Yes" = 1, "No" = 2, "Already doing" = 3), selected = 2),
                helpText("Reference: F163"))
       ),
       # Thermostat Adjustment
       fluidRow(
         column(6, radioButtons("f126", "Turn down your household's heating thermostat on winter nights:", choices = c("Yes" = 1, "No" = 2, "Already doing" = 3)),
                helpText("Reference: F126")),
         column(6, numericInput("b127", "Degree(s) Fahrenheit", value = NULL),
                helpText("Degree(s) to turn down by", "Reference: B127"))
       ),
       
       
       
       # Submit Button
       actionButton("submit", "Submit Responses")
  
    ),
    
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
        
        textAreaInput("ChatGPT Question Box", "ChatGPT Question Box"),
        actionButton("submit2", "Submit ChatGPT Question"),
        textAreaInput("ChatGPT Answer Box", "ChatGPT Answer Box")
        
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
             
    )
    
  )
)
