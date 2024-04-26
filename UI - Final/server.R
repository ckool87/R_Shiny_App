library(shiny)
library(ggplot2)

# Define the server logic for the Shiny application
server <- function(input, output, session) {
  
  ## Liam
  
  # Open AI API messaging
  source("GPT_API_6.R")
  observeEvent(input$submit2, {
    req(input$chatgpt_question)
    answer <- openAI_message(input$chatgpt_question)
    updateTextAreaInput(session, "chatgpt_answer", value = answer)
  })
  
  # linear regression square footage to list price
  data <- read.csv("rets1201.csv")
  filtered_data <- na.omit(data[c("LISTPRICE", "SQFTAPPROX")])
  model <- lm(LISTPRICE ~ SQFTAPPROX, data = filtered_data)
  output$regression_plot <- renderPlot({
    plot(filtered_data$SQFTAPPROX, filtered_data$LISTPRICE,
         xlab = "Square Footage (SQFTAPPROX)",
         ylab = "List Price $ (LISTPRICE)",
         main = "Linear Regression: List Price vs. Square Footage",
         xlim = c(0, 10000))
    abline(model, col = "red")
    legend("topleft", legend = "Regression Line", col = "red", lty = 1)
    grid()
  })
  
  # slider for regression value
  output$regression_value <- renderText({
    sqft_value <- input$sqft_slider
    regression_value <- predict(model, newdata = data.frame(SQFTAPPROX = sqft_value))
    paste("Predicted List Price:", round(regression_value))
  })
  
  
  ## Clayton
  
  # Load the zip factor data from a text file
  zip_factors <- read.table("ZipFactor.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  colnames(zip_factors) <- c("Zip", "State", "Primary", "CO2e_Rate")
  
  # eGRID Subregion Data
  eGRID_data <- data.frame(
    SRNAME = c("ASCC Alaska Grid", "ASCC Miscellaneous", "ERCOT All", "FRCC All", 
               "HICC Miscellaneous", "HICC Oahu", "MRO East", "MRO West",
               "NPCC Long Island", "NPCC New England", "NPCC NYC/Westchester", 
               "NPCC Upstate NY", "RFC East", "RFC Michigan", "RFC West", 
               "SERC Midwest", "SERC Mississippi Valley", "SERC South", 
               "SERC Tennessee Valley", "SERC Virginia/Carolina", "SPP North", 
               "SPP South", "WECC California", "WECC Northwest", "WECC Rockies", "WECC Southwest"),
    SUBRGN = c("AKGD", "AKMS", "ERCT", "FRCC", "HIMS", "HIOA", "MROE", "MROW", 
               "NYLI", "NEWE", "NYCW", "NYUP", "RFCE", "RFCM", "RFCW", "SRMW", 
               "SRMV", "SRSO", "SRTV", "SRVC", "SPNO", "SPSO", "CAMX", "NWPP", 
               "RMPA", "AZNM"),
    SRC2ERTA = c(1259.639, 450.104, 1222.883, 1201.793, 1336.021, 1630.897, 
                 1619.845, 1545.110, 1341.010, 727.603, 623.784, 548.374, 
                 1007.037, 1638.342, 1511.525, 1820.425, 1033.584, 1361.049, 
                 1396.520, 1079.572, 1808.759, 1587.554, 613.285, 846.969, 
                 1906.270, 1182.892)
  )
  
  # Constants definition
  constants <- list(
    AC_electricity_percent = 0.14,
    Average_elec_CO2_emissions = 14019.99772,
    Average_FO_CO2_emissions = 12460,
    average_mpg = 21.6,
    average_waste_emissions = 692,
    boiler_replacement_cost_savings = 78.34,
    boiler_replacement_savings_FO = 1056,
    boiler_replacement_savings_NG = 728,
    BTU_per_1000cf_NG = 1023000,
    BTU_per_gallon_FO = 138691.09,
    BTU_per_gallon_propane = 91335.94,
    BTU_per_kWh = 3412,
    CO2_C_ratio = 3.67,
    computer_energy_monitor_off = 66.5,
    computer_energy_off = 143,
    computer_energy_sleep_monitor_off = 31.7,
    computer_energy_sleep_off = 70.7,
    computer_sleep_savings = 107.1,
    conventional_fridge_kWh = 810,
    conversion_1000cf_to_therm = 10.23,
    conversion_QBtu_to_Btu = 1E+15,
    conversion_Tg_to_lb = 2204622620,
    cost_per_kWh = 0.1188,
    cost_per_mile = 0.1964,
    dryer_energy = 769,
    EF_fuel_oil_gallon = 22.61,
    EF_fuel_oil_MMBtu = 163.05,
    EF_natural_gas = 119.58,
    EF_natural_gas_therm = 11.68890913,
    EF_passenger_vehicle = 19.6,
    EF_propane = 12.43,
    ENERGYSTAR_fridge_kWh = 488,
    fridge_replacement_kWh_savings = 322,
    fuel_oil_cost = 4.02,
    gas_cost_gallon = 3.68,
    glass_recycling_avoided_emissions = -25.39,
    green_power_premium = 0.02,
    heating_percent_electricity = 0.09,
    heating_percent_fuel_oil = 0.87,
    heating_percent_NG = 0.63,
    heating_percent_propane = 0.70,
    HH_size = 2.57,
    HHV_fuel_oil = 138691.09,
    HHV_natural_gas = 1023000,
    HHV_propane = 91335.94,
    kWh_per_load_laundry = 0.96,
    lamp_cost_savings = 4.00,
    lamp_kWh_savings = 33,
    mag_recycling_avoided_emissions = -27.46,
    metal_recycling_avoided_emissions = -89.38,
    monthly_elec_consumption = 943,
    monthly_FO_consumption = 46,
    monthly_NG_Consumption = 5500,
    monthly_propane_consumption = 39,
    Natural_gas_cost_1000CF = 10.68,
    Natural_gas_cost_therm = 1.04,
    newspaper_recycling_avoided_emissions = -113.14,
    NG_CO2_annual_emissions = 7892,
    nonCO2_vehicle_emissions_ratio = 1.01,
    plastic_recycling_avoided_emissions = -35.56,
    propane_cost = 2.47,
    thermostat_cooling_savings = 0.06,
    thermostat_heating_savings = 0.03,
    vehicle_efficiency_improvements = 0.04,
    window_replacement_cost_savings = 150,
    window_replacement_energy_savings = 25210000
  )
  
  # Conversion factors data frame
  conversion_factors <- data.frame(
    Energy_Source = c("Natural Gas", "Electricity", "Fuel Oil", "Propane"),
    Conversion_Factor = c(50, 40, 30, 20),
    Unit_Price = c(1.00, 0.80, 0.60, 0.40),
    Unit_Type = c(2, 1, 1, 1)  # Example unit types
  )
  
  # Helper function to safely convert to numeric and avoid NA/NULL issues
  safe_numeric <- function(x) {
    if (is.null(x) || !is.numeric(x) || is.na(as.numeric(x))) {
      return(0)
    } else {
      return(as.numeric(x))
    }
  }
  
  safe_numeric <- function(x) {
    if (is.null(x) || is.na(x)) {
      print(paste("Null or NA input detected, value:", x))
      return(0)
    } else if (!is.numeric(x)) {
      x_as_numeric <- as.numeric(x)
      if (is.na(x_as_numeric)) {
        print(paste("Conversion to numeric resulted in NA, original value:", x))
        return(0)
      } else {
        return(x_as_numeric)
      }
    } else {
      return(x)
    }
  }
  
  # Reactive expression to render vehicle inputs
  output$vehicleInputs <- renderUI({
    # Clear previous outputs if any
    output$options <- renderUI({})
    
    # Take the input value for number of vehicles
    num_vehicles <- input$numVehicles
    
    # Check if the number of vehicles is greater than 0
    if(num_vehicles > 0) {
      # Generate UI elements for each vehicle
      input_list <- lapply(1:num_vehicles, function(i) {
        fluidRow(
          column(6, numericInput(paste0("miles_driven_", i), sprintf("Miles driven for Vehicle %d", i), value = 0)),
          column(6, numericInput(paste0("mpg_", i), sprintf("Miles per Gallon for Vehicle %d", i), value = 21.6))
        )
      })
      do.call(tagList, input_list)
    } else {
      # If no vehicles are specified, return nothing
      return(NULL)
    }
  })
  
  observeEvent(input$submit, {
    
    print("Submit clicked!")
    
    # Function to calculate t126 based on Energy_Source
    t126 <- function(energy_source) {
      source_index <- which(conversion_factors$Energy_Source == energy_source)
      if (length(source_index) > 0) {
        return(conversion_factors$Conversion_Factor[source_index])
      } else {
        return(1)  # Default conversion factor if not found
      }
    }
    
    # Function to calculate t127 based on Energy_Source
    t127 <- function(energy_source) {
      source_index <- which(conversion_factors$Energy_Source == energy_source)
      if (length(source_index) > 0) {
        return(conversion_factors$Unit_Price[source_index])
      } else {
        return(1)  # Default unit price if not found
      }
    }
    
    h126 <- reactive({
      # Check if input is correctly triggering the reactive
      print(paste("Input f126:", input$f126))
      
      if (input$f126 == "1") {
        energy_source <- "Natural Gas"
        factor = t126(energy_source)
        price = t127(energy_source)
        adjustment = safe_numeric(input$b127)  # Ensure b127 is connected in UI
        savings = factor * price * constants$thermostat_heating_savings * adjustment
        
        # Debugging outputs
        print(paste("Energy Source:", energy_source))
        print(paste("Conversion Factor (t126):", factor))
        print(paste("Unit Price (t127):", price))
        print(paste("Thermostat Adjustment Savings:", constants$thermostat_heating_savings))
        print(paste("Degree Adjustment (safe_numeric):", adjustment))
        print(paste("Calculated Savings:", savings))
        
        return(savings)
      } else {
        print("Thermostat adjustment not applicable.")
        return(0)
      }
    })
    # Output for h126 - Heating Savings
    output$h126_output <- renderText({
      savings <- h126()
      if (savings > 0) {
        paste("Estimated heating savings from thermostat adjustment: $", round(savings, 2))
      } else {
        "No heating savings calculated or applicable."
      }
    })
    
    # Simplified conditional checks
    h130 <- reactive({
      print(paste("Input f130 (AC adjust?):", input$f130))
      print(paste("Input h42 (Billing method):", input$h42))
      print(paste("Input b131 (Degrees to adjust AC):", input$b131))
      print(paste("Input f42 (Monthly usage):", input$f42))
      print(paste("AC Electricity Percent:", constants$AC_electricity_percent))
      print(paste("Thermostat Cooling Savings:", constants$thermostat_cooling_savings))
      print(paste("Cost per kWh:", constants$cost_per_kWh))
      # Directly using inputs for conditions
      if (input$f130 == "1") {  # Assuming the input is received as a character string
        print("AC adjustment is applicable")
        degrees = as.numeric(input$b131)  # Ensure conversion to numeric
        usage = as.numeric(input$f42)  # Ensure conversion to numeric
        
        if (input$h42 == "1") {
          print("Calculating savings based on dollars")
          cooling_savings <- usage * constants$AC_electricity_percent * constants$thermostat_cooling_savings * degrees * 12
          print(paste("Cooling Savings Calculated (Dollars):", cooling_savings))
        } else if (input$h42 == "2") {
          print("Calculating savings based on kWh")
          cooling_savings <- usage * constants$AC_electricity_percent * constants$thermostat_cooling_savings * degrees * constants$cost_per_kWh * 12
          print(paste("Cooling Savings Calculated (kWh):", cooling_savings))
        } else {
          print("Billing method not recognized")
          cooling_savings <- 0
        }
        
        return(cooling_savings)
      } else {
        print("No AC adjustment made due to input f130")
        return(0)
      }
    })
    # Output Cooling Savings to UI
    output$h130_output <- renderText({
      cooling_savings <- h130()
      if (cooling_savings > 0) {
        paste("Estimated cooling savings: $", format(round(cooling_savings, 2), nsmall = 2))
      } else {
        "No cooling savings calculated."
      }
    })
    
    # H134 - Computer Sleep Savings
    h134 <- reactive({
      print(paste("Input f134:", input$f134))
      numeric_f134 <- safe_numeric(input$f134)
      print(paste("Numeric f134:", numeric_f134))
      savings <- if (safe_numeric(input$f134) == 1) {
        print("Computer sleep mode savings applicable")
        computed_savings <- constants$computer_sleep_savings * constants$cost_per_kWh
        print(paste("Computer Sleep Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No computer sleep mode savings calculated")
        0
      }
      print(paste("Output Savings for H134:", savings))
      return(savings)
    })
    # Output for h134 - Computer Sleep Savings
    output$h134_output <- renderText({
      savings <- h134()
      if (savings > 0) {
        paste("Estimated computer sleep mode savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No computer sleep mode savings."
      }
    })
    
    # H137 - Laundry Load Savings
    h137 <- reactive({
      print(paste("Input f137:", input$f137))
      print(paste("Input b139 (Loads per week):", input$b139))
      savings <- if (safe_numeric(input$f137) == 1) {
        print("Laundry load savings applicable")
        computed_savings <- constants$kWh_per_load_laundry * constants$cost_per_kWh * 52 * safe_numeric(input$b139)
        print(paste("Laundry Load Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No laundry load savings calculated")
        0
      }
      print(paste("Output Savings for H137:", savings))
      return(savings)
    })
    # Output for h137 - Laundry Load Savings
    output$h137_output <- renderText({
      savings <- h137()
      if (savings > 0) {
        paste("Estimated laundry load savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No laundry load savings."
      }
    })
    
    # H140 - Dryer Energy Savings
    h140 <- reactive({
      print(paste("Input f140:", input$f140))
      savings <- if (safe_numeric(input$f140) == 1) {
        print("Dryer energy savings applicable")
        computed_savings <- (constants$dryer_energy / 2) * constants$cost_per_kWh
        print(paste("Dryer Energy Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No dryer energy savings calculated")
        0
      }
      print(paste("Output Savings for H140:", savings))
      return(savings)
    })
    # Output for h140 - Dryer Energy Savings
    output$h140_output <- renderText({
      savings <- h140()
      if (savings > 0) {
        paste("Estimated dryer energy savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No dryer energy savings."
      }
    })
    
    # H147 - Lamp Cost Savings
    h147 <- reactive({
      print(paste("Input f147:", input$f147))
      print(paste("Input b147 (Number of bulbs):", input$b147))
      savings <- if (safe_numeric(input$f147) == 1) {
        print("Lamp cost savings applicable")
        computed_savings <- safe_numeric(input$b147) * constants$lamp_cost_savings
        print(paste("Lamp Cost Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No lamp cost savings calculated")
        0
      }
      print(paste("Output Savings for H147:", savings))
      return(savings)
    })
    # Output for h147 - Lamp Cost Savings
    output$h147_output <- renderText({
      savings <- h147()
      if (savings > 0) {
        paste("Estimated lamp cost savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No lamp cost savings."
      }
    })
    
    # H151 - Fridge Replacement kWh Savings
    h151 <- reactive({
      print(paste("Input f151:", input$f151))
      savings <- if (safe_numeric(input$f151) == 1) {
        print("Fridge replacement kWh savings applicable")
        computed_savings <- constants$fridge_replacement_kWh_savings * constants$cost_per_kWh
        print(paste("Fridge Replacement kWh Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No fridge replacement kWh savings calculated")
        0
      }
      print(paste("Output Savings for H151:", savings))
      return(savings)
    })
    # Output for h151 - Fridge Replacement kWh Savings
    output$h151_output <- renderText({
      savings <- h151()
      if (savings > 0) {
        paste("Estimated fridge replacement kWh savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No fridge replacement kWh savings."
      }
    })
    
    # H158 - Boiler Replacement Cost Savings
    h158 <- reactive({
      print(paste("Input f158:", input$f158))
      print(paste("Input f7 (Primary heating source):", input$f7))
      savings <- if (safe_numeric(input$f158) == 1 && safe_numeric(input$f7) %in% c(1, 3)) {
        print("Boiler replacement cost savings applicable")
        computed_savings <- constants$boiler_replacement_cost_savings
        print(paste("Boiler Replacement Cost Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No boiler replacement cost savings calculated")
        0
      }
      print(paste("Output Savings for H158:", savings))
      return(savings)
    })
    # Output for h158 - Boiler Replacement Cost Savings
    output$h158_output <- renderText({
      savings <- h158()
      if (savings > 0) {
        paste("Estimated boiler replacement cost savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No boiler replacement cost savings."
      }
    })
    
    # H163 - Window Replacement Cost Savings
    h163 <- reactive({
      print(paste("Input f163:", input$f163))
      print(paste("Input f7 (Primary heating source):", input$f7))
      savings <- if (safe_numeric(input$f163) == 1 && safe_numeric(input$f7) != 6) {
        print("Window replacement cost savings applicable")
        computed_savings <- constants$window_replacement_cost_savings
        print(paste("Window Replacement Cost Savings Calculated:", computed_savings))
        computed_savings
      } else {
        print("No window replacement cost savings calculated")
        0
      }
      print(paste("Output Savings for H163:", savings))
      return(savings)
    })
    
    # Output for h163 - Window Replacement Cost Savings
    output$h163_output <- renderText({
      savings <- h163()
      if (savings > 0) {
        paste("Estimated window replacement cost savings: $", format(round(savings, 2), nsmall = 2))
      } else {
        "No window replacement cost savings."
      }
    })
    
    #  })
    #}
    #
    
    #
    # # Reactive expressions for the J cell calculations
    #     # J27 - Vehicle emissions calculation with debugging
    #     J27 <- reactive({
    #       print(paste("Input D15 (miles driven):", input$D15))
    #       print(paste("Input G15 (calculation method):", input$G15))
    #       print(paste("Input K15 (fuel efficiency):", input$K15))
    #       result <- 0
    #       if (safe_numeric(input$D15) != 0) {
    #         result <- ifelse(safe_numeric(input$G15) == 1,
    #                          (safe_numeric(input$D15) * 52) / safe_numeric(input$K15) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio,
    #                          safe_numeric(input$D15) / safe_numeric(input$K15) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio)
    #       }
    #       print(paste("J27 Vehicle emissions result:", result))
    #       result + J29()  # Adding J29 since it's part of the formula
    #     })
    #     
    #     # Assuming J29 is similar to J27
    #     J29 <- reactive({
    #       J27()  # Directly use J27 if J29 is indeed the same
    #     })
    
    # J37 - Natural gas CO2 emissions with debugging
    J37 <- reactive({
      print(paste("Input H37 (billing unit):", input$H37))
      print(paste("Input F37 (usage amount):", input$F37))
      case_when(
        safe_numeric(input$H37) == 1 ~ (safe_numeric(input$F37) / constants$Natural_gas_cost_1000CF) * constants$EF_natural_gas * 12,
        safe_numeric(input$H37) == 2 ~ constants$EF_natural_gas * safe_numeric(input$F37) * 12,
        safe_numeric(input$H37) == 3 ~ constants$EF_natural_gas_therm * safe_numeric(input$F37) * 12,
        TRUE ~ {
          print("No natural gas CO2 emissions calculated due to invalid billing unit.")
          0
        }
      )
    })
    
    # J42 - Electricity CO2 emissions with debugging
    J42 <- reactive({
      print(paste("Input F45 (green power purchase):", input$F45))
      print(paste("Input H42 (billing unit):", input$H42))
      print(paste("Input F42 (usage amount):", input$F42))
      print(paste("Input F49 (green power percentage):", input$F49))
      if (safe_numeric(input$F45) == 2) {
        case_when(
          safe_numeric(input$H42) == 1 ~ (safe_numeric(input$F42) / constants$cost_per_kWh) * constants$e_factor_value * 12,
          safe_numeric(input$H42) == 2 ~ safe_numeric(input$F42) * constants$e_factor_value * 12,
          TRUE ~ 0
        )
      } else {
        case_when(
          safe_numeric(input$H42) == 1 ~ ((safe_numeric(input$F42) / constants$cost_per_kWh) * constants$e_factor_value * 12) * (1 - safe_numeric(input$F49)),
          safe_numeric(input$H42) == 2 ~ safe_numeric(input$F42) * 12 * (1 - safe_numeric(input$F49)) * constants$e_factor_value,
          TRUE ~ 0
        )
      }
    })
    
    
    #     
    # J27 <- reactive({
    #   # Example translation of the Excel formula for J27
    #   # Replace 'input$D15', 'input$G15', etc., with your actual input IDs from the Shiny app
    #   result <- 0
    #   if (input$D15 != 0) {
    #     result <- ifelse(input$G15 == 1,
    #                      (input$D15 * 52) / input$K15 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio,
    #                      input$D15 / input$K15 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio)
    #   }
    #   result + J29()  # Adding J29 since it's part of the formula
    # })
    # 
    # # J29 appears to be the same as J27, so you can actually reuse J27
    # J29 <- reactive({
    #   J27()  # Directly use J27 if J29 is indeed the same
    # })
    # 
    # # J37 - Natural gas CO2 emissions
    # J37 <- reactive({
    #   case_when(
    #     input$H37 == 1 ~ (input$F37 / Natural_gas_cost_1000CF) * EF_natural_gas * 12,
    #     input$H37 == 2 ~ EF_natural_gas * input$F37 * 12,
    #     input$H37 == 3 ~ EF_natural_gas_therm * input$F37 * 12,
    #     TRUE ~ 0
    #   )
    # })
    # 
    # # J42 - Electricity CO2 emissions
    # J42 <- reactive({
    #   if (input$F45 == 2) {
    #     case_when(
    #       input$H42 == 1 ~ (input$F42 / cost_per_kWh) * e_factor_value * 12,
    #       input$H42 == 2 ~ input$F42 * e_factor_value * 12,
    #       TRUE ~ 0
    #     )
    #   } else {
    #     case_when(
    #       input$H42 == 1 ~ ((input$F42 / cost_per_kWh) * e_factor_value * 12) * (1 - input$F49),
    #       input$H42 == 2 ~ input$F42 * 12 * (1 - input$F49) * e_factor_value,
    #       TRUE ~ 0
    #     )
    #   }
    # })
    
    # # J53 - Fuel oil CO2 emissions
    # J53 <- reactive({
    #   case_when(
    #     input$H53 == 1 ~ (input$F53 / fuel_oil_cost) * EF_fuel_oil_gallon * 12,
    #     input$H53 == 2 ~ EF_fuel_oil_gallon * input$F53 * 12,
    #     TRUE ~ 0
    #   )
    # })
    # 
    # # J57 - Propane CO2 emissions
    # J57 <- reactive({
    #   case_when(
    #     input$H57 == 1 ~ (input$F57 / propane_cost) * EF_propane * 12,
    #     input$H57 == 2 ~ EF_propane * input$F57 * 12,
    #     TRUE ~ 0
    #   )
    # })
    # 
    # # J63 - Waste emissions
    # J63 <- reactive({
    #   input$F5 * average_waste_emissions  # Assuming input$F5 is the number of people in the household
    # })
    # 
    # # J65 - Metal recycling avoided emissions
    # J65 <- reactive({
    #   if (input$F65 == 1) {
    #     input$F5 * metal_recycling_avoided_emissions  # Assuming input$F5 is the number of people in the household
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J67 - Plastic recycling avoided emissions
    # J67 <- reactive({
    #   if(input$F67 == 1) {
    #     input$F5 * plastic_recycling_avoided_emissions
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J69 - Glass recycling avoided emissions
    # J69 <- reactive({
    #   if(input$F69 == 1) {
    #     input$F5 * glass_recycling_avoided_emissions
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J71 - Newspaper recycling avoided emissions
    # J71 <- reactive({
    #   if(input$F71 == 1) {
    #     input$F5 * newspaper_recycling_avoided_emissions
    #   } else {
    #     0
    #   }
    # })
    
    #     
    #     
    # # J73 - Magazine recycling avoided emissions
    # J73 <- reactive({
    #   if(input$F73 == 1) {
    #     input$F5 * mag_recycling_avoided_emissions
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J77 - Sum of recycling emissions
    # J77 <- reactive({
    #   J63() + J65() + J67() + J69() + J71() + J73()
    # })
    # 
    # # J82 - Total emissions
    # J82 <- reactive({
    #   # Assuming J26 is defined elsewhere
    #   J26() + J37() + J42() + J53() + J57() + J77()
    # })
    
    # J53 - Fuel oil CO2 emissions with debugging
    J53 <- reactive({
      print(paste("Input H53:", input$H53))
      print(paste("Input F53:", input$F53))
      result <- case_when(
        input$H53 == 1 ~ {
          calculated <- (input$F53 / constants$fuel_oil_cost) * constants$EF_fuel_oil_gallon * 12
          print(paste("Calculated J53 (Dollars):", calculated))
          calculated
        },
        input$H53 == 2 ~ {
          calculated <- constants$EF_fuel_oil_gallon * input$F53 * 12
          print(paste("Calculated J53 (Gallons):", calculated))
          calculated
        },
        TRUE ~ {
          print("Input H53 not recognized for J53 calculation.")
          0
        }
      )
      print(paste("Result for J53:", result))
      result
    })
    
    # J57 - Propane CO2 emissions with debugging
    J57 <- reactive({
      print(paste("Input H57:", input$H57))
      print(paste("Input F57:", input$F57))
      result <- case_when(
        input$H57 == 1 ~ {
          calculated <- (input$F57 / constants$propane_cost) * constants$EF_propane * 12
          print(paste("Calculated J57 (Dollars):", calculated))
          calculated
        },
        input$H57 == 2 ~ {
          calculated <- constants$EF_propane * input$F57 * 12
          print(paste("Calculated J57 (Gallons):", calculated))
          calculated
        },
        TRUE ~ {
          print("Input H57 not recognized for J57 calculation.")
          0
        }
      )
      print(paste("Result for J57:", result))
      result
    })
    
    # J63 - Waste emissions with debugging
    J63 <- reactive({
      print(paste("Input F5 (number of people):", input$F5))
      result <- input$F5 * constants$average_waste_emissions
      print(paste("Calculated J63:", result))
      result
    })
    
    # J65 - Metal recycling avoided emissions with debugging
    J65 <- reactive({
      print(paste("Input F65:", input$F65))
      print(paste("Input F5 (number of people):", input$F5))
      result <- if (input$F65 == 1) {
        input$F5 * constants$metal_recycling_avoided_emissions
      } else {
        0
      }
      print(paste("Result for J65:", result))
      result
    })
    
    # J67 - Plastic recycling avoided emissions with debugging
    J67 <- reactive({
      print(paste("Input F67:", input$F67))
      print(paste("Input F5 (number of people):", input$F5))
      result <- if(input$F67 == 1) {
        input$F5 * constants$plastic_recycling_avoided_emissions
      } else {
        0
      }
      print(paste("Result for J67:", result))
      result
    })
    
    # J69 - Glass recycling avoided emissions with debugging
    J69 <- reactive({
      print(paste("Input F69:", input$F69))
      print(paste("Input F5 (number of people):", input$F5))
      result <- if(input$F69 == 1) {
        input$F5 * constants$glass_recycling_avoided_emissions
      } else {
        0
      }
      print(paste("Result for J69:", result))
      result
    })
    
    # J71 - Newspaper recycling avoided emissions with debugging
    J71 <- reactive({
      print(paste("Input F71:", input$F71))
      print(paste("Input F5 (number of people):", input$F5))
      result <- if(input$F71 == 1) {
        input$F5 * constants$newspaper_recycling_avoided_emissions
      } else {
        0
      }
      print(paste("Result for J71:", result))
      result
    })
    
    # J77 - Sum of recycling emissions with debugging
    J77 <- reactive({
      print("Calculating sum of all recycling avoided emissions.")
      result <- J63() + J65() + J67() + J69() + J71() + J73()
      print(paste("Result for J77:", result))
      result
    })
    
    # J82 - Total emissions with debugging
    J82 <- reactive({
      print("Calculating total emissions.")
      result <- J26() + J37() + J42() + J53() + J57() + J77()
      print(paste("Result for J82:", result))
      result
    })
    
    # 
    # 
    # # J91 - Personal vehicle emissions reduction
    # J91 <- reactive({
    #   if(input$F91 == 1) {
    #     if(input$F109 == 1) {
    #       ((input$D91 * 52) / (input$K15 + input$D109)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     } else {
    #       (input$D91 * 52) / input$K15 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     }
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J93 - Reduction due to change in car for D93
    # J93 <- reactive({
    #   if(input$F93 == 1) {
    #     if(input$F111 == 1) {
    #       ((input$D93 * 52) / (input$K17 + input$D111)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     } else {
    #       (input$D93 * 52) / input$K17 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     }
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J95 - Reduction due to change in car for D95
    # J95 <- reactive({
    #   if(input$F95 == 1) {
    #     if(input$F113 == 1) {
    #       ((input$D95 * 52) / (input$K19 + input$D113)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     } else {
    #       (input$D95 * 52) / input$K19 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     }
    #   } else {
    #     0
    #   }
    # })
    # 
    # # J104 - Vehicle efficiency improvements
    # J104 <- reactive({
    #   if(input$F29 == 1) {
    #     # Calculation would include contributions from D15, D17, D19, D21, and D23
    #     # Replace the following with the correct calculation logic for vehicle efficiency improvements
    #   } else {
    #     0
    #   }
    # })
    # 
    # J91 <- reactive({
    #   if (safe_numeric(input$F91) == 1) {
    #     miles <- safe_numeric(input$D91)
    #     mpg_current <- safe_numeric(input$K15)
    #     mpg_improvement <- safe_numeric(input$D109)
    #     improvement_selected <- safe_numeric(input$F109)
    #     
    #     emission_factor <- EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
    #     weeks_per_year <- 52
    #     
    #     # Calculate with or without improvement
    #     if (improvement_selected == 1 && mpg_improvement > 0) {
    #       ((miles * weeks_per_year) / (mpg_current + mpg_improvement)) * emission_factor
    #     } else {
    #       (miles * weeks_per_year) / mpg_current * emission_factor
    #     }
    #   } else {
    #     0
    #   }
    # })
    # # J93 - Reduction due to change in car for D93 with debugging
    # J93 <- reactive({
    #   print(paste("Input F93:", input$F93))
    #   print(paste("Input D93 (miles):", input$D93))
    #   print(paste("Input K17 (mpg for D93):", input$K17))
    #   print(paste("Input F111 (improvement for D93?):", input$F111))
    #   print(paste("Input D111 (mpg improvement for D93):", input$D111))
    #   
    #   result <- if(input$F93 == 1) {
    #     if(input$F111 == 1) {
    #       calculated <- ((input$D93 * 52) / (input$K17 + input$D111)) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
    #       print(paste("Calculated J93 with improvement:", calculated))
    #       calculated
    #     } else {
    #       calculated <- (input$D93 * 52) / input$K17 * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
    #       print(paste("Calculated J93 without improvement:", calculated))
    #       calculated
    #     }
    #   } else {
    #     print("No J93 savings - input F93 is not 1.")
    #     0
    #   }
    #   
    #   print(paste("Result for J93:", result))
    #   result
    # })
    # 
    # # J95 - Reduction due to change in car for D95 with debugging
    # J95 <- reactive({
    #   print(paste("Input F95:", input$F95))
    #   print(paste("Input D95 (miles):", input$D95))
    #   print(paste("Input K19 (mpg for D95):", input$K19))
    #   print(paste("Input F113 (improvement for D95?):", input$F113))
    #   print(paste("Input D113 (mpg improvement for D95):", input$D113))
    #   
    #   result <- if(input$F95 == 1) {
    #     if(input$F113 == 1) {
    #       calculated <- ((input$D95 * 52) / (input$K19 + input$D113)) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
    #       print(paste("Calculated J95 with improvement:", calculated))
    #       calculated
    #     } else {
    #       calculated <- (input$D95 * 52) / input$K19 * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
    #       print(paste("Calculated J95 without improvement:", calculated))
    #       calculated
    #     }
    #   } else {
    #     print("No J95 savings - input F95 is not 1.")
    #     0
    #   }
    #   
    #   print(paste("Result for J95:", result))
    #   result
    # })
    # 
    # # J104 - Vehicle efficiency improvements with debugging
    # # Assuming the logic for J104 would depend on multiple inputs for various vehicles (D15, D17, etc.)
    # # For this example, let's consider improvements based on a single vehicle as a placeholder
    # J104 <- reactive({
    #   print(paste("Input F29 (efficiency improvement selected?):", input$F29))
    #   print(paste("Vehicle data (D15, D17, etc.):", "Placeholder for actual inputs"))
    #   
    #   result <- if(input$F29 == 1) {
    #     # Placeholder for the correct calculation logic for vehicle efficiency improvements
    #     # The following is a dummy calculation as an example
    #     dummy_improvement <- 0.1  # Assuming a 10% efficiency improvement
    #     savings <- sum(input$D15, input$D17) * dummy_improvement * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
    #     print(paste("Calculated J104 vehicle efficiency savings:", savings))
    #     savings
    #   } else {
    #     print("No J104 savings - input F29 is not 1.")
    #     0
    #   }
    #   
    #   print(paste("Result for J104:", result))
    #   result
    # })
    
    
    J109 <- reactive({
      print("J109 calculation triggered")
      print(paste("Input F109:", input$F109))
      
      if(input$F109 == 1) {
        reduction <- J91() - J27()
        print(paste("J91 value:", J91()))
        print(paste("J27 value:", J27()))
        print(paste("Emissions reduction (J109):", reduction))
        return(reduction)
      } else {
        print("No change in car, no reduction (J109)")
        return(0)
      }
    })
    J111 <- reactive({
      print("J111 calculation triggered")
      print(paste("Input F111:", input$F111))
      
      if(input$F111 == 1) {
        old_emissions <- ifelse(input$G17 == 1, (input$D17 * 52) / input$K17, input$D17 / input$K17) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G17 == 1, (input$D17 * 52) / (input$K17 + input$D111), input$D17 / (input$K17 + input$D111)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        reduction <- old_emissions - new_emissions
        
        print(paste("Old emissions (J111):", old_emissions))
        print(paste("New emissions (J111):", new_emissions))
        print(paste("Emissions reduction (J111):", reduction))
        
        return(reduction)
      } else {
        print("No car change for D17, no reduction (J111)")
        return(0)
      }
    })
    J113 <- reactive({
      print("J113 calculation triggered")
      print(paste("Input F113:", input$F113))
      
      if(input$F113 == 1) {
        old_emissions <- ifelse(input$G19 == 1, (input$D19 * 52) / input$K19, input$D19 / input$K19) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G19 == 1, (input$D19 * 52) / (input$K19 + input$D113), input$D19 / (input$K19 + input$D113)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        reduction <- old_emissions - new_emissions
        
        print(paste("Old emissions (J113):", old_emissions))
        print(paste("New emissions (J113):", new_emissions))
        print(paste("Emissions reduction (J113):", reduction))
        
        return(reduction)
      } else {
        print("No car change for D19, no reduction (J113)")
        return(0)
      }
    })
    # Output for J113 - Reduction due to change in car for D19
    output$J113_output <- renderText({
      result <- J113()
      if (result > 0) {
        paste("Reduction due to change in car for D19: ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No reduction calculated for car change D19."
      }
    })
    
    J115 <- reactive({
      print("J115 calculation triggered")
      print(paste("Input F115:", input$F115))
      
      if (input$F115 == 1) {
        old_emissions <- ifelse(input$G21 == 1, (input$D21 * 52) / input$K21, input$D21 / input$K21) * 19.4 * (100/95)
        new_emissions <- ifelse(input$G21 == 1, (input$D21 * 52) / (input$K21 + input$D115), input$D21 / (input$K21 + input$D115)) * 19.4 * (100/95)
        reduction <- old_emissions - new_emissions
        
        print(paste("Old emissions for D21 (J115):", old_emissions))
        print(paste("New emissions for D21 (J115):", new_emissions))
        print(paste("Emissions reduction for D21 (J115):", reduction))
        
        return(reduction)
      } else {
        print("No vehicle efficiency improvement for D21 (J115)")
        return(0)
      }
    })
    # Output for J115
    output$J115_output <- renderText({
      result <- J115()
      if (result != 0) {
        paste("Estimated emissions reduction for vehicle D21:", format(round(result, 2), nsmall = 2), "units")
      } else {
        "No emissions reduction calculated for vehicle D21."
      }
    })
    
    J117 <- reactive({
      print("J117 calculation triggered")
      print(paste("Input F117:", input$F117))
      
      if (input$F117 == 1) {
        old_emissions <- ifelse(input$G23 == 1, (input$D23 * 52) / input$K23, input$D23 / input$K23) * 19.4 * (100/95)
        new_emissions <- ifelse(input$G23 == 1, (input$D23 * 52) / (input$K23 + input$D117), input$D23 / (input$K23 + input$D117)) * 19.4 * (100/95)
        reduction <- old_emissions - new_emissions
        
        print(paste("Old emissions for D23 (J117):", old_emissions))
        print(paste("New emissions for D23 (J117):", new_emissions))
        print(paste("Emissions reduction for D23 (J117):", reduction))
        
        return(reduction)
      } else {
        print("No vehicle efficiency improvement for D23 (J117)")
        return(0)
      }
    })
    # Output for J117
    output$J117_output <- renderText({
      result <- J117()
      if (result != 0) {
        paste("Estimated emissions reduction for vehicle D23:", format(round(result, 2), nsmall = 2), "units")
      } else {
        "No emissions reduction calculated for vehicle D23."
      }
    })
    
    J119 <- reactive({
      total_sum <- sum(
        J117(), J115(), J113(), J111(), J109(), J29(),
        J99(), J97(), J95(), J93(), J91()
      )
      
      print("J119 calculation triggered")
      print(paste("Components of J119 sum:", list(J117(), J115(), J113(), J111(), J109(), J29(), J99(), J97(), J95(), J93(), J91())))
      print(paste("Total sum for J119:", total_sum))
      
      return(total_sum)
    })
    # Output for J119
    output$J119_output <- renderText({
      total_sum <- J119()
      if (total_sum != 0) {
        paste("Total emissions reduction from various sources (J119):", format(round(total_sum, 2), nsmall = 2), "units")
      } else {
        "No total emissions reduction calculated."
      }
    })
    
    J120 <- reactive({
      total_sum <- sum(
        J117(), J115(), J113(), J111(), J109(), J29(),
        J99(), J97(), J95(), J93(), J91()
      )
      
      print("J120 calculation triggered")
      print(paste("Sum of specified Js (J120):", total_sum))
      
      return(total_sum)
    })
    # Output for J120 - Sum of Specific J's
    output$J120_output <- renderText({
      result <- J120()
      if (result != 0) {
        paste("Sum of specified J calculations: ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No cumulative savings calculated across specified categories."
      }
    })
    
    # J126 - Savings from thermostat heating adjustments based on heating source
    J126 <- reactive({
      print(paste("Input f126 (thermostat adjustment?):", input$f126))
      print(paste("Input f7 (primary heating source):", input$f7))
      print(paste("Input b127 (degrees to adjust):", input$b127))
      
      result <- if (safe_numeric(input$f126) == 1) {
        adjustment <- safe_numeric(input$b127)
        print(paste("Heating source adjustment value:", adjustment))
        
        savings <- switch(as.character(input$f7),
                          "1" = (J37() * constants$heating_percent_NG * constants$thermostat_heating_savings * adjustment),
                          "2" = (J42() * constants$heating_percent_electricity * constants$thermostat_cooling_savings * adjustment),
                          "3" = (J53() * constants$heating_percent_fuel_oil * constants$thermostat_heating_savings * adjustment),
                          "4" = (J57() * constants$heating_percent_propane * constants$thermostat_heating_savings * adjustment),
                          print("No valid heating source selected for J126."),
                          0
        )
        print(paste("Calculated J126 savings:", savings))
        savings
      } else {
        print("No J126 savings - thermostat adjustment not selected.")
        0
      }
      
      print(paste("Result for J126:", result))
      result
    })
    # Output for J126 - Savings from Thermostat Heating Adjustments
    output$J126_output <- renderText({
      result <- J126()
      if (result > 0) {
        paste("Savings from thermostat heating adjustments: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from thermostat heating adjustments."
      }
    })
    
    # J130 - Savings from cooling adjustments
    J130 <- reactive({
      print(paste("Input f130 (AC adjustment?):", input$f130))
      print(paste("Input b131 (degrees to cool):", input$b131))
      
      result <- if (safe_numeric(input$f130) == 1) {
        adjustment <- safe_numeric(input$b131)
        savings <- J42() * constants$AC_electricity_percent * constants$thermostat_cooling_savings * adjustment
        print(paste("Calculated J130 cooling savings:", savings))
        savings
      } else {
        print("No J130 savings - AC adjustment not selected.")
        0
      }
      
      print(paste("Result for J130:", result))
      result
    })
    # Output for J130 - Savings from Cooling Adjustments
    output$J130_output <- renderText({
      result <- J130()
      if (result > 0) {
        paste("Savings from cooling adjustments: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from cooling adjustments."
      }
    })
    
    # J134 - Savings from enabling computer sleep
    J134 <- reactive({
      print(paste("Input f134 (enable sleep mode?):", input$f134))
      
      result <- if (safe_numeric(input$f134) == 1) {
        savings <- constants$computer_sleep_savings * constants$cost_per_kWh
        print(paste("Calculated J134 computer sleep savings:", savings))
        savings
      } else {
        print("No J134 savings - computer sleep mode not enabled.")
        0
      }
      
      print(paste("Result for J134:", result))
      result
    })
    # Output for J134 - Savings from Enabling Computer Sleep
    output$J134_output <- renderText({
      result <- J134()
      if (result > 0) {
        paste("Savings from enabling computer sleep: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from enabling computer sleep."
      }
    })
    
    # J137 - Savings from washing clothes in cold water
    J137 <- reactive({
      print(paste("Input f137 (wash in cold water?):", input$f137))
      print(paste("Input b139 (loads per week):", input$b139))
      
      result <- if (safe_numeric(input$f137) == 1) {
        loads <- safe_numeric(input$b139)
        savings <- constants$kWh_per_load_laundry * constants$cost_per_kWh * 52 * loads
        print(paste("Calculated J137 laundry savings:", savings))
        savings
      } else {
        print("No J137 savings - not washing in cold water.")
        0
      }
      
      print(paste("Result for J137:", result))
      result
    })
    # Output for J137 - Savings from Washing Clothes in Cold Water
    output$J137_output <- renderText({
      result <- J137()
      if (result > 0) {
        paste("Savings from washing clothes in cold water: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from washing clothes in cold water."
      }
    })
    
    # J140 - Savings from using a drying rack or line drying half the time
    J140 <- reactive({
      print(paste("Input f140 (use drying rack?):", input$f140))
      
      result <- if (safe_numeric(input$f140) == 1) {
        savings <- (constants$dryer_energy / 2) * constants$cost_per_kWh
        print(paste("Calculated J140 drying rack savings:", savings))
        savings
      } else {
        print("No J140 savings - drying rack not used.")
        0
      }
      
      print(paste("Result for J140:", result))
      result
    })
    # Output for J140 - Savings from Using a Drying Rack
    output$J140_output <- renderText({
      result <- J140()
      if (result > 0) {
        paste("Savings from using a drying rack or line drying: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from using a drying rack or line drying."
      }
    })
    
    # J143 - Green power purchase emissions savings
    J143 <- reactive({
      print(paste("Input f143 (buy green power?):", input$f143))
      print(paste("Input b143 (percentage of use):", input$b143))
      
      result <- if (safe_numeric(input$f143) == 1) {
        percentage <- safe_numeric(input$b143) / 100
        savings <- percentage * J42()
        print(paste("Calculated J143 green power savings:", savings))
        savings
      } else {
        print("No J143 savings - green power not purchased.")
        0
      }
      
      print(paste("Result for J143:", result))
      result
    })
    # Output for J143 - Green Power Purchase Emissions Savings
    output$J143_output <- renderText({
      result <- J143()
      if (result > 0) {
        paste("Green power purchase emissions savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No green power purchase emissions savings."
      }
    })
    
    # J147 - ENERGY STAR lights savings
    J147 <- reactive({
      print(paste("Input f147 (replace bulbs?):", input$f147))
      print(paste("Input b147 (number of bulbs):", input$b147))
      
      result <- if (safe_numeric(input$f147) == 1) {
        bulbs <- safe_numeric(input$b147)
        savings <- bulbs * constants$lamp_kWh_savings * constants$cost_per_kWh
        print(paste("Calculated J147 ENERGY STAR bulb savings:", savings))
        savings
      } else {
        print("No J147 savings - bulbs not replaced with ENERGY STAR.")
        0
      }
      
      print(paste("Result for J147:", result))
      result
    })
    # Output for J147 - ENERGY STAR Lights Savings
    output$J147_output <- renderText({
      result <- J147()
      if (result > 0) {
        paste("ENERGY STAR lights savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No ENERGY STAR lights savings."
      }
    })
    
    # J151 - ENERGY STAR fridge kWh savings
    J151 <- reactive({
      print(paste("Input f151 (replace fridge?):", input$f151))
      
      result <- if (safe_numeric(input$f151) == 1) {
        savings <- constants$fridge_replacement_kWh_savings * constants$cost_per_kWh
        print(paste("Calculated J151 fridge replacement savings:", savings))
        savings
      } else {
        print("No J151 savings - fridge not replaced with ENERGY STAR.")
        0
      }
      
      print(paste("Result for J151:", result))
      result
    })
    # Output for J151 - ENERGY STAR Fridge kWh Savings
    output$J151_output <- renderText({
      result <- J151()
      if (result > 0) {
        paste("ENERGY STAR fridge kWh savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No ENERGY STAR fridge kWh savings."
      }
    })
    
    # J158 - Boiler replacement savings
    J158 <- reactive({
      print(paste("Input f158 (replace boiler?):", input$f158))
      print(paste("Input f7 (primary heating source):", input$f7))
      
      result <- if (safe_numeric(input$f158) == 1) {
        savings <- switch(as.character(input$f7),
                          "1" = constants$boiler_replacement_savings_NG,
                          "3" = constants$boiler_replacement_savings_FO,
                          print("No valid primary heating source for boiler replacement."),
                          0
        )
        print(paste("Calculated J158 boiler replacement savings:", savings))
        savings
      } else {
        print("No J158 savings - boiler not replaced.")
        0
      }
      
      print(paste("Result for J158:", result))
      result
    })
    # Output for J158 - Boiler Replacement Savings
    output$J158_output <- renderText({
      result <- J158()
      if (result > 0) {
        paste("Boiler replacement savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No boiler replacement savings."
      }
    })
    
    # J163 - ENERGY STAR window replacement savings
    J163 <- reactive({
      print(paste("Input f163 (replace windows?):", input$f163))
      print(paste("Input f7 (primary heating source):", input$f7))
      
      result <- if (safe_numeric(input$f163) == 1) {
        savings <- switch(as.character(input$f7),
                          "1" = constants$EF_natural_gas * (constants$window_replacement_energy_savings / constants$BTU_per_1000cf_NG),
                          "2" = constants$e_factor_value * (constants$window_replacement_energy_savings / constants$BTU_per_kWh),
                          "3" = (constants$window_replacement_energy_savings / constants$BTU_per_gallon_FO) * constants$EF_fuel_oil_gallon,
                          "4" = (constants$window_replacement_energy_savings / constants$BTU_per_gallon_propane) * constants$EF_propane,
                          print("No valid primary heating source for window replacement."),
                          0
        )
        print(paste("Calculated J163 window replacement savings:", savings))
        savings
      } else {
        print("No J163 savings - windows not replaced.")
        0
      }
      
      print(paste("Result for J163:", result))
      result
    })
    # Output for J163 - ENERGY STAR Window Replacement Savings
    output$J163_output <- renderText({
      result <- J163()
      if (result > 0) {
        paste("ENERGY STAR window replacement savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No ENERGY STAR window replacement savings."
      }
    })
    
    # J175 - Recycling emissions savings
    J175 <- reactive({
      print(paste("Input f175 (participate in recycling?):", input$f175))
      print(paste("Household size (input$f5):", input$f5))
      
      result <- if (safe_numeric(input$f175) == 1) {
        savings <- -sum(
          if (safe_numeric(input$f65) == 2) { safe_numeric(input$f5) * constants$newspaper_recycling_avoided_emissions } else { 0 },
          if (safe_numeric(input$f67) == 2) { safe_numeric(input$f5) * constants$glass_recycling_avoided_emissions } else { 0 },
          if (safe_numeric(input$f69) == 2) { safe_numeric(input$f5) * constants$plastic_recycling_avoided_emissions } else { 0 },
          if (safe_numeric(input$f71) == 2) { safe_numeric(input$f5) * constants$metal_recycling_avoided_emissions } else { 0 },
          if (safe_numeric(input$f73) == 2) { safe_numeric(input$f5) * constants$mag_recycling_avoided_emissions } else { 0 }
        )
        print(paste("Calculated J175 recycling savings:", savings))
        savings
      } else {
        print("No J175 savings - not participating in recycling.")
        0
      }
      
      print(paste("Result for J175:", result))
      result
    })
    # Output for J175 - Recycling Emissions Savings
    output$J175_output <- renderText({
      result <- J175()
      if (result != 0) {
        paste("Recycling emissions savings: $", format(round(result, 2), nsmall = 2))
      } else {
        "No recycling emissions savings."
      }
    })
    
    
    
    
    
    
    # L91 - Percent of total emissions for J91
    L91 <- reactive({
      print(paste("J91:", J91(), "J82:", J82()))
      if(J82() > 0) { 
        result <- J91() / J82() 
        print(paste("L91 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L91.")
        0 
      }
    })
    
    # L93 - Percent of total emissions for J93
    L93 <- reactive({
      print(paste("J93:", J93(), "J82:", J82()))
      if(J82() > 0) {
        result <- J93() / J82()
        print(paste("L93 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L93.")
        0 
      }
    })
    
    # L95 - Percent of total emissions for J95
    L95 <- reactive({
      print(paste("J95:", J95(), "J82:", J82()))
      if(J82() > 0) {
        result <- J95() / J82()
        print(paste("L95 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L95.")
        0 
      }
    })
    
    # L104 - Percent of total emissions for J104
    L104 <- reactive({
      print(paste("J104:", J104(), "J82:", J82()))
      if(J82() > 0) {
        result <- J104() / J82()
        print(paste("L104 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L104.")
        0 
      }
    })
    
    # L109 - Percent of total emissions for J109
    L109 <- reactive({
      print(paste("J109:", J109(), "J82:", J82()))
      if(J82() > 0) {
        result <- J109() / J82()
        print(paste("L109 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L109.")
        0 
      }
    })
    
    # L111 - Percent of total emissions for J111
    L111 <- reactive({
      print(paste("J111:", J111(), "J82:", J82()))
      if(J82() > 0) {
        result <- J111() / J82()
        print(paste("L111 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L111.")
        0 
      }
    })
    
    # L113 - Percent of total emissions for J113
    L113 <- reactive({
      print(paste("J113:", J113(), "J82:", J82()))
      if(J82() > 0) {
        result <- J113() / J82()
        print(paste("L113 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L113.")
        0 
      }
    })
    
    # L126 - Percent of total emissions for J126
    L126 <- reactive({
      print(paste("J126:", J126(), "J82:", J82()))
      if(J82() > 0) {
        result <- J126() / J82()
        print(paste("L126 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L126.")
        0 
      }
    })
    
    # L130 - Percent of total emissions for J130
    L130 <- reactive({
      print(paste("J130:", J130(), "J82:", J82()))
      if(J82() > 0) {
        result <- J130() / J82()
        print(paste("L130 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L130.")
        0 
      }
    })
    
    # L134 - Percent of total emissions for J134
    L134 <- reactive({
      print(paste("J134:", J134(), "J82:", J82()))
      if(J82() > 0) {
        result <- J134() / J82()
        print(paste("L134 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L134.")
        0 
      }
    })
    
    # L137 - Percent of total emissions for J137
    L137 <- reactive({
      print(paste("J137:", J137(), "J82:", J82()))
      if(J82() > 0) {
        result <- J137() / J82()
        print(paste("L137 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L137.")
        0 
      }
    })
    
    # L140 - Percent of total emissions for J140
    L140 <- reactive({
      print(paste("J140:", J140(), "J82:", J82()))
      if(J82() > 0) {
        result <- J140() / J82()
        print(paste("L140 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L140.")
        0 
      }
    })
    
    # L143 - Percent of total emissions for J143
    L143 <- reactive({
      print(paste("J143:", J143(), "J82:", J82()))
      if(J82() > 0) {
        result <- J143() / J82()
        print(paste("L143 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L143.")
        0 
      }
    })
    
    # L147 - Percent of total emissions for J147
    L147 <- reactive({
      print(paste("J147:", J147(), "J82:", J82()))
      if(J82() > 0) {
        result <- J147() / J82()
        print(paste("L147 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L147.")
        0 
      }
    })
    
    # L151 - Percent of total emissions for J151
    L151 <- reactive({
      print(paste("J151:", J151(), "J82:", J82()))
      if(J82() > 0) {
        result <- J151() / J82()
        print(paste("L151 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L151.")
        0 
      }
    })
    
    # L158 - Percent of total emissions for J158
    L158 <- reactive({
      print(paste("J158:", J158(), "J82:", J82()))
      if(J82() > 0) {
        result <- J158() / J82()
        print(paste("L158 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L158.")
        0 
      }
    })
    
    # L163 - Percent of total emissions for J163
    L163 <- reactive({
      print(paste("J163:", J163(), "J82:", J82()))
      if(J82() > 0) {
        result <- J163() / J82()
        print(paste("L163 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L163.")
        0 
      }
    })
    
    # L175 - Percent of total emissions for J175
    L175 <- reactive({
      print(paste("J175:", J175(), "J82:", J82()))
      if(J82() > 0) {
        result <- J175() / J82()
        print(paste("L175 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L175.")
        0 
      }
    })
    
    # L189 - Percent of total emissions for hypothetical H189
    L189 <- reactive({
      print(paste("H189 hypothetical value:", H189(), "J82:", J82()))
      if(J82() > 0) {
        result <- H189() / J82()
        print(paste("L189 result:", result))
        result
      } else { 
        print("J82 is 0 or less - cannot compute L189.")
        0 
      }
    })
    
    
    # Output the L cell percentages to the UI
    output$l91_output <- renderText({ sprintf("%.2f%%", l91() * 100) })
    output$l93_output <- renderText({ sprintf("%.2f%%", l93() * 100) })
    output$l95_output <- renderText({ sprintf("%.2f%%", l95() * 100) })
    output$l104_output <- renderText({ sprintf("%.2f%%", l104() * 100) })
    output$l109_output <- renderText({ sprintf("%.2f%%", l109() * 100) })
    output$l111_output <- renderText({ sprintf("%.2f%%", l111() * 100) })
    output$l113_output <- renderText({ sprintf("%.2f%%", l113() * 100) })
    output$l126_output <- renderText({ sprintf("%.2f%%", l126() * 100) })
    output$l130_output <- renderText({ sprintf("%.2f%%", l130() * 100) })
    output$l134_output <- renderText({ sprintf("%.2f%%", l134() * 100) })
    output$l137_output <- renderText({ sprintf("%.2f%%", l137() * 100) })
    output$l140_output <- renderText({ sprintf("%.2f%%", l140() * 100) })
    output$l143_output <- renderText({ sprintf("%.2f%%", l143() * 100) })
    output$l147_output <- renderText({ sprintf("%.2f%%", l147() * 100) })
    output$l151_output <- renderText({ sprintf("%.2f%%", l151() * 100) })
    output$l158_output <- renderText({ sprintf("%.2f%%", l158() * 100) })
    output$l163_output <- renderText({ sprintf("%.2f%%", l163() * 100) })
    output$l175_output <- renderText({ sprintf("%.2f%%", l175() * 100) })
    # 
    # # Output the J cell calculations to the UI
    # output$j37_output <- renderText({ j37() })
    # output$j42_output <- renderText({ j42() })
    # output$j53_output <- renderText({ j53() })
    # output$j57_output <- renderText({ j57() })
    # output$j63_output <- renderText({ j63() })
    # output$j65_output <- renderText({ j65() })
    # output$j67_output <- renderText({ j67() })
    # output$j69_output <- renderText({ j69() })
    # output$j71_output <- renderText({ j71() })
    # output$j73_output <- renderText({ j73() })
    # output$j77_output <- renderText({ j77() })
    # output$j82_output <- renderText({ j82() })
    # output$j91_output <- renderText({ j91() })
    # output$j93_output <- renderText({ J93() })
    # output$j95_output <- renderText({ J95() })
    # output$j104_output <- renderText({ J104() })
    # output$j109_output <- renderText({ J109() })
    # output$j111_output <- renderText({ J111() })
    # output$j113_output <- renderText({ J113() })
    # output$j120_output <- renderText({ J120() })
    # output$j126_output <- renderText({ J126() })
    # output$j130_output <- renderText({ J130() })
    # output$j134_output <- renderText({ J134() })
    # output$j137_output <- renderText({ J137() })
    # output$j140_output <- renderText({ J140() })
    # output$j143_output <- renderText({ J143() })
    # output$j147_output <- renderText({ J147() })
    # output$j151_output <- renderText({ J151() })
    # output$j158_output <- renderText({ J158() })
    # output$j163_output <- renderText({ J163() })
    # output$j175_output <- renderText({ J175() })
    
    # Output for J53 - Fuel Oil CO2 Emissions
    output$J53_output <- renderText({
      emissions <- J53()
      if (emissions > 0) {
        paste("Estimated fuel oil CO2 emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No fuel oil CO2 emissions calculated."
      }
    })
    
    # Output for J57 - Propane CO2 Emissions
    output$J57_output <- renderText({
      emissions <- J57()
      if (emissions > 0) {
        paste("Estimated propane CO2 emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No propane CO2 emissions calculated."
      }
    })
    
    # Output for J63 - Waste Emissions
    output$J63_output <- renderText({
      emissions <- J63()
      if (emissions > 0) {
        paste("Estimated waste emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No waste emissions calculated."
      }
    })
    
    # Output for J65 - Metal Recycling Avoided Emissions
    output$J65_output <- renderText({
      savings <- J65()
      if (savings != 0) {
        paste("Avoided emissions from metal recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from metal recycling."
      }
    })
    
    # Output for J67 - Plastic Recycling Avoided Emissions
    output$J67_output <- renderText({
      savings <- J67()
      if (savings != 0) {
        paste("Avoided emissions from plastic recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from plastic recycling."
      }
    })
    
    # Output for J69 - Glass Recycling Avoided Emissions
    output$J69_output <- renderText({
      savings <- J69()
      if (savings != 0) {
        paste("Avoided emissions from glass recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from glass recycling."
      }
    })
    
    # Output for J71 - Newspaper Recycling Avoided Emissions
    output$J71_output <- renderText({
      savings <- J71()
      if (savings != 0) {
        paste("Avoided emissions from newspaper recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from newspaper recycling."
      }
    })
    
    # Output for J91 - Personal Vehicle Emissions Reduction
    output$J91_output <- renderText({
      reduction <- J91()
      if (reduction > 0) {
        paste("Reduction in personal vehicle emissions: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in personal vehicle emissions."
      }
    })
    
    # Output for L91 - Percent of Total Emissions for J91
    output$L91_output <- renderText({
      percent <- L91()
      if (percent > 0) {
        paste("Percent of total emissions for personal vehicle reduction: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Personal vehicle reduction does not contribute to total emissions."
      }
    })
    
    # Output for J93 - Reduction due to change in car for D93
    output$J93_output <- renderText({
      reduction <- J93()
      if (reduction > 0) {
        paste("Reduction in emissions due to car change for D93: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in emissions for car change D93."
      }
    })
    
    # Output for L93 - Percent of Total Emissions for J93
    output$L93_output <- renderText({
      percent <- L93()
      if (percent > 0) {
        paste("Percent of total emissions for car change D93: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D93 does not contribute to total emissions."
      }
    })
    
    # Output for J95 - Reduction due to change in car for D95
    output$J95_output <- renderText({
      reduction <- J95()
      if (reduction > 0) {
        paste("Reduction in emissions due to car change for D95: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in emissions for car change D95."
      }
    })
    
    # Output for L95 - Percent of Total Emissions for J95
    output$L95_output <- renderText({
      percent <- L95()
      if (percent > 0) {
        paste("Percent of total emissions for car change D95: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D95 does not contribute to total emissions."
      }
    })
    
    # Output for J104 - Vehicle efficiency improvements
    output$J104_output <- renderText({
      savings <- J104()
      if (savings > 0) {
        paste("Savings from vehicle efficiency improvements: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from vehicle efficiency improvements."
      }
    })
    
    # Output for L104 - Percent of Total Emissions for J104
    output$L104_output <- renderText({
      percent <- L104()
      if (percent > 0) {
        paste("Percent of total emissions from vehicle efficiency: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Vehicle efficiency improvements do not contribute to total emissions."
      }
    })
    
    # Output for J109 - Personal vehicle emissions reduction if changing car
    output$J109_output <- renderText({
      reduction <- J109()
      if (reduction > 0) {
        paste("Reduction in personal vehicle emissions (change in car): ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction from changing personal vehicle."
      }
    })
    
    # Output for L109 - Percent of Total Emissions for J109
    output$L109_output <- renderText({
      percent <- L109()
      if (percent > 0) {
        paste("Percent of total emissions for change in personal vehicle: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in personal vehicle does not contribute to total emissions."
      }
    })
    
    # Output for J111 - Reduction due to change in car for D17
    output$J111_output <- renderText({
      reduction <- J111()
      if (reduction > 0) {
        paste("Reduction for car change D17: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction for car change D17."
      }
    })
    
    # Output for L111 - Percent of Total Emissions for J111
    output$L111_output <- renderText({
      percent <- L111()
      if (percent > 0) {
        paste("Percent of total emissions for car change D17: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D17 does not contribute to total emissions."
      }
    })
    
    # Output for J113 - Reduction due to change in car for D19
    output$J113_output <- renderText({
      reduction <- J113()
      if (reduction > 0) {
        paste("Reduction for car change D19: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction for car change D19."
      }
    })
    
    # Output for L113 - Percent of Total Emissions for J113
    output$L113_output <- renderText({
      percent <- L113()
      if (percent > 0) {
        paste("Percent of total emissions for car change D19: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D19 does not contribute to total emissions."
      }
    })
    
    # Output for J126 - Savings from thermostat heating adjustments
    output$J126_output <- renderText({
      savings <- J126()
      if (savings > 0) {
        paste("Savings from thermostat heating adjustments: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from thermostat heating adjustments."
      }
    })
    
    # Output for L126 - Percent of Total Emissions for J126
    output$L126_output <- renderText({
      percent <- L126()
      if (percent > 0) {
        paste("Percent of total emissions from thermostat heating adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Thermostat heating adjustments do not contribute to total emissions."
      }
    })
    
    # Output for J130 - Savings from cooling adjustments
    output$J130_output <- renderText({
      savings <- J130()
      if (savings > 0) {
        paste("Savings from cooling adjustments: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from cooling adjustments."
      }
    })
    
    # Output for L130 - Percent of Total Emissions for J130
    output$L130_output <- renderText({
      percent <- L130()
      if (percent > 0) {
        paste("Percent of total emissions from cooling adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Cooling adjustments do not contribute to total emissions."
      }
    })
    
    # Output for J134 - Savings from enabling computer sleep
    output$J134_output <- renderText({
      savings <- J134()
      if (savings > 0) {
        paste("Savings from enabling computer sleep: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from enabling computer sleep."
      }
    })
    
    # Output for L134 - Percent of Total Emissions for J134
    output$L134_output <- renderText({
      percent <- L134()
      if (percent > 0) {
        paste("Percent of total emissions from enabling computer sleep: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Enabling computer sleep does not contribute to total emissions."
      }
    })
    
    # Output for J137 - Savings from washing clothes in cold water
    output$J137_output <- renderText({
      savings <- J137()
      if (savings > 0) {
        paste("Savings from washing clothes in cold water: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from washing clothes in cold water."
      }
    })
    
    # Output for L137 - Percent of Total Emissions for J137
    output$L137_output <- renderText({
      percent <- L137()
      if (percent > 0) {
        paste("Percent of total emissions from washing clothes in cold water: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Washing clothes in cold water does not contribute to total emissions."
      }
    })
    
    # Output for J140 - Savings from using a drying rack or line drying half the time
    output$J140_output <- renderText({
      savings <- J140()
      if (savings > 0) {
        paste("Savings from using a drying rack or line drying half the time: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from using a drying rack or line drying."
      }
    })
    
    # Output for L140 - Percent of Total Emissions for J140
    output$L140_output <- renderText({
      percent <- L140()
      if (percent > 0) {
        paste("Percent of total emissions from using a drying rack or line drying: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Using a drying rack or line drying does not contribute to total emissions."
      }
    })
    
    # Output for J143 - Green power purchase emissions savings
    output$J143_output <- renderText({
      savings <- J143()
      if (savings > 0) {
        paste("Savings from green power purchase: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from green power purchase."
      }
    })
    
    # Output for L143 - Percent of Total Emissions for J143
    output$L143_output <- renderText({
      percent <- L143()
      if (percent > 0) {
        paste("Percent of total emissions from green power purchase: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Green power purchase does not contribute to total emissions."
      }
    })
    
    # Output for J147 - ENERGY STAR lights savings
    output$J147_output <- renderText({
      savings <- J147()
      if (savings > 0) {
        paste("Savings from ENERGY STAR light bulbs: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR light bulbs."
      }
    })
    
    # Output for L147 - Percent of Total Emissions for J147
    output$L147_output <- renderText({
      percent <- L147()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR light bulbs: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR light bulbs do not contribute to total emissions."
      }
    })
    
    # Output for J151 - ENERGY STAR fridge kWh savings
    output$J151_output <- renderText({
      savings <- J151()
      if (savings > 0) {
        paste("Savings from ENERGY STAR fridge: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR fridge."
      }
    })
    
    # Output for L151 - Percent of Total Emissions for J151
    output$L151_output <- renderText({
      percent <- L151()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR fridge: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR fridge does not contribute to total emissions."
      }
    })
    
    # Output for J158 - Boiler replacement savings
    output$J158_output <- renderText({
      savings <- J158()
      if (savings > 0) {
        paste("Savings from boiler replacement: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from boiler replacement."
      }
    })
    
    # Output for L158 - Percent of Total Emissions for J158
    output$L158_output <- renderText({
      percent <- L158()
      if (percent > 0) {
        paste("Percent of total emissions from boiler replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Boiler replacement does not contribute to total emissions."
      }
    })
    
    # Output for J163 - ENERGY STAR window replacement savings
    output$J163_output <- renderText({
      savings <- J163()
      if (savings > 0) {
        paste("Savings from ENERGY STAR window replacement: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR window replacement."
      }
    })
    
    # Output for L163 - Percent of Total Emissions for J163
    output$L163_output <- renderText({
      percent <- L163()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR window replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR window replacement does not contribute to total emissions."
      }
    })
    
    # Output for J175 - Recycling emissions savings
    output$J175_output <- renderText({
      savings <- J175()
      if (savings < 0) {  # Assuming savings are negative because they represent a reduction
        paste("Total recycling emissions savings: ", format(round(-savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No recycling emissions savings calculated."
      }
    })
    
    # Output for L175 - Percent of Total Emissions for J175
    output$L175_output <- renderText({
      percent <- L175()
      if (percent > 0) {
        paste("Percent of total emissions from recycling: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Recycling does not contribute to total emissions."
      }
    })
    
    
    # Output for J53 - Fuel Oil CO2 Emissions
    output$J53_output <- renderText({
      emissions <- J53()
      if (emissions > 0) {
        paste("Estimated fuel oil CO2 emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No fuel oil CO2 emissions calculated."
      }
    })
    
    # Output for J57 - Propane CO2 Emissions
    output$J57_output <- renderText({
      emissions <- J57()
      if (emissions > 0) {
        paste("Estimated propane CO2 emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No propane CO2 emissions calculated."
      }
    })
    
    # Output for J63 - Waste Emissions
    output$J63_output <- renderText({
      emissions <- J63()
      if (emissions > 0) {
        paste("Estimated waste emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No waste emissions calculated."
      }
    })
    
    # Output for J65 - Metal Recycling Avoided Emissions
    output$J65_output <- renderText({
      savings <- J65()
      if (savings != 0) {
        paste("Avoided emissions from metal recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from metal recycling."
      }
    })
    
    # Output for J67 - Plastic Recycling Avoided Emissions
    output$J67_output <- renderText({
      savings <- J67()
      if (savings != 0) {
        paste("Avoided emissions from plastic recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from plastic recycling."
      }
    })
    
    # Output for J69 - Glass Recycling Avoided Emissions
    output$J69_output <- renderText({
      savings <- J69()
      if (savings != 0) {
        paste("Avoided emissions from glass recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from glass recycling."
      }
    })
    
    # Output for J71 - Newspaper Recycling Avoided Emissions
    output$J71_output <- renderText({
      savings <- J71()
      if (savings != 0) {
        paste("Avoided emissions from newspaper recycling: ", format(round(savings, 2), nsmall = 2), " kg CO2 avoided")
      } else {
        "No emissions savings from newspaper recycling."
      }
    })
    
    # Output for J91 - Personal Vehicle Emissions Reduction
    output$J91_output <- renderText({
      reduction <- J91()
      if (reduction > 0) {
        paste("Reduction in personal vehicle emissions: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in personal vehicle emissions."
      }
    })
    
    # Output for L91 - Percent of Total Emissions for J91
    output$L91_output <- renderText({
      percent <- L91()
      if (percent > 0) {
        paste("Percent of total emissions for personal vehicle reduction: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Personal vehicle reduction does not contribute to total emissions."
      }
    })
    
    # Output for J93 - Reduction due to change in car for D93
    output$J93_output <- renderText({
      reduction <- J93()
      if (reduction > 0) {
        paste("Reduction in emissions due to car change for D93: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in emissions for car change D93."
      }
    })
    
    # Output for L93 - Percent of Total Emissions for J93
    output$L93_output <- renderText({
      percent <- L93()
      if (percent > 0) {
        paste("Percent of total emissions for car change D93: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D93 does not contribute to total emissions."
      }
    })
    
    # Output for J95 - Reduction due to change in car for D95
    output$J95_output <- renderText({
      reduction <- J95()
      if (reduction > 0) {
        paste("Reduction in emissions due to car change for D95: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction in emissions for car change D95."
      }
    })
    
    # Output for L95 - Percent of Total Emissions for J95
    output$L95_output <- renderText({
      percent <- L95()
      if (percent > 0) {
        paste("Percent of total emissions for car change D95: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D95 does not contribute to total emissions."
      }
    })
    
    # Output for J104 - Vehicle efficiency improvements
    output$J104_output <- renderText({
      savings <- J104()
      if (savings > 0) {
        paste("Savings from vehicle efficiency improvements: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from vehicle efficiency improvements."
      }
    })
    
    # Output for L104 - Percent of Total Emissions for J104
    output$L104_output <- renderText({
      percent <- L104()
      if (percent > 0) {
        paste("Percent of total emissions from vehicle efficiency: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Vehicle efficiency improvements do not contribute to total emissions."
      }
    })
    
    # Output for J109 - Personal vehicle emissions reduction if changing car
    output$J109_output <- renderText({
      reduction <- J109()
      if (reduction > 0) {
        paste("Reduction in personal vehicle emissions (change in car): ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction from changing personal vehicle."
      }
    })
    
    # Output for L109 - Percent of Total Emissions for J109
    output$L109_output <- renderText({
      percent <- L109()
      if (percent > 0) {
        paste("Percent of total emissions for change in personal vehicle: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in personal vehicle does not contribute to total emissions."
      }
    })
    
    # Output for J111 - Reduction due to change in car for D17
    output$J111_output <- renderText({
      reduction <- J111()
      if (reduction > 0) {
        paste("Reduction for car change D17: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction for car change D17."
      }
    })
    
    # Output for L111 - Percent of Total Emissions for J111
    output$L111_output <- renderText({
      percent <- L111()
      if (percent > 0) {
        paste("Percent of total emissions for car change D17: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D17 does not contribute to total emissions."
      }
    })
    
    # Output for J113 - Reduction due to change in car for D19
    output$J113_output <- renderText({
      reduction <- J113()
      if (reduction > 0) {
        paste("Reduction for car change D19: ", format(round(reduction, 2), nsmall = 2), " kg CO2")
      } else {
        "No reduction for car change D19."
      }
    })
    
    # Output for L113 - Percent of Total Emissions for J113
    output$L113_output <- renderText({
      percent <- L113()
      if (percent > 0) {
        paste("Percent of total emissions for car change D19: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Car change D19 does not contribute to total emissions."
      }
    })
    
    # Output for J126 - Savings from thermostat heating adjustments
    output$J126_output <- renderText({
      savings <- J126()
      if (savings > 0) {
        paste("Savings from thermostat heating adjustments: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from thermostat heating adjustments."
      }
    })
    
    # Output for L126 - Percent of Total Emissions for J126
    output$L126_output <- renderText({
      percent <- L126()
      if (percent > 0) {
        paste("Percent of total emissions from thermostat heating adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Thermostat heating adjustments do not contribute to total emissions."
      }
    })
    
    # Output for J130 - Savings from cooling adjustments
    output$J130_output <- renderText({
      savings <- J130()
      if (savings > 0) {
        paste("Savings from cooling adjustments: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from cooling adjustments."
      }
    })
    
    # Output for L130 - Percent of Total Emissions for J130
    output$L130_output <- renderText({
      percent <- L130()
      if (percent > 0) {
        paste("Percent of total emissions from cooling adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Cooling adjustments do not contribute to total emissions."
      }
    })
    
    # Output for J134 - Savings from enabling computer sleep
    output$J134_output <- renderText({
      savings <- J134()
      if (savings > 0) {
        paste("Savings from enabling computer sleep: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from enabling computer sleep."
      }
    })
    
    # Output for L134 - Percent of Total Emissions for J134
    output$L134_output <- renderText({
      percent <- L134()
      if (percent > 0) {
        paste("Percent of total emissions from enabling computer sleep: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Enabling computer sleep does not contribute to total emissions."
      }
    })
    
    # Output for J137 - Savings from washing clothes in cold water
    output$J137_output <- renderText({
      savings <- J137()
      if (savings > 0) {
        paste("Savings from washing clothes in cold water: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from washing clothes in cold water."
      }
    })
    
    # Output for L137 - Percent of Total Emissions for J137
    output$L137_output <- renderText({
      percent <- L137()
      if (percent > 0) {
        paste("Percent of total emissions from washing clothes in cold water: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Washing clothes in cold water does not contribute to total emissions."
      }
    })
    
    # Output for J140 - Savings from using a drying rack or line drying half the time
    output$J140_output <- renderText({
      savings <- J140()
      if (savings > 0) {
        paste("Savings from using a drying rack or line drying half the time: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from using a drying rack or line drying."
      }
    })
    
    # Output for L140 - Percent of Total Emissions for J140
    output$L140_output <- renderText({
      percent <- L140()
      if (percent > 0) {
        paste("Percent of total emissions from using a drying rack or line drying: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Using a drying rack or line drying does not contribute to total emissions."
      }
    })
    
    # Output for J143 - Green power purchase emissions savings
    output$J143_output <- renderText({
      savings <- J143()
      if (savings > 0) {
        paste("Savings from green power purchase: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from green power purchase."
      }
    })
    
    # Output for L143 - Percent of Total Emissions for J143
    output$L143_output <- renderText({
      percent <- L143()
      if (percent > 0) {
        paste("Percent of total emissions from green power purchase: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Green power purchase does not contribute to total emissions."
      }
    })
    
    # Output for J147 - ENERGY STAR lights savings
    output$J147_output <- renderText({
      savings <- J147()
      if (savings > 0) {
        paste("Savings from ENERGY STAR light bulbs: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR light bulbs."
      }
    })
    
    # Output for L147 - Percent of Total Emissions for J147
    output$L147_output <- renderText({
      percent <- L147()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR light bulbs: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR light bulbs do not contribute to total emissions."
      }
    })
    
    # Output for J151 - ENERGY STAR fridge kWh savings
    output$J151_output <- renderText({
      savings <- J151()
      if (savings > 0) {
        paste("Savings from ENERGY STAR fridge: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR fridge."
      }
    })
    
    # Output for L151 - Percent of Total Emissions for J151
    output$L151_output <- renderText({
      percent <- L151()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR fridge: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR fridge does not contribute to total emissions."
      }
    })
    
    # Output for J158 - Boiler replacement savings
    output$J158_output <- renderText({
      savings <- J158()
      if (savings > 0) {
        paste("Savings from boiler replacement: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from boiler replacement."
      }
    })
    
    # Output for L158 - Percent of Total Emissions for J158
    output$L158_output <- renderText({
      percent <- L158()
      if (percent > 0) {
        paste("Percent of total emissions from boiler replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Boiler replacement does not contribute to total emissions."
      }
    })
    
    # Output for J163 - ENERGY STAR window replacement savings
    output$J163_output <- renderText({
      savings <- J163()
      if (savings > 0) {
        paste("Savings from ENERGY STAR window replacement: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR window replacement."
      }
    })
    
    # Output for L163 - Percent of Total Emissions for J163
    output$L163_output <- renderText({
      percent <- L163()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR window replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR window replacement does not contribute to total emissions."
      }
    })
    
    # Output for J175 - Recycling emissions savings
    output$J175_output <- renderText({
      savings <- J175()
      if (savings < 0) {  # Assuming savings are negative because they represent a reduction
        paste("Total recycling emissions savings: ", format(round(-savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No recycling emissions savings calculated."
      }
    })
    
    # Output for L175 - Percent of Total Emissions for J175
    output$L175_output <- renderText({
      percent <- L175()
      if (percent > 0) {
        paste("Percent of total emissions from recycling: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Recycling does not contribute to total emissions."
      }
    })
    
    # Output for L91 - Percent of Total Emissions for J91
    output$L91_output <- renderText({
      percent <- l91()
      if (percent > 0) {
        paste("Percent of total emissions for personal vehicle emissions reduction (J91): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Personal vehicle emissions reduction does not contribute to total emissions."
      }
    })
    
    # Output for L93 - Percent of Total Emissions for J93
    output$L93_output <- renderText({
      percent <- l93()
      if (percent > 0) {
        paste("Percent of total emissions for reduction due to change in car for D93 (J93): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in car for D93 does not contribute to total emissions."
      }
    })
    
    # Output for L95 - Percent of Total Emissions for J95
    output$L95_output <- renderText({
      percent <- l95()
      if (percent > 0) {
        paste("Percent of total emissions for reduction due to change in car for D95 (J95): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in car for D95 does not contribute to total emissions."
      }
    })
    
    # Output for L104 - Percent of Total Emissions for J104
    output$L104_output <- renderText({
      percent <- l104()
      if (percent > 0) {
        paste("Percent of total emissions for vehicle efficiency improvements (J104): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Vehicle efficiency improvements do not contribute to total emissions."
      }
    })
    
    # Output for L109 - Percent of Total Emissions for J109
    output$L109_output <- renderText({
      percent <- l109()
      if (percent > 0) {
        paste("Percent of total emissions for personal vehicle emissions reduction if changing car (J109): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in personal vehicle does not contribute to total emissions."
      }
    })
    
    # Output for L111 - Percent of Total Emissions for J111
    output$L111_output <- renderText({
      percent <- l111()
      if (percent > 0) {
        paste("Percent of total emissions for reduction due to change in car for D17 (J111): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in car for D17 does not contribute to total emissions."
      }
    })
    
    # Output for L113 - Percent of Total Emissions for J113
    output$L113_output <- renderText({
      percent <- l113()
      if (percent > 0) {
        paste("Percent of total emissions for reduction due to change in car for D19 (J113): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Change in car for D19 does not contribute to total emissions."
      }
    })
    
    # Output for L126 - Percent of Total Emissions for J126
    output$L126_output <- renderText({
      percent <- l126()
      if (percent > 0) {
        paste("Percent of total emissions from thermostat heating adjustments (J126): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Thermostat heating adjustments do not contribute to total emissions."
      }
    })
    
    # Output for L130 - Percent of Total Emissions for J130
    output$L130_output <- renderText({
      percent <- l130()
      if (percent > 0) {
        paste("Percent of total emissions from cooling adjustments (J130): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Cooling adjustments do not contribute to total emissions."
      }
    })
    
    # Output for L134 - Percent of Total Emissions for J134
    output$L134_output <- renderText({
      percent <- l134()
      if (percent > 0) {
        paste("Percent of total emissions from enabling computer sleep (J134): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Enabling computer sleep does not contribute to total emissions."
      }
    })
    
    # Output for L137 - Percent of Total Emissions for J137
    output$L137_output <- renderText({
      percent <- l137()
      if (percent > 0) {
        paste("Percent of total emissions from washing clothes in cold water (J137): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Washing clothes in cold water does not contribute to total emissions."
      }
    })
    
    # Output for L140 - Percent of Total Emissions for J140
    output$L140_output <- renderText({
      percent <- l140()
      if (percent > 0) {
        paste("Percent of total emissions from using a drying rack or line drying (J140): ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Using a drying rack or line drying does not contribute to total emissions."
      }
    })
    
    # Output for J143 - Green power purchase emissions savings
    output$J143_output <- renderText({
      savings <- J143()
      if (savings > 0) {
        paste("Savings from green power purchase: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from green power purchase."
      }
    })
    
    # Output for L143 - Percent of Total Emissions for J143
    output$L143_output <- renderText({
      percent <- L143()
      if (percent > 0) {
        paste("Percent of total emissions from green power purchase: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Green power purchase does not contribute to total emissions."
      }
    })
    
    # Output for J147 - ENERGY STAR lights savings
    output$J147_output <- renderText({
      savings <- J147()
      if (savings > 0) {
        paste("Savings from ENERGY STAR lights: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR lights."
      }
    })
    
    # Output for L147 - Percent of Total Emissions for J147
    output$L147_output <- renderText({
      percent <- L147()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR lights: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR lights do not contribute to total emissions."
      }
    })
    
    # Output for J151 - ENERGY STAR fridge kWh savings
    output$J151_output <- renderText({
      savings <- J151()
      if (savings > 0) {
        paste("Savings from ENERGY STAR fridge: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR fridge."
      }
    })
    
    # Output for L151 - Percent of Total Emissions for J151
    output$L151_output <- renderText({
      percent <- L151()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR fridge: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR fridge does not contribute to total emissions."
      }
    })
    
    # Output for J158 - Boiler replacement savings
    output$J158_output <- renderText({
      savings <- J158()
      if (savings > 0) {
        paste("Savings from boiler replacement: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from boiler replacement."
      }
    })
    
    # Output for L158 - Percent of Total Emissions for J158
    output$L158_output <- renderText({
      percent <- L158()
      if (percent > 0) {
        paste("Percent of total emissions from boiler replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Boiler replacement does not contribute to total emissions."
      }
    })
    
    # Output for J163 - ENERGY STAR window replacement savings
    output$J163_output <- renderText({
      savings <- J163()
      if (savings > 0) {
        paste("Savings from ENERGY STAR window replacement: ", format(round(savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No savings from ENERGY STAR window replacement."
      }
    })
    
    # Output for L163 - Percent of Total Emissions for J163
    output$L163_output <- renderText({
      percent <- L163()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR window replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR window replacement does not contribute to total emissions."
      }
    })
    
    # Output for J175 - Recycling emissions savings
    output$J175_output <- renderText({
      savings <- J175()
      if (savings < 0) {  # Assuming savings are negative because they represent a reduction
        paste("Total recycling emissions savings: ", format(round(-savings, 2), nsmall = 2), " kg CO2")
      } else {
        "No recycling emissions savings calculated."
      }
    })
    
    # Output for L175 - Percent of Total Emissions for J175
    output$L175_output <- renderText({
      percent <- L175()
      if (percent > 0) {
        paste("Percent of total emissions from recycling: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Recycling does not contribute to total emissions."
      }
    })
    
    
    # # Output the H cell calculations to the UI
    # output$h126_output <- renderText({ h126() })
    # output$h130_output <- renderText({ h130() })
    # output$h134_output <- renderText({ h134() })
    # output$h137_output <- renderText({ h137() })
    # output$h140_output <- renderText({ h140() })
    # output$h147_output <- renderText({ h147() })
    # output$h151_output <- renderText({ h151() })
    # output$h158_output <- renderText({ h158() })
    # output$h163_output <- renderText({ h163() })
    # output$h188_output <- renderText({ h188() })
    # output$h189_output <- renderText({ h189() })
    # output$h190_output <- renderText({ h190() })
    # output$h191_output <- renderText({ h191() })
    # output$h192_output <- renderText({ h192() })
    
    # Output for h126 - Heating Adjustment Savings
    output$h126_output <- renderText({
      savings <- h126()
      if (savings > 0) {
        paste("Heating adjustment savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from heating adjustment."
      }
    })
    
    # Output for h130 - Cooling Adjustment Savings
    output$h130_output <- renderText({
      savings <- h130()
      if (savings > 0) {
        paste("Cooling adjustment savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from cooling adjustment."
      }
    })
    
    # Output for h134 - Computer Sleep Savings
    output$h134_output <- renderText({
      savings <- h134()
      if (savings > 0) {
        paste("Computer sleep mode savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from computer sleep mode."
      }
    })
    
    # Output for h137 - Laundry Savings
    output$h137_output <- renderText({
      savings <- h137()
      if (savings > 0) {
        paste("Laundry savings from using cold water: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from laundry water temperature adjustment."
      }
    })
    
    # Output for h140 - Dryer Savings
    output$h140_output <- renderText({
      savings <- h140()
      if (savings > 0) {
        paste("Dryer energy savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No dryer energy savings."
      }
    })
    
    # Output for h147 - Lamp Savings
    output$h147_output <- renderText({
      savings <- h147()
      if (savings > 0) {
        paste("Lamp energy savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from lamp energy reduction."
      }
    })
    
    # Output for h151 - Fridge Savings
    output$h151_output <- renderText({
      savings <- h151()
      if (savings > 0) {
        paste("Fridge replacement energy savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No energy savings from fridge replacement."
      }
    })
    
    # Output for h158 - Boiler Replacement Savings
    output$h158_output <- renderText({
      savings <- h158()
      if (savings > 0) {
        paste("Boiler replacement savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from boiler replacement."
      }
    })
    
    # Output for h163 - Window Replacement Savings
    output$h163_output <- renderText({
      savings <- h163()
      if (savings > 0) {
        paste("Window replacement savings: ", format(round(savings, 2), nsmall = 2), " units")
      } else {
        "No savings from window replacement."
      }
    })
    
    # Output for h188 - Custom Calculation
    output$h188_output <- renderText({
      result <- h188()
      if (result > 0) {
        paste("Custom calculation result (H188): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No result from custom calculation H188."
      }
    })
    
    # Output for h189 - Another Custom Calculation
    output$h189_output <- renderText({
      result <- h189()
      if (result > 0) {
        paste("Custom calculation result (H189): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No result from custom calculation H189."
      }
    })
    
    # Output for h190 - Additional Custom Calculation
    output$h190_output <- renderText({
      result <- h190()
      if (result > 0) {
        paste("Additional custom calculation result (H190): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No result from additional custom calculation H190."
      }
    })
    
    # Output for h191 - Further Custom Calculation
    output$h191_output <- renderText({
      result <- h191()
      if (result > 0) {
        paste("Further custom calculation result (H191): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No result from further custom calculation H191."
      }
    })
    
    # Output for h192 - Final Custom Calculation
    output$h192_output <- renderText({
      result <- h192()
      if (result > 0) {
        paste("Final custom calculation result (H192): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No result from final custom calculation H192."
      }
    })
    
    # A reactive expression that creates a dataframe based on inputs
    reactiveData <- reactive({
      # Create a dataframe with the values of interest
      df <- data.frame(
        Category = c("AC Adjustment", "Computer Sleep Mode", "Dryer Energy Savings", "Laundry Load", "Thermostat Heating Adjustment"),
        Savings = c(input$f130, input$f134, input$f140, input$f137, input$f126) # Replace with actual reactive values or calculations
      )
      return(df)
    })
    output$savingsPlot <- renderPlot({
      # Create a data frame of savings
      savings_data <- data.frame(
        Category = c("Thermostat Heating Adjustment", "AC Adjustment", "Computer Sleep Mode",
                     "Laundry Load", "Dryer Energy Savings", "Lamp Cost Savings",
                     "Fridge Replacement kWh Savings", "Boiler Replacement Cost Savings",
                     "Window Replacement Cost Savings"),
        Savings = c(75, 252, 12.72348, 296.5248, 45.6786, 0, 0, 0, 0)
      )
      
      # Order the factor levels based on the savings, so highest savings come first
      savings_data$Category <- factor(savings_data$Category, levels = savings_data$Category[order(savings_data$Savings, decreasing = TRUE)])
      
      # Remove categories with no savings for a cleaner plot
      savings_data <- savings_data[savings_data$Savings > 0, ]
      
      # Check if there are any savings to plot
      if (nrow(savings_data) > 0) {
        # Plot
        ggplot(savings_data, aes(x = Category, y = Savings, fill = Category)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(y = "Dollars Saved", x = "", title = "Estimated Savings from Energy-Saving Actions")
      } else {
        # Return a blank plot or a message
        return(ggplot() +
                 theme_void() +
                 labs(title = "No savings data to display."))
      }
    })
    
    ## Liam
    # textOutput("J175_output")
    question <- "Please generate a detailed action plan to increase cost savings and sustainability for a given home. Below is a list of relevant characteristics to base your sugestions on:"
    if (J134() > 0) { question <- paste(question, "\nSavings from enabling computer sleep: ", format(round(J134(), 2), nsmall = 2), " kg CO2")}
    if (J137() > 0) { question <- paste(question, "\nSavings from washing clothes in cold water: ", format(round(J137(), 2), nsmall = 2), " kg CO2")}
    if (J140() > 0) { question <- paste(question, "\nSavings from using a drying rack or line drying half the time: ", format(round(J140(), 2), nsmall = 2), " kg CO2")}
    if (J143() > 0) { question <- paste(question, "\nSavings from green power purchase: ", format(round(J143(), 2), nsmall = 2), " kg CO2")}
    if (J147() > 0) { question <- paste(question, "\nSavings from ENERGY STAR lights: ", format(round(J147(), 2), nsmall = 2), " kg CO2")}
    if (J151() > 0) { question <- paste(question, "\nSavings from ENERGY STAR fridge: ", format(round(J151(), 2), nsmall = 2), " kg CO2")}
    if (J158() > 0) { question <- paste(question, "\nSavings from boiler replacement: ", format(round(J158(), 2), nsmall = 2), " units")}
    if (J163() > 0) { question <- paste(question, "\nSavings from ENERGY STAR window replacement: ", format(round(J163(), 2), nsmall = 2), " kg CO2")}
    if (J175() < 0) { question <- paste(question, "\nTotal recycling emissions savings: ", format(round(-J175(), 2), nsmall = 2), " kg CO2")}
    updateTextAreaInput(session, "chatgpt_question", value = question)
    
  })
  
  
  
}