library(shiny)
library(ggplot2)
library(dplyr)


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
  
  safe_numeric <- function(x) {
    if (is.null(x)) {
      print("Input is NULL")
      return(0)
    }
    x_as_numeric <- suppressWarnings(as.numeric(x))
    if (is.na(x_as_numeric)) {
      print(paste("Conversion to numeric resulted in NA, original value:", x))
      return(0)
    }
    return(x_as_numeric)
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
    
    # Define J26 as a reactive expression
    J26 <- reactive({
      # Calculate emissions for multiple vehicles
      vehicles <- c(input$D15, input$D17, input$D19, input$D21, input$D23)
      methods <- c(input$G15, input$G17, input$G19, input$G21, input$G23)
      efficiencies <- c(input$K15, input$K17, input$K19, input$K21, input$K23)
      
      # Calculate emissions per vehicle
      emissions <- mapply(function(d, g, k) {
        if (d == 0) {
          0  # No distance traveled, no emissions
        } else {
          if (g == 1) {
            (d * 52) / k * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
          } else {
            d / k * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
          }
        }
      }, vehicles, methods, efficiencies)
      
      # Sum emissions from all vehicles and add J29
      sum(emissions) + J29()
    })
    # Output for J26 - Vehicle Operations Emissions
    output$J26_output <- renderText({
      emissions <- J26()
      if (emissions > 0) {
        paste("Vehicle operations emissions: ", format(round(emissions, 2), nsmall = 2), " kg CO2")
      } else {
        "No vehicle operations emissions calculated."
      }
    })
    
    # Helper function to calculate emissions for a single vehicle
    vehicle_emissions <- function(d, g, k) {
      # Check if any inputs are NULL or NA and return 0 emissions if so
      if (is.null(d) || is.null(g) || is.null(k) || is.na(d) || is.na(g) || is.na(k)) {
        return(0)
      }
      if (d == 0) {
        return(0)  # No emissions if no distance traveled
      }
      # Calculate emissions based on whether travel is weekly
      if (g == 1) {
        return((d * 52 / k) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio)
      } else {
        return((d / k) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio)
      }
    }
    
    # J29 - Total emissions calculation for various vehicles
    J29 <- reactive({
      # Applying the function to each vehicle and summing the emissions
      sum(
        vehicle_emissions(input$D15, input$G15, input$K15),
        vehicle_emissions(input$D17, input$G17, input$K17),
        vehicle_emissions(input$D19, input$G19, input$K19),
        vehicle_emissions(input$D21, input$G21, input$K21),
        vehicle_emissions(input$D23, input$G23, input$K23),
        na.rm = TRUE  # Safely handle NA values
      )
    })
    
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
    
    # J42
    J42 <- reactive({
      if (is.null(input$H42) || is.null(input$F42) || !is.numeric(input$H42) || !is.numeric(input$F42)) {
        print("Invalid or missing inputs for J42 calculation.")
        return(0) 
      }
      
      billing_unit <- as.numeric(input$H42)
      usage_amount <- as.numeric(input$F42)
      
      calculated_value <- billing_unit * usage_amount * constants$AC_electricity_percent 
      print(paste("J42 calculation:", calculated_value))
      return(calculated_value)
    })
    
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
    
    # Helper function to safely calculate sum, avoiding non-numeric types
    safe_sum <- function(x) {
      sum(sapply(x, function(item) if(is.numeric(item)) item else 0), na.rm = TRUE)
    }
    
    # J82 Calculation
    J82 <- reactive({
      components <- list(J26(), J37(), J42(), J53(), J57(), J77())  # list all contributing reactives
      safe_sum(components)
    })
    
    output$J82_output <- renderText({
      sprintf("Total J82 emissions: %.2f units", J82())
    })
    
    J91 <- reactive({
      # Check for NULL or NA to prevent execution
      if(is.null(input$F91) || is.na(input$F91) || is.null(input$F109) || is.na(input$F109)) {
        return(0)
      }
      
      if(input$F91 == 1) {
        if(input$F109 == 1) {
          ((input$D91 * 52) / (input$K15 + input$D109)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        } else {
          (input$D91 * 52) / input$K15 * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        }
      } else {
        0
      }
    })
    
    # L91 - Percent of total emissions for J91
    L91 <- reactive({
      total_emissions <- J82()
      if (total_emissions > 0) {
        J91() / total_emissions
      } else {
        0
      }
    })
    
    # Output for L91 - Percent of Total Emissions for J91
    output$L91_output <- renderText({
      percent <- L91()
      if (percent > 0) {
        sprintf("Percent of total emissions for personal vehicle reduction: %.2f%%", percent * 100)
      } else {
        "Personal vehicle reduction does not contribute to total emissions."
      }
    })
    
    # J93 - Emissions calculation for vehicle 2
    J93 <- reactive({
      if (!is.null(input$num_vehicles) && !is.null(input$F93) && input$num_vehicles >= 2 && input$F93 == 1 &&
          !is.null(input$D93) && !is.null(input$K17) && !is.null(input$F111) &&
          !is.null(EF_passenger_vehicle) && !is.null(nonCO2_vehicle_emissions_ratio)) {
        additional_factor <- ifelse(input$F111 == 1, input$D111, 0)
        emissions <- ((input$D93 * 52) / (input$K17 + additional_factor)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        if (!is.nan(emissions) && !is.infinite(emissions)) {
          return(emissions)
        } else {
          return(0)
        }
      } else {
        return(0)
      }
    })
    
    L93 <- reactive({
      total_emissions <- J82()  # Total emissions computation
      if (total_emissions > 0) {
        J93() / total_emissions
      } else {
        0
      }
    })
    
    output$L93_output <- renderText({
      percent <- L93()
      if (percent > 0) {
        sprintf("Percent of total emissions for car change D93: %.2f%%", percent * 100)
      } else {
        "Car change D93 does not contribute to total emissions."
      }
    })
    
    # J95 - Emissions calculation for vehicle 3
    J95 <- reactive({
      if (!is.null(input$num_vehicles) && !is.null(input$F95) && input$num_vehicles >= 3 && input$F95 == 1 &&
          !is.null(input$D95) && !is.null(input$K19) && !is.null(input$F113) &&
          !is.null(EF_passenger_vehicle) && !is.null(nonCO2_vehicle_emissions_ratio)) {
        additional_factor <- ifelse(input$F113 == 1, input$D113, 0)
        emissions <- ((input$D95 * 52) / (input$K19 + additional_factor)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        if (!is.nan(emissions) && !is.infinite(emissions)) {
          return(emissions)
        } else {
          return(0)
        }
      } else {
        return(0)
      }
    })
    
    # J97 - Emissions calculation for vehicle 4
    J97 <- reactive({
      if (!is.null(input$num_vehicles) && !is.null(input$F97) && input$num_vehicles >= 4 && input$F97 == 1 &&
          !is.null(input$D97) && !is.null(input$K21) && !is.null(input$F115) &&
          !is.null(GHGCalculatorAssumptions$D5) && !is.null(GHGCalculatorAssumptions$D6)) {
        additional_factor <- ifelse(input$F115 == 1, input$D115, 0)
        emissions <- ((input$D97 * 52) / (input$K21 + additional_factor)) * GHGCalculatorAssumptions$D5 * GHGCalculatorAssumptions$D6
        if (!is.nan(emissions) && !is.infinite(emissions)) {
          return(emissions)
        } else {
          return(0)
        }
      } else {
        return(0)
      }
    })
    
    # L97 - Percent of total emissions for vehicle 4
    L97 <- reactive({
      total_emissions <- J82()  # Total emissions computation
      if (total_emissions > 0) {
        J97() / total_emissions
      } else {
        0
      }
    })
    
    # Output for L97 - Percent of Total Emissions for Vehicle 4
    output$L97_output <- renderText({
      percent <- L97()
      if (percent > 0) {
        sprintf("Percent of total emissions for vehicle 4: %.2f%%", percent * 100)
      } else {
        "Vehicle 4 does not contribute to total emissions."
      }
    })
    
    # J99 - Emissions calculation for vehicle 5
    J99 <- reactive({
      if (!is.null(input$num_vehicles) && !is.null(input$F99) && input$num_vehicles == 5 && input$F99 == 1 &&
          !is.null(input$D99) && !is.null(input$K23) && !is.null(input$F117) && !is.null(GHGCalculatorAssumptions$D5) &&
          !is.null(GHGCalculatorAssumptions$D6)) {
        additional_factor <- ifelse(input$F117 == 1, input$D117, 0)
        emissions <- ((input$D99 * 52) / (input$K23 + additional_factor)) * GHGCalculatorAssumptions$D5 * GHGCalculatorAssumptions$D6
        if (!is.nan(emissions) && !is.infinite(emissions)) {
          return(emissions)
        } else {
          return(0)
        }
      } else {
        return(0)
      }
    })
    
    # L99 - Percent of total emissions for vehicle 5
    L99 <- reactive({
      total_emissions <- J82()  # Total emissions computation
      if (total_emissions > 0) {
        J99() / total_emissions
      } else {
        0
      }
    })
    
    # Output for L99 - Percent of Total Emissions for Vehicle 5
    output$L99_output <- renderText({
      percent <- L99()
      if (percent > 0) {
        sprintf("Percent of total emissions for vehicle 5: %.2f%%", percent * 100)
      } else {
        "Vehicle 5 does not contribute to total emissions."
      }
    })
    
    # J104 - Vehicle efficiency improvements for all vehicles
    J104 <- reactive({
      if (input$F29 == 1) {
        sum(sapply(1:input$num_vehicles, function(v) {
          if (input[[paste0("F", 29 + v * 2)]]) {
            (input[[paste0("D", 15 + v * 2)]] * 52) / input[[paste0("K", 15 + v * 2)]] * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
          } else {
            0
          }
        }))
      } else {
        0
      }
    })
    
    # L104 - Percent of total emissions for J104
    L104 <- reactive({
      if (J82() > 0) {
        J104() / J82()
      } else {
        0
      }
    })
    
    output$L104_output <- renderText({
      percent <- L104()
      if (percent > 0) {
        sprintf("Percent of total emissions from vehicle efficiency: %.2f%%", percent * 100)
      } else {
        "Vehicle efficiency improvements do not contribute to total emissions."
      }
    })
    
    # J109 - Reduction due to change in car for the first vehicle
    J109 <- reactive({
      if (input$num_vehicles >= 1 && !is.null(input$F109) && input$F109 == 1) {
        old_emissions <- J91()  
        new_emissions <- J27()  
        old_emissions - new_emissions
      } else {
        0
      }
    })
    
    # L109 - Percent of total emissions for J109
    L109 <- reactive({
      if (J82() > 0) {
        J109() / J82()
      } else {
        0
      }
    })
    
    output$L109_output <- renderText({
      percent <- L109()
      if (percent > 0) {
        sprintf("Percent of total emissions for change in personal vehicle: %.2f%%", percent * 100)
      } else {
        "Change in personal vehicle does not contribute to total emissions."
      }
    })
    
    J111 <- reactive({
      print("J111 calculation triggered")
      print(paste("Input F111:", input$F111))
      
      # Check if input$F111 is either NULL or NA
      if(is.null(input$F111) || is.na(input$F111)) {
        print("Input F111 is not valid, no reduction (J111)")
        return(0)
      }
      
      # Proceed if input is valid
      if(input$F111 == 1) {
        old_emissions <- ifelse(input$G17 == 1, 
                                (input$D17 * 52) / input$K17, 
                                input$D17 / input$K17) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G17 == 1, 
                                (input$D17 * 52) / (input$K17 + input$D111), 
                                input$D17 / (input$K17 + input$D111)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio
        reduction <- old_emissions - new_emissions
        
        print(paste("Old emissions (J111):", old_emissions))
        print(paste("New emissions (J111):", new_emissions))
        print(paste("Emissions reduction (J111):", reduction))
        
        reduction
      } else {
        print("Input F111 indicates no change, no reduction (J111)")
        0
      }
    })
    
    # Output for J111
    output$J111_output <- renderText({
      reduction <- J111()
      if (reduction > 0) {
        sprintf("Emissions reduction for car change D17: %.2f units", reduction)
      } else {
        "No reduction calculated for car change D17."
      }
    })
    
    # J113 - Reduction due to change in car for the third vehicle
    J113 <- reactive({
      if (input$num_vehicles >= 3 && !is.null(input$F113) && input$F113 == 1) {
        old_emissions <- ifelse(input$G19 == 1, (input$D19 * 52) / input$K19, input$D19 / input$K19) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G19 == 1, (input$D19 * 52) / (input$K19 + input$D113), input$D19 / (input$K19 + input$D113)) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        old_emissions - new_emissions
      } else {
        0
      }
    })
    
    output$J113_output <- renderText({
      reduction <- J113()
      if (reduction > 0) {
        sprintf("Emissions reduction for car change D19: %.2f units", reduction)
      } else {
        "No reduction calculated for car change D19."
      }
    })
    
    # J115
    J115 <- reactive({
      if (input$num_vehicles >= 4 && !is.null(input$F115) && input$F115 == 1) {
        old_emissions <- ifelse(input$G21 == 1, (input$D21 * 52) / input$K21, input$D21 / input$K21) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G21 == 1, (input$D21 * 52) / (input$K21 + input$D115), input$D21 / (input$K21 + input$D115)) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        old_emissions - new_emissions
      } else {
        0
      }
    })
    
    output$J115_output <- renderText({
      reduction <- J115()
      if (reduction > 0) {
        sprintf("Emissions reduction for car change D21: %.2f units", reduction)
      } else {
        "No reduction calculated for car change D21."
      }
    })
    
    # J117
    J117 <- reactive({
      if (input$num_vehicles >= 5 && !is.null(input$F117) && input$F117 == 1) {
        old_emissions <- ifelse(input$G23 == 1, (input$D23 * 52) / input$K23, input$D23 / input$K23) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        new_emissions <- ifelse(input$G23 == 1, (input$D23 * 52) / (input$K23 + input$D117), input$D23 / (input$K23 + input$D117)) * constants$EF_passenger_vehicle * constants$nonCO2_vehicle_emissions_ratio
        old_emissions - new_emissions
      } else {
        0
      }
    })
    
    output$J117_output <- renderText({
      reduction <- J117()
      if (reduction > 0) {
        sprintf("Emissions reduction for car change D23: %.2f units", reduction)
      } else {
        "No reduction calculated for car change D23."
      }
    })
    
    safe_sum <- function(x) {
      sum(sapply(x, function(item) if(is.numeric(item)) item else 0), na.rm = TRUE)
    }
    
    
    # J119 Calculation
    J119 <- reactive({
      components <- list(J117(), J115(), J113(), J111(), J109(), J29(), J99(), J97(), J95(), J93(), J91())
      safe_sum(components)
    })
    
    
    
    # J120 Calculation
    J120 <- reactive({
      components <- list(J117(), J115(), J113(), J111(), J109(), J29(), J99(), J97(), J95(), J93(), J91())
      safe_sum(components)
    })
    
    output$J119_output <- renderText({
      result <- J119()
      if (result != 0) {
        sprintf("Total emissions reduction from various sources (J119): %.2f units", result)
      } else {
        "No total emissions reduction calculated."
      }
    })
    
    output$J120_output <- renderText({
      result <- J120()
      if (result != 0) {
        sprintf("Sum of specified J calculations: %.2f units", result)
      } else {
        "No cumulative savings calculated across specified categories."
      }
    })
    
    h126 <- reactive({
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
    
    J126 <- reactive({1
      # if (input$f126 == 1) {
      #   adjustment <- input$b127
      #   savings <- switch(
      #     as.character(input$f7),
      #     "1" = (adjustment * constants$EF_passenger_vehicle),
      #     "2" = (adjustment * constants$EF_electric_vehicle),
      #     # other cases
      #     0  # Default case
      #   )
      #   print(paste("Calculated J126 savings:", savings))
      #   savings
      # } else {
      #   print("No J126 savings - thermostat adjustment not selected.")
      #   0
      # }
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
    # Output for L126 - Percent of Total Emissions for J126
    output$L126_output <- renderText({
      percent <- L126()
      if (percent > 0) {
        paste("Percent of total emissions from thermostat heating adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Thermostat heating adjustments do not contribute to total emissions."
      }
    })
    
    # Function to calculate t127 based on Energy_Source
    t127 <- function(energy_source) {
      source_index <- which(conversion_factors$Energy_Source == energy_source)
      if (length(source_index) > 0) {
        return(conversion_factors$Unit_Price[source_index])
      } else {
        return(1)  # Default unit price if not found
      }
    }
    
    h130 <- reactive({
      print(paste("Input f130 (AC adjust?):", input$f130))
      print(paste("Input h42 (Billing method):", input$h42))
      print(paste("Input b131 (Degrees to adjust AC):", input$b131))
      print(paste("Input f42 (Monthly usage):", input$f42))
      print(paste("AC Electricity Percent:", constants$AC_electricity_percent))
      print(paste("Thermostat Cooling Savings:", constants$thermostat_cooling_savings))
      print(paste("Cost per kWh:", constants$cost_per_kWh))
      # Directly using inputs for conditions
      if (input$f130 == "1") { 
        print("AC adjustment is applicable")
        degrees = as.numeric(input$b131)  
        usage = as.numeric(input$f42)  
        
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
    
    # J130 - Savings from cooling adjustments
    J130 <- reactive({
      print(paste("Input f130 (AC adjustment?):", input$f130))
      print(paste("Input b131 (Degrees to adjust AC):", input$b131))
      print(paste("Input H42 (Billing unit):", input$H42))
      print(paste("Input F42 (Usage amount):", input$F42))
      
      if (is.null(input$f130) || input$f130 != 1) {
        print("AC adjustment not applicable or inputs are invalid.")
        return(0)
      }
      
      adjustment <- as.numeric(input$b131)
      ac_savings_component <- J42()
      print(paste("AC savings component from J42():", ac_savings_component))
      
      if (is.numeric(ac_savings_component) && !is.na(ac_savings_component) && adjustment > 0) {
        savings <- ac_savings_component * constants$AC_electricity_percent * constants$thermostat_cooling_savings * adjustment
        print(paste("Calculated J130 cooling savings:", savings))
        savings
      } else {
        print("AC savings component is not numeric or adjustment factor is invalid.")
        0
      }
    })
    # Output for J130 - Savings from Cooling Adjustments
    output$J130_output <- renderText({
      result <- J130()
      print(paste("Output result for J130:", result))
      if (result > 0) {
        paste("Savings from cooling adjustments: $", format(round(result, 2), nsmall = 2))
      } else {
        "No savings from cooling adjustments."
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
    # Output for L130 - Percent of Total Emissions for J130
    output$L130_output <- renderText({
      percent <- L130()
      if (percent > 0) {
        paste("Percent of total emissions from cooling adjustments: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Cooling adjustments do not contribute to total emissions."
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
    # Output for L134 - Percent of Total Emissions for J134
    output$L134_output <- renderText({
      percent <- L134()
      if (percent > 0) {
        paste("Percent of total emissions from enabling computer sleep: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Enabling computer sleep does not contribute to total emissions."
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
    # Output for L137 - Percent of Total Emissions for J137
    output$L137_output <- renderText({
      percent <- L137()
      if (percent > 0) {
        paste("Percent of total emissions from washing clothes in cold water: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Washing clothes in cold water does not contribute to total emissions."
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
    # Output for L140 - Percent of Total Emissions for J140
    output$L140_output <- renderText({
      percent <- L140()
      if (percent > 0) {
        paste("Percent of total emissions from using a drying rack or line drying: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Using a drying rack or line drying does not contribute to total emissions."
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
    # Output for L143 - Percent of Total Emissions for J143
    output$L143_output <- renderText({
      percent <- L143()
      if (percent > 0) {
        paste("Percent of total emissions from green power purchase: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Green power purchase does not contribute to total emissions."
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
    # Output for L147 - Percent of Total Emissions for J147
    output$L147_output <- renderText({
      percent <- L147()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR light bulbs: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR light bulbs do not contribute to total emissions."
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
    # Output for L151 - Percent of Total Emissions for J151
    output$L151_output <- renderText({
      percent <- L151()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR fridge: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR fridge does not contribute to total emissions."
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
    # Output for L158 - Percent of Total Emissions for J158
    output$L158_output <- renderText({
      percent <- L158()
      if (percent > 0) {
        paste("Percent of total emissions from boiler replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Boiler replacement does not contribute to total emissions."
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
    # Output for L163 - Percent of Total Emissions for J163
    output$L163_output <- renderText({
      percent <- L163()
      if (percent > 0) {
        paste("Percent of total emissions from ENERGY STAR window replacement: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "ENERGY STAR window replacement does not contribute to total emissions."
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
    # Output for L175 - Percent of Total Emissions for J175
    output$L175_output <- renderText({
      percent <- L175()
      if (percent > 0) {
        paste("Percent of total emissions from recycling: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Recycling does not contribute to total emissions."
      }
    })
    
    H188 <- reactive({
      J82()
    })
    output$H188_output <- renderText({
      result <- H188()
      if (result > 0) {
        paste("Total emissions (H188): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No total emissions calculated for H188."
      }
    })
    
    H189 <- reactive({
      sum(J91(), J93(), J95(), J97(), J99(), J104(), J109(), J111(), J113(), J115(), J117(), J126(), J130(), J134(), J137(), J140(), J143(), J147(), J151(), J158(), J163(), J175())
    })
    output$H189_output <- renderText({
      result <- H189()
      if (result != 0) {
        paste("Sum of selected emissions components (H189): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No calculated emissions components for H189."
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
    # Output for L189 - Percent of Total Emissions for H189
    output$L189_output <- renderText({
      percent <- L189()
      if (percent > 0) {
        paste("Percent of total emissions for hypothetical H189: ", format(round(percent * 100, 2), nsmall = 2), "%")
      } else {
        "Hypothetical H189 does not contribute to total emissions."
      }
    })
    
    H190 <- reactive({
      sum(H91(), H93(), H95(), H97(), H99(), H104(), H109(), H111(), H113(), H115(), H117(), H126(), H130(), H134(), H137(), H140(), H147(), H151(), H158(), H163())
    })
    output$H190_output <- renderText({
      result <- H190()
      if (result != 0) {
        paste("Sum of hypothetical emissions components (H190): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No calculated hypothetical emissions components for H190."
      }
    })
    
    H191 <- reactive({
      J82() - H189()
    })
    output$H191_output <- renderText({
      result <- H191()
      if (result != 0) {
        paste("Remaining emissions after accounting for H189 (H191): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No remaining emissions calculated for H191."
      }
    })
    
    H192 <- reactive({
      if (input$F5 > 0) { 
        H191() / input$F5
      } else {
        0  
      }
    })
    output$H192_output <- renderText({
      result <- H192()
      if (result != 0) {
        paste("Emissions per household member (H192): ", format(round(result, 2), nsmall = 2), " units")
      } else {
        "No emissions per household member calculated for H192."
      }
    })
    
    
    # 
    # # A reactive expression that creates a dataframe based on inputs
    # reactiveData <- reactive({
    #   # Create a dataframe with the values of interest
    #   df <- data.frame(
    #     Category = c("AC Adjustment", "Computer Sleep Mode", "Dryer Energy Savings", "Laundry Load", "Thermostat Heating Adjustment"),
    #     Savings = c(input$f130, input$f134, input$f140, input$f137, input$f126)
    #   )
    #   return(df)
    # })
    
    output$savingsPlot <- renderPlot({
      # Initialize an empty vector to store savings
      savings <- numeric(15)
      
      # Check and calculate savings for each category
      if (length(J126()) > 0) savings[1] <- J126() else savings[1] <- 0
      if (length(J130()) > 0) savings[2] <- J130() else savings[2] <- 0
      if (length(J134()) > 0) savings[3] <- J134() else savings[3] <- 0
      if (length(J137()) > 0) savings[4] <- J137() else savings[4] <- 0
      if (length(J140()) > 0) savings[5] <- J140() else savings[5] <- 0
      if (length(J147()) > 0) savings[6] <- J147() else savings[6] <- 0
      if (length(J151()) > 0) savings[7] <- J151() else savings[7] <- 0
      if (length(J158()) > 0) savings[8] <- J158() else savings[8] <- 0
      if (length(J163()) > 0) savings[9] <- J163() else savings[9] <- 0
      if (length(J91()) > 0) savings[10] <- J91() else savings[10] <- 0
      if (length(J93()) > 0) savings[11] <- J93() else savings[11] <- 0
      if (length(J95()) > 0) savings[12] <- J95() else savings[12] <- 0
      if (length(J97()) > 0) savings[13] <- J97() else savings[13] <- 0
      if (length(J99()) > 0) savings[14] <- J99() else savings[14] <- 0
      #if (length(J104()) > 0) savings[15] <- J104() else savings[15] <- 0
      
      # Create a data frame of savings
      savings_data <- data.frame(
        Category = c("Thermostat Heating Adjustment", "AC Adjustment", "Computer Sleep Mode",
                     "Laundry Load", "Dryer Energy Savings", "Lamp Cost Savings",
                     "Fridge Replacement kWh Savings", "Boiler Replacement Cost Savings",
                     "Window Replacement Cost Savings", "Vehicle 1", "Vehicle 2", "Vehicle 3",
                     "Vehicle 4", "Vehicle 5", "Maintenance"),
        Savings = savings
      )
      
      # Order the factor levels based on the savings
      savings_data$Category <- factor(savings_data$Category, levels = savings_data$Category[order(savings_data$Savings, decreasing = TRUE)])
      
      # Remove categories with no savings
      savings_data <- savings_data[savings_data$Savings > 0, ]
      
      # Check if there are any savings to plot
      if (nrow(savings_data) > 0) {
        # Plot
        ggplot(savings_data, aes(x = Category, y = Savings, fill = Category)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(y = "Pounds of Co2/Year", x = "", title = "Estimated Savings from Energy-Saving Actions and Vehicle Upgrades")
      } else {
        # Return a blank plot or a message
        return(ggplot() +
                 theme_void() +
                 labs(title = "No savings data to display."))
      }
    })
    
    
    ## Liam
    question <- paste("Please generate a detailed action plan to increase cost savings and sustainability for a given home.",
                      "Use the list of relevant characteristics below to prioritize your sugestions off of.")
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