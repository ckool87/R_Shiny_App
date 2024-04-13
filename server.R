library(shiny)

# Define the server logic for the Shiny application
server <- function(input, output, session) {
  
  ### Liam
  observeEvent(input$submit, {
    chat_log <- list()
    question <- input$ChatGPT_Question_Box
    send_message(question, chat_log)
    updateTextAreaInput(session, "ChatGPT_Answer_Box", value = response)
  })
  
  # linear regression
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
  
  # regression value based on slider input
  output$regression_value <- renderText({
    sqft_value <- input$sqft_slider
    regression_value <- predict(model, newdata = data.frame(SQFTAPPROX = sqft_value))
    paste("Predicted List Price:", round(regression_value))
  })
  ### Liam
  
  
  
  observeEvent(input$submit, {
    print("Submit clicked!")
    
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
    
    # Function to fetch unit type based on heating source, known as t125
    t125 <- function(heating_source) {
      source_index <- which(conversion_factors$Energy_Source == heating_source)
      if (length(source_index) > 0) {
        return(conversion_factors$Unit_Type[source_index])
      } else {
        return(NA)  # Return NA if source not found or input is NA
      }
    }
    
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
    
    # Additional outputs and logic for other inputs and calculations can be added similarly
  })
}
# 
# # H130 - Thermostat cooling savings
# h130 <- reactive({
#   if (safe_numeric(input$f130) == 1) {
#     if (safe_numeric(input$h42) == 1) {
#       return(safe_numeric(input$f42) * AC_electricity_percent * thermostat_cooling_savings * safe_numeric(input$b131) * 12)
#     } else if (safe_numeric(input$h42) == 2) {
#       return(safe_numeric(input$f42) * AC_electricity_percent * thermostat_cooling_savings * safe_numeric(input$b131) * cost_per_kWh * 12)
#     } else {
#       return(0)
#     }
#   } else {
#     return(0)
#   }
# })
# 
# # H134 - Computer sleep savings
# h134 <- reactive({
#   if (safe_numeric(input$f134) == 1) {
#     return(computer_sleep_savings * cost_per_kWh)
#   } else {
#     return(0)
#   }
# })
# 
# # H137 - Laundry load savings
# h137 <- reactive({
#   if (safe_numeric(input$f137) == 1) {
#     return(kWh_per_load_laundry * cost_per_kWh * 52 * safe_numeric(input$b139))
#   } else {
#     return(0)
#   }
# })
# 
# # H140 - Dryer energy savings
# h140 <- reactive({
#   if (safe_numeric(input$f140) == 1) {
#     return((dryer_energy / 2) * cost_per_kWh)
#   } else {
#     return(0)
#   }
# })
# 
# # H147 - Lamp cost savings
# h147 <- reactive({
#   if (safe_numeric(input$f147) == 1) {
#     return(safe_numeric(input$b147) * lamp_cost_savings)
#   } else {
#     return(0)
#   }
# })
# 
# # H151 - Fridge replacement kWh savings
# h151 <- reactive({
#   if (safe_numeric(input$f151) == 1) {
#     return(fridge_replacement_kWh_savings * cost_per_kWh)
#   } else {
#     return(0)
#   }
# })
# 
# # H158 - Boiler replacement cost savings
# h158 <- reactive({
#   if (safe_numeric(input$f158) == 1 && safe_numeric(input$f7) %in% c(1, 3)) {
#     return(boiler_replacement_cost_savings)
#   } else {
#     return(0)
#   }
# })
# 
# # H163 - Window replacement cost savings
# h163 <- reactive({
#   if (safe_numeric(input$f163) == 1 && safe_numeric(input$f7) != 6) {
#     return(window_replacement_cost_savings)
#   } else {
#     return(0)
#   }
# })
# 
# # Reactive expressions for the J cell calculations
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
# 
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
# # J109 - Personal vehicle emissions reduction if changing car
# J109 <- reactive({
#   if(input$F109 == 1) {
#     J91() - J27()  # Assuming J27 also includes D109 in its calculation
#   } else {
#     0
#   }
# })
# 
# # J111 - Reduction due to change in car for D17
# J111 <- reactive({
#   if(input$F111 == 1) {
#     (ifelse(input$G17 == 1, (input$D17 * 52) / (input$K17 + input$D111), input$D17 / (input$K17 + input$D111)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio) -
#       (ifelse(input$G17 == 1, (input$D17 * 52) / input$K17, input$D17 / input$K17) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio)
#   } else {
#     0
#   }
# })
# 
# # J113 - Reduction due to change in car for D19
# J113 <- reactive({
#   if(input$F113 == 1) {
#     (ifelse(input$G19 == 1, (input$D19 * 52) / (input$K19 + input$D113), input$D19 / (input$K19 + input$D113)) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio) -
#       (ifelse(input$G19 == 1, (input$D19 * 52) / input$K19, input$D19 / input$K19) * EF_passenger_vehicle * nonCO2_vehicle_emissions_ratio)
#   } else {
#     0
#   }
# })
# 
# # J120 - Sum of specific Js
# J120 <- reactive({
#   sum(
#     J117(), J115(), J113(), J111(), J109(), J29(),
#     J99(), J97(), J95(), J93(), J91()
#   )
# })
# 
# # J126 - Savings from thermostat heating adjustments based on heating source
# j126 <- reactive({
#   if (safe_numeric(input$f126) == 1) {
#     switch(safe_numeric(input$f7),
#            "1" = (j37() * heating_percent_NG * thermostat_heating_savings * safe_numeric(input$b127)),
#            "2" = (j42() * heating_percent_electricity * thermostat_cooling_savings * safe_numeric(input$b127)),
#            "3" = (j53() * heating_percent_fuel_oil * thermostat_heating_savings * safe_numeric(input$b127)),
#            "4" = (j57() * heating_percent_propane * thermostat_heating_savings * safe_numeric(input$b127)),
#            0
#     )
#   } else {
#     0
#   }
# })
# 
# # J130 - Savings from cooling adjustments
# j130 <- reactive({
#   if (safe_numeric(input$f130) == 1) {
#     j42() * AC_electricity_percent * thermostat_cooling_savings * safe_numeric(input$b131)
#   } else {
#     0
#   }
# })
# 
# # J134 - Savings from enabling computer sleep
# j134 <- reactive({
#   if (safe_numeric(input$f134) == 1) {
#     computer_sleep_savings * e_factor_value
#   } else {
#     0
#   }
# })
# 
# # J137 - Savings from washing clothes in cold water
# j137 <- reactive({
#   if (safe_numeric(input$f137) == 1) {
#     kWh_per_load_laundry * e_factor_value * 52 * safe_numeric(input$b139)
#   } else {
#     0
#   }
# })
# 
# # J140 - Savings from using a drying rack or line drying half the time
# j140 <- reactive({
#   if (safe_numeric(input$f140) == 1) {
#     (dryer_energy / 2) * e_factor_value
#   } else {
#     0
#   }
# })
# 
# # J143 - Green power purchase emissions savings
# j143 <- reactive({
#   if (safe_numeric(input$f143) == 1) {
#     safe_numeric(input$b143) * j42()
#   } else {
#     0
#   }
# })
# 
# # J147 - ENERGY STAR lights savings
# j147 <- reactive({
#   if (safe_numeric(input$f147) == 1) {
#     safe_numeric(input$b147) * lamp_kWh_savings * e_factor_value
#   } else {
#     0
#   }
# })
# 
# # J151 - ENERGY STAR fridge kWh savings
# j151 <- reactive({
#   if (safe_numeric(input$f151) == 1) {
#     fridge_replacement_kWh_savings * e_factor_value
#   } else {
#     0
#   }
# })
# 
# # J158 - Boiler replacement savings
# j158 <- reactive({
#   if (safe_numeric(input$f158) == 1) {
#     switch(as.character(safe_numeric(input$f7)),
#            "1" = boiler_replacement_savings_NG,
#            "3" = boiler_replacement_savings_FO,
#            0
#     )
#   } else {
#     0
#   }
# })
# 
# # J163 - ENERGY STAR window replacement savings
# j163 <- reactive({
#   if (safe_numeric(input$f163) == 1) {
#     switch(as.character(safe_numeric(input$f7)),
#            "1" = EF_natural_gas * (window_replacement_energy_savings / BTU_per_1000cf_NG),
#            "2" = e_factor_value * (window_replacement_energy_savings / BTU_per_kWh),
#            "3" = (window_replacement_energy_savings / BTU_per_gallon_FO) * EF_fuel_oil_gallon,
#            "4" = (window_replacement_energy_savings / BTU_per_gallon_propane) * EF_propane,
#            0
#     )
#   } else {
#     0
#   }
# })
# 
# # J175 - Recycling emissions savings
# j175 <- reactive({
#   if (safe_numeric(input$f175) == 1) {
#     -sum(
#       if (safe_numeric(input$f65) == 2) { safe_numeric(input$f5) * newspaper_recycling_avoided_emissions } else { 0 },
#       if (safe_numeric(input$f67) == 2) { safe_numeric(input$f5) * glass_recycling_avoided_emissions } else { 0 },
#       if (safe_numeric(input$f69) == 2) { safe_numeric(input$f5) * plastic_recycling_avoided_emissions } else { 0 },
#       if (safe_numeric(input$f71) == 2) { safe_numeric(input$f5) * metal_recycling_avoided_emissions } else { 0 },
#       if (safe_numeric(input$f73) == 2) { safe_numeric(input$f5) * mag_recycling_avoided_emissions } else { 0 }
#     )
#   } else {
#     0
#   }
# })

# # L91 - L175 (Percent of total emissions for various categories)
# l91 <- reactive({ if(j82() > 0) { j91() / j82() } else { 0 } })
# l93 <- reactive({ if(j82() > 0) { j93() / j82() } else { 0 } })
# l95 <- reactive({ if(j82() > 0) { j95() / j82() } else { 0 } })
# l104 <- reactive({ if(j82() > 0) { j104() / j82() } else { 0 } })
# l109 <- reactive({ if(j82() > 0) { j109() / j82() } else { 0 } })
# l111 <- reactive({ if(j82() > 0) { j111() / j82() } else { 0 } })
# l113 <- reactive({ if(j82() > 0) { j113() / j82() } else { 0 } })
# l126 <- reactive({ if(j82() > 0) { j126() / j82() } else { 0 } })
# l130 <- reactive({ if(j82() > 0) { j130() / j82() } else { 0 } })
# l134 <- reactive({ if(j82() > 0) { j134() / j82() } else { 0 } })
# l137 <- reactive({ if(j82() > 0) { j137() / j82() } else { 0 } })
# l140 <- reactive({ if(j82() > 0) { j140() / j82() } else { 0 } })
# l143 <- reactive({ if(j82() > 0) { j143() / j82() } else { 0 } })
# l147 <- reactive({ if(j82() > 0) { j147() / j82() } else { 0 } })
# l151 <- reactive({ if(j82() > 0) { j151() / j82() } else { 0 } })
# l158 <- reactive({ if(j82() > 0) { j158() / j82() } else { 0 } })
# l163 <- reactive({ if(j82() > 0) { j163() / j82() } else { 0 } })
# l175 <- reactive({ if(j82() > 0) { j175() / j82() } else { 0 } })
# l189 <- reactive({ if(j82() > 0) { h189() / j82() } else { 0 } })
# 
# Output the L cell percentages to the UI
# output$l91_output <- renderText({ sprintf("%.2f%%", l91() * 100) })
# output$l93_output <- renderText({ sprintf("%.2f%%", l93() * 100) })
# output$l95_output <- renderText({ sprintf("%.2f%%", l95() * 100) })
# output$l104_output <- renderText({ sprintf("%.2f%%", l104() * 100) })
# output$l109_output <- renderText({ sprintf("%.2f%%", l109() * 100) })
# output$l111_output <- renderText({ sprintf("%.2f%%", l111() * 100) })
# output$l113_output <- renderText({ sprintf("%.2f%%", l113() * 100) })
# output$l126_output <- renderText({ sprintf("%.2f%%", l126() * 100) })
# output$l130_output <- renderText({ sprintf("%.2f%%", l130() * 100) })
# output$l134_output <- renderText({ sprintf("%.2f%%", l134() * 100) })
# output$l137_output <- renderText({ sprintf("%.2f%%", l137() * 100) })
# output$l140_output <- renderText({ sprintf("%.2f%%", l140() * 100) })
# output$l143_output <- renderText({ sprintf("%.2f%%", l143() * 100) })
# output$l147_output <- renderText({ sprintf("%.2f%%", l147() * 100) })
# output$l151_output <- renderText({ sprintf("%.2f%%", l151() * 100) })
# output$l158_output <- renderText({ sprintf("%.2f%%", l158() * 100) })
# output$l163_output <- renderText({ sprintf("%.2f%%", l163() * 100) })
# output$l175_output <- renderText({ sprintf("%.2f%%", l175() * 100) })
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


#})
#}