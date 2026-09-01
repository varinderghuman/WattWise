# ================================
# BUS 439 - Main Project: WattWise
# Team = DJGSS
# Author = Varinder
# ================================


library(tibble)
library(lubridate)
library(dplyr)

# ================================
# 1. PARAMETERS
# ================================
floor_area <- 115112
EUI <- 561.5
electricity_share <- 0.65
utilization_factor <- 1.10
electricity_cost <- 0.14  # CAD/kWh

annual_elec <- floor_area * EUI * electricity_share * utilization_factor
avg_load <- annual_elec / 8760

# ================================
# 2. TIME + WEATHER
# ================================

# Generate hourly timestamps for 2025
time <- seq(ymd_h("2025-01-01 00"), ymd_h("2025-12-31 23"), by = "hour")

# Create base dataframe
df <- tibble(time = time) %>%
  mutate(
    doy = yday(time),      # day of year
    hour = hour(time),     # hour of day
    month = month(time)    # numeric month 1-12
  )

# Vancouver monthly day/night temperatures (°C)
month_temps <- data.frame(
  month = 1:12,
  day = c(7, 7, 10, 13, 17, 20, 24, 23, 20, 14, 9, 7),
  night = c(3, 2, 4, 6, 9, 12, 15, 15, 13, 9, 5, 3)
)

# Generate hourly temperatures using cosine interpolation
df <- df %>%
  mutate(
    temp = sapply(1:nrow(.), function(i){
      m <- month[i]
      hr <- hour[i]
      day_temp <- month_temps$day[month_temps$month == m]
      night_temp <- month_temps$night[month_temps$month == m]
      # simple cosine curve from night->day->night
      night_temp + (day_temp - night_temp) * sin(pi * hr / 24)
    })
  )

# Compute heating and cooling degree hours
df <- df %>%
  mutate(
    heating_degree = pmax(18 - temp, 0),
    cooling_degree = pmax(temp - 20, 0)
  )

# ================================
# 3. BASE LOADS
# ================================
df <- df %>%
  mutate(
    Fans = avg_load * 0.40,
    Cooling = avg_load * 0.22 * (cooling_degree / max(cooling_degree + 1e-6)),
    Heating = avg_load * 0.18 * (heating_degree / max(heating_degree + 1e-6)),
    Lighting = avg_load * 0.20
  )

df <- df %>%
  mutate(
    Facility_base = Fans + Cooling + Heating + Lighting
  )

# ================================
# 4. MODEL (WITH OCCUPANCY)
# ================================
df <- df %>%
  mutate(
    # Occupancy profile (hospital = always high but variable)
    occ = 0.825 + 0.175 * sin(2 * pi * (hour - 8) / 24),
    
    # --- LIGHTING (occupancy + dimming) ---
    Lighting_ems = Lighting * (0.5 + 0.5 * occ), # 50% reduction at low occupancy, 0% at high occupancy
    
    # --- FANS (cube law + occupancy control) ---
    flow_ratio = 0.6 + 0.4 * occ,
    Fans_ems = Fans * (flow_ratio^3), # 30-40% reduction based on occupancy and cube law for fans
    
    # --- HVAC (demand-controlled ventilation) ---
    Cooling_ems = Cooling * (0.7 + 0.3 * occ), # 30% reduction at low occupancy
    Heating_ems = Heating * (0.7 + 0.3 * occ), # 30% reduction at low occupancy
    
    # --- TOTAL EMS ---
    Facility_ems = Fans_ems + Cooling_ems + Heating_ems + Lighting_ems,
    
    Savings_kW = Facility_base - Facility_ems,
    Savings_pct = Savings_kW / Facility_base
  )

# ================================
# 5. MODEL (WITH OCCUPANCY + RESEARCH CALIBRATION)
# ================================

current_savings <- sum(df$Facility_base - df$Facility_ems) /
  sum(df$Facility_base)

target_savings <- 0.33   # 33% (middle of 30–35%)

scale_factor <- (1 - target_savings) / (1 - current_savings)

df <- df %>%
  mutate(
    Fans_ems = Fans * ((flow_ratio^3) * scale_factor),
    Cooling_ems = Cooling * ((0.7 + 0.3 * occ) * scale_factor),
    Heating_ems = Heating * ((0.7 + 0.3 * occ) * scale_factor),
    Lighting_ems = Lighting * ((0.5 + 0.5 * occ) * scale_factor),

    Facility_ems = Fans_ems + Cooling_ems + Heating_ems + Lighting_ems,

    Savings_kW = Facility_base - Facility_ems,
    Savings_pct = Savings_kW / Facility_base
  )


# ================================
# 6. Override + sensor failure + missing data
# ================================

df <- df %>%
  mutate(
    override_flag = rbinom(n(), 1, 0.05), # 5% of hours have manual override (e.g. maintenance, special events)
    sensor_fail = rbinom(n(), 1, 0.03), # 3% of hours have sensor failure (random noise added to temp)
    missing_flag = rbinom(n(), 1, 0.02), #
    
    temp_faulty = ifelse(sensor_fail == 1, temp + rnorm(n(), 0, 5), temp),
    
    Facility_operational = case_when(
      missing_flag == 1 ~ NA,
      override_flag == 1 ~ Facility_base,
      TRUE ~ Facility_ems
    )
  )


# ================================
# 7. Save to CSV
# ================================

write.csv(df, "FULL_hourly_data.csv", row.names = FALSE)

# ================================
# 8. Cost of implementation
# ================================

occ_sensors <- floor_area / 60 # 1 sensor per 60 m2 for occupancy
daylight_sensors <- floor_area / 84 # 1 sensor per 84 m2 for daylight
co2_sensors <- floor_area / 175 # 1 sensor per 175 m2 for CO2
hvac_occ_sensors <- floor_area / 114 # 1 sensor per 114 m2 for HVAC occupancy

occ_cost <- occ_sensors * 435 * 1.0 # assume $435 per occupancy sensor (including installation)
daylight_cost <- daylight_sensors * 625 * 1.0 # assume $625 per daylight sensor (including installation)
co2_cost <- co2_sensors * 1000 * 1.0 # assume $1000 per CO2 sensor (including installation)
hvac_occ_cost <- hvac_occ_sensors * 250 * 1.0 # assume $250 per HVAC occupancy sensor (including installation)

total_cost <- occ_cost + daylight_cost + co2_cost + hvac_occ_cost
total_cost

total_cost_adjusted <- total_cost * 0.7 # assumes 70% of the area is suitable for implementation

annual_savings <- sum(df$Savings_kW) * electricity_cost  # Total kWh saved annually * cost per kWh
payback_years <- total_cost_adjusted / annual_savings

maintenance_cost <- total_cost_adjusted * 0.03 # assume 5% of initial cost annually for maintenance 
maintenance_cost

df %>%
  summarise(
    Total_Cost = total_cost_adjusted,
    Annual_Savings = annual_savings,
    Payback_Years = payback_years,
    Maintenance_Cost = maintenance_cost
  )

# ================================
