
# install.packages("ggthemes")
# install.packages("reshape2")
# install.packages("zoo")
# install.packages("rstan")
# install.packages("readrba")
# install.packages("lubridate")
# install.packages("readr")
# install.packages("devtools")
# devtools::install_github("mattcowgill/readabs")

library(ggthemes)
library(reshape2)
library(readabs)
library(dplyr)
library(ggplot2)
library(zoo)
library(rstan)
library(readrba)
library(lubridate)
library(readr)
library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(writexl)


abs_monthly_detailed_cpi <- read_abs(series_id = c("A130393720C", "A130395477A", "A130391704T", "A130393202C",
                                                   "A130399324K", "A130393223R", "A130398061A", "A130394364L",
                                                   "A130393244A", "A130395337X", "A130399373F", "A130391921L",
                                                   "A130391928C", "A130390598F", "A130393034C", "A130391844V", 
                                                   "A130390535W", "A130393195T", "A130395092L", "A130392446F", 
                                                   "A130397487T", "A130391480T", "A130390619F", "A130391963K", 
                                                   "A130392215W", "A130394819W", "A130396254K", "A130390647R", 
                                                   "A130390227V", "A130390640X", "A130395211T", "A130390437T",
                                                   "A130391942X", "A130390612R", "A130393027F", "A130394413W", 
                                                   "A130393440K", "A130390892V", "A130393412A", "A130398012F", 
                                                   "A130397115C", "A130391669W", "A130394105V", "A130399709X", 
                                                   "A130398467A", "A130391018T", "A130391760K", "A130393279A", 
                                                   "A130395743F", "A130391991V", "A130397290L", "A130396016C", 
                                                   "A130399478A", "A130399184V", "A130390094A", "A130397970T", 
                                                   "A130391536T", "A130399772R", "A130397844C", "A130394588X", 
                                                   "A130399779F", "A130393090W", "A130391060W", "A130390731F", 
                                                   "A130392614F", "A130399198J", "A130397795V", "A130394595W", 
                                                   "A130397823T", "A130396492T", "A130395379W", "A130398516K", 
                                                   "A130399506X", "A130391249A", "A130391263W", "A130392033J", 
                                                   "A130394917A", "A130399520V", "A130393531T", "A130391116W", 
                                                   "A130397339R", "A130390059V", "A130393545F", "A130395197J", 
                                                   "A130389989T", "A130397851A", "A130393517W", "A130393482J", 
                                                   "A130390283L", "A130390920T", "A130393496W", "A130396688V", 
                                                   "A130391788L", "A130395834L", "A130393391A", "A130398320J", 
                                                   "A130392110A", "A130390850W", "A130393118L", "A130398362F", 
                                                   "A130391291F", "A130390479R", "A130399548W", "A130398292K", 
                                                   "A130393832W", "A130399079T", "A130397185K", "A130397865R", 
                                                   "A130394196L", "A130394952J", "A130392880W", "A130391830F", 
                                                   "A130392194W", "A130394798W", "A130395148L", "A130397360J", 
                                                   "A130392334L", "A130390808T", "A130392054V", "A130399821X", 
                                                   "A130398635A", "A130390024V", "A130393573R", "A130398757X", 
                                                   "A130390052C", "A130396240W", "A130392761F", "A130393944R", 
                                                   "A130392635T", "A130399114L", "A130399100X", "A130392474R"))


cleaned_cpi_monthly <- abs_monthly_detailed_cpi %>%
  select(date, series, value) %>% 
  distinct(date, series, value) %>% 
  mutate(series = series %>%
           str_remove_all("Index Numbers ;") %>%
           str_remove_all(" ;  Australia ;") %>%
           str_trim()) %>% 
  mutate(series = case_match(series, 
                             "All groups CPI" ~ "Headline CPI",
                             .default = series 
  )) %>% 
  pivot_wider(names_from = series,
              values_from = value)


write_xlsx(cleaned_cpi_monthly, "C:/Users/anthony.malouf_ebury/Documents/Cleaned R Data/cpi_detailed_data_release.xlsx")




abs_monthly_analytical_cpi <- read_abs(series_id = c("A130393720C", "A130607789R" , "A130400381L" , "A130400612K",
                                                     "A130392362W", "A130395029W" , "A130397424J" , "A130396114J")) 


cleaned_monthly_analytical_cpi <- abs_monthly_analytical_cpi %>% 
  select(date, series, value) %>% 
  distinct(date, series, value) %>% 
  mutate(series = series %>%
           str_remove_all("Index Numbers ;") %>%
           str_remove_all(" ;  Australia ;") %>%
           str_trim()) %>% 
  mutate(series = case_match(series, 
                             "All groups CPI" ~ "Headline CPI",
                             .default = series 
  )) %>% 
  pivot_wider(names_from = series,
              values_from = value)


write_xlsx(cleaned_monthly_analytical_cpi, "C:/Users/anthony.malouf_ebury/Documents/Cleaned R Data/cpi_analytical_data_release.xlsx")








df_detailed <- cleaned_cpi_monthly %>%
  mutate(date = as.Date(date)) %>% 
  arrange(date)

df_growth_detailed <- df_detailed %>%
  mutate(across(where(is.numeric), list(
    # 1. m/m: Month-on-month
    mm = ~ (.x / lag(.x, 1) - 1) * 100,
    
    # 2. q/q (Point-to-Point): Dec vs Sep
    qq_point = ~ (.x / lag(.x, 3) - 1) * 100,
    
    # 3. q/q (3m average): (Avg Last 3m / Avg Prior 3m)
    qq_3m_avg = ~ (rollmeanr(.x, k = 3, fill = NA) / 
                     lag(rollmeanr(.x, k = 3, fill = NA), n = 3) - 1) * 100,
    
    # 4. y/y: Year-on-year
    yy = ~ (.x / lag(.x, 12) - 1) * 100,
    
    # 5. 3m annualised: ((Current / 3m ago)^4) - 1
    ar3m = ~ ((.x / lag(.x, 3)) ^ 4 - 1) * 100,
    
    # 6. 6m annualised: ((Current / 6m ago)^2) - 1
    ar6m = ~ ((.x / lag(.x, 6)) ^ 2 - 1) * 100
    
  ), .names = "{.col}_{.fn}")) 


target_var <- "Beef and veal"  

cols_to_plot <- c(
  paste0(target_var, "_mm"),             
  paste0(target_var, "_qq_point"),       
  paste0(target_var, "_qq_3m_avg"),
  paste0(target_var, "_yy"),             
  paste0(target_var, "_ar3m"),         
  paste0(target_var, "_ar6m")          
)

chart_data <- df_growth_detailed %>%
  select(date, any_of(cols_to_plot)) %>%
  pivot_longer(cols = -date, names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = cols_to_plot))

last_points <- chart_data %>%
  group_by(Metric) %>%
  filter(!is.na(Value)) %>%      # Remove NAs to find true last point
  filter(date == max(date)) %>%  # Keep only the latest date
  ungroup()

ggplot(chart_data, aes(x = date, y = Value)) +
  geom_line(color = "darkblue", size = 1) + 
  
  # Add a red dot at the end for visual clarity
  geom_point(data = last_points, color = "red", size = 2) +
  
  # Add the text label (The "Vertical Axis Value")
  geom_text(data = last_points, 
            aes(label = round(Value, 2)), # Round to 2 decimal places
            hjust = -0.3,                 # Nudge label to the right
            color = "red", 
            fontface = "bold", 
            size = 3.5) +
  
  # CRITICAL: Extend the x-axis limits so the label doesn't get cut off
  scale_x_date(expand = expansion(mult = c(0.05, 0.25))) +
  
  facet_wrap(~Metric, scales = "free", ncol = 2) + 
  
  labs(title = paste("Analysis of:", target_var),
       x = "Date",
       y = NULL) + 
  
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

                                                     
df <- cleaned_monthly_analytical_cpi %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date)


df_growth <- df %>%
  mutate(across(where(is.numeric), list(
    # 1. m/m: Month-on-month percent change
    mom = ~ (.x / lag(.x, 1) - 1) * 100,
    
    # 2. q/q (
    qoq = ~ (.x / lag(.x, 3) - 1) * 100,
    
    # 3. (3m/3m): 
    qoq3mavg = ~ (rollmeanr(.x, k = 3, fill = NA) / 
                     lag(rollmeanr(.x, k = 3, fill = NA), n = 3) - 1) * 100,
    
    # 4. y/y: Year-on-year percent change
    yoy = ~ (.x / lag(.x, 12) - 1) * 100,
    
    # 5. 3m annualised: 
    ar3m = ~ ((.x / lag(.x, 3)) ^ 4 - 1) * 100,
    
    # 6. 6m annualised:
    ar6m = ~ ((.x / lag(.x, 6)) ^ 2 - 1) * 100
    
  ), .names = "{.col}_{.fn}")) # Creates new columns like 'Headline CPI_mm'

head(df_growth)

# write.csv(df_growth, "cpi_growth_rates.csv", row.names = FALSE)

target_var <- "Headline CPI"


cols_to_plot <- c(
  paste0(target_var, "_mom"),
  paste0(target_var, "_qoq"),
  paste0(target_var, "_qoq3mavg"),
  paste0(target_var, "_yoy"),
  paste0(target_var, "_ar3m"),
  paste0(target_var, "_ar6m")
)

chart_data <- df_growth %>%
  select(date, any_of(cols_to_plot)) %>%
  pivot_longer(cols = -date, names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = cols_to_plot))


last_points <- chart_data %>%
  group_by(Metric) %>%
  filter(!is.na(Value)) %>%
  filter(date == max(date)) %>%
  ungroup()


ggplot(chart_data, aes(x = date, y = Value)) +
  geom_line(color = "darkblue", size = 1) + 
  

  geom_point(data = last_points, color = "red", size = 2) +
  
  geom_text(data = last_points, 
            aes(label = round(Value, 2)), 
            hjust = -0.3, 
            color = "red", 
            fontface = "bold", 
            size = 3.5) +
  
 
  scale_x_date(expand = expansion(mult = c(0.05, 0.25))) +
  
 
  facet_wrap(~Metric, scales = "free", ncol = 2) + 
  
  labs(title = paste("Analysis of:", target_var),
       x = "Date",
       y = NULL) + 
  
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )                            

