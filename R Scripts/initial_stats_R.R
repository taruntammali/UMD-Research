#===============================================
# Retail Electricity Code

# ===============================================
# Add libraries
library("tidyverse")
library("ggplot2")
library("corrplot")
library("lubridate")
library("psych")
library("gt")
library("webshot2")
library("leaflet")
library("sf")
library("tigris")
library("car")

# =========== Read Datasets =====================


# ======= Residential Retail Price ========
avg_res_price <- read.csv("data_clean/Avg_Monthly_Residential_Price_EIA861_2020_2026_Clean.csv")
avg_res_price$Residential.electricity.price <- as.numeric(avg_res_price$Residential.electricity.price)
avg_res_price$Date <- ym(as.character(avg_res_price$Date))


# ======= Monthly Dataset ========
monthly <- read.csv("data_clean/monthly_datasets_2018_2026.csv")
monthly$Date <- ym(as.character(monthly$Date))

# ===============================================


# =========== Residential Plot ==============
plot1 <- ggplot(avg_res_price, aes(x = Date, y = Residential.electricity.price)) + 
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE, linewidth=0.8) +
  geom_point(size = 2) + 
  labs(title = "Average Monthly Residential Retail Electricity Price (2018-2026)", x = 'Date', y = "Avg Residential Retail Electricity Price (cents/KW)") +
  theme_minimal()
plot1
ggsave("figures/retail_reg.png", plot1, width = 7, height = 5, dpi = 300)

# ================================================


# ========== Monthly Correlation Matrix ==============
monthly_corr <- data.frame(
  `Residential Price`  = monthly$Residential.electricity.price, 
  `Total Generation`   = monthly$Total.Generation, 
  `Natural Gas Price`  = monthly$Natural.Gas.Prices, 
  `Customers`          = monthly$Customers,
  `Average Load DOM`   = monthly$Average_load_DOM,
  `Heating Degree Days` = monthly$HDD,
  `Cooling Degree Days` = monthly$CDD,
  `Sales`              = monthly$Sales,
  `Revenue`            = monthly$Revenue,
  check.names = FALSE  
)

monthly_cor_matrix <- cor(monthly_corr, use = "pairwise.complete.obs")

col_palette <- colorRampPalette(c("#BB4444", "#EE9999", "#FFFFFF", "#77AADD", "#4477AA"))(200)

corrplot(monthly_cor_matrix, 
         method = "color",          
         col = col_palette,         
         type = "upper",         
         order = "hclust",        
         addCoef.col = "black",    
         tl.col = "black",         
         tl.srt = 45,     
         diag = FALSE,         
         number.cex = 0.75, 
         tl.cex = 0.85)       

# =================================================


# ======= Monthly Summary Statistics =======

library(janitor)
library(modelsummary)
monthly_summary <- monthly_corr %>%
  select(where(is.numeric)) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "Variable", 
    values_to = "Value"
  ) %>% 
  group_by(Variable) %>% 
  summarise(
    n = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE), 
    Min = min(Value, na.rm = TRUE), 
    Max = max(Value, na.rm = TRUE), 
  ) 

monthly_summary_png <- monthly_summary %>%
  gt() %>%
  tab_header(
    title = "Monthly Summary Statistics"
  ) %>%
  fmt_number(
    columns = -c(Variable, Observations), 
    decimals = 2
  ) %>%
  fmt_integer(
    columns = Observations
  )

gtsave(
  monthly_summary_png,
  "figures/monthly_summary_statistics.png", 
  vwidth = 1600,
  vheight = 1200
)
