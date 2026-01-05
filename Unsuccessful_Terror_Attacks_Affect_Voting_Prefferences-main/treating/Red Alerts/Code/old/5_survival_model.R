# Luiz Bines - 2025
# Simplified Cox Proportional Hazards model script

# Libraries
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(modelsummary)
library(ggplot2)
library(broom)
library(survMisc)

# Load data
setwd('C:/Users/luizb/Desktop/Dissertation/Dissertation/Red-Alerts-and-Votes/')
df <- read.csv('treating/Red Alerts/Output/2_all_years_cities_grid.csv')
df$date <- as.Date(df$date)

# Count cumulative previous alerts
df <- df %>%
  arrange(SEMEL_YISHUV, date) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(quantity_of_alarms = cumsum(lag(alert, default = 0))) %>%
  ungroup()


# Counting localities that had 0 vs 1 vs 2 vs 3 vs 4+
df %>%
  filter(alert == 1) %>%
  count(SEMEL_YISHUV) %>%
  mutate(cat = case_when(
    n == 1 ~ "1 alert",
    n == 2 ~ "2 alerts",
    n == 3 ~ "3 alerts",
    n >= 4 ~ "4+ alerts"
  )) %>%
  count(cat) %>%
  bind_rows(
    tibble(cat = "0 alerts",
           n = n_distinct(df$SEMEL_YISHUV) - n_distinct(df %>% filter(alert == 1) %>% pull(SEMEL_YISHUV)))
  )


# Criar base para modelo de sobrevivência com censura
surv_data <- df %>%
  filter(alert == 1) %>%
  arrange(SEMEL_YISHUV, date) %>%
  group_by(SEMEL_YISHUV) %>%
  mutate(
    next_date = lead(date),  # próxima data de alerta
    time_to_next_alert = as.numeric(next_date - date),  # tempo até o próximo alerta
    status = ifelse(is.na(time_to_next_alert), 0, 1),   # 0 = censurado, 1 = evento
    previous_alerts = row_number() - 1,  # número de alertas anteriores
    # se censurado, tempo até o fim do período observado
    time_to_next_alert = ifelse(status == 0, as.numeric(as.Date('2022-02-09') - date), time_to_next_alert),
    year = year(date)
  ) %>%
  ungroup()

cox_model <- coxph(
  Surv(time_to_next_alert, status) ~ previous_alerts + distance + 
    previous_alerts:log(time_to_next_alert + 1),  
  data = surv_data
)

# Verifique o resultado:
summary(cox_model)



# Plot baseline survival curve
ggsurvplot(
  survfit(cox_model), 
  data = surv_data,
  conf.int = TRUE,
  ggtheme = theme_minimal(),
  xlab = "Days",
  ylab = "Probability of Remaining Without a New Red Alert",
  title = "Survival Curve from Cox Model"
)




surv_data_filtered <- surv_data %>% 
  mutate(previous_alerts_cat = cut(previous_alerts, 
                                   breaks = c(-1, 0, 1, 2, Inf),
                                   labels = c("0", "1", "2", "3+")))  # Note the labels as character

km_fit <- survfit(Surv(time_to_next_alert, status) ~ previous_alerts_cat, 
                  data = surv_data_filtered)

ggsurvplot(
  km_fit,
  data = surv_data_filtered,  # Must use the filtered dataset!
  conf.int = FALSE,
  palette = "hue",
  linetype = 1,
  ggtheme = theme_minimal(),
  title = "Survival Curves by Number of Previous Alerts",
  xlab = "Days",
  ylab = "Survival Probability (No New Alert)",
  legend.title = "Previous Alerts",
  legend.labs = levels(surv_data_filtered$previous_alerts_cat)  # Use the factor levels from the actual data
)
