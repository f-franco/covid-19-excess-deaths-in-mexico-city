library(dplyr)
library(lubridate)
library(ggplot2)

covid19 <- read.csv("covid_cases.csv")
actas <- read.csv("death_certs.csv")

covid_data <- covid19 %>%
  filter(ENTIDAD_RES == 9, FECHA_DEF <= max(actas$fecha_defuncion)) %>%
  group_by(FECHA_DEF) %>%
  summarise(DEF_T = n())

actas_data <- actas %>%
  filter(fecha_defuncion >= "2020-02-28" | 
           (fecha_defuncion >= "2019-02-28" & fecha_defuncion <= as.Date(max(actas$fecha_defuncion)) - years(1)) | 
           (fecha_defuncion >= "2018-02-28" & fecha_defuncion <= as.Date(max(actas$fecha_defuncion)) - years(2)) | 
           (fecha_defuncion >= "2017-02-28" & fecha_defuncion <= as.Date(max(actas$fecha_defuncion)) - years(3))) %>%
  group_by(fecha_defuncion) %>%
  summarise(DEF_T = n())

covid_data$FECHA <- as.Date(covid_data$FECHA_DEF)
covid_data$CATEGORIA <- "COVID"
actas_data$fecha_defuncion <- as.Date(actas_data$fecha_defuncion)
actas_data$FECHA <- as.Date(format(actas_data$fecha_defuncion, "2020-%m-%d"))
actas_data$CATEGORIA <- as.factor(year(actas_data$fecha_defuncion))

mean_data <- actas_data %>%
  filter(CATEGORIA %in% c("2017", "2018", "2019")) %>%
  group_by(FECHA) %>%
  summarise(DEF_T = mean(DEF_T)) %>%
  mutate(CATEGORIA = "PROM_3")

covidmean_data <- mean_data %>%
  inner_join(covid_data, by = "FECHA") %>%
  summarise(FECHA, DEF_T = DEF_T.x + DEF_T.y, CATEGORIA = "COVID + PROM")

actas_data <- filter(actas_data, CATEGORIA == "2020")

plot_data <- bind_rows(actas_data, mean_data, covidmean_data)

ggplot(plot_data, aes(x = FECHA, y = DEF_T, color = CATEGORIA)) +
  geom_line(lineend = "round") +
  scale_x_date(date_breaks = "1 month") +
  scale_color_viridis_d()