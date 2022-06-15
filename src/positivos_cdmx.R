pacman::p_load(tidyverse, tidyquant, here, lubridate)

bd <- read_csv(here("data", "casos_positivos.csv")) 


bd %>% 
  gather(tipo, total, pruebas_totales:tasa_positividad_cdmx) %>% 
  as_tibble() %>% 
  mutate(fecha_toma_muestra=ymd(fecha_toma_muestra)) %>% 
  filter(fecha_toma_muestra> today()-90 & 
           fecha_toma_muestra <= max(fecha_toma_muestra)-1) %>% 
  group_by(fecha_toma_muestra, tipo) %>%
  summarise(total=sum(total, na.rm=T)) %>%
  
  ggplot( aes(x = fecha_toma_muestra, y = total)) +
  geom_line(size = 1.1, alpha = .2, color = "#008FD5") +
  geom_point(size = 1.1, alpha = .3, color = "#008FD5") +
  geom_ma(size = 1.1, color = "#FF2700", n = 7,
          linetype = 1) +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En la CDMX", y="")+
  facet_wrap(~tipo, scales = "free")+
  scale_x_date(date_breaks= "3 weeks", date_labels = "%d/%b", name = "")+
  ggthemes::theme_fivethirtyeight()


ggsave(here("out","positivos_cdmx_update.png"), width = 11, height = 5, units="in")


# semana geom_step ---------------------------------------------------------------


bd %>% 
  gather(tipo, total, pruebas_totales:tasa_positividad_cdmx) %>% 
  as_tibble() %>% 
  filter(fecha_toma_muestra> "2021-04-11" & 
           fecha_toma_muestra <= max(fecha_toma_muestra)-2) %>%
  mutate(mes=floor_date(fecha_toma_muestra, unit = "week")) %>%
  group_by(mes, tipo) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  
  
  ggplot( aes(x = mes, y = totales)) +
  geom_step(size = 1.1, color = "#098154") +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En la CDMX")+
  facet_wrap(~tipo, scales = "free")+
  scale_x_date(date_breaks= "2 months", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()
ggsave(here("out","positivos_cdmx_anual_538.png"), width = 11, height = 5, units="in")


# Todo el año solo con geom_ma --------------------------------------------

bd %>%
  gather(tipo, total, pruebas_totales:tasa_positividad_cdmx) %>%
  as_tibble() %>%
  filter(fecha_toma_muestra> "2021-01-14" &
           fecha_toma_muestra <= max(fecha_toma_muestra)-3) %>%
  # mutate(mes=floor_date(fecha_toma_muestra, unit = "week")) %>% 
  # group_by(mes, tipo) %>% 
  # summarise(totales=sum(total, na.rm=T)) %>% 
  
  ggplot( aes(x = fecha_toma_muestra, y = total)) +
  # geom_line(size = 1.1, alpha = .2, color = "#008FD5") +
  # geom_point(size = 1.1, alpha = .3, color = "#008FD5") +
  geom_ma(size = 1.1, color = "#FF2700", n = 7,
          linetype = 1) +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En la CDMX")+
  facet_wrap(~tipo, scales = "free")+
  scale_x_date(date_breaks= "2 months", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()



# Por mes -----------------------------------------------------------------
sevan <- c("tasa_positividad", "tasa_positividad_cdmx")
bd %>% 
  gather(tipo, total, pruebas_totales:tasa_positividad_cdmx) %>% 
  as_tibble() %>% 
  filter(fecha_toma_muestra> "2021-04-11") %>%
  mutate(mes=floor_date(fecha_toma_muestra, unit = "month")) %>%
  group_by(mes, tipo) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  filter(!tipo %in% sevan) %>% 
  ggplot( aes(x = mes, y = totales)) +
  geom_step(size = 1.1, color = "#FF2700") +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En la CDMX")+
  facet_wrap(~tipo, scales = "free")+
  scale_x_date(date_breaks= "1 months", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()