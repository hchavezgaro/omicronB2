# Evolución casos confirmados COVID (positivos, hospitalizados, intubados)
# Solo CDMX
# Mejoras: la línea de geom_ma no llega al 100% de los casos

pacman::p_load(tidyverse, foreign, 
               janitor, readxl,
               ggrepel, tidyquant, 
               here)



hosp <- read_csv(here("data", "personas_hospitalizadas_con_diagnostico_covid19.csv"))
casos <- read_csv(here("data", "casos_positivos.csv"))

max_casos<- casos %>% 
  # filter(fecha_toma_muestra<"2021-12-01") %>%
  mutate(maximo_casos=max(positivos_totales_cdmx)) %>% 
  select(maximo_casos) %>% 
  unique() %>% 
  pull()


fa <- casos%>% 
  mutate(fecha=ymd(fecha_toma_muestra)) %>% 
  mutate(maximo_casos=max_casos) %>% 
  mutate(porc_casos=positivos_totales_cdmx/maximo_casos) %>% 
  select(fecha=fecha, Casos=porc_casos)

# Solo se hace fa, no están actualizadas las hospitalizaciones
fa %>%  
  filter(fecha>="2020-11-15" & fecha< max(fecha)-2) %>%
  ggplot( ) +
  # geom_line(aes(x = fecha, y = Casos), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fecha, y = Casos),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fecha, y = Casos), size = 1.1, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#FF2700"))+
  labs(title="Evolución de casos COVID-19, CDMX",
       subtitle = "Porcentaje con respecto al pico de Omicron (enero)", color="")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ggthemes::theme_fivethirtyeight()


ggsave(here("out","vs_omicronb1_wave.png"), width = 10, height =5, units="in")


fe <- hosp%>% 
  mutate(fecha=ymd(fecha)) %>% 
  mutate(maximo_hosp=max(hospitalizados_totales_cdmx), 
         maximo_int=max(camas_intubados_cdmx)) %>% 
  mutate(porc_hosp=hospitalizados_totales_cdmx/maximo_hosp, 
         porc_int=camas_intubados_cdmx/maximo_int) %>% 
  select(fecha=fecha, Hospitalizaciones=porc_hosp, Intubados=porc_int) 


fi <- left_join(fa, fe) %>% 
  gather(tipo, porc, Casos:Intubados)


fi %>%  
  filter(fecha>="2021-11-15" & fecha< max(fecha)-1) %>% 
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = vacunado), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = vacunado),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fecha, y = porc, color = tipo), size = 1.1, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#FF2700", "#77AB43", "#008FD5"))+
  labs(title="Evolución de hospitalizaciones COVID-19, CDMX",
       subtitle = "Porcentaje con respecto al pico de Delta", color="")+
  scale_x_date(date_breaks= "4 weeks", date_labels = "%d/%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ggthemes::theme_fivethirtyeight()

ggsave(here("out","casos_vs_delta.png"), width = 11, height = 5, units="in")


# ZM ----------------------------------------------------------------------

max_casos<- casos %>% 
  filter(fecha_toma_muestra<"2021-12-01") %>% 
  mutate(maximo_casos=max(positivos_totales)) %>% 
  select(maximo_casos) %>% 
  unique() %>% 
  pull()



fa <- casos%>% 
  mutate(fecha=ymd(fecha_toma_muestra)) %>% 
  mutate(maximo_casos=max_casos) %>% 
  mutate(porc_casos=positivos_totales/maximo_casos) %>% 
  select(fecha=fecha, Casos=porc_casos)

fe <- hosp%>% 
  mutate(fecha=ymd(fecha)) %>% 
  mutate(maximo_hosp=max(hospitalizados_totales), 
         maximo_int=max(camas_intubados_totales)) %>% 
  mutate(porc_hosp=hospitalizados_totales/maximo_hosp, 
         porc_int=camas_intubados_totales/maximo_int) %>% 
  select(fecha=fecha, Hospitalizaciones=porc_hosp, Intubados=porc_int) 





fi <- left_join(fa, fe) %>% 
  gather(tipo, porc, Casos:Intubados)


fi %>%  
  filter(fecha>="2021-11-15" & fecha< max(fecha)-2) %>% 
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = vacunado), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = vacunado),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fecha, y = porc, color = tipo), size = 1.1, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#FF2700", "#77AB43", "#008FD5"))+
  labs(title="Evolución de hospitalizaciones COVID-19",
       subtitle = "En CDMX", fill="")+
  scale_x_date(date_breaks= "2 weeks", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()



# Solo hospitalizados -----------------------------------------------------
# vs. Delta pick
fe <- hosp%>% 
  mutate(fecha=ymd(fecha)) %>%
  filter(fecha>"2021-06-06") %>% 
  mutate(maximo_hosp=max(hospitalizados_totales_cdmx), 
         maximo_int=max(camas_intubados_cdmx)) %>% 
  mutate(porc_hosp=hospitalizados_totales_cdmx/maximo_hosp, 
         porc_int=camas_intubados_cdmx/maximo_int) %>% 
  select(fecha=fecha, Hospitalizaciones=porc_hosp, Intubados=porc_int) %>% 
  gather(tipo, porc, Hospitalizaciones:Intubados)


fe %>%  
  filter(fecha>="2020-06-15") %>% 
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = vacunado), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = vacunado),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fecha, y = porc, color = tipo), size = 1.1, n = 7,
          linetype = 1) +
  scale_color_manual(values=c( "#77AB43", "#008FD5"))+
  labs(title="Evolución de hospitalizaciones COVID-19",
       subtitle = "En CDMX", color="")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()

ggsave(here("out","hosp_vs_delta.png"), width = 11, height = 5, units="in")