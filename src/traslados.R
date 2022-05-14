pacman::p_load(tidyverse, tidyquant, here)

bd <- read_csv(here("data", "traslados_abr_2022.csv")) 

max_trasla<- bd %>% 
  mutate(fecha=lubridate::ymd(fecha)) %>% 
  filter(fecha> "2022-01-01") %>% 
  group_by(fecha) %>%
  summarise(total=n()) %>% 
  mutate(maximo=max(total)) %>% 
  select(maximo) %>% 
  unique() %>% 
  pull()

bd %>% 
  mutate(fecha=lubridate::ymd(fecha)) %>% 
  filter(fecha> today()-90) %>% 
  group_by(fecha) %>%
  summarise(total=n(), 
            porc_tras=total/max_trasla) %>% 
  ggplot( aes(x = fecha, y = porc_tras)) +
  geom_line(size = 1.1, alpha = .2, color = "#008FD5") +
  geom_point(size = 1.1, alpha = .3, color = "#008FD5") +
  geom_ma(size = 1.1, color = "#FF2700", n = 7,
          linetype = 1) +
  labs(title="Evolución de traslados a hospitales \npor probable caso positivo de COVID-19",
       subtitle = "En la CDMX", y="")+
  scale_x_date(date_breaks= "2 weeks", date_labels = "%d/%b", name = "")+
  ggthemes::theme_fivethirtyeight()
