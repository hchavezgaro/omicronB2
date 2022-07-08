pacman::p_load(tidyverse, foreign, 
               janitor, readxl,
               ggrepel, tidyquant, 
               here, lubridate)

# remotes::install_github("RodrigoZepeda/covidmx")

require(covidmx)
variantes   <- covidmx::descarga_datos_variantes_GISAID("nacional")

fechas <- tibble(some_dates=1:366,
                 valores=NA) %>% 
  mutate(valores=seq(ymd(20200101), ymd(20201231), 'day'),
         semana=epiweek(valores), 
         date=floor_date(valores, "month"),
         ano=substr(date,1,4)) %>% 
  select(ano, semana, date) %>% 
  unique() %>% 
  group_by(semana) %>% 
  filter(date==max(date)) %>% 
  ungroup()

prueba <- variantes %>% 
  filter(ano==2020) %>% 
  left_join()



# prueba 2022 -------------------------------------------------------------


prueba <- variantes %>% 
  filter(ano==2022, 
         semana!=52) 

relleno <- tibble(variant=c("Omicron B.1.", "Omicron BA.5"), 
                 semana=c(25, 25), 
                 freq=c(0,0))

prueba <- bind_rows(prueba, relleno)

# fechas <- tibble(some_dates=1:181,
#                  valores=NA) %>% 
#   mutate(valores=seq(ymd(20220101), ymd(20220630), 'day'),
#          semana=epiweek(valores), 
#          date=floor_date(valores, "month"),
#          ano=substr(date,1,4)) %>% 
#   select(ano, semana, date) %>% 
#   unique() %>% 
#   group_by(semana) %>% 
#   filter(date==max(date)) %>% 
#   ungroup() %>% 
#   mutate(ano=as.integer(ano), 
#          semana=as.integer(semana))

# parag <- prueba %>% 
#   mutate(mes=case_when(semana==1 ~ "Enero 2022",
#                        semana==5 ~ "Febrero 2022",
#                        semana==10 ~ "Marzo 2022",
#                        semana==15 ~ "Abril 2022",
#                        semana==20 ~ "Mayo 2022",
#                        semana==25 ~ "Junio 2022"))
# 
# meses <- c("Enero 2022", 2, 3, 4, "Febrero 2022", 6, 7, 8, 9, "Marzo 2022", 
#            11, 12, 13, 14, "Abril 2022", 16, 17, 18, 19, "Mayo 2022", 21, 22, 23, 24, "Junio 2022") 
# 
# parag$semana <- factor(x=parag$semana , labels=meses)

ggplot(prueba, aes(x=semana, y=freq, fill=variant)) + 
  geom_area(alpha=0.6) + 
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25),
                     label = c("Enero 2022", "Febrero 2022", 
                               "Marzo 2022", "Abril 2022", 
                               "Mayo 2022", "Junio 2022"))+
  labs(x="", y="Frecuencia", 
       title=str_wrap("Tendencia de la proporción de las variantes de SAR-COV-2 en México", width = 30),
       subtitle="Durante 2022",
       caption="Fuente: GISAID EpiFlu", 
       fill=" ")+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()

ggsave(here("out", "variantes.png"), width = 12, height = 6, units="in")

