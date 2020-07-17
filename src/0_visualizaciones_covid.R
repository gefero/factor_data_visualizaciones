## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggpubr)
library(janitor)


## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------

covid <-  read.delim("./data/20200716_Covid19Casos.csv", 
                     stringsAsFactors = FALSE, 
                     fileEncoding = 'UTF-16LE',
                     sep = ",")


## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------

covid <- covid %>%
        mutate( 
                tipo = case_when(
                        stringr::str_detect(covid$CLASIFICACION, 'Caso sosp') ~ 'Sospechoso',
                        stringr::str_detect(covid$CLASIFICACION, 'Caso Sosp') ~ 'Sospechoso',
                        stringr::str_detect(covid$CLASIFICACION, 'Caso Desc') ~ 'Otro',
                        stringr::str_detect(covid$CLASIFICACION, 'Caso Inv') ~ 'Otro',
                        stringr::str_detect(covid$CLASIFICACION, 'Otro d') ~ 'Otro',
                        stringr::str_detect(covid$CLASIFICACION, 'Sin clas') ~ 'Otro',
                        stringr::str_detect(covid$CLASIFICACION, 'Caso confirmado - F') ~ 'Fallecido',
                        TRUE ~ 'Confirmado'
                ),
                edad_r = case_when(
                        edad >= 0 & edad <=4 ~ '0-04',
                        edad >= 5 & edad <=9 ~ '05-09',
                        edad >= 10 & edad <=14 ~ '10-14',
                        edad >= 15 & edad <=19 ~ '15-19',
                        edad >= 20 & edad <=24 ~ '20-24',
                        edad >= 25 & edad <=29 ~ '25-29',
                        edad >= 30 & edad <=34 ~ '30-34',
                        edad >= 35 & edad <=39 ~ '35-39',
                        edad >= 40 & edad <=44 ~ '40-44',
                        edad >= 45 & edad <=49 ~ '45-49',
                        edad >= 50 & edad <=54 ~ '50-54',
                        edad >= 55 & edad <=59 ~ '55-59',
                        edad >= 60 & edad <=64 ~ '60-64',
                        edad >= 65 & edad <=69 ~ '65-69',
                        edad >= 70 & edad <=74 ~ '70-74',
                        edad >= 75 & edad <=79 ~ '75-79',
                        edad >= 80 & edad <=84 ~ '80-84',
                        edad >= 85 & edad <=89 ~ '85-89',
                        edad >= 90 & edad <=199 ~ '90-199'
                ),
                fecha_apertura_r = as_date(fecha_apertura),
                sexo = case_when(
                        sexo == 'F' ~ 'Femenino',
                        sexo == 'M' ~ 'Masculino'
                )
        )



## ----echo=FALSE, fig.height=10, fig.width=10, message=FALSE, warning=FALSE-------------------------------------------------------------------------
covid %>%
        drop_na() %>%
        filter(sexo!='NR' & fecha_apertura_r > '2020-03-21', tipo == 'Confirmado') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_line(aes(x=fecha_apertura_r, y=n, color=edad_r)) +
                facet_wrap(~sexo, nrow=2) +
                #scale_color_viridis_c(option='A') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
        labs(title='Gráfico 1. Cantidad de casos fallecidos por día, edad y sexo',
                     color='Edad',
                     x = 'Fecha registro',
                     y= 'N casos')

ggsave('./images/grafico1.png')        


# Formateamos los datos
covid %>%
        drop_na() %>%
        mutate(edad_r = case_when(
                edad >= 0 & edad <= 13 ~ '0-13',
                edad >= 14 & edad <= 34 ~ '14-34',
                edad >= 34 & edad <= 64 ~ '35-64',
                edad >= 65  ~ '65 y +')
               ) %>%
        filter(sexo!='NR' & fecha_apertura_r > '2020-03-21', tipo == 'Confirmado') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_line(aes(x=fecha_apertura_r, y=n, color=edad_r)) +
                facet_wrap(~sexo, nrow=2) +
                #scale_color_viridis_c(option='A') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
        labs(title='Gráfico 2. Cantidad de casos fallecidos por día, edad y sexo',
                     color='Edad',
                     x = 'Fecha registro',
                     y= 'N casos')

ggsave('./images/grafico2.png')        




# Comparacióne scalas
ggpubr::ggarrange(nrow=2,

        covid %>%
        drop_na() %>%
        filter(sexo!='NR' & sexo!='Masculino' & fecha_apertura_r > '2020-03-21', tipo == 'Confirmado') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_tile(aes(x=fecha_apertura_r, y=edad_r, fill=n)) +
                #facet_wrap(~sexo, nrow=2) +
                scale_fill_gradient(low='green', high='red') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
                labs(title='Gráfico 3. Cantidad de casos confirmados según fecha y edad (Mujeres)',
                subtitle='Rojo a verde',
                     fill='N casos',
                     x = 'Fecha registro',
                     y= 'Edad en grupos quinquenales'),
        
covid %>%
        drop_na() %>%
        filter(sexo!='NR' & sexo!='Masculino' & fecha_apertura_r > '2020-03-21', tipo == 'Confirmado') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_tile(aes(x=fecha_apertura_r, y=edad_r, fill=n)) +
                #facet_wrap(~sexo, nrow=2) +
                scale_fill_viridis_c(option='A') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
                labs(subtitle='Magma',
                     fill='N casos',
                     x = 'Fecha registro',
                     y= 'Edad en grupos quinquenales')
)

ggsave('./images/grafico3.png')        



# Grafico confirmados
covid %>%
        drop_na() %>%
        filter(sexo!='NR' & fecha_apertura_r > '2020-03-21', tipo == 'Confirmado') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_tile(aes(x=fecha_apertura_r, y=edad_r, fill=n)) +
                facet_wrap(~sexo, nrow=2) +
                scale_fill_viridis_c(option='A') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
                labs(title='Gráfico 4. Cantidad de casos confirmados por día, edad y sexo',
                     fill='N casos',
                     x = 'Fecha registro',
                     y= 'Edad en grupos quinquenales')

ggsave('./images/grafico4.png')        



# Grafico fallecidos
covid %>%
        drop_na() %>%
        filter(sexo!='NR' & fecha_apertura_r > '2020-03-21', tipo == 'Fallecido') %>%
        select(sexo, edad_r, fecha_apertura_r) %>%
        group_by(fecha_apertura_r, sexo, edad_r) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        complete(fecha_apertura_r, sexo, edad_r, fill=list(n=0)) %>%
        ggplot() +
                geom_tile(aes(x=fecha_apertura_r, y=edad_r, fill=n)) +
                facet_wrap(~sexo, nrow=2) +
                scale_fill_viridis_c(option='A') +
                #scale_fill_continuous(na.value='black')
                theme_minimal() +
             labs(title='Gráfico 5. Cantidad de casos fallecidos por día, edad y sexo',
                     fill='N casos',
                     x = 'Fecha registro',
                     y= 'Edad en grupos quinquenales')
        
ggsave('./images/grafico5.png')        

