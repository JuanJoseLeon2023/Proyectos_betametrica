Se_aprende_haciendo_intro_estadistica
================
JUAN JOSE LEON
2023-11-08

# LLAMANDO A LA BASE DE DATOS

``` r
data <- read.csv("C:\\Users\\ASUS_PC\\Documents\\CURSOS RSTUDIO\\PROGRAMA EXPERTO EN CIENCIA DE DATOS\\BASES_DATOS_BASES\\Iowa_Liquor_Sales_YG84CD.csv",
                 stringsAsFactors = F,
                 header = T)

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'tidyr' was built under R version 4.3.2

    ## Warning: package 'readr' was built under R version 4.3.2

    ## Warning: package 'purrr' was built under R version 4.3.2

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
datos <- data %>% 
  mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
         City = toupper(City),
         Store.Name = toupper(Store.Name),
         Date = as.Date(Date, format = "%m/%d/%Y"),
         anio = lubridate::year(Date)) %>%
  rename(ventas = Sale..Dollars.,
         ciudad = City,
         categoria = Category.Name,
         nombre_tienda = Store.Name)
```

# TOP 5 TIENDAS (PROMEDIO VENTAS) AÑO 2016 PARA LA CIUDAD CEDAR RAPIDS

``` r
PromventasCedarRapids <- datos %>% select(Store.Number,nombre_tienda,ciudad,ventas,anio) %>% 
  filter(ciudad=="CEDAR RAPIDS" & anio == 2016) %>% 
  group_by(Store.Number) %>% 
  summarise(prom_ventas_2016 = mean(ventas)) %>% 
  arrange(-prom_ventas_2016)

Top5tiendas2016 <- PromventasCedarRapids[1:5,]
```

# TOP 5 PEORES VENDEDORES (PROMEDIO VENTAS) PARA EL AÑO 2016 PARA DAVENPORT

``` r
ventasXvendedor2016 <- datos %>% select(Vendor.Number,Vendor.Name,anio,ciudad,ventas) %>% 
  mutate(Vendor.Name = toupper(Vendor.Name)) %>% 
  filter(ciudad == "DAVENPORT" & anio ==2016) %>% 
  group_by(Vendor.Number) %>% 
  summarise(prom_ventas_2016 = mean(ventas)) %>% 
  arrange(-prom_ventas_2016)

peores5vendedores_Davenport <- ventasXvendedor2016[1:5,]
```

# TOP 5 de productos más vendidos, para el 2016 y 2017

``` r
suma_ventasXproducto2016 <- datos %>% select(Item.Number,Item.Description,anio,ventas) %>% 
  mutate(Item.Description = toupper(Item.Description)) %>%
  filter(anio == 2016) %>% 
  group_by(Item.Description , anio) %>% 
  summarise(suma_ventas = sum(ventas)) %>% 
  arrange(-suma_ventas) %>% 
  pivot_wider(names_from = anio,
              values_from = c(suma_ventas), names_prefix = "Ventas_",
              values_fill = 0)
```

    ## `summarise()` has grouped output by 'Item.Description'. You can override using
    ## the `.groups` argument.

``` r
Top5productos2016 <- suma_ventasXproducto2016[1:5,]


suma_ventasXproducto2017 <- datos %>% select(Item.Number,Item.Description,anio,ventas) %>% 
  mutate(Item.Description = toupper(Item.Description)) %>%
  filter(anio == 2017) %>% 
  group_by(Item.Description , anio) %>% 
  summarise(suma_ventas = sum(ventas)) %>% 
  arrange(-suma_ventas) %>% 
  pivot_wider(names_from = anio,
              values_from = c(suma_ventas), names_prefix = "Ventas_",
              values_fill = 0)
```

    ## `summarise()` has grouped output by 'Item.Description'. You can override using
    ## the `.groups` argument.

``` r
Top5productos2017 <- suma_ventasXproducto2017[1:5,]
```

# Resumen de resultados

``` r
Top5tiendas2016
```

    ## # A tibble: 5 × 2
    ##   Store.Number prom_ventas_2016
    ##          <int>            <dbl>
    ## 1         3385             354.
    ## 2         5097             338.
    ## 3         3773             171.
    ## 4         5222             163.
    ## 5         3666             162.

``` r
peores5vendedores_Davenport
```

    ## # A tibble: 5 × 2
    ##   Vendor.Number prom_ventas_2016
    ##           <int>            <dbl>
    ## 1           101             396.
    ## 2           461             205.
    ## 3           306             147.
    ## 4           370             128.
    ## 5           305             119.

``` r
Top5productos2016
```

    ## # A tibble: 5 × 2
    ## # Groups:   Item.Description [5]
    ##   Item.Description              Ventas_2016
    ##   <chr>                               <dbl>
    ## 1 MALIBU COCONUT RUM                294984.
    ## 2 BAILEY'S ORIGINAL IRISH CREAM     205765.
    ## 3 RUMCHATA                          196674.
    ## 4 UV BLUE (RASPBERRY) VODKA         152508.
    ## 5 BACARDI LIMON                     128448.

``` r
Top5productos2017
```

    ## # A tibble: 5 × 2
    ## # Groups:   Item.Description [5]
    ##   Item.Description              Ventas_2017
    ##   <chr>                               <dbl>
    ## 1 MALIBU COCONUT RUM                 26019.
    ## 2 BAILEY'S ORIGINAL IRISH CREAM      21731.
    ## 3 RUMCHATA                           19710.
    ## 4 BACARDI LIMON                      11924.
    ## 5 BAILEY'S SALTED CARAMEL             5100
