library(tidyverse)

cities <-
   maps::us.cities %>%
   filter(pop > 300000) %>%
   sample_n(10) %>%
   select(-capital, -country.etc) %>%
   set_names(c("City and State", "Population", "Latitude", "Longitude")) %>%
   bind_rows(
      tibble(
         `City and State` = "Online",
         `Population` = NA,
         `Latitude` = NA,
         `Longitude` = NA
      )
   )

out <-
   cities %>%
   as_tibble() %>%
   select(1) %>%
   crossing(Date = as.Date( paste( as.character(2017), "-1-1", sep="") ) + seq( 0,364 )) %>%
   mutate(
      transactions = 1
   ) %>%
   mutate(
      phone = rnorm(nrow(.), 1, 0.2),
      charger = rnorm(nrow(.), 2, 0.2),
      case = rnorm(nrow(.), 1, 0.2)
   ) %>%
   mutate(
      phone = map2(transactions, phone, rpois),
      charger = map2(transactions, charger, rpois),
      case = map2(transactions, case, rpois)
   ) %>%
   unnest()

make_basket <- function(phone, charger, case, ...){
   c(
      rep("phone", phone),
      rep("charger", charger),
      rep("case", case)
   ) %>%
      sample()
}

out %>%
   mutate(
      Date = format(Date, format="%m/%d"),
      `Total Profit` = rnorm(nrow(.), 30, 20) %>% round(2),
      `Total Revenue` = rnorm(nrow(.), 100, 30) %>% round(2),
      `Items Purchased` = pmap(., make_basket) %>% map_chr(paste0, collapse = ", ")
   ) %>%
   select(2, 1, 8, 7, 9)  %>%
   write_csv("~/Desktop/Angelas stores.csv")

cities %>%
   write_csv("~/Desktop/Angelas stores cities.csv")


