#loading the database 
ghs <- read_csv("ghs.csv")
gaq <- read_csv("gaq.csv")
#save datasets


  
# kan zien welke handig zijn 
subset(ghs, Year >= 2014 & Year <= 2024 & `Disease Category` == 	"Respiratory")
names(ghs)

#om tidyversre en die andere te laten werken nadat je ze hebt installed the laten nadat je het hebt installed
library(tidyverse)
library(yaml)
library(rmarkdown)
library(janitor)
library(naniar)
library(tinytex)
#om te zien welke versie je hebt 
version
#name op te schonen en consistent te hebben 
ghs <- janitor::clean_names(ghs)
gaq <- janitor::clean_names(gaq)
glimpse(ghs)
glimpse(gaq)
#voor de goeie data types per colum voor ghs en gaq
ghs <-
  ghs |>
  mutate(country = as_factor(country),
         gender = as_factor(gender),
         disease_name = as_factor(disease_name),
         disease_category  = as_factor(disease_category),
         age_group  = as_factor(age_group),
         treatment_type  = as_factor(treatment_type),
         availability_of_vaccines_treatment  = as_factor(availability_of_vaccines_treatment)
  )
gaq <-
  gaq |>
  mutate(country = as_factor(country),
  )


glimpse(gaq)
glimpse(ghs)

#alleen de rijen met Respiratory als disease_category voor ghs
ghs <- ghs %>%
  filter(disease_category == "Respiratory")
#alleen rijen van 2014 of later voor ghs
ghs <- ghs %>%
  filter(year >= 2014)
#alleen rijen tot 2025 
ghs <- ghs %>%
  filter(year >= 2014)
#verwijderd rijen met landen die niet in beide voorkomen
ghs <- ghs %>%
  filter(!country %in% c("Italy", "Canada", "Argentina"))
gaq <- gaq %>%
  filter(!country %in% c("Bangladesh", "Egypt", "Ethiopia", "Pakistan", "Phillipines", "Thailand", "Vietnam"))

names(gaq)
head(gaq)
#kolom state verwijderen 
gaq <- gaq %>% select(-state)

# gegevens van landen bij elkaar zetten dus geen states/provincies meer van global air pollution
gaq_country_year <- gaq %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(country, year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
#onnodige kolommen verwijderen van global air pollution
gaq_country_year <- gaq_country_year %>%
  select(-c(wind_speed_km_h, humidity_percent, deforestation_rate_percent, aqi))
#onnodige kolommen verwijderen van global health statistics 
ghs_country_year <- ghs %>%
  select(-c(prevalence_rate_percent, age_group, gender, healthcare_access_percent, doctors_per_1000, hospital_beds_per_1000, treatment_type, availability_of_vaccines_treatment, average_treatment_cost_usd, dal_ys, improvement_in_5_years_percent))


# verschillende respiratory diseases van een jaar bij elkaar brengen 
ghs_country_year <- ghs_country_year %>%
  group_by(country, year) %>%
  summarise(
    population_affected = sum(population_affected, na.rm = TRUE),
    incidence_rate_percent = mean(incidence_rate_percent, na.rm = TRUE),
    mortality_rate_percent = mean(mortality_rate_percent, na.rm = TRUE),
    recovery_rate_percent = mean(recovery_rate_percent, na.rm = TRUE),
    per_capita_income_usd = mean(per_capita_income_usd, na.rm = TRUE),
    education_index = mean(education_index, na.rm = TRUE),
    urbanization_rate_percent = mean(urbanization_rate_percent, na.rm = TRUE),
    .groups = "drop"
  )
#year goeie datatype make in bijde datasets 
gaq_country_year <- gaq_country_year %>%
  mutate(year = as.numeric(year))

ghs_country_year <- ghs_country_year %>%
  mutate(year = as.numeric(year))

#variabele toevoegen total air pollution is een index door de verschillende vervuilinge te standariseren en dan opt te tellen. uiteindelijk komt er een gemiddelde uit die vergeleken is met de vervuiling van alle lande in de dataset 
# waarde >0 meer vervuild dan gemiddeld, waarde = 0 gemiddeld vervuild, waarde 0< mnder vervuild gemiddeld 
gaq_country_year <- gaq_country_year %>%
  mutate(
    total_air_pollution = rowMeans(scale(select(., pm2_5_ug_m3, pm10_ug_m3,
                                                no_ug_m3, no2_ug_m3,
                                                n_ox_ppb, nh3_ug_m3,
                                                co_mg_m3, so2_ug_m3,
                                                o3_ug_m3, benzene_ug_m3,
                                                toluene_ug_m3, xylene_ug_m3)),
                                   na.rm = TRUE)
  )
#geeft aan welke 10 landen het meest vervuild zijn 
top10_polluted <- gaq_country_year %>%
  group_by(country) %>%
  summarise(avg_pollution = mean(total_air_pollution, na.rm = TRUE)) %>%
  arrange(desc(avg_pollution)) %>%
  slice_head(n = 10)

#alleen de kolommen selecteren die bij elkaar mergen voor gaq
gaq_clean <- gaq_country_year %>%
  select(country, year, total_air_pollution)
#de databases mergen 
result <- gaq_clean %>%
  inner_join(ghs_country_year, by = c("country", "year"))


# Voegt de 2 onderstaande variabele toe 
# Disease intesity laat zien dit jaar  het meer of minder getroffen door respiratoire ziektes dan gemiddeld, 1 veel hogere ziekte last, 0 gemiddeld, -1 veel lagere ziekte last.
# Disease per population ratio die laat zien hoe de pollution is in vergelijking met aantal diseases laagwaarde=minder ziekte relatief aan pollution, hoge waarde = veel ziekte ondanks lage/medium pollution
result <- result %>%
  mutate(
    disease_intensity = scale(population_affected),
    disease_per_pollution = population_affected / (abs(total_air_pollution) + 1)
  )

#safe databases
saveRDS(gaq_country_year, "gaq_country_year.rds")
saveRDS(ghs_country_year, "ghs_country_year.rds")
saveRDS(result, "result.rds")


#kijken voor missing data 
vis_miss(ghs) #geen missing data 
vis_miss(gaq_country_year) # geen missing data 

plot_data <- result %>%
  mutate(disease_intensity = disease_intensity[,1]) %>%
  pivot_longer(
    cols = c(total_air_pollution, disease_intensity),
    names_to = "type",
    values_to = "index"
  ) %>%
  mutate(country_year = paste(country, year))

print(
  ggplot(plot_data, aes(x = country_year, y = index, fill = type)) +
    geom_col(position = "dodge") +
    labs(x = "Country - Year", y = "Index (-1 to 1)", fill = "Metric") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
)




names(result)
head(result)

