library(readr)
library(readxl)

# Loading Data ----

## Response Variable: ----- 

### Electricity % from Oil----

E <- read_csv(
  "Documents/Intro to Econometrics/Average Gas Price Analysis/API_EG.ELC.PETR.ZS_DS2_en_csv_v2_7269.csv", 
  skip = 4, 
  show_col_types = FALSE)
View(E)

## Quantitative Variables: ----- 

### Population ----

Pop <- read_csv(
  "Documents/Intro to Econometrics/Average Gas Price Analysis/API_SP.POP.TOTL_DS2_en_csv_v2_196954.csv", 
    skip = 4, 
      show_col_types = FALSE)
View(Pop)

### GDP----

GDP <- read_csv(
  "Documents/Intro to Econometrics/Average Gas Price Analysis/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_133326.csv", 
   skip = 4, 
    show_col_types = FALSE)
View(GDP)

### Net Trade----

NT <- read_csv(
  "Documents/Intro to Econometrics/Average Gas Price Analysis/API_BN.GSR.GNFS.CD_DS2_en_csv_v2_5131.csv", 
     skip = 4, 
      show_col_types = FALSE)
View(NT)

### Primary Industry ----
industry <- read_csv("48a6dccd-3a1a-469b-9b28-c1de02e0acb7_Data.csv")
industry <- industry %>% select(`Country Name`, `Series Name`, `2023 [YR2023]`)

names(industry) <- c("country_name", "series_name", "value")
industry$value <- as.numeric(industry$value)

industry$series_name <- recode(
  industry$series_name,
  "Agriculture, forestry, and fishing, value added (% of GDP)" = "agriculture",
  "Services, value added (% of GDP)" = "services",
  "Industry (including construction), value added (% of GDP)" = "industry"
)

industry <- industry %>% 
  pivot_wider(
    names_from = series_name,
    values_from = value
  )

industry <- industry %>%
  mutate(
    main_industry = case_when(
      agriculture >= industry & agriculture >= services ~ "Agriculture",
      industry >= agriculture & industry >= services ~ "Industry",
      services >= agriculture & services >= industry ~ "Services",
      TRUE ~ NA_character_
    )
  )

industry <- industry %>%
  select(-any_of("NA"))

clean <- clean %>%
  left_join(
    industry %>%
      select(-any_of("NA")) %>%
      filter(country_name %in% clean$country_name) %>%
      select(country_name, agriculture, services, industry),
    by = "country_name"
  )
clean <- clean %>%
  rename(
    agriculture_pct = agriculture,
    services_pct = services,
    industry_pct = industry
  )


### Number of Nuclear Power Plants ----
### Data pulled from the world nuclear association
### (https://world-nuclear.org/information-library/facts-and-figures/world-nuclear-power-reactors-and-uranium-requireme)

nuclear <- data.frame(
  country_name = c(
    "United States","France","China","Russia","Japan","South Korea",
    "India","Canada","Ukraine","United Kingdom","Spain","Sweden",
    "Belgium","Czech Republic","Finland","Switzerland","Hungary",
    "Slovakia","Bulgaria","Romania","Mexico","Argentina","Brazil",
    "South Africa","Pakistan","Iran","United Arab Emirates","Belarus",
    "Armenia","Slovenia","Netherlands"
  ),
  reactors = c(
    94,57,57,37,14,26,
    20,19,15,9,7,6,
    5,6,5,4,4,
    5,2,2,2,3,2,
    2,6,1,4,2,
    1,1,1
  )
)

clean <- merge(clean, nuclear, by = "country_name", all.x = TRUE)
clean$reactors[is.na(clean$reactors)] <- 0

## Qualitative Variables: ----

###Landlocked ----
###Compiled from a list of landlocked countries from Britannica.com (https://www.britannica.com/topic/landlocked-country)

landlocked_countries <- c(
  "Kazakhstan","Mongolia","Chad","Niger","Mali","Bolivia","Ethiopia",
  "Zambia","Afghanistan","South Sudan","Central African Republic",
  "Botswana","Turkmenistan","Uzbekistan","Paraguay","Zimbabwe",
  "Burkina Faso","Uganda","Laos","Belarus","Kyrgyzstan","Nepal",
  "Tajikistan","Malawi","Hungary","Azerbaijan","Austria",
  "Czech Republic","Serbia","Slovakia","Switzerland","Bhutan",
  "Moldova","Lesotho","Armenia","Burundi","Rwanda",
  "North Macedonia","Eswatini","Luxembourg","Andorra",
  "Liechtenstein","San Marino","Vatican City"
)

clean$LL <- ifelse(clean$country_name %in% landlocked_countries, 1, 0)
  

### Region ----
region <- read_delim("UNSD — Methodology.csv", delim = ";")

names(region)[names(region) == "Country or Area"] <- "country_name"
names(region)[names(region) == "Region Name"] <- "region_name"

region <- region[, c("country_name", "region_name")]
region$country_name <- trimws(region$country_name)

clean$country_name <- trimws(df$country_name)
clean <- merge(clean, region, by = "country_name", all.x = TRUE)

clean$region_name[clean$country_code == "USA"] <- "Americas"
clean$region_name[clean$country_code == "GBR"] <- "Europe"
clean$region_name[clean$country_code == "MDA"] <- "Europe"
clean$region_name[clean$country_code == "NLD"] <- "Europe"

region_map <- c(
  "Asia" = 1,
  "Europe" = 2,
  "Americas" = 3,
  "Africa" = 4,
  "Oceania" = 5
)

clean$region <- unname(region_map[clean$region_name])
clean <- clean[, !(names(clean) %in% "region_name")]

### Corruption Score ----
### Data taken from Transparency.org from their comprehensive free speech rankings (https://www.transparency.org/en/cpi/2023)

cpi_2023 <- data.frame(
  country_name = c(
    "Denmark",
    "Finland",
    "New Zealand",
    "Norway",
    "Singapore",
    "Sweden",
    "Switzerland",
    "Netherlands",
    "Germany",
    "Luxembourg",
    "Ireland",
    "Canada",
    "Estonia",
    "Australia",
    "Hong Kong",
    "Belgium",
    "Japan",
    "Uruguay",
    "Iceland",
    "Austria",
    "France",
    "Seychelles",
    "United Kingdom",
    "Barbados",
    "United States",
    "Bhutan",
    "United Arab Emirates",
    "Taiwan",
    "Chile",
    "Bahamas",
    "Cabo Verde",
    "Korea, South",
    "Israel",
    "Lithuania",
    "Portugal",
    "Latvia",
    "Saint Vincent and the Grenadines",
    "Spain",
    "Botswana",
    "Qatar",
    "Czechia",
    "Dominica",
    "Italy",
    "Slovenia",
    "Costa Rica",
    "Saint Lucia",
    "Poland",
    "Slovakia",
    "Cyprus",
    "Georgia",
    "Grenada",
    "Rwanda",
    "Fiji",
    "Saudi Arabia",
    "Malta",
    "Mauritius",
    "Croatia",
    "Malaysia",
    "Greece",
    "Namibia",
    "Vanuatu",
    "Armenia",
    "Jordan",
    "Kuwait",
    "Montenegro",
    "Romania",
    "Bulgaria",
    "Sao Tome and Principe",
    "Jamaica",
    "Benin",
    "Ghana",
    "Oman",
    "Senegal",
    "Solomon Islands",
    "Timor-Leste",
    "Bahrain",
    "China",
    "Cuba",
    "Hungary",
    "Moldova",
    "North Macedonia",
    "Trinidad and Tobago",
    "Burkina Faso",
    "Kosovo",
    "South Africa",
    "Vietnam",
    "Colombia",
    "Côte d’Ivoire",
    "Guyana",
    "Suriname",
    "Tanzania",
    "Tunisia",
    "India",
    "Kazakhstan",
    "Lesotho",
    "Maldives",
    "Morocco",
    "Argentina",
    "Albania",
    "Belarus",
    "Ethiopia",
    "Gambia",
    "Zambia",
    "Algeria",
    "Brazil",
    "Serbia",
    "Ukraine",
    "Bosnia and Herzegovina",
    "Dominican Republic",
    "Egypt",
    "Nepal",
    "Panama",
    "Sierra Leone",
    "Thailand",
    "Ecuador",
    "Indonesia",
    "Malawi",
    "Philippines",
    "Sri Lanka",
    "Turkey",
    "Angola",
    "Mongolia",
    "Peru",
    "Uzbekistan",
    "Niger",
    "El Salvador",
    "Kenya",
    "Mexico",
    "Togo",
    "Djibouti",
    "Eswatini",
    "Mauritania",
    "Bolivia",
    "Pakistan",
    "Papua New Guinea",
    "Gabon",
    "Laos",
    "Mali",
    "Paraguay",
    "Cameroon",
    "Guinea",
    "Kyrgyzstan",
    "Russia",
    "Uganda",
    "Liberia",
    "Madagascar",
    "Mozambique",
    "Nigeria",
    "Bangladesh",
    "Central African Republic",
    "Iran",
    "Lebanon",
    "Zimbabwe",
    "Azerbaijan",
    "Guatemala",
    "Honduras",
    "Iraq",
    "Cambodia",
    "Congo",
    "Guinea-Bissau",
    "Eritrea",
    "Afghanistan",
    "Burundi",
    "Chad",
    "Comoros",
    "Democratic Republic of the Congo",
    "Myanmar",
    "Sudan",
    "Tajikistan",
    "Libya",
    "Turkmenistan",
    "Equatorial Guinea",
    "Haiti",
    "Korea, North",
    "Nicaragua",
    "Yemen",
    "South Sudan",
    "Syria",
    "Venezuela",
    "Somalia"
  ),
  score = c(
    90,
    87,
    85,
    84,
    83,
    82,
    82,
    79,
    78,
    78,
    77,
    76,
    76,
    75,
    75,
    73,
    73,
    73,
    72,
    71,
    71,
    71,
    71,
    69,
    69,
    68,
    68,
    67,
    66,
    64,
    64,
    63,
    62,
    61,
    61,
    60,
    60,
    60,
    59,
    58,
    57,
    56,
    56,
    56,
    55,
    55,
    54,
    54,
    53,
    53,
    53,
    53,
    52,
    52,
    51,
    51,
    50,
    50,
    49,
    49,
    48,
    47,
    46,
    46,
    46,
    46,
    45,
    45,
    44,
    43,
    43,
    43,
    43,
    43,
    43,
    42,
    42,
    42,
    42,
    42,
    42,
    42,
    41,
    41,
    41,
    41,
    40,
    40,
    40,
    40,
    40,
    40,
    39,
    39,
    39,
    39,
    38,
    37,
    37,
    37,
    37,
    37,
    37,
    36,
    36,
    36,
    36,
    35,
    35,
    35,
    35,
    35,
    35,
    35,
    34,
    34,
    34,
    34,
    34,
    34,
    33,
    33,
    33,
    33,
    32,
    31,
    31,
    31,
    31,
    30,
    30,
    30,
    29,
    29,
    29,
    28,
    28,
    28,
    28,
    27,
    26,
    26,
    26,
    26,
    25,
    25,
    25,
    25,
    24,
    24,
    24,
    24,
    24,
    23,
    23,
    23,
    23,
    22,
    22,
    22,
    21,
    20,
    20,
    20,
    20,
    20,
    20,
    20,
    20,
    18,
    18,
    17,
    17,
    17,
    17,
    16,
    13,
    13,
    13,
    11
  ),
  stringsAsFactors = FALSE
)

clean$qual_score <- case_when(
  clean$score < 33 ~ 1,
  clean$score < 66 ~ 2,
  clean$score <= 100 ~ 3
)

# Merging Data Set: ----
library(dplyr)
library(tidyr)
library(janitor)

##Quantitative Data: ----
### Isolating 2023 Data ----
Pop_2023 <- Pop |>
  select(`Country Name`, `Country Code`, `2023`) |>
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    population = `2023`
  )
rm(Pop)

GDP_2023 <- GDP |>
  select(`Country Name`, `Country Code`, `2023`) |>
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    gdp = `2023`
  )
rm(GDP)

E_2023 <- E |>
  select(`Country Name`, `Country Code`, `2023`) |>
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    electricity_oil_pct = `2023`
  )
rm(E)  

NT_2023 <- NT |>
  select(`Country Name`, `Country Code`, `2023`) |>
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    trade = `2023`
  )
rm(NT)

### Full Merge ----

qual_merged_2023 <- Pop_2023 |>
  left_join(GDP_2023, by = c("country_name", "country_code")) |>
  left_join(NT_2023, by = c("country_name", "country_code")) |>
  left_join(E_2023, by = c("country_name", "country_code")) |>
  left_join(cpi_2023, by = c("country_name"))

clean <- qual_merged_2023 |>
  drop_na()
clean <- clean |> 
  drop_na()
clean <- clean %>%
  filter(!is.na(agriculture_pct) & 
           !is.na(services_pct) & 
           !is.na(industry_pct))

