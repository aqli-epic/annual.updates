# Central Africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon",
                               "Central African Republic", "Chad",
                               "Republic of the Congo",
                               "Democratic Republic of the Congo",
                               "Equatorial Guinea", "Gabon",
                               "São Tomé and Príncipe", "Rwanda")
# West Africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", "Gambia", 
                            "Ghana", "Guinea", "Guinea-Bissau", "Côte d'Ivoire", 
                            "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", 
                            "Senegal", "Sierra Leone", "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# South East Asia definition
se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", 
                 "Laos", "Malaysia", "Philippines", "Singapore", "Thailand", 
                 "Vietnam")

# Indo-gangetic plains states
indo_gangetic_plains_states <- c("NCT of Delhi", "Uttar Pradesh", "Bihar", "Haryana",
                                 "Punjab", "Chandigarh", "West Bengal")

# European countries
european_countries <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/europe_countries.csv")

# Western European countries
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco", 
                                "Luxembourg", "Belgium", "France", "Netherlands", 
                                "Andorra", "Spain", "United Kingdom", "Portugal", 
                                "Denmark", "Ireland", "Iceland", "Austria",
                                "Liechtenstein", "San Marino")


# European Union countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                  "Czechia", "Denmark", "Estonia", "Finland", "France", 
                  "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                  "Latvia", "Lithuania", "Luxembourg", "Malta", 
                  "Netherlands",  "Poland", "Portugal", "Romania", 
                  "Slovakia", "Slovenia", "Spain", "Sweden")

# South Asia definition
south_asia_def <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", 
                    "Nepal", "Pakistan", "Sri Lanka")

# Latin America definition
latin_america_countries_vec <- c("México", "Guatemala", "Honduras", "El Salvador", 
                                 "Nicaragua", "Costa Rica", "Panama", "Colombia", 
                                 "Venezuela", "Ecuador", "Peru", "Bolivia", 
                                 "Brazil", "Paraguay", "Chile", "Argentina", 
                                 "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# Middle East definition
mid_east_countries <- c("Bahrain", "Iran", "Iraq", "Israel", "Jordan", 
                        "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", 
                        "Syria", "United Arab Emirates", "Yemen")

# North Africa
north_africa_countries <- c("Algeria", "Djibouti", "Egypt", "Libya", "Morocco", "Tunisia")

# middle east and north africa
mena_countries <- c(mid_east_countries, north_africa_countries)
