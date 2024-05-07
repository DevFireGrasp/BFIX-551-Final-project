require(tidycensus) 

# Set your census API key
census_api_key("f55a180f51955b1f67d3a703d629105d8f24eb71")

# Fetch and prepare the data outside of the Shiny server function
Variables_ALL <- load_variables(2020, "acs5")

get_data <- function(year){
  acs_data <- get_acs(
    geography = "state", #geographic level of data to retrieve(so in this case state level)
    variables = list(    #Specifying the variables to retrieve from ACS
      median_age = "B01002_001",           #Variable for median age
      median_earnings = "B08121_001",      #Variable for median earnings
      population_n = "B01003_001"           #Variable for Population
    ),
    year = year,     #the year of ACS data to retrieve
    geometry = TRUE  #geographic boundaries 
  ) |> 
    #Mutate and manipulate the data
    #Added a new column called Variable to the dataset (acs_data) to conditionally assign values 
    mutate(Variable = case_when(variable == "median_age" ~ "Median Age",
                                variable == "population_n" ~ "Population",
                                variable == "median_earnings" ~ "Median Earnings")) |> 
    
    #columns to exclude
    dplyr::select(-moe, -GEOID, -variable) |> 
    #transforming the dataset from a long format to a wide format
    pivot_wider(names_from = Variable, values_from = estimate) |> 
    #filtering data the continental US states only
    filter(!NAME %in% c("Alaska", "Puerto Rico", "Hawaii"))
  
  
  state_df <- data.frame(NAME=state.name,state_abv=state.abb)
  acs_data <- acs_data |>
    left_join(state_df,by="NAME") 
  return (acs_data)
}

all_data <- list()
for (year in 2018:2022){
  all_data[[as.character(year)]] <- get_data(year)
}

save(all_data, file='acs_data.Rdata')

