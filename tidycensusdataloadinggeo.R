# acs_data2018 <- get_acs(
#   geography = "state", #geographic level of data to retrieve(so in this case state level)
#   variables = list(    #Specifying the variables to retrieve from ACS
#     median_age = "B01002_001",           #Variable for median age
#     median_earnings = "B20002_001",      #Variable for median earnings
#     cost_of_living_index = "B19013_001"  #Variable for cost of living
#   ),
#   year = 2018,     #the year of ACS data to retrieve
#   geometry = TRUE  #geographic boundaries 
# ) |> 
#   
#   #Mutate and manipulate the data
#   #Added a new column called Variable to the dataset (acs_data) to conditionally assign values 
#   mutate(Variable = case_when(variable == "median_age" ~ "Median Age",
#                               variable == "cost_of_living_index" ~ "Cost of Living Index",
#                               variable == "median_earnings" ~ "Median Earnings")) |> 
#   #columns to exclude
#   dplyr::select(-moe, -GEOID, -variable) |> 
#   #transforming the dataset from a long format to a wide format
#   pivot_wider(names_from = Variable, values_from = estimate) |> 
#   #filtering data the continental US states only
#   filter(!NAME %in% c("Alaska", "Puerto Rico", "Hawaii"))|>
#   mutate(year=2018)
#   
# asc_data2020

acs_data<-rbind(acs_data2022, acs_data2021, acs_data2020,acs_data2019,acs_data2018)


#write_csv(acs_data, "Acsdata.csv")
write_sf(acs_data,"acs_data.geojson")
acs_data<-read_sf("acs_data.geojson")



# Set your census API key
census_api_key("f55a180f51955b1f67d3a703d629105d8f24eb71")


acs_data2018 <- get_acs(
  geography = "state", #geographic level of data to retrieve(so in this case state level)
  variables = list(    #Specifying the variables to retrieve from ACS
        cost_of_living_index = "B19013_001"  #Variable for cost of living
  ),
  year = 2018,     #the year of ACS data to retrieve
  geometry = TRUE  #geographic boundaries 
  



