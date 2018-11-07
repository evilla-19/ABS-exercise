## ABS exercise

Main purpose: given housing data from the Australian Bureau of Statistics, transform SDMX JSON into timestamp-indexed tabular format, then perform modeling to come up with a 3-year forecast. Subtasks as outlined below.

## Requirements

I used packages dplyr, jsonlite, tidyjson, tidyr, forecast, and ggplot
tidyjson requires installation through 


    devtools::install_github("sailthru/tidyjson")


### Task 1: transform JSON into tabular

Data was read into R using [tidyjson](), which I was not familiar with before, but seems to allow very neat navigation and extraction from json files.


    json = read_json('ABS_data.json')


and then data wrangling was done to get data into a tabular format. In the code below, data are transformed to a tibble and the json object is navigated to get to the target information in the SDMX-JSON format, namely 'observations'

    jsonTabular = 
    json %>% as.tbl_json %>%                                # conver to tibble
    enter_object('dataSets') %>%                            # enter dataSets object to dive into data
    gather_array %>%                                        # enter array
    enter_object('observations') %>%                        # enter observations
    gather_keys() %>%                                       # get they keys, which will be decoded  
    gather_array %>%                                        # enter the array
    append_values_number('array_values') %>%                # get the array value data
    data.frame()                                            # convert into df

Next I create keys for each of the fields and separate by ':' to get each field separately

    keys = paste0('key', 1:8)                               # prepare keys

    jsonTabularExpanded = 
    jsonTabular %>% separate(key, keys, ':')                # separate each of the 8 keys into a column

Get rid of seemingly non-informative columns, rename main data column to make it more informative:

    jsonTabularFiltered = 
    jsonTabularExpanded %>% select(-c('key1', 'key2', 'key3', 'key5', 'key7')) %>%  # get rid of non-informative keys
    filter(array.index == 1)                                                        # keep only index 1, corresponding to number of dwelling units

    jsonTabularFiltered = 
    jsonTabularFiltered %>% rename(Number.of.new.dwelling.units = array_values)     # rename main 

Next up, I generate a general lookup table from the raw json, similar to what I did before to get to the 'observations' section within the JSON. In this case, I need to get to all the 'keyPositions' and get their names

    generalLUT = json %>% as.tbl_json %>%
    enter_object('structure') %>% 
    enter_object('dimensions') %>% 
    enter_object('observation') %>%                         # navigate to 'observation level'
    gather_array %>%                                        # Dive into each array element
    spread_values(keyPosition = jstring('keyPosition'), name = jstring('name')) %>% # Keep keyPosition and name information
    enter_object('values') %>%                              # enter values array to get the encoding for keys 4, 6, and 8 (building type, region and time period)
    gather_array %>%                                        # Dive into each array element
    spread_values(array_value = jstring('name')) %>% # Keep meaningful name for every index position within array 
    mutate(array_key = array.index - 1)                     # Add a column for 0-based indexing of array keys

I will generate a LUT (lookup table) for each variable I want to keep: building type, region and timestamp

    buildingTypeLUT = generalLUT %>% filter(keyPosition == 3) %>% select(c(array_key, array_value)) 
    regionLUT = generalLUT %>% filter(keyPosition == 5) %>% select(c(array_key, array_value)) 
    timestampLUT = generalLUT %>% filter(name == 'Time') %>% select(c(array_key, array_value)) 

And then map these onto the jsonTabular object, also handling conversin of timestamps to date format:

    jsonTabularAnnotated = 
    jsonTabularFiltered %>% mutate(building_type = recode(key4, !!!buildingTypeLUT)) %>% # buildingType LUT
    mutate(region = recode(key6, !!!regionLUT)) %>%                                      # region LUT
    mutate(timestamp = recode(key8, !!!timestampLUT)) %>%                                # time LUT
    mutate(timestamp = as.Date(paste('01', timestamp, sep = '-'), format = '%d-%b-%Y'))  # convert 

Clean up once more to get the final object to work with:

    jsonTabularAnnotated = 
    jsonTabularAnnotated %>% select(-c(document.id, array.index, key4, key6, key8))     # get rid of non-informative columns

And index by timestamp:

    jsonTabularAnnotated = 
    jsonTabularAnnotated %>% select(c(timestamp, building_type, region, Number.of.new.dwelling.units)) %>% # select final columns in correct order
    arrange(timestamp) # arrange by timestamp


### Task 2: extract time series for New South Wales

First, a short sanity check to see that I can really get the expected number of new Houses in New South Wales for July 2011: 

    jsonTabularAnnotated %>% filter(region == 'New South Wales') %>% 
    filter(timestamp == '2011-07-01', building_type == 'Houses') 

And now up to the task of extracting the time series for New South Wales. Since there are multiple building types, first I want to aggregate all of them in a single variable called 'Total number of new dwellings'

    aggrPerRegion = 
    jsonTabularAnnotated %>% 
    group_by(timestamp, region) %>% 
    summarise(total = sum(Number.of.new.dwelling.units))

And now get the aggregate number of new dwelling for New South Wales (NSW) only:

    aggrNSW = aggrPerRegion %>% filter(region == 'New South Wales')

Sanity check to make sure that values align with an independent extraction:

    jsonTabularAnnotated %>% 
    filter(region == 'New South Wales') %>% 
    filter(timestamp == '2011-07-01') %>%
    summarise(total = sum(Number.of.new.dwelling.units))


### Task 3: 3-year forecast

I will be using the 'forecast' package to have a go at forecasting the time series. 
**Important note**: since I was not familiar with forecasting models before this task, the first thing I did was inform myself to have a guide. I worked through [this datacamp course](https://campus.datacamp.com/courses/forecasting-using-r/) to get acquainted with methods and concepts, which will be applied to the best of my judgement in the following sections.

#### Using ARIMA models