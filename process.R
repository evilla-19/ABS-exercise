######################################
###### pre-requisites ################
######################################

devtools::install_github("sailthru/tidyjson")


require(dplyr)
require(jsonlite)
require(tidyjson)
require(tidyr)

######################################
###### read in data ##################
######################################


json = read_json('ABS_data.json')

######################################
###### data wrangling ################
######################################



#### Transform json into tidy data.frame, extract relevant information

jsonTabular = 
json %>% as.tbl_json %>%                                # conver to tibble
enter_object('dataSets') %>%                            # enter dataSets object to dive into data
gather_array %>%                                        # enter array
enter_object('observations') %>%                        # enter observations
gather_keys() %>%                                       # get they keys, which will be decoded  
gather_array %>%                                        # enter the array
append_values_number('array_values')                    # get the array value data
%>% data.frame()                                        # convert into df



keys = paste0('key', 1:8)                               # prepare keys

jsonTabularExpanded = 
jsonTabular %>% separate(key, keys, ':')                # separate each of the 8 keys into a column

jsonTabularFiltered = 
jsonTabularExpanded %>% select(-c('key1', 'key2', 'key3', 'key5', 'key7')) %>%  # get rid of non-informative keys
filter(array.index == 1)                                                        # keep only index 1, corresponding to number of dwelling units

# head(jsonTabularFiltered, 10) # sanity check

jsonTabularFiltered = 
jsonTabularFiltered %>% rename(Number.of.new.dwelling.units = array_values)     # rename main data column


#### Lookup tables that will be used to map indices for each observation onto meaningful variables (untangle SDMX format)

generalLUT = json %>% as.tbl_json %>%
enter_object('structure') %>% 
enter_object('dimensions') %>% 
enter_object('observation') %>%                         # navigate to 'observation level'
gather_array %>%                                        # Dive into each array element
spread_values(keyPosition = jstring('keyPosition'), name = jstring('name')) %>% # Keep keyPosition and name information
enter_object('values') %>% # enter values array to get the encoding for keys 4, 6, and 8 (building type, region and time period)
gather_array %>% # Dive into each array element
spread_values(array_value = jstring('name')) %>% # Keep meaningful name for every index position within array 
mutate(array_key = array.index - 1) # Add a column for 0-based indexing of array keys

##### individual LUTS for each informative variable

###############
## TO-DO: filter all on name instead of keyPosition? ##
###############

buildingTypeLUT = generalLUT %>% filter(keyPosition == 3) %>% select(c(array_key, array_value)) 
regionLUT = generalLUT %>% filter(keyPosition == 5) %>% select(c(array_key, array_value)) 
timestampLUT = generalLUT %>% filter(name == 'Time') %>% select(c(array_key, array_value)) 

#### convert to named vectors that can be used with dplyr's function 'recode'

buildingTypeLUT = setNames(buildingTypeLUT$array_value, buildingTypeLUT$array_key)
regionLUT = setNames(regionLUT$array_value, regionLUT$array_key)
timestampLUT = setNames(timestampLUT$array_value, timestampLUT$array_key)

#### Construct final tabular object

jsonTabularAnnotated = 
jsonTabularFiltered %>% mutate(building_type = recode(key4, !!!buildingTypeLUT)) %>% # buildingType LUT
mutate(region = recode(key6, !!!regionLUT)) %>%                                      # region LUT
mutate(timestamp = recode(key8, !!!timestampLUT)) %>%                                # time LUT
mutate(timestamp = as.Date(paste('01', timestamp, sep = '-'), format = '%d-%b-%Y'))  # convert timestamp to date

jsonTabularAnnotated = 
jsonTabularAnnotated %>% select(-c(document.id, array.index, key4, key6, key8))     # get rid of non-informative columns

jsonTabularAnnotated = 
jsonTabularAnnotated %>% select(c(timestamp, building_type, region, Number.of.new.dwelling.units)) %>% # select final columns in correct order
arrange(timestamp) # arrange by timestamp



#### Sanity Check: get new dwelling units in New South Wales in July 2011,check that it matches expected 1511.0

jsonTabularAnnotated %>% filter(region == 'New South Wales') %>% 
filter(timestamp == '2011-07-01') 


#####################################
######### Forecasting    ############
#####################################


install.packages('forecast')
require(forecast)


ts = ts(jsonTabularAnnotated %>% select(c(timestamp, Number.of.new.dwelling.units)))

mf = meanf(ts[,1],h=12,level=c(90,95),fan=FALSE,lambda=NULL)
plot(mf) 
accuracy(mf)

plot(ts(jsonTabularAnnotated %>% select(c(timestamp, Number.of.new.dwelling.units))), type = 'o', col = 'red')

ts()