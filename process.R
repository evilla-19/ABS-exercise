######################################
###### load libraries ################
######################################

library(dplyr)
library(jsonlite)
library(tidyjson)
library(tidyr)
library(forecast)
library(ggplot2)


######################################
###### read in data ##################
######################################


json = read_json('ABS_data.json')

######################################
###### data wrangling ################
######################################



## Transform json into tidy data.frame, extract relevant information ##

jsonTabular = 
json %>% as.tbl_json %>%                                # conver to tibble
enter_object('dataSets') %>%                            # enter dataSets object to dive into data
gather_array %>%                                        # enter array
enter_object('observations') %>%                        # enter observations
gather_keys() %>%                                       # get they keys, which will be decoded  
gather_array %>%                                        # enter the array
append_values_number('array_values') %>%                # get the array value data
data.frame()                                            # convert into df



keys = paste0('key', 1:8)                               # prepare keys

jsonTabularExpanded = 
jsonTabular %>% separate(key, keys, ':')                # separate each of the 8 keys into a column

jsonTabularFiltered = 
jsonTabularExpanded %>% select(-c('key1', 'key2',
'key3', 'key5', 'key7')) %>%                            # get rid of non-informative keys
filter(array.index == 1)                                # keep only index 1, corresponding to number of dwelling units


jsonTabularFiltered = 
jsonTabularFiltered %>% rename(Number.of.new.dwelling.units = array_values)     # rename main data column


## Lookup tables that will be used to map indices for each observation onto meaningful variables (untangle SDMX format) ##

generalLUT = json %>% as.tbl_json %>%
enter_object('structure') %>% 
enter_object('dimensions') %>% 
enter_object('observation') %>%                         # navigate to 'observation level'
gather_array %>%                                        # Dive into each array element
spread_values(keyPosition = jstring('keyPosition'),     # Keep keyPosition and name information
name = jstring('name')) %>% 
enter_object('values') %>%                              # enter values array to get the encoding for keys 4, 6, and 8 (building type, region and time period)
gather_array %>%                                        # Dive into each array element
spread_values(array_value = jstring('name')) %>%        # Keep meaningful name for every index position within array 
mutate(array_key = array.index - 1)                     # Add a column for 0-based indexing of array keys

## individual LUTS for each informative variable  ##

buildingTypeLUT = generalLUT %>% filter(keyPosition == 3) %>% select(c(array_key, array_value)) 
regionLUT = generalLUT %>% filter(keyPosition == 5) %>% select(c(array_key, array_value)) 
timestampLUT = generalLUT %>% filter(name == 'Time') %>% select(c(array_key, array_value)) 

## convert to named vectors that can be used with dplyr's function 'recode'  ##

buildingTypeLUT = setNames(buildingTypeLUT$array_value, buildingTypeLUT$array_key)
regionLUT = setNames(regionLUT$array_value, regionLUT$array_key)
timestampLUT = setNames(timestampLUT$array_value, timestampLUT$array_key)

## Construct final tabular object  ##

jsonTabularAnnotated = 
jsonTabularFiltered %>% mutate(building_type = recode(key4, !!!buildingTypeLUT)) %>% # buildingType LUT
mutate(region = recode(key6, !!!regionLUT)) %>%                                      # region LUT
mutate(timestamp = recode(key8, !!!timestampLUT)) %>%                                # time LUT
mutate(timestamp = as.Date(paste('01', timestamp, sep = '-'), format = '%d-%b-%Y'))  # convert timestamp to date

jsonTabularAnnotated = 
jsonTabularAnnotated %>% select(-c(document.id, array.index, key4, key6, key8))     # get rid of non-informative columns

jsonTabularAnnotated = 
jsonTabularAnnotated %>% select(c(timestamp, building_type, 
region, Number.of.new.dwelling.units)) %>%                                          # select final columns in correct order
arrange(timestamp)                                                                  # arrange by timestamp



## Sanity Check: get new dwelling units in New South Wales in July 2011,check that it matches expected 1511.0 ##

jsonTabularAnnotated %>% filter(region == 'New South Wales') %>% 
filter(timestamp == '2011-07-01', building_type == 'Houses') 


########################################################
######### Preparing data for forecasting    ############
########################################################

aggrPerRegion = 
jsonTabularAnnotated %>% 
group_by(timestamp, region) %>% 
summarise(total = sum(Number.of.new.dwelling.units))


aggrNSW = aggrPerRegion %>% filter(region == 'New South Wales')

## Sanity check to see if the first value is what was exepcted, i.e. 8758: ##

jsonTabularAnnotated %>% 
filter(region == 'New South Wales') %>% 
filter(timestamp == '2011-07-01') %>%
summarise(total = sum(Number.of.new.dwelling.units))


## convert into time series ##

tsdataNSW = ts(aggrNSW[,'total'], start = c(2011, 7), end = c(2017, 7),  frequency = 12)


## Visualize data ##

autoplot(tsdataNSW) + ylab('Number of new dwellings in New South Wales')    # standard autoplot from forecast package

ggplot(aggrNSW, aes(timestamp, total)) +                                    # same result with ggplot
geom_line() + 
scale_x_date('month') + 
ylab('Number of new dwellings') + 
xlab('')


ggseasonplot(tsdataNSW, polar = FALSE)                                      # seasonal plot

## Decompose individual components ##

decompose(tsdataNSW) %>% autoplot()

###########################################
######## Forecasting!       ###############
###########################################

## Split dataset into train and test, take all but last year as train ##
train = subset(tsdataNSW, end = length(tsdataNSW) - 12)


## Fit an ETS model ##
ets = train %>% ets()                               # fit model
checkresiduals(ets)
summary(ets)                                        # check residuals   
fcETS = forecast(ets, h = 12)                       # forecast last 12 months of data, compare to 
accuracy(fcETS, tsdataNSW)                          # check accuracy based on RSME on train and test set, looks pretty bad
autoplot(tsdataNSW, series = 'orig data') +         # visualize overlap of forecast with original data aes(alpha = 0.5)     
autolayer(fcETS, series = 'ETS forecast', alpha = 0.3) 


## Define parameter space ##

paramSpace = expand.grid(p = 0:1, d = 0:1, q = 0:1, P = 0:1, D = 0:1, Q = 0:1)

## Receiver containers for the output of the model ##

aiccs = list()
rsme = list()

scanParam = function(){
    for (i in 1:nrow(paramSpace)){
        print(i)
        tryCatch({    
            fit = Arima(train, order=c(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i]), 
            seasonal = list(order = c(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i]), period = 12),
            method = 'ML', include.constant = TRUE)
            fcOnTestSet = forecast(fit, h = 12)
            acc = accuracy(fcOnTestSet, tsdataNSW)    
                }, error=function(e) {cat('error calculating ARIMA model, skipping...')})
        aiccs[[i]] = fit$aicc
        rsme[[i]] = acc[4]                                                                  # 4 is the position of the test set RSME
        cat(
            'model parameters: ',
            '(', 
            paste(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i], sep = ','),
            ')',
            '(P,D,Q)',
            '(',
            paste(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i],sep = ','),
             ')',
            ' [12]',
            paste('aicc is: ', fit$aicc),
            paste('RSME is: ',acc[[4]])
            )
        }
    return(list(aiccs, rsme))
}

scanningOutput = scanParam()


## Select the best model based on minimum AICC ##

minAICc = grep(min(unlist(scanningOutput[[1]])), scanningOutput[[1]])       #scanningOutput[[1]] contains AICc values

print(paramSpace[minAICc,])


## Fit that model and make the prediction on test set ##


fit = Arima(train, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12),
            method = 'ML', include.constant = TRUE)
fcARIMAOnTestSet = forecast(fit, h = 12)
acc = accuracy(fcARIMAOnTestSet, tsdataNSW)    


## Plot the prediction and the real data for the last 12 months ##

autoplot(tsdataNSW, series = 'orig data') +                         # visualize overlap of forecast with original data aes(alpha = 0.5)              
autolayer(fcARIMAOnTestSet, series = 'ARIMA forecast', 
alpha = 0.3) 

## Now produce the forecast for the 3 years as requested ##

fit = Arima(tsdataNSW, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12),
            method = 'ML', include.constant = TRUE)
fcARIMA = forecast(fit, h = 36)


## Check residuals ##

checkresiduals(fit)

## Plot the prediction ## 

autoplot(fcARIMA) + ylab('Number of new dwellings in New South Wales')

## Plot the differenced dataset to make sure that it's looking stationary ##

autoplot(diff(tsdataNSW))
ggAcf(diff(tsdataNSW))

