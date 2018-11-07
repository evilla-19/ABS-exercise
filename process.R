######################################
###### pre-requisites ################
######################################

require(dplyr)
require(jsonlite)
require(tidyjson)
require(tidyr)
require(forecast)
require(ggplot2)


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
append_values_number('array_values') %>%                # get the array value data
data.frame()                                            # convert into df



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
enter_object('values') %>%                              # enter values array to get the encoding for keys 4, 6, and 8 (building type, region and time period)
gather_array %>%                                        # Dive into each array element
spread_values(array_value = jstring('name')) %>%        # Keep meaningful name for every index position within array 
mutate(array_key = array.index - 1)                     # Add a column for 0-based indexing of array keys

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
filter(timestamp == '2011-07-01', building_type == 'Houses') 


########################################################
######### Preparing data for forecasting    ############
########################################################

aggrPerRegion = 
jsonTabularAnnotated %>% 
group_by(timestamp, region) %>% 
summarise(total = sum(Number.of.new.dwelling.units))


# housesNSW = 
# jsonTabularAnnotated %>% 
# filter(region == 'New South Wales') %>% 
# filter(building_type == 'Houses')



# head(jsonTabularAnnotated) %>% )
# jsonTabularAnnotated %>% group_by(region) %>% summarize(total_new = sum(Number.of.new.dwelling.units))

aggrNSW = aggrPerRegion %>% filter(region == 'New South Wales')

## Sanity check to see if the first value is what was exepcted, i.e. 8758:

jsonTabularAnnotated %>% 
filter(region == 'New South Wales') %>% 
filter(timestamp == '2011-07-01') %>%
summarise(total = sum(Number.of.new.dwelling.units))


## convert into time series

tsdataNSW = ts(aggrNSW[,'total'], start = c(2011, 7), end = c(2017, 7),  frequency = 12)


# Visualize data

autoplot(tsdataNSW) 

ggplot(aggrNSW, aes(timestamp, total)) + 
geom_line() + 
scale_x_date('month') + 
ylab('Number of new dwellings') + 
xlab('')




ggseasonplot(tsdataNSW, polar = FALSE)
ggsubseriesplot(tsdataNSW)

decompose(tsdataNSW) %>% autoplot()





ets = tsdataNSW %>% ets() %>% forecast()
checkresiduals(ets)
tsdataNSW %>% ets() %>% forecast() %>% autoplot()


hw = tsdataNSW %>% hw(seasonal = 'additive', h = 36, damped = FALSE)
checkresiduals(hw)
autoplot(hw)

naive = tsdataNSW %>% snaive( h = 36) 
autoplot(naive)

tsdataNSW %>% ses(h = 36) %>% autoplot()


tsdataNSW %>% ets() %>% forecast(h = 36) %>% autoplot()

autoplot(tsdataNSW)
autoplot(diff(diff(log(tsdataNSW), lag = 12)))
ggAcf(diff(diff(log(tsdataNSW), lag = 12)))



fit = auto.arima(tsdataNSW)
summary(fit)
autoplot(forecast(fit, h = 36))
# auto.arima(diff(diff(log(tsdataNSW), lag = 12))) %>% forecast(h = 36) %>% autoplot()

 ## best model so far by visual inspection!!
Arima(tsdataNSW, order = c(40,1,1), include.constant = TRUE) %>% forecast(h= 36) %>% autoplot() ## best model so far by visual inspection!!

auto.arima(tsdataNSW, lambda = 0) %>% forecast(h = 6) %>% autoplot()

autoplot(tsdataNSW)

library(fpp2)
data(debitcards)
autoplot(debitcards)
fit = auto.arima(debitcards, lambda = 0) 
fit %>% forecast(h = 36) %>% aut4plot()
# log-transformation

# ts = ts(jsonTabularAnnotated %>% select(c(timestamp, Number.of.new.dwelling.units)))
auto.arima(tsdataNSW, lambda = 0, stepwise = FALSE) %>% forecast(h = 18) %>% autoplot()


meanf(tsdataNSW, h = 6) %>% autoplot()

autoplot(tsdataNSW)

tsdataNSW %>% nnetar() %>% forecast(PI = TRUE, h = 36) %>% autoplot()

aiccs <- list()
count = 1
for(i in 0:1){
  for(j in 0:1){
      for(w in 0:1){
          for(z in 0:1){
              for(m in 0:1){
                  for(l in 0:1){
                      try(
                          {
                            fit <- Arima(tsdataNSW, order=c(i,j,w), seasonal = list(order = c(z,m,l), period = 12), method = 'ML');
                            # acc <- accuracy(fit)
                            aiccs[[count]] <- fit$aicc # Number 5 indicates the position of MAPE in the accuracy list
                          }
                      )
                        paramCombination = paste0('(p,d,q)', '(', paste(i,j,w, sep = ','), ')', '(P,D,Q)', '(',paste(z,m,l, sep = ','), ')', '(12)', ' - ', fit$aicc)
                        names(aiccs[[count]]) = paramCombination
                    print(count)
                    print(paste0('(p,d,q)', '(', paste(i,j,w, sep = ','), ')', '(P,D,Q)', '(',paste(z,m,l, sep = ','), ')', '(12)', ' - ', fit$aicc))
                    count = count + 1
}}}}}}

minAICc = min(unlist(aiccs))
indexMinAICc = which(x == minAICc)
x[[28]]



### Cross validation

trainNSW = subset(tsdataNSW, end = length(tsdataNSW)-12)

trainNSW %>% Arima(order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12), method = 'ML') %>% forecast(h = 12) %>% autoplot()

testNSW  = subset(tsdataNSW, start = length(tsdataNSW)-11)

fit = trainNSW %>% Arima(order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12), method = 'ML') %>% forecast(h = 12)



autoplot(tsdataNSW) + autolayer(fit) + autolayer(fitted(fit))

autoplot(tsdataNSW)
autoplot(fit)


?Arima
str(Arima(tsdataNSW, order = c(1,1,1)))

accuracy(fit)[2]







library(fpp2)
data(debitcards)
autoplot(debitcards)
fit = auto.arima(debitcards, lambda = 0) 
fit %>% forecast(h = 36) %>% autoplot()
# log-transformation


# mf = meanf(ts[,1],h=12,level=c(90,95),fan=FALSE,lambda=NULL)
# plot(mf) 
# accuracy(mf)

# plot(ts(jsonTabularAnnotated %>% select(c(timestamp, Number.of.new.dwelling.units))), type = 'o', col = 'red')

# ts()

## can't really use training-test set, so will use tsCV, check the MSE and compare ETS to auto arima to see which gives the smallest error
