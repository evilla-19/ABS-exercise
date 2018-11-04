## ABS exercise

Main purpose: given housing data from the Australian Bureau of Statistics, transform SDMX JSON into timestamp-indexed tabular format, then perform modeling to come up with a 3-year forecast. Subtasks as outlined below.

### Task 1: transform JSON into tabular

Data was read into R using [tidyjson]()

```
json = read_json('ABS_data.json')

```

### Task 2: extract time series for New South Wales

### Task 3: 3-year forecast

#### Using ARIMA models