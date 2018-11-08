

hw = tsdataNSW %>% hw(seasonal = 'additive', h = 36, damped = FALSE)
checkresiduals(hw)
autoplot(hw)

naive = tsdataNSW %>% snaive(h = 36) 
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