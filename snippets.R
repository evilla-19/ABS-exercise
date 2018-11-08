

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


## best model so far by visual inspection!!
Arima(tsdataNSW, order = c(40,1,1), include.constant = TRUE) %>% forecast(h= 36) %>% autoplot() ## best model so far by visual inspection!!

auto.arima(tsdataNSW, lambda = 0) %>% forecast(h = 6) %>% autoplot()


autoplot(tsdataNSW)

tsdataNSW %>% diff(lag = 12) %>% ggtsdisplay()

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



X = expand.grid(p = 0:1, d = 0:1, q = 0:1)
for (i in 1:length(X[,1])){
    print(Arima(tsdataNSW, order=c(X$p[i],X$d[i],X$q[i]), seasonal = list(order = c(X$p[i],X$d[i],X$q[i]), period = 12), method = 'ML'))
    }


# for (i in 1:length(paramSpace[,1])){
#     print(paste('(', paste(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i], sep = ','), ')', '(P,D,Q)', '(',paste(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i],sep = ','), ')'))
#     # print(Arima(train, order=c(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i]), seasonal = list(order = c(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i]), period = 12), method = 'ML')$aicc)
#     fit = Arima(train, order=c(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i]), seasonal = list(order = c(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i]), period = 12), method = 'ML')
#     print(fit$aicc)
# }

# out <- apply(paramSpace, MARGIN = 1,
# function(p,d,q) one.generation(N, x[1],x[2],x[3],x[4]))


        # aiccs[[count]] = fit$aicc # Number 5 indicates the position of MAPE in the accuracy list
        # rsme[[count]] = acc[4]
        # paramCombination = paste0('(p,d,q)', '(', paste(paramSpace$p[i],paramSpace$d[i],paramSpace$q[i], sep = ','), ')', '(P,D,Q)', '(',paste(paramSpace$P[i],paramSpace$D[i],paramSpace$Q[i],sep = ','), ')', '[12]', ' - ', 'AICc: ', fit$aicc, ' ', 'RSME: ', acc)
        # print(count)
        # print(paramCombination)
        # names(aiccs[[count]]) = paramCombination
        # count = count + 1


for(i in 0:1){
  for(j in 0:1){
      for(w in 0:1){
          for(z in 0:1){
              for(m in 0:1){
                  for(l in 0:1){
                      try(
                          {
                            fit = Arima(tsdataNSW, order=c(i,j,w), seasonal = list(order = c(z,m,l), period = 12), method = 'ML');
                            # acc = accuracy(fit)
                            aiccs[[count]] = fit$aicc # Number 5 indicates the position of MAPE in the accuracy list
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