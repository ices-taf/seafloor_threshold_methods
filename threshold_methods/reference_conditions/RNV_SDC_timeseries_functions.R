### functions for RNV and SDC using reference state timeseries data

# range of natural variation threshold
RNV_timeseries = function(data, state, year){
  log.state = log(data[[state]])
  year = data[[year]]
  log.residuals = resid(lm(log.state~year))
  
  threshold = qlnorm(p=0.15, mean = log(1), sd = sd(log.residuals))
}

# statistically detectable change threshold
SDC_timeseries = function(data, state, year){
  log.state = log(data[[state]])
  year = data[[year]]
  log.residuals = resid(lm(log.state~year))
  
  n = length(log.residuals)
  se = (sd(log.residuals))/sqrt(n)
  t.score = qt(p= 0.05/2, df=(n-1), lower.tail=F)
  margin = t.score * se
  
  threshold = exp(mean(log.residuals) - margin)
}

# test
data1 = data.frame(biomass = rnorm(n = 10, mean = 50, sd = 30), year = seq(2000, 2009, by = 1))
RNV_output = RNV_timeseries(data = data1, state = 'biomass', year = 'year')
SDC_output = SDC_timeseries(data = data1, state = 'biomass', year = 'year')

data2 = data.frame(density = rnorm(n = 15, mean = 1000, sd = 400), Year = seq(2000, 2014, by = 1))
RNV_output = RNV_timeseries(data = data2, state = 'density', year = 'Year')
SDC_output = SDC_timeseries(data = data2, state = 'density', year = 'Year')
