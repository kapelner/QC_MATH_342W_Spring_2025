options(java.parameters = "-Xmx8000m")
pacman::p_load(ggplot2, tidyverse, magrittr, YARF, xgboost, data.table, nycflights13, skimr, missForest)

# Problem 1
flights = nycflights13::flights %>%
  mutate_if(sapply(nycflights13::flights, is.character), as.factor)
flights
flights = flights %>% select(-time_hour)
planes = nycflights13::planes %>%
  mutate_if(sapply(nycflights13::planes, is.character), as.factor)

# skim_without_charts(flights)
# skim_without_charts(planes)

planes = planes %>% select(-speed) %>% na.omit

flights = flights %>% 
  filter(!is.na(dep_time)) %>% 
  mutate(tailnum = factor(if_else(is.na(tailnum), "missing", tailnum))) %>% 
  mutate(arr_time = if_else(is.na(arr_time), mean(arr_time, na.rm = TRUE), arr_time)) %>% 
  mutate(arr_delay = if_else(is.na(arr_delay), mean(arr_delay, na.rm = TRUE), arr_delay)) %>% 
  mutate(air_time = if_else(is.na(air_time), mean(air_time, na.rm = TRUE), air_time))

skim_without_charts(flights)
skim_without_charts(planes)

setdiff(planes$tailnum, flights$tailnum)

flights_with_plane_information = na.omit(left_join(flights, planes, by = "tailnum"))
flights_with_plane_information = droplevels(flights_with_plane_information)
n = nrow(flights_with_plane_information)
y = flights_with_plane_information$arr_delay
X = flights_with_plane_information[, c(
  "sched_dep_time",  #in 24hr format (numeric)
  "sched_arr_time",  #in 24hr format (numeric)
  "carrier",         #the airline
  "origin",          #the code of airport
  "dest",            #the code of airport
  "distance",        #in miles
  "model",           #the plane's model
  "year.y"           #the year the plane was constructed
)]
skim_without_charts(X)

ncol(model.matrix(y ~ ., X))
interactions = colnames(model.matrix(y ~ . * ., X))
length(interactions)

set.seed(1)
n_train = 5000
idx_train = sample(1 : n, n_train)
idx_test  = setdiff(1 : n, idx_train)
y_train = y[idx_train]
X_train = X[idx_train, ]
y_test = y[idx_test]
X_test = X[idx_test, ]

round(coef(lm(y_train ~ carrier * sched_dep_time, X_train)), 3)
-19.278 + -8.321 + 0.017 * (1000) + 0.004 * (1000)
-19.278 + 0.017 * (1000)


round(coef(lm(y_train ~ poly(distance, 3), X_train)), 3)
round(coef(lm(y_train ~ poly(distance, 3, raw = TRUE), X_train)), 3)

round(coef(lm(y_train ~ log(distance), X_train)), 3)

ols_mod = lm(y_train ~ ., X_train)
tree_mod = YARFCART(X_train, y_train)
illustrate_trees(tree_mod, max_depth = 4, open_file = TRUE, length_in_px_per_half_split  = 40)

bag_tree_mod = YARFBAG(X_train, y_train, nodesize = 5, num_trees = 2000)
bag_tree_mod$rmse_oob
rf_mod = YARF(X_train, y_train, nodesize = 5, num_trees = 2000)
rf_mod$rmse_oob

Xmm = model.matrix(~ ., X)
boost_mod = xgboost(
  data = Xmm[idx_train, ], 
  label = y_train,
  objective = "reg:squarederror", #for a regression problem
  booster = "gbtree", #shallow trees, default depth = 6
  nrounds = 2000, #num trees (should be cross-validated)
  eta = 0.2, #learning rate (should be cross-validated)
  print_every_n = 100
)

n_test = 500
y_hat_test = predict(boost_mod, Xmm[idx_test, ][1 : n_test, ])
sqrt(mean((y_test[1 : n_test] - y_hat_test)^2)) #oosRMSE calculation

y_train_bin = as.numeric(y_train > 30)
table(y_train_bin)
logistic_mod = glm(y_train_bin ~ . - model, X_train, family = "binomial")
coef(logistic_mod)

exp(3.171) / (1 + exp(3.171))

n_test = 1000
y_test_bin = as.numeric(y_test[1 : n_test] > 30)
table(y_test_bin)
X_test_temp = X_test %>% 
  filter(dest != "EGE" & dest != "JAC") %>% 
  filter(!(model %in% c("172M", "737-5H4", "787-8", "PA-32R-300", "VANS AIRCRAFT RV6")))
X_test_temp = droplevels(X_test_temp)
p_hat_test = predict(logistic_mod, X_test_temp[1 : n_test, ], type = "response")
-mean((p_hat_test - y_test_bin)^2)


compute_metrics_prob_classifier = function(p_hats, y_true, res = 0.001){
  #we first make the grid of all prob thresholds
  p_thresholds = seq(0 + res, 1 - res, by = res) #values of 0 or 1 are trivial
  
  #now we create a matrix which will house all of our results
  performance_metrics = matrix(NA, nrow = length(p_thresholds), ncol = 12)
  colnames(performance_metrics) = c(
    "p_th",
    "TN",
    "FP",
    "FN",
    "TP",
    "miscl_err",
    "precision",
    "recall",
    "FDR",
    "FPR",
    "FOR",
    "miss_rate"
  )
  
  #now we iterate through each p_th and calculate all metrics about the classifier and save
  n = length(y_true)
  for (i in 1 : length(p_thresholds)){
    p_th = p_thresholds[i]
    y_hats = factor(ifelse(p_hats >= p_th, 1, 0))
    confusion_table = table(
      factor(y_true, levels = c(0, 1)),
      factor(y_hats, levels = c(0, 1))
    )
    
    fp = confusion_table[1, 2]
    fn = confusion_table[2, 1]
    tp = confusion_table[2, 2]
    tn = confusion_table[1, 1]
    npp = sum(confusion_table[, 2])
    npn = sum(confusion_table[, 1])
    np = sum(confusion_table[2, ])
    nn = sum(confusion_table[1, ])
    
    performance_metrics[i, ] = c(
      p_th,
      tn,
      fp,
      fn,
      tp,
      (fp + fn) / n,
      tp / npp, #precision
      tp / np,  #recall
      fp / npp, #false discovery rate (FDR)
      fp / nn,  #false positive rate (FPR)
      fn / npn, #false omission rate (FOR)
      fn / np   #miss rate
    )
  }
  
  #finally return the matrix
  performance_metrics
}


perf = data.table(compute_metrics_prob_classifier(p_hat_test, y_test_bin))
perf

ggplot(perf) +
  geom_line(aes(x = FPR, y = recall), lwd = 2) +
  geom_abline(intercept = 0, slope = 1) + 
  coord_fixed() + xlim(0, 1) + ylim(0, 1)


table(y_test_bin, as.numeric(p_hat_test > 0.05))
table(y_test_bin, as.numeric(p_hat_test > 0.1))
table(y_test_bin, as.numeric(p_hat_test > 0.2))
table(y_test_bin, as.numeric(p_hat_test > 0.3))
table(y_test_bin, as.numeric(p_hat_test > 0.4))
