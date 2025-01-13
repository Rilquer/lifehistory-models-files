require(tidyverse)
require(tidymodels)
require(tictoc)
require(parallel)

### Running models ####
#### Setting up ####
cores <- parallel::detectCores()
log <- read_csv('logstats.csv')
rf <- log %>% 
  select(vRep,gt,Ne,
         pi_mean,D_mean,maf_mean,dist_mean,
         pi_h1,pi_h2,pi_h3,pi_h4,pi_hdiff,
         maf_h1,maf_h2,maf_h3,maf_h4,maf_hdiff,
         sfs1,sfs2,sfs3,sfs4,sfs5
  )

reps <- 1000

# Retrieving empirical dataset
emp_stats <- read_csv('emp_stats.csv') 

#### Model for Variance in Reproductive Success ####
message('Model for Variance in Reproductive Success!!!')
message('Tuning recipe...')
rec <-
  recipe(vRep ~ ., data = rf %>% select(-c(gt,Ne)))

message('')
message('Creating split for tuning...')
set.seed(234)
val_set <- initial_validation_split(rf %>% select(-c(gt,Ne)), strata=vRep) %>% validation_set()
message('')

message('Creating model for tuning...')
tune_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")
message('')

message('Creating workflow for tuning...')
tune_workflow <- 
  workflow() %>% 
  add_model(tune_mod) %>% 
  add_recipe(rec)
message('')

message('Running tuning...')
tic()
# Run tuning
tune_res <- 
  tune_workflow %>% 
  tune_grid(resamples = val_set,
            grid = 30,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(yardstick::rsq))
toc()

best <- tune_res %>% 
  collect_metrics() %>%
  arrange(desc(mean)) %>% 
  slice(1)

rf_mod <- 
  rand_forest(mtry = best$mtry, min_n = best$min_n, trees = best$trees) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")
message('')

wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec)

set.seed(NULL)
rsq <- vector('numeric',reps)
var_imp <- vector('list',reps)
emp_predictions <- vector('list',reps)
g <- mclapply(1:reps, function(x){return(initial_validation_split(rf %>% select(-c(gt,Ne)),strata=vRep) %>% validation_set())},mc.cores = cores)
# Then we run
tic()
for (i in 1:reps) {
  message('Rep ',i)
  tic()
  message('Fitting model...')
  fit <-
    wflow %>% 
    last_fit(g[[i]]$splits[[1]])
  
  message('Collecting metrics...')
  rsq[i] <- fit$.metrics[[1]]$.estimate[2]
  emp_predictions[[i]] <- predict(fit$.workflow[[1]],
                                  (emp_stats %>%
                                     select(-name))) %>%
    mutate(name = emp_stats$name)
  toc()
  message('')
  message('')
}
message('Total run time:')
toc()
rsq_vRep <- rsq
var_imp_vRep <- var_imp
emp_pred_vRep <- emp_predictions
message('')
message('')

#### Model for Generation time ####
message('Model for Generation Time!!!')
message('Tuning recipe...')
rec <-
  recipe(gt ~ ., data = rf %>% select(-c(vRep,Ne)))

message('')
message('Creating split for tuning...')
set.seed(234)
val_set <- initial_validation_split(rf %>% select(-c(vRep,Ne)), strata=gt) %>% validation_set()
message('')

message('Creating model for tuning...')
tune_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")
message('')

message('Creating workflow for tuning...')
tune_workflow <- 
  workflow() %>% 
  add_model(tune_mod) %>% 
  add_recipe(rec)
message('')

message('Running tuning...')
tic()
# Run tuning
tune_res <- 
  tune_workflow %>% 
  tune_grid(resamples = val_set,
            grid = 30,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(yardstick::rsq))
toc()

best <- tune_res %>% 
  collect_metrics() %>%
  arrange(desc(mean)) %>% 
  slice(1)

rf_mod <- 
  rand_forest(mtry = best$mtry, min_n = best$min_n, trees = best$trees) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")
message('')

wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec)

set.seed(NULL)
rsq <- vector('numeric',reps)
var_imp <- vector('list',reps)
emp_predictions <- vector('list',reps)
g <- mclapply(1:reps, function(x){return(initial_validation_split(rf %>% select(-c(vRep,Ne)),strata=gt) %>% validation_set())},mc.cores = cores)
# Then we run
tic()
for (i in 1:reps) {
  message('Rep ',i)
  tic()
  message('Fitting model...')
  fit <-
    wflow %>% 
    last_fit(g[[i]]$splits[[1]])
  
  message('Collecting metrics...')
  rsq[i] <- fit$.metrics[[1]]$.estimate[2]
  emp_predictions[[i]] <- predict(fit$.workflow[[1]],
                                  (emp_stats %>%
                                     select(-name))) %>%
    mutate(name = emp_stats$name)
  toc()
  message('')
  message('')
}
message('Total run time:')
toc()
rsq_gt <- rsq
var_imp_gt <- var_imp
emp_pred_gt <- emp_predictions
message('')
message('')

#### Model for Effective population size ####
message('Model for Effective Population Size!!!')
message('Tuning recipe...')
rec <-
  recipe(Ne ~ ., data = rf %>% select(-c(vRep,gt)))

message('')
message('Creating split for tuning...')
set.seed(234)
val_set <- initial_validation_split(rf %>% select(-c(vRep,gt)), strata=Ne) %>% validation_set()
message('')

message('Creating model for tuning...')
tune_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")
message('')

message('Creating workflow for tuning...')
tune_workflow <- 
  workflow() %>% 
  add_model(tune_mod) %>% 
  add_recipe(rec)
message('')

message('Running tuning...')
tic()
# Run tuning
tune_res <- 
  tune_workflow %>% 
  tune_grid(resamples = val_set,
            grid = 30,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(yardstick::rsq))
toc()

best <- tune_res %>% 
  collect_metrics() %>%
  arrange(desc(mean)) %>% 
  slice(1)

rf_mod <- 
  rand_forest(mtry = best$mtry, min_n = best$min_n, trees = best$trees) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")
message('')

wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec)

set.seed(NULL)
rsq <- vector('numeric',reps)
var_imp <- vector('list',reps)
emp_predictions <- vector('list',reps)
g <- mclapply(1:reps, function(x){return(initial_validation_split(rf %>% select(-c(vRep,gt)),strata=Ne) %>% validation_set())},mc.cores = cores)
# Then we run
tic()
for (i in 1:reps) {
  message('Rep ',i)
  tic()
  message('Fitting model...')
  fit <-
    wflow %>% 
    last_fit(g[[i]]$splits[[1]])
  
  message('Collecting metrics...')
  rsq[i] <- fit$.metrics[[1]]$.estimate[2]
  emp_predictions[[i]] <- predict(fit$.workflow[[1]],
                                  (emp_stats %>%
                                     select(-name))) %>%
    mutate(name = emp_stats$name)
  toc()
  message('')
  message('')
}
message('Total run time:')
toc()
rsq_Ne <- rsq
var_imp_Ne <- var_imp
emp_pred_Ne <- emp_predictions
message('')
message('')

rsq <- list(rsq_vRep,rsq_gt,rsq_Ne)
emp_pred <- list(emp_pred_vRep,emp_pred_gt,emp_pred_Ne)

### Formatting RF results ####
require(tidyverse)
#### Accuracy histograms ####
# Variance in reproductive success
ggplot(data.frame(rsq=rsq[[1]]),aes(x=rsq))+geom_density(color='lightblue',fill='lightblue',alpha=0.8)+
  scale_x_continuous(name=bquote(R^2),limits=c(0,1))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=20,face="bold"),
        axis.title.y=element_blank())

# Generation time
ggplot(data.frame(rsq=rsq[[2]]),aes(x=rsq))+geom_density(color='lightgreen',fill='lightgreen',alpha=0.8)+
  scale_x_continuous(name=bquote(R^2),limits=c(0,1))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=20,face="bold"),
        axis.title.y=element_blank())

# Effective Population Size
ggplot(data.frame(rsq=rsq[[3]]),aes(x=rsq))+geom_density(color='lightsalmon',fill='lightsalmon',alpha=0.8)+
  scale_x_continuous(name=bquote(R^2),limits=c(0,1))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=20,face="bold"),
        axis.title.y=element_blank())

#### Values predicted for empirical datasets ####

# Predicted variance in reproductive success
emp_pred[[1]] %>% do.call(what = rbind.data.frame) %>%
  ggplot(aes(x=.pred,color=name,fill=name))+geom_density(alpha=0.8)+
  scale_x_continuous(name = bquote(italic(.('Variance in reproductive success'))))+
  scale_fill_discrete(name = 'Species',
                      labels = c(bquote(italic(.('P. filicauda'))),
                                 bquote(italic(.('T. aethiops')))),
                      type = c('red2','blue'))+
  scale_color_discrete(name = 'Species',
                       labels = c(bquote(italic(.('P. filicauda'))),
                                  bquote(italic(.('T. aethiops')))),
                       type = c('red2','blue'))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=22,face="bold"),
        axis.title.y=element_blank(),
        legend.position = 'none')
  
# Predicted gt
emp_pred[[2]] %>% do.call(what = rbind.data.frame) %>%
  ggplot(aes(x=.pred,color=name,fill=name))+geom_density(alpha=0.8)+
  scale_x_continuous(name = bquote(italic(.('Generation time'))))+
  scale_fill_discrete(name = 'Species',
                      labels = c(bquote(italic(.('P. filicauda'))),
                                 bquote(italic(.('T. aethiops')))),
                      type = c('red2','blue'))+
  scale_color_discrete(name = 'Species',
                       labels = c(bquote(italic(.('P. filicauda'))),
                                  bquote(italic(.('T. aethiops')))),
                       type = c('red2','blue'))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=22,face="bold"),
        axis.title.y=element_blank(),
        legend.position = 'none')

# Predicted effective population size
emp_pred[[3]] %>% do.call(what = rbind.data.frame) %>%
  ggplot(aes(x=.pred,color=name,fill=name))+geom_density(alpha=0.8)+
  scale_x_continuous(name = bquote(italic(N[e])))+
  scale_fill_discrete(name = 'Species',
                      labels = c(bquote(italic(.('P. filicauda'))),
                                 bquote(italic(.('T. aethiops')))),
                      type = c('red2','blue'))+
  scale_color_discrete(name = 'Species',
                       labels = c(bquote(italic(.('P. filicauda'))),
                                  bquote(italic(.('T. aethiops')))),
                       type = c('red2','blue'))+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=22,face="bold"),
        axis.title.y=element_blank(),
        legend.position = 'none')