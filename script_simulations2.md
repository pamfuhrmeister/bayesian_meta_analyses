---
title: "Simulations semantic interference"
author: "Pam Fuhrmeister & Audrey Buerki"
date: "8 June 2021"
output:
    html_document:
      keep_md: yes
---





## Simulate data from an ex-gaussian distribution

```r
set.seed(8675309)
# exgaussian parameters that are estimated from data sets from meta analysis
rel_mu <- 578
rel_sigma <- 68
rel_tau <- 219
unr_mu <- 570
unr_sigma <- 53
unr_tau <- 202

id = rep(1:100, each = 100) # make a vector of participants 
item = rep(1:50, each = 2, length.out = length(id))
condition = rep(c("related", "unrelated"), times = length(id)/2)

dat <- data.frame(id,item,condition)

per_participant_intercept = rnorm(100, mean = 0, sd = 100)
per_item_intercept = rnorm(50, mean = 0, sd = 70) # sd estimated from pilot data
participant <- data.frame(id = rep(1:100), per_participant_intercept)
ggplot(participant, aes(per_participant_intercept)) +
  geom_density()
```

![](script_simulations2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
items <- data.frame(item = rep(1:50), per_item_intercept)
ggplot(items, aes(per_item_intercept)) +
  geom_density()
```

![](script_simulations2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
dat <- merge(dat, participant, by = "id")
dat <- merge(dat, items, by = "item")

dat$err <- rnorm(n = length(dat), mean = 0, sd = 100)

dat$response <- ifelse(dat$condition == "related", rexgauss(length(id), mu = rel_mu + dat$per_participant_intercept + dat$per_item_intercept + dat$err, sigma = rel_sigma, tau = rel_tau), rexgauss(length(id), mu = unr_mu + dat$per_participant_intercept + dat$per_item_intercept + dat$err, sigma = unr_sigma, tau = unr_tau))
```


## Look at data


```r
# plot distributions of data by condition (related/unrelated)
ggplot(dat, aes(response, fill = condition)) +
  geom_density(alpha = .5)
```

![](script_simulations2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dat$condition <- as.factor(dat$condition)
contrasts(dat$condition) <- c(.5,-.5)

m0 <- lmer(response ~ condition + (1|id) + (1|item), data = dat) 
mat_trt <- model.matrix(m0)
dat$cont.cond <- mat_trt[,2] 
# fit model with simulated data
summary(mod <- lmer(log(response) ~ cont.cond + 
                      (cont.cond || id) +
                      (cont.cond || item),
                    data = dat))
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: log(response) ~ cont.cond + (cont.cond || id) + (cont.cond ||  
##     item)
##    Data: dat
## 
## REML criterion at convergence: 4072
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.935 -0.651 -0.035  0.587  4.579 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.017696 0.1330  
##  id.1     cont.cond   0.000349 0.0187  
##  item     (Intercept) 0.006719 0.0820  
##  item.1   cont.cond   0.000000 0.0000  
##  Residual             0.083925 0.2897  
## Number of obs: 10000, groups:  id, 100; item, 50
## 
## Fixed effects:
##              Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)   6.62802    0.01788 133.97943  370.67  < 2e-16 ***
## cont.cond     0.02506    0.00609  98.99944    4.12  0.00008 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr)
## cont.cond 0.000 
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod1 <- lmer(response ~ cont.cond + 
                      (cont.cond || id) +
                      (cont.cond || item),
                    data = dat))
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: response ~ cont.cond + (cont.cond || id) + (cont.cond || item)
##    Data: dat
## 
## REML criterion at convergence: 138329
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -1.989 -0.682 -0.162  0.453  8.686 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept)  8804     93.8   
##  id.1     cont.cond     253     15.9   
##  item     (Intercept)  3447     58.7   
##  item.1   cont.cond       0      0.0   
##  Residual             57227    239.2   
## Number of obs: 10000, groups:  id, 100; item, 50
## 
## Fixed effects:
##             Estimate Std. Error     df t value Pr(>|t|)    
## (Intercept)   797.12      12.76 130.68   62.49  < 2e-16 ***
## cont.cond      21.65       5.04  99.00    4.29 0.000041 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr)
## cont.cond 0.000 
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

## Compute quantiles


```r
# calculate delta plot slopes and mean interference effects from simulated data
quantiles <- dat %>%
  group_by(id, condition) %>%
  mutate(quantile = cut(response, breaks = quantile(response, probs = seq(0,1,.2), type = 7),
                        include.lowest = TRUE, labels = 1:5)) %>%
  group_by(id, condition, quantile) %>%
  #ungroup()%>%
  #group_by(quantile) %>%
  summarize(mean_quantile = mean(response)) 


quantiles_item <- dat %>%
  group_by(item, condition) %>%
  mutate(quantile = cut(response, breaks = quantile(response, probs = seq(0,1,.2), type = 7),
                        include.lowest = TRUE, labels = 1:5)) %>%
  group_by(item, condition, quantile) %>%
    #ungroup() %>%
    #group_by(quantile) %>%
    summarize(mean_quantile_items = mean(response))

quantiles_wide <- quantiles %>%
  pivot_wider(names_from = c(condition, quantile), values_from = mean_quantile)

quantiles_wide_item <- quantiles_item %>%
  pivot_wider(names_from = c(condition, quantile), values_from = mean_quantile_items)

quantiles_plot_data <- quantiles %>%
  group_by(condition, quantile) %>%
  summarize(mean_RT = mean(mean_quantile))

quantiles_plot_data_items <- quantiles_item %>%
  group_by(condition, quantile) %>%
  summarize(mean_RT = mean(mean_quantile_items))

quantiles_plot_data$condition <- as.factor(as.character(quantiles_plot_data$condition))
plot_pp <-ggplot(quantiles_plot_data, aes(x = mean_RT, y = quantile, color = condition, group = condition)) +
  geom_point(stat = "identity") +
  geom_line() +
  scale_color_viridis_d("condition")
plot_pp
```

![](script_simulations2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
quantiles_plot_data_items$condition <- as.factor(as.character(quantiles_plot_data_items$condition))

plot_item <-ggplot(quantiles_plot_data_items, aes(x = mean_RT, y = quantile, color = condition, group = condition)) +
  geom_point(stat = "identity") +
  geom_line() +
  scale_color_viridis_d("condition")
plot_item
```

![](script_simulations2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

# Compute slopes


```r
slope_2_1 <- quantiles_wide %>%
  mutate(delta_2 = related_2 - unrelated_2) %>%
  mutate(delta_1 = related_1 - unrelated_1) %>%
  mutate(mean_2 = ((related_2 + unrelated_2)/2)) %>%
  mutate(mean_1 = ((related_1 + unrelated_1)/2)) %>%
  group_by(id) %>%
  summarize(slope_2_1 = (delta_2 - delta_1)/(mean_2 - mean_1))

slope_2_1_item <- quantiles_wide_item %>%
  mutate(delta_2 = related_2 - unrelated_2) %>%
  mutate(delta_1 = related_1 - unrelated_1) %>%
  mutate(mean_2 = ((related_2 + unrelated_2)/2)) %>%
  mutate(mean_1 = ((related_1 + unrelated_1)/2)) %>%
  group_by(item) %>%
  summarize(slope_2_1_item = (delta_2 - delta_1)/(mean_2 - mean_1))

slope_5_4 <- quantiles_wide %>%  
  mutate(delta_5 = related_5 - unrelated_5) %>%
  mutate(delta_4 = related_4 - unrelated_4) %>%
  mutate(mean_5 = ((related_5 + unrelated_5)/2)) %>%
  mutate(mean_4 = ((related_4 + unrelated_4)/2)) %>%
  group_by(id) %>%
  summarize(slope_5_4 = (delta_5 - delta_4)/(mean_5 - mean_4))

slope_5_4_item <- quantiles_wide_item %>%
  mutate(delta_5 = related_5 - unrelated_5) %>%
  mutate(delta_4 = related_4 - unrelated_4) %>%
  mutate(mean_5 = ((related_5 + unrelated_5)/2)) %>%
  mutate(mean_4 = ((related_4 + unrelated_4)/2)) %>%
  group_by(item) %>%
  summarize(slope_5_4_item = (delta_5 - delta_4)/(mean_5 - mean_4))
```


# Correlations



```r
sem_int <- dat %>%
  group_by(id, condition) %>%
  summarize(mean_cond = mean(response)) %>%
  pivot_wider(names_from = condition, values_from = mean_cond) %>%
  mutate(sem_int = related - unrelated)

# semantic interference effect by item
sem_int_item <- dat %>%
  group_by(item,condition) %>%
  summarize(mean_cond = mean(response)) %>%
  pivot_wider(names_from = condition, values_from = mean_cond) %>%
  mutate(sem_int_item = related - unrelated)

cor_participants <- merge(sem_int, slope_5_4, by = "id")
cor_participants <- merge(cor_participants, slope_2_1, by = "id")
cor_items <- merge(sem_int_item, slope_5_4_item, by = "item")
cor_items <- merge(cor_items, slope_2_1_item, by = "item")

cor.test(cor_participants$slope_5_4, cor_participants$sem_int)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cor_participants$slope_5_4 and cor_participants$sem_int
## t = 5.1, df = 98, p-value = 0.000002
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.2882 0.6010
## sample estimates:
##    cor 
## 0.4587
```

```r
cor.test(cor_participants$slope_2_1, cor_participants$sem_int)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cor_participants$slope_2_1 and cor_participants$sem_int
## t = 3, df = 98, p-value = 0.003
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.1026 0.4629
## sample estimates:
##    cor 
## 0.2931
```

```r
cor.test(cor_items$slope_5_4_item, cor_items$sem_int_item)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cor_items$slope_5_4_item and cor_items$sem_int_item
## t = 3.9, df = 48, p-value = 0.0003
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.2427 0.6747
## sample estimates:
##    cor 
## 0.4881
```

```r
cor.test(cor_items$slope_2_1_item, cor_items$sem_int_item)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cor_items$slope_2_1_item and cor_items$sem_int_item
## t = 2.4, df = 48, p-value = 0.02
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.04734 0.55054
## sample estimates:
##    cor 
## 0.3215
```

# Add plot of effect size as a function of segment

```r
fast_slope_pp <-left_join(sem_int, slope_2_1, by = NULL, copy = FALSE)

plot_fast_pp <-ggplot(fast_slope_pp, aes(x = sem_int, y = slope_2_1)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Participants: fast delta segment", x = "Mean semantic interference effect", y = "Slope of fastest delta segment")
plot_fast_pp
```

![](script_simulations2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
fast_slope_item <-left_join(sem_int_item, slope_2_1_item, by = NULL, copy = FALSE)

plot_fast_item <-ggplot(fast_slope_item, aes(x = sem_int_item, y = slope_2_1_item)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Items: fast delta segment", x = "Mean semantic interference effect", y = "Slope of fastest delta segment")
plot_fast_item
```

![](script_simulations2_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
slow_slope_pp <-left_join(sem_int, slope_5_4, by = NULL, copy = FALSE)

plot_slow_pp <-ggplot(slow_slope_pp, aes(x = sem_int, y = slope_5_4)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Participants: slow delta segment", x = "Mean semantic interference effect", y = "Slope of slowest delta segment")
plot_slow_pp
```

![](script_simulations2_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
slow_slope_item <-left_join(sem_int_item, slope_5_4_item, by = NULL, copy = FALSE)

plot_slow_item <-ggplot(slow_slope_item, aes(x = sem_int_item, y = slope_5_4_item)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Items: slow delta segment", x = "Mean semantic interference effect", y = "Slope of slowest delta segment")
plot_slow_item
```

![](script_simulations2_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```r
figure <- cowplot::plot_grid(plot_slow_pp, plot_fast_pp, plot_slow_item, plot_fast_item, ncol = 2, labels = c("A","B","C","D"))
figure
```

![](script_simulations2_files/figure-html/unnamed-chunk-7-5.png)<!-- -->

