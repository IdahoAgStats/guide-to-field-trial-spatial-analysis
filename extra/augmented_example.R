
library(dplyr); library(nlme); library(ggplot2)
library(gstat); library(sp)

setwd("content/workshops/spatial-workshop/practice")

# read in data
aug_data_origin <- read.csv("AB19F5_LIND.csv", 
                     na.strings = c("", "NA", ".", "999999")) %>% 
  slice(-1) 

# summarise the genoytypic data by checks/not checks
gen_sum <- group_by(aug_data_origin, name) %>% summarise(counts = n()) %>% 
  mutate(delta =  case_when(
    counts > 1 ~ "check",
    counts == 1 ~ "unrep"))

# need info on just the checks
checks <- gen_sum %>% filter(delta == "check") 

# more summarise steps for different augmented modes
gen_sum2 <- gen_sum  %>%  mutate(gamma = name) %>% 
  mutate(tau = case_when(
    delta == "check" ~ gamma,
    delta == "unrep" ~ "unreplicate_obs")) %>% 
  mutate(beta = case_when(
    delta == "unrep" ~ gamma,
    delta == "check" ~ NA_integer_)) # or maybe "check"

# merge original data set with info on treatment levels
aug_data <- aug_data_origin %>% 
  select(name, prow, pcol, yieldg) %>%
  mutate(row = prow*11.7, col = pcol*5.5) %>% 
  full_join(gen_sum2, by = "name") 

# calculate denominator degrees of freedom
ddf = sum(checks$counts) - nrow(checks) - 1

## initial linear model:

# tau estimates effects of checks versus all unreplicated genotypes)
# beta predicts effects each unreplicated genotypes, nested in tau 
aug1 <- lme(fixed = yieldg ~ tau,
                 random = ~ 1|tau/beta,
                 data = aug_data, na.action = na.exclude)

# another formulation
# delta estimates effects of replicated versus unreplicated genotypes
# gamma estimates the effecs of all genotypes evaluated in the trial
aug2 <- lme(fixed = yieldg ~ delta,
                 random = ~ 1|delta/gamma,
                 data = aug_data, na.action = na.exclude)

# extract residuals
aug_data$res <- residuals(aug1)

# plot residual chlorpleth map:
ggplot(aug_data, aes(y = row, x = col)) +
  geom_tile(aes(fill = res)) +
  scale_fill_gradient(low = "yellow", high = "black") +
  scale_x_continuous(breaks = seq(1,max(aug_data$row), 1)) +
  scale_y_continuous(breaks = 1:max(aug_data$col)) +
  coord_equal() +
  theme_void() 

# add spatial covariates

# the long way: 

aug_spatial <- aug_data %>% filter(!is.na(res))
coordinates(aug_spatial) <- ~ col + row
max_dist = 0.5*max(dist(coordinates(aug_spatial)))

aug_vario <- gstat::variogram(res ~ 1, 
                               cutoff = max_dist,
                               width = max_dist/10, # 20 is the number of bins
                               data = aug_spatial)
plot(aug_vario)

nugget_start <- min(aug_vario$gamma)
aug_vgm <- vgm(model = "Exp", nugget = nugget_start) 
aug_variofit <- fit.variogram(aug_vario, aug_vgm)

plot(aug_vario, aug_variofit, main = "Exponential model")     

cor_exp <- corSpatial(form = ~ row + col, 
                      nugget = T, fixed = F,
                      type = "exponential")

aug1_sp <- update(aug1, corr = cor_exp)

# spatial parameters:
aug1_sp$modelStruct$corStruct

# extract BLUPs for unreplicated lines:
aug_blups <- ranef(aug1_sp)$beta %>% rename(yieldg = '(Intercept)')

# look at variance components
VarCorr(aug1_sp)

