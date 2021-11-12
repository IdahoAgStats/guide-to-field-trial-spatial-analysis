# load libraries
library(dplyr); library(ggplot2); library(desplot); 
library(spdep); library(sf); library(nlme)

# read in data and prepare it
Nin <- read.csv("data/stroup_nin_wheat.csv") %>% 
  mutate(col.width = col * 1.2, row.length = row * 4.3) %>% 
  mutate(name = case_when(is.na(as.character(rep)) ~ NA_character_, 
                          TRUE ~ as.character(gen))) %>% 
  arrange(col, row)

Nin_na <- filter(Nin, !is.na(yield))

ggplot(Nin, aes(x = row, y = col)) +
  geom_tile(aes(fill = yield), col = "white") +
  geom_tileborder(aes(group = 1, grp = rep), lwd = 1.2) +
  labs(x = "row", y = "column", title = "field plot layout") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

## moran's I

# set neighbors with convenience function for grids
xy_rook <- cell2nb(nrow = max(Nin$row), ncol = max(Nin$col), 
                   type="rook", torus = FALSE, legacy = FALSE)  

# run linear mixed model and extract residuals
nin.lme <- lme(fixed = yield ~ gen, random = ~1|rep,
               data = Nin, na.action = na.exclude)

resid_lme <- residuals(nin.lme)
names(resid_lme) <- Nin$plot

# two version of the Moran's I test: 
moran.test(resid_lme, nb2listw(xy_rook), na.action = na.exclude)

moran.mc(resid_lme, nb2listw(xy_rook), 999, na.action = na.exclude)

#################################
# load libraries
library(gstat); library(spaMM)

# set up spatial object
Nin_spatial <- Nin_na
coordinates(Nin_spatial) <- ~ col.width + row.length # add attribte
class(Nin_spatial)

# establish max distance for variogram estimation
max_dist = 0.6*max(dist(coordinates(Nin_spatial)))

# calculate empirical variogram
resid_var1 <- gstat::variogram(yield ~ rep + gen, 
                               cutoff = max_dist,
                               width = max_dist/15, # 20 is the number of bins
                               data = Nin_spatial)
plot(resid_var1)  # empirical variogram

# starting value for the nugget
nugget_start <- min(resid_var1$gamma) 

# initialise the model (this does not do much)
Nin_vgm_exp <- vgm(model = "Exp", nugget = nugget_start) # exponential
Nin_vgm_gau <- vgm(model = "Gau", nugget = nugget_start) # Gaussian
Nin_vgm_mat <- vgm(model = "Mat", nugget = nugget_start) # Matern

# actually do some fitting! 
Nin_variofit_exp <- fit.variogram(resid_var1, Nin_vgm_exp)
Nin_variofit_gau <- fit.variogram(resid_var1, Nin_vgm_gau)
Nin_variofit_mat <- fit.variogram(resid_var1, Nin_vgm_mat, fit.kappa = TRUE)

plot(resid_var1, Nin_variofit_exp, main = "Exponential model")
plot(resid_var1, Nin_variofit_gau, main = "Gaussian model")
plot(resid_var1, Nin_variofit_mat, main = "Matern model") 

attr(Nin_variofit_exp, "SSErr")
attr(Nin_variofit_gau, "SSErr")
attr(Nin_variofit_mat, "SSErr")

##### model fitting

library(emmeans); library(nlme)
# (nlme and gstat should already be loaded)
library(spaMM) # for running `corMatern()`

# standard linear model
nin_lme <- lme(fixed = yield ~ gen, 
               random = ~1|rep,
               data = Nin,
               na.action = na.exclude)

# extract the esimated marginal means for variety
preds_lme <- as.data.frame(emmeans(nin_lme, "gen"))

# use information from the variogram fitting for intialising the parameters
nugget <- Nin_variofit_gau$psill[1] 
range <- Nin_variofit_gau$range[2]  
sill <- sum(Nin_variofit_gau$psill) 
nugget.effect <-  nugget/sill

# initalise the covariance structure (from the nlme package)
cor.gaus <- corSpatial(value = c(range, nugget.effect), 
                       form = ~ row.length + col.width, 
                       nugget = T, fixed = F,
                       type = "gaussian", 
                       metric = "euclidean")

# update the rcbd model
nin_gaus <- update(nin_lme, corr = cor.gaus)
# extract predictions for 'gen'
preds_gaus <- as.data.frame(emmeans(nin_gaus, "gen"))

# other covariance structures

# exponential
cor.exp <- corSpatial(form = ~ row.length + col.width, 
                      nugget = T, fixed = F)

nin_exp <- update(nin_lme, corr = cor.exp)
preds_exp <- as.data.frame(emmeans(nin_exp, "gen"))

# Matern structure
cor.mat <- corMatern(form = ~ row.length + col.width, 
                     nugget = T, fixed = F)
nin_matern <- update(nin_lme, corr = cor.mat)
preds_mat <- as.data.frame(emmeans(nin_matern, "gen"))
         
###### after lunch

library(tidyr)

# assemble objects into a list
nlme_mods <- list(nin_lme, nin_exp, nin_gaus, nin_matern)
names(nlme_mods) <- c("LMM", "exponential", "gaussian", "matern")

# extract log likelihood, AIC, BIC
data.frame(loglik = sapply(nlme_mods, logLik),  
           AIC = sapply(nlme_mods, AIC),
           BIC = sapply(nlme_mods, AIC, k = log(nrow(Nin_na)))) %>% 
  arrange(desc(loglik))
# (higher is better for loglik, 
# lower is better for AIC and BIC)

### anova

anovas <- lapply(nlme_mods, function(x){ 
  aov <- as.data.frame(anova(x))[2,]})
# bind all the output together
a <- bind_rows(anovas) %>% 
  mutate(model = c("LMM", "exponential", "gaussian", "matern")) %>% 
  arrange(desc(`p-value`)) %>% select(c(model, 1:4)) 
rownames(a) <- 1:nrow(a)
a

## 
## compare precision of estimates
all.preds <- mget(ls(pattern = "^preds_*"))
errors <- lapply(all.preds, "[", "SE")
pred.names <- gsub("preds_", "", names(errors))
error_df <- bind_cols(errors)
colnames(error_df) <- pred.names
boxplot(error_df, ylab = "standard errors", xlab = "linear model", 
        col = "dodgerblue3")

# compare predictions 
preds <- lapply(all.preds, "[", "emmean")
preds_df <- bind_cols(preds)
colnames(preds_df) <- pred.names
preds_df$gen <- preds_exp$gen

lev <- c("lme", "exp", "gaus", "mat")
pivot_longer(preds_df, cols = !gen, names_to = "model", values_to = "emmeans") %>% 
  mutate(model = factor(model, levels = lev)) %>% 
  ggplot(aes(x = model, y = emmeans, group = gen)) +
  geom_point(size = 5, alpha = 0.5, col = "navy") +
  geom_line() +
  ylab("yield means for gen") + 
  theme_minimal(base_size = 14)

### trends

library(lme4)

# exploratory plots 
boxplot(yield ~ rep, data = Nin, xlab = "block", col = "red2")
boxplot(yield ~ row, data = Nin, xlab = "row", col = "dodgerblue2")
boxplot(yield ~ col, data = Nin, xlab = "column", col = "gold")

## row/column model ##

# data prep
Nin$rowF = as.factor(Nin$row)
Nin$colF = as.factor(Nin$col)

# specify model
nin.rc <- lmer(yield ~ gen + (1|colF) + (1|rowF),
               data = Nin, na.action = na.exclude)
# extract random effects for row and column
ranef(nin.rc)

preds_rc <- as.data.frame(emmeans(nin.rc, "gen"))


#### splines

library(SpATS)

nin_spline <- SpATS(response = "yield", 
                    spatial = ~ PSANOVA(col, row, nseg = c(10,20),
                                        degree = 3, pord = 2), 
                    genotype = "gen",  
                    random = ~ rowF + colF, 
                    data = Nin, 
                    control = list(tolerance = 1e-03, monitoring = 0))

preds_spline <- predict(nin_spline, which = "gen") %>% 
  dplyr::select(gen, emmean = "predicted.values", SE = "standard.errors")

#### augmented design

# (if not already loaded)
library(dplyr); library(nlme); library(ggplot2)
library(gstat); library(sp)

# read in data
aug_data_origin <- read.csv("data/augmented_lind.csv", 
                            na.strings = c("", "NA", ".", "999999")) %>% 
  slice(-1) %>% # first line not needed
  mutate(yieldkg = yieldg/1000)  # to prevent overflow

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
    delta == "check" ~ gamma))

# merge original data set with info on treatment levels
aug_data <- aug_data_origin %>% 
  select(name, prow, pcol, yieldkg, yieldg) %>%
  mutate(row = prow*11.7, col = pcol*5.5) %>% 
  full_join(gen_sum2, by = "name") 

## modelling

aug1 <- lme(fixed = yieldg ~ tau,
            random = ~ 1|tau/beta,
            data = aug_data, na.action = na.exclude)

# extract residuals
aug_data$res <- residuals(aug1)

# plot residual chloroepleth map:
ggplot(aug_data, aes(y = row, x = col)) +
  geom_tile(aes(fill = res)) +
  scale_fill_gradient(low = "yellow", high = "black") +
  scale_x_continuous(breaks = seq(1,max(aug_data$row), 1)) +
  scale_y_continuous(breaks = 1:max(aug_data$col)) +
  coord_equal() +
  theme_void() 

# add spatial covariates
aug_spatial <- aug_data %>% filter(!is.na(res))
coordinates(aug_spatial) <- ~ col + row
max_dist = 0.5*max(dist(coordinates(aug_spatial)))

aug_vario <- gstat::variogram(res ~ 1, 
                              cutoff = max_dist,
                              width = max_dist/10, 
                              data = aug_spatial)

# optional to run: 
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
aug1_blups <- ranef(aug1_sp)$beta %>% rename(yieldg = '(Intercept)')

# look at variance components
VarCorr(aug1_sp)


##### OR #######

# another formulation
# delta estimates effects of replicated versus unreplicated genotypes
# gamma estimates the effecs of all genotypes evaluated in the trial
aug2 <- lme(fixed = yieldkg ~ delta,
            random = ~ 1|delta/gamma,
            data = aug_data, na.action = na.exclude)

aug2_sp <- update(aug2, corr = cor_exp)

# spatial parameters:
aug2_sp$modelStruct$corStruct

# extract BLUPs for unreplicated lines:
aug_blups2 <- ranef(aug2_sp)$gamma %>% rename(yieldg = '(Intercept)')

# look at variance components
VarCorr(aug1_sp)
