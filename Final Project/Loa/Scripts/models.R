
##### OLS model 
model <- lm(
  perc_pos ~ max_NDVI + cap_dist + cap2_dist + elevation,
  data = loa
)

summary.lm(model)

##### Obtain residuals 

loa$resid <- resid(model)

autoplot(
  model,
  smooth.colour = NA
) + 
  theme_classic(
    
  )
 
full_model <- lm(
  perc_pos ~ . - resid - lat - lon  - number_positives - number_tested,
  data = loa
) 

summary.lm(full_model)

ggplot(
  data = loa,
  aes(
    x = resid
  )
) + 
  geom_histogram(
    aes(
      y=..density..
      ), 
    colour = "black", 
    fill = "lightblue",
    bins = 20
    ) +
  geom_density(
    alpha = .2
    ) + 
  theme_classic(
    
  ) + 
  labs(
    y = "Density",
    x = "Residual"
  )


##### Cutoff and for variogram
cutoff <- .65*max(
  dist(
    cbind(
      loa$lon,
      loa$lat
    )
  )
)

bins <- 15

###### Variogram/Covariogram (with scale variables)
variogram <- variogram(
  scale(perc_pos) ~ 1, 
  locations = ~lon + lat,
  data = loa,
  cutoff = cutoff,
  width = cutoff/bins
)

plot(
  variogram,
  pch = 16, 
  cex = 1.5,
  ylab = expression(
    paste(
      "Average ", (0.65*(Y(s[i]) - Y(s[j])^2))
    )
  ),
  xlab = "Euclidean Distance (degrees)"
)


covariogram <- variogram(
  scale(perc_pos) ~ 1, 
  locations = ~lon + lat,
  data = loa,
  cutoff = cutoff,
  width = cutoff/bins,
  covariogram = T,
  map = T
)

plot(
  covariogram,
  contour = T
)


##### Spatial Models #####


###### Anisotropic Model (with scaled variables)
fit_aniso <- lgm(
  data = loa_model_sp, 
  perc_pos ~ scale(max_NDVI) + scale(elevation) + scale(cap_dist) + scale(cap2_dist),
  grid = 100,                    
  shape = .5,
  fixShape = FALSE,   
  nugget = .5,
  fixNugget = FALSE,  
  aniso = TRUE,
  reml = TRUE
) 

summary(fit_aniso)

AIC(fit_aniso)

##### Isotropic model (with scaled variables)
fit_iso <- lgm(
  perc_pos ~ scale(max_NDVI) + scale(elevation)+ scale(cap_dist) + scale(cap2_dist),
  data = loa_model_sp,                
  grid = 100,                    
  shape = .5,
  fixShape = FALSE,   
  nugget = .5,
  fixNugget = FALSE,  
  aniso = FALSE,
  reml = TRUE
) 

summary(fit_iso)

AIC(fit_iso)

##### Non-normal Spatial Model 

spamm_fit <- fitme(
  cbind(number_positives, number_tested - number_positives) ~ scale(max_NDVI) + scale(elevation) + scale(cap_dist) + scale(cap2_dist) + 
    Matern(1|lon + lat),
  fixed = list(
    nu = .5
  ),
  method = "REML",
  data = loa,
  family = "binomial"
)

spamm_fit

AIC(spamm_fit)

latent <- ranef(spamm_fit)[[1]]

loa$latent <- latent

##### Figures for An- and Isotropic Model (Predicted SD and Predicted Prev.)

par(mfcol=c(2,1), mai=c(0.5,0.5,0.5,0.5))

plot(fit_aniso$predict[["predict"]],main='Anisotropic Predicted Prevalence')

plot(fit_iso$predict[["predict"]],main='Isotropic Predicted Prevalence')


par(mfcol=c(2,1), mai=c(0.5,0.5,0.5,0.5))

plot(fit_aniso$predict[["krigeSd"]],main='Anisotropic Prediction SDs')

plot(fit_iso$predict[["krigeSd"]],main='Isotropic Prediction SDs')


##### Figure for non-normal model 

filled.mapMM(
  spamm_fit,
  map.asp = 3,
  nlevels = 10,
  gridSteps = 50,
  add.map = TRUE,
  axes = F,
  yrange = c(
    min_lat - correction, 
    max_lat + correction
  ),
  xrange = c(
    min_lon - correction, 
    max_lon +  correction
    )
  )


#### Observered ~ fitted figure (observed vs predicted)

predicted <- as.numeric(predict.HLfit(spamm_fit)*100)

loa$predicted <- predicted

int <- get_intervals(spamm_fit)*100

loa <- data.frame(loa, int)

## Observed vs Fitted 

ggplot(
  data = loa,
  aes(
    x = perc_pos,
    y = predicted
    )
  ) + 
  geom_point(
    size = 3
  ) +
  geom_linerange(
    aes(
      ymin = respVar_0.025,
      ymax = respVar_0.975
    )
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "blue"
  ) + 
  labs(
    x = "Observed Prevalence",
    y = "Predicted Prevalence"
  ) +
  theme_classic(
    
  )

## w/ Elevation 

ggplot(
  data = loa,
  aes(
    x = perc_pos,
    y = predicted,
    col = elevation
  )
) + 
  geom_point(
    size = 3
  ) +
  geom_linerange(
    aes(
      ymin = respVar_0.025,
      ymax = respVar_0.975
    )
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "blue"
  ) +
  labs(
    x = "Observed Prevalence",
    y = "Predicted Prevalence",
    col = "Elevation (m)"
  ) +
  theme_classic(
    
  )

# w/ Distance from Cap. 

ggplot(
  data = loa,
  aes(
    x = perc_pos,
    y = predicted,
    col = cap_dist
  )
) + 
  geom_point(
    size = 3
  )+
  geom_linerange(
    aes(
      ymin = respVar_0.025,
      ymax = respVar_0.975
    )
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "blue"
  ) + 
  labs(
    x = "Observed Prevalence",
    y = "Predicted Prevalence",
    col = expression(atop("Distance to", paste("Yaounde (km)")))
  ) +
  theme_classic(
    
  )


# w/ Distance to Daoula

ggplot(
  data = loa,
  aes(
    x = perc_pos,
    y = predicted,
    col = cap2_dist
  )
) + 
  geom_point(
    size = 3
  )+
  geom_linerange(
    aes(
      ymin = respVar_0.025,
      ymax = respVar_0.975
    )
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "blue"
  ) + 
  labs(
    x = "Observed Prevalence",
    y = "Predicted Prevalence",
    col = expression(atop("Distance to", paste("Douala (km)")))
  ) +
  theme_classic(
    
  )

# w/ NDVI 

ggplot(
  data = loa,
  aes(
    x = perc_pos,
    y = predicted,
    col = max_NDVI
  )
) + 
  geom_point(
    size = 3
  ) +
  geom_linerange(
    aes(
      ymin = respVar_0.025,
      ymax = respVar_0.975
    )
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "blue"
  ) +
  labs(
    x = "Observed Prevalence",
    y = "Predicted Prevalence",
    col = "Max. NDVI"
  ) +
  scale_color_gradient(
    low="lightgreen",
    high="darkgreen"
    ) +
  theme_classic(
    
  )


#### Model comparison

spamm_aic_1 <- format(as.numeric(AIC(spamm_fit)[1])[1], digits = 6, format = "f")
spamm_aic_2 <- format(as.numeric(AIC(spamm_fit)[2])[1], digits = 6, format = "f")
spamm_ef <- format(3/(as.numeric(spamm_fit$corrPars$`1`[2]))*111, digits = 6, format = "f")

spamm_aic_1 <- paste(spamm_aic_1, "(marginal)")
spamm_aic_2 <- paste(spamm_aic_2, "(conditional)")


comparison <- data.frame(
  "Model" = c("OLS", "Isotropic", "Anisotropic", "Non-normal", "Non-normal"),
  "AIC" = c(format(AIC(model), digits = 6, format = "f"), format(AIC(fit_iso), digits = 6, format = "f"), format(AIC(fit_aniso),digits = 6, format = "f"), spamm_aic_1, spamm_aic_2),
  "Effective Range km" = c("---", format((fit_iso$parameters[1])*111, digits = 6, format = "f"), format((fit_aniso$parameters[1])*111,digits = 6, format = "f"), spamm_ef, "---"))

comparison
# Spatial variance estimate 


### End of Models ### 