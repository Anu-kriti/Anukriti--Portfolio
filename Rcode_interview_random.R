def maxDiff(n, a): 
  diff = 0 
  
  for i in range(1, n):
    for j in range(0, i): 
        if(a[i] - a[j] > diff):  
          diff = a[i] - a[j] 
    return diff
    return paste0("Maximum difference between two elements in array " +  a +  " is ", max_diff)
    
  
    
    
    
    
    
    
    
    
    
    f <- file("stdin")
    open(f)
    filename <- readLines(f, n = 1)
    n <- as.numeric(readLines(f, n = 1))
    library(data.table)
    moneyFlowIndex <- function(filename, n) {
      # please enter your code in this function
      
      # you need to assign values to these objects
      df_output <- as.data.table(filename)
      output_filename <- df_output[, `:=` (`Typical Price` = (High + Low + Close)/3, 
                                           typical_price_old = c(`Typical Price`[-1],NA), 
                                           `Positive Money Flow` = ifelse(Day == 1 |  `Typical Price` < typical_price_old, "", `Typical Price` * Volume )
                                           `Negative Money Flow` = ifelse(Day == 1 |  `Typical Price` > typical_price_old, "", `Typical Price` * Volume))]
      output_filename[, `:=`(`Positive Money Flow Sum` = ifelse(Day >= n+1, cumsum(`Positive Money Flow`, na.rm = T),NA),
                             `Negative Money Flow Sum` = ifelse(Day >= n+1, cumsum(`Negative Money Flow`, na.rm = T),NA))
                      ]        
      output_filename[, `:=`(`Money Flow Index` = ifelse(Day >= n+1, 100 * (`Positive Money Flow Sum`/`Negative Money Flow Sum`)/(1+ ( `Positive Money Flow Sum`/`Negative Money Flow Sum`)), NA))]   
      
      output_filename[, c("typical_price_old") := NULL]
      # write the output:
      write.csv(x = df_output, file = output_filename, row.names = FALSE, col.names = TRUE)
    }
    
    moneyFlowIndex(filename=filename,n=n)
    
    
    ################Additional Codes
    
    
    
    ## Regression Analysis
    ### Stepwise Logistic Regression
    
    ```{r cluster 1, include=F}
    wt <- round(clus1_data_filt[vlapse_fg == 0, .N] / clus1_data_filt[vlapse_fg == 1, .N])
    wts1 <- ifelse(clus1_data_filt$vlapse_fg == 0, 1, wt)
    
    glm.fit_full <- glm(vlapse_fg ~ .,
                        data = clus1_data_filt,
                        family = binomial,
                        weights = wts1)
    
    glm.fit_nothing <- glm(vlapse_fg ~ 1,
                           data = clus1_data_filt,
                           family = binomial,
                           weights = wts1)
    
    c1_forward <- step(glm.fit_nothing,
                       scope = list(lower = formula(glm.fit_nothing),
                                    upper = formula(glm.fit_full)),
                       direction = "forward")
    
    c1_coefs <- data.table(summary(c1_forward)$coefficients,
                           var = rownames(summary(c1_forward)$coefficients))
    c1_coefs[, `:=` (lower = Estimate - 1.96*`Std. Error`,
                     upper = Estimate + 1.96*`Std. Error`)]
    ```
    
    ```{r, fig.height=10}
    tmp <- c1_coefs[`Std. Error` < 10]
    ggplot(tmp) +
      geom_pointrange(aes(x = var, y = Estimate, ymin = lower, 
                          ymax = upper, color = factor(sign(Estimate)))) +
      scale_x_discrete(limits = tmp[order(-Estimate)][,var]) +
      theme(legend.position = "none") +
      coord_flip() +
      labs(y = "Coefficients and 95% Confidence Intervals", x = NULL, 
           title = "Cluster 4 - Forward Selection")
    ```
    
    ```{r, include=F}
    dat4_c1 <- dat4[cluster == 1]
    dat4_c1[, `:=` (
      oop_per_rx_clm = as.numeric(scale(oop_rx_clm / n_rx_clm)),
      pct_drug_thyroid = drug_thyroid / n_rx_clm,
      pct_auth_rep_s = auth_rep_s_case / s_case,
      pct_find_prov_s = find_prov_s / s_case,
      oon_ratio = as.numeric(scale(oon / inn)),
      pct_manage_rider_s = manage_rider_s / s_case,
      pct_assist_rx_s = assist_rx_s / s_case,
      pct_billing_s = billing_s / s_case,
      pct_vg = vg_s_case / s_case,
      pct_pharmacy_s = pharmacy_s / s_case,
      pct_dispute_billing_s = dispute_billing_s / s_case,
      pct_drug_antihypertensive = drug_antihypertensive / n_rx_clm,
      pct_update_info_s = update_info_s / s_case,
      pct_drug_antihyperlipidemic = drug_antihyperlipidemic / n_rx_clm,
      pct_drug_antiasthmatic = drug_antiasthmatic / n_rx_clm,
      pct_drug_anticoagulants = drug_anticoagulants / n_rx_clm,
      pct_drug_ophthalmic = drug_ophthalmic / n_rx_clm,
      pct_vg_sales_marketing_s = vg_sales_marketing_s / s_case,
      pct_aux_s = aux_s / s_case,
      pct_drug_antidepressants = drug_antidepressants / n_rx_clm,
      pct_renew_rewards_s = renew_rewards_s / s_case,
      pct_claims_s = claims_s / s_case,
      pct_drug_ulcer_drugs = drug_ulcer_drugs / n_rx_clm,
      pct_drug_beta_blockers = drug_beta_blockers / n_rx_clm,
      pct_edu_ben_s = edu_ben_s / s_case,
      mins_per_portal_vis = as.numeric(scale(total_time_mins / total_visits)),
      pct_view_benefits_s = view_benefits_s / s_case,
      ob_interaction = as.numeric(scale(ob_attempt_y * ob_answered_y)),
      ob_att_hc = as.numeric(scale(ob_att_2018_housecalls))
    )]
    
    clus1 <- cbind(clus1_data_filt, 
                   dat4_c1[, .(oop_per_rx_clm, pct_drug_thyroid, pct_auth_rep_s, pct_find_prov_s,
                               oon_ratio, pct_manage_rider_s, pct_assist_rx_s, pct_billing_s,
                               pct_vg, pct_pharmacy_s, pct_dispute_billing_s, pct_drug_antihypertensive,
                               pct_update_info_s, pct_drug_antihyperlipidemic, pct_drug_antiasthmatic,
                               pct_drug_anticoagulants, pct_drug_ophthalmic, pct_vg_sales_marketing_s,
                               pct_aux_s, pct_drug_antidepressants, pct_renew_rewards_s, pct_claims_s,
                               pct_drug_ulcer_drugs, pct_drug_beta_blockers, pct_edu_ben_s,
                               mins_per_portal_vis, pct_view_benefits_s, ob_interaction, ob_att_hc, ob_pos_2018_housecalls)])
    clus1[, age_sq := age_final ^ 2]
    clus1[is.na(clus1)] <- 0
    glm.fit_nothing_2 <- glm(vlapse_fg ~ 1,
                             data = clus1,
                             family = binomial,
                             weights = wts1)
    glm.fit_full_2 <- glm(vlapse_fg ~ . + ob_pos_2018_housecalls:ob_att_hc + hc_completed:ob_att_hc,
                          data = clus1,
                          family = binomial,
                          weights = wts1)
    
    c1_forward_2 <- step(glm.fit_nothing_2,
                         scope = list(lower = formula(glm.fit_nothing_2),
                                      upper = formula(glm.fit_full_2)),
                         direction = "forward")
    
    c1_coefs_2 <- data.table(summary(c1_forward_2)$coefficients,
                             var = rownames(summary(c1_forward_2)$coefficients))
    c1_coefs_2[, `:=` (lower = Estimate - 1.96*`Std. Error`,
                       upper = Estimate + 1.96*`Std. Error`)]
    ```
    
    ```{r, fig.height=10}
    tmp <- c1_coefs_2[`Std. Error` < 10]
    ggplot(tmp) +
      geom_pointrange(aes(x = var, y = Estimate, ymin = lower, 
                          ymax = upper, color = factor(sign(Estimate)))) +
      scale_x_discrete(limits = tmp[order(-Estimate)][,var]) +
      theme(legend.position = "none") +
      coord_flip() +
      labs(y = "Coefficients and 95% Confidence Intervals", x = NULL, 
           title = "Cluster 1 - Forward Selection")
    
    ```
    
    ### Bootstrapping Lasso Logistic Regression
    
    ```{r, fig.height=12}
    tmp1_all <- readRDS("Data/all_vars_lasso_coefs_cluster_1.RDS")
    ggplot(tmp1_all[abs(Mean) > .01]) +
      geom_pointrange(aes(x = variable, y = Mean, ymin = lower, ymax = upper,
                          color = factor(sign(Mean)))) +
      scale_x_discrete(limits = tmp1_all[order(-Mean)][abs(Mean) > .01,variable]) +
      coord_flip() + 
      theme(legend.position = "none") +
      labs(title = "Cluster 4 - Bootstrapping Lasso")
    ```
    
    #############Modeling codes
    
    #randomforest ----
    
    wts = c(1-as.numeric(prop.table(table(cl1$vlapse_fg))[2]),as.numeric(prop.table(table(cl1$vlapse_fg))[2]))
    
    set.seed(42)
    index <- createDataPartition(cl1$vlapse_fg, p = 0.7, list = FALSE)
    train_data <- cl1[index, ]
    test_data  <- cl1[-index, ]
    
    nzv = nearZeroVar(train_data, freqCut = 100/0)
    train_data[, c(names(train_data[,nzv, with = F])) := NULL]
    
    model_rf <- caret::train(vlapse_fg ~ .,
                             data = train_data,
                             method = "rf",
                             trControl = trainControl(method = "repeatedcv", 
                                                      number = 10, 
                                                      repeats = 10, 
                                                      verboseIter = FALSE))
    
    
    
    final <- data.frame(actual = test_data$vlapse_fg,
                        predict(model_rf, newdata = test_data, type = "prob"))
    
    cm_original <- confusionMatrix(final$predict, test_data$classes)
    
    
    ctrl <- trainControl(method = "repeatedcv", 
                         number = 10, 
                         repeats = 10, 
                         verboseIter = FALSE,
                         sampling = "down")
    
    set.seed(42)
    model_rf_under <- caret::train(classes ~ .,
                                   data = train_data,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = ctrl)
    
    
    
    final_under <- data.frame(actual = test_data$classes,
                              predict(model_rf_under, newdata = test_data, type = "prob"))
    final_under$predict <- ifelse(final_under$benign > 0.5, "benign", "malignant")
    
    
    
    cm_under <- confusionMatrix(final_under$predict, test_data$classes)
    
    
    
    
    
    Oversampling
    
    For over- (also called up-) sampling we simply specify sampling = "up".
    
    
    ctrl <- trainControl(method = "repeatedcv", 
                         number = 10, 
                         repeats = 10, 
                         verboseIter = FALSE,
                         sampling = "up")
    
    set.seed(42)
    model_rf_over <- caret::train(classes ~ .,
                                  data = train_data,
                                  method = "rf",
                                  preProcess = c("scale", "center"),
                                  trControl = ctrl)
    
    
    
    final_over <- data.frame(actual = test_data$classes,
                             predict(model_rf_over, newdata = test_data, type = "prob"))
    final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")
    
    
    
    cm_over <- confusionMatrix(final_over$predict, test_data$classes)
    
    
    
    ctrl <- trainControl(method = "repeatedcv", 
                         number = 10, 
                         repeats = 10, 
                         verboseIter = FALSE,
                         sampling = "rose")
    
    set.seed(42)
    model_rf_rose <- caret::train(classes ~ .,
                                  data = train_data,
                                  method = "rf",
                                  preProcess = c("scale", "center"),
                                  trControl = ctrl)
    
    
    
    final_rose <- data.frame(actual = test_data$classes,
                             predict(model_rf_rose, newdata = test_data, type = "prob"))
    final_rose$predict <- ifelse(final_rose$benign > 0.5, "benign", "malignant")
    
    
    
    cm_rose <- confusionMatrix(final_rose$predict, test_data$classes)
    
    
    
    
    
    SMOTE
    
    … or by choosing sampling = "smote" in the trainControl settings.
    
    
    From Nitesh V. Chawla, Kevin W. Bowyer, Lawrence O. Hall and W. Philip Kegelmeyer’s “SMOTE: Synthetic Minority Over-sampling Technique” (Journal of Artificial Intelligence Research, 2002, Vol. 16, pp. 321–357): “This paper shows that a combination of our method of over-sampling the minority (abnormal) class and under-sampling the majority (normal) class can achieve better classifier performance (in ROC space) than only under-sampling the majority class. This paper also shows that a combination of our method of over-sampling the minority class and under-sampling the majority class can achieve better classifier performance (in ROC space) than varying the loss ratios in Ripper or class priors in Naive Bayes. Our method of over-sampling the minority class involves creating synthetic minority class examples.”
    
    
    ctrl <- trainControl(method = "repeatedcv", 
                         number = 10, 
                         repeats = 10, 
                         verboseIter = FALSE,
                         sampling = "smote")
    
    set.seed(42)
    model_rf_smote <- caret::train(classes ~ .,
                                   data = train_data,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = ctrl)
    
    
    
    final_smote <- data.frame(actual = test_data$classes,
                              predict(model_rf_smote, newdata = test_data, type = "prob"))
    final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")
    
    
    
    cm_smote <- confusionMatrix(final_smote$predict, test_data$classes)
    
    
    
    
    for(i in 1:ncol(data)){
      var <- data.table()
      intr <- function(data, data[i], data[i+1]) {
        glm_n <- glm(vlapse_fg ~ get(var1) + get(var2) + get(var1)*get(var2),
                     data = data,
                     family = binomial,
                     weights = wts1)
        
        c1_coefs <- data.table(summary(glm_n)$coefficients,
                               var = rownames(summary(glm_n)$coefficients))[`Pr(>|z|)` <= 0.05]
        return (c1_coefs)
        var = c(var,c1_coefs$var)
      }
    }
    glm_n <- intr(data = clus1_data_filt, names(clus1_data_filt[,1]), names(clus1_data_filt[,2]))
    
    
    