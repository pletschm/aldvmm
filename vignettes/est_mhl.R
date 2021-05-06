est_mhl <- function(fit, ngroup) {
  
  # Select sample
  yhat <- fit[["pred"]][["yhat"]]
  res <- fit[["pred"]][["res"]]
  
  nsample <- length(yhat)
  sample <- sample(1:length(yhat), nsample, replace = FALSE)
  
  yhat <- yhat[sample]
  res <- res[sample]
  
  # Make groups
  group <- as.numeric(cut(yhat, breaks = ngroup), na.rm = TRUE)
  
  # Auxiliary regression
  fit <- lm(res ~ factor(group))
  
  # Data set of predictions from auxiliary regressions
  tmpdat <- data.frame(group = unique(group)[order(unique(group))])
  predict <- predict(fit, 
                     newdata = tmpdat, 
                     se.fit = TRUE, 
                     interval = 'confidence', 
                     level = 0.95)
  
  plotdat <- as.data.frame(rbind(
    cbind(group = tmpdat$group, 
          outcome = "mean",
          value = predict$fit[ , 'fit']),
    cbind(group = tmpdat$group, 
          outcome = "ll",
          value = predict$fit[ , 'lwr']),
    cbind(group = tmpdat$group, 
          outcome = "ul",
          value = predict$fit[ , 'upr'])
  ))
  
  
  # Make plot
  lab <- paste0(seq(from = 1/ngroup, to = 1, by = 1/ngroup)*100, '%')
  
  plot <- ggplot2::ggplot(plotdat, 
                          aes(x = factor(as.numeric(group)), 
                              y = as.numeric(value), 
                              group = factor(outcome))) +
    geom_line(aes(linetype = factor(outcome))) +
    geom_hline(yintercept = 0) +
    scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
    scale_x_discrete(breaks = 1:ngroup, labels = lab) +
    coord_cartesian(ylim = c(-0.2, 0.2)) +
    xlab("Precentiles of expected values") +
    ylab("Mean residuals") +
    ggplot_theme +
    theme(legend.position = "none")
  
  return(plot)
  
}