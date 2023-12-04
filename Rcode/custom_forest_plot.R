# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: custom_forest_plot
#
# Purpose: This function creates custom forest plots for 
# my model outputs. Used in script 5.
#
# Date: 2023-12-01
# -----------------------------------------------------------

plot_data_models = function(data_model, p_title, gglayer_theme) {
  
  # #~~Test function
  # p_title <- "(a) bla"
  # data_model <- m1survival
  # data_model <- Survival.model.froh
  
  # Color non sig effects
  col1 = "dimgrey"
  # Color sig effects
  col2 = "#fa7876" #"#ea4f88"  # "#872ca2"
  
  #~~ Set up base plot
  p <- plot_model(data_model, 
                  show.intercept = F,
                  type="est",
                  #order.terms = c(1, 4, 2, 3)
                  ci.lvl=0.95, 
                  axis.labels = plot_label,
                  title=p_title,
                  colors = "black",
                  line.size=0.5,
                  vline.color = "#cccccc") # Look into theme_sjplot to make pretty #, sort.est = TRUE
  
  p <- p + gglayer_theme
  
  #p
  
  #~~ Adjust line size vertical line
  p$layers[[1]]$aes_params$size <- 1.3
  
  #~~ Possible sjPlot bug, but in the binomial model, the whiskers for one of the fixed effects are missing. Fix this:
  
  if(class(data_model)[1]=="glm") {
    if(data_model$family[1]=="binomial") {
      
      #message("passed if clause")
      
      p <- p + scale_y_log10(labels = scales::label_log())
      
      q <- ggplot_build(p)
      
      # In q$data[[3]], y = the model estimate, and ymin and ymax are the 95% CI (check with: confint(m1birthmass, level = 0.95) ) for LM
      # For the binomial model it is the transferred Odds ratio, which is done for the figure.
      # Therefore the values do NOT match the odds ratios (which one can calculate with exp(cbind(Odds_Ratio = confint(m1survival))) for example).
      # Instead the values are log10 transformed. So they match with log10(exp(cbind(Odds_Ratio = confint(m1survival))))
      
      
      # Replace ymin and ymax with correct values
      
      q$data[[3]][["ymin"]] <- log10(exp(cbind(Odds_Ratio = confint(data_model))))[-1,1]
      q$data[[3]][["ymax"]] <- log10(exp(cbind(Odds_Ratio = confint(data_model))))[-1,2]
      
    }
  } else if(class(data_model)[1]=="glmerMod") {
    
    p <- p + scale_y_log10(labels = scales::label_log())
    
    q <- ggplot_build(p)
    
    # In q$data[[3]], y = the model estimate, and ymin and ymax are the 95% CI (check with: confint(m1birthmass, level = 0.95) ) for LM
    # For the binomial model it is the transferred Odds ratio, which is done for the figure.
    # Therefore the values do NOT match the odds ratios (which one can calculate with exp(cbind(Odds_Ratio = confint(m1survival))) for example).
    # Instead the values are log10 transformed. So they match with log10(exp(cbind(Odds_Ratio = confint(m1survival))))
    
    # The values are actually the same in the q object, so it seems this problem does only occur with a glm object, not with a glmer object
    
    # q$data[[3]][["ymin"]] <- log10(exp(cbind(Odds_Ratio = confint.merMod(data_model, method = "Wald"))))[-c(1:2),1]
    # q$data[[3]][["ymax"]] <- log10(exp(cbind(Odds_Ratio = confint.merMod(data_model, method = "Wald"))))[-c(1:2):2,2]
    
    
  } else { 
    p <- p + scale_y_continuous(labels = scales::label_number())
    
    q <- ggplot_build(p) }
  
  
  
  #~~ Identify significant variables and give them a different color
  
  q$data[[3]] = q$data[[3]] %>%
    mutate(fill = ifelse(ymin < 0 & ymax > 0, col1, col2))
  
  #q$data[[2]]$colour = q$data[[3]]$fill # only necessary if you keep the dots instead of the squares
  
  # Line below to remove the dots complete. Gives a warning for missing values, but that is not a problem
  q$data[[2]]$colour = NA
  
  q$data[[4]]$fill = q$data[[3]]$fill
  
  final_plot <- ggplot_gtable(q)
  
  #plot(final_plot)
  
  return(final_plot) 
  
}