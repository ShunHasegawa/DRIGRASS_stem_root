# compute SE
se <- function(...){
  ci(...)[4]
}





# compute number of observations
get_n <- function(x){
  sum(!is.na(x))
}




# this function generates box-whisker plots with common transformations

create_trans_boxplot <- function(x, data, ...){ # x = formula
  
  # get box-cox value
  a <- boxcox(x, data = data, plotit = FALSE, ...)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black")  # use red color for negative values
  
  # create formulas for each transformation
  f_cha     <- as.character(x)
  f_log     <- as.formula(paste("log(", f_cha[2], ") ~ ", f_cha[3]))
  f_sqrt    <- as.formula(paste("sqrt(", f_cha[2], ") ~ ", f_cha[3]))
  f_pw      <- as.formula(paste("(", f_cha[2], ")^(1/3) ~ ", f_cha[3]))
  f_inv     <- as.formula(paste("1 / (", f_cha[2], ") ~ ", f_cha[3]))
  f_boxcox  <- as.formula(paste("(", f_cha[2], ")^(BCmax) ~ ", f_cha[3]))
  f_list    <- list('raw' = x,
                    'log' = f_log,
                    'sqrt' = f_sqrt,
                    'power(1/3)' = f_pw,
                    'inverse' = f_inv)
  par(mfrow = c(2, 3))
  l_ply(names(f_list), function(x) boxplot(f_list[[x]], data = data, main = x))
  boxplot(f_boxcox, main = "", sep = "=", data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), col.main = texcol)
  par(mfrow = c(1,1))
}




ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}




save_png600 <- function(...){
  png(..., res = 600, units = "in")
}




# This funciton plot model diagnosing plots
plot_diag <- function(model, ...){
  par(mfrow = c(2, 2))
  plot(model, ...)
  mfrow = c(1,1)
}




# set graphic parameters --------------------------------------------------


theme_set(theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

# define graphic background
science_theme <- theme(panel.border      = element_rect(color = "black"),
                       panel.grid.major  = element_blank(), 
                       panel.grid.minor  = element_blank(), 
                       legend.position   = c(.91, .91),
                       legend.title      = element_blank(),
                       legend.background = element_blank(),
                       legend.key        = element_blank(),
                       legend.key.width  = unit(2.5, "lines"),
                       legend.key.height = unit(.8, "lines"),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.text.x       = element_text(margin = margin(5)),
                       axis.text.y       = element_text(margin = margin(0, 5)),
                       axis.title.y      = element_text(margin = margin(0, 10)))



## this performs anova by lm for given data and formula, and then generates 
## summary table with assocaited stats

get_all_anova_tbl <- function(.data, formula){
  
  tryCatch({                                                                      # some spp don't have sufficient replicates to run ANOVA and return error. tryCatch prevent error from stoping a loop
    
    
    rm(m, anova_m, treat_p, fval, symb, symbol_d)
    
    ## perform anova
    m <- lm(formula, data = .data)
    
    if(exists("m")){
      anova_m <- anova(m)
      treat_p <- anova_m$`Pr(>F)`[1]                                                # get P value for treatment
      fval    <- paste0("F(", anova_m$Df[1], ",", anova_m$Df[2], ")=",              # get F value and associated DF
                        round(anova_m$`F value`[1], 2))
      
      
      ### post-hoc test when treatment was significant
      if(!is.na(treat_p) & treat_p <= 0.05){
        symb <- cld(glht(m, linfct = mcp(treatment = "Tukey")),                  # get symbols representing significant difference between treatmens 
                    decreasing = TRUE)$mcletters$Letters
        symbol_d <- data.frame(treatment = names(symb), symb, row.names = NULL) # store the result in df
      } else {
        symbol_d <- data.frame(treatment = unique(.data$treatment), symb = "")   # if there's no significant treatment effect, a post-hoc test is not performed
      }
    
      
    }
  }, error = function(e){                                                         # action when error occurs
    cat("ERROR :",conditionMessage(e), "\n")                                      # return error messgae
  })
  
  
  ## when above codes didn't generate the following object
  if(!exists("m"))        m        <- NA
  if(!exists("fval"))     fval     <- NA
  if(!exists("treat_p"))  treat_p  <- NA
  if(!exists("symbol_d")) symbol_d <- data.frame(treatment = unique(.data$treatment), symb = "")     # no post-hoc test
    
  
  ## create summary table (Mean(SE) and significant symbols)
  summary_tbl <- .data %>% 
    group_by(treatment, spp, dmclass) %>%                                         # summarise for each group
    summarise_each(funs(M = mean(., na.rm = TRUE), SE = se(., na.rm = TRUE)), -plot) %>%                            # mean and SE of original value
    ungroup() %>%  
    left_join(symbol_d, by = "treatment") %>%                                     # merge with a post-hoc table 
    transmute(spp, dmclass, treatment,                                            # concatenate mean, se and symbols
              value = paste0(round(M, 2), "(", round(SE, 2), ")", symb)) %>% 
    spread(treatment, value) %>%                                                  # turn df to a wide format
    mutate('F' = fval, P = round(treat_p, 3)) %>%                                 # add P-value for treatment
    select(dmclass, spp, F, P, everything())                                      # reorder columns
  
  
  l <- list('model' = m, 'summary_tbl' = summary_tbl)                             # store the model and summary table in a list for output           
  return(l)
}




## this runs F test for given model, and perfome post-hoc test, and then 
## generates summary table by spp
get_all_anova_tbl_bySpp <- function(model, .data, variable){
  # .data: data frame to create summary table
  # variable: variable to be sumamrised
  
  
  ## F test
  anova_m <- Anova(model, test.statistic = "F")       # F test
  pval    <- round(anova_m$`Pr(>F)`, 3)[1]            # pval
  fval    <- paste0("F(",                             # F value and associated DFs
                    round(anova_m$Df[1], 0), ",", 
                    round(anova_m$Df.res[1], 0), ")=", 
                    round(anova_m$F[1], 2))
  
  
  ### post-hoc
  symbols_d <- cld(lsmeans::lsmeans(model, spec = "spp"),  Letters = letters) %>%  # get symbols representing significant difference between treatmens 
    select(spp, .group) %>% 
    mutate(.group = gsub(" ", "", .group))
  
  
  ## sumamry table
  smmry_tbl <- .data %>% 
    group_by(spp) %>%                                                               # summarise for each group
    summarise_each_(funs(M = mean(., na.rm = TRUE), SE = se(., na.rm = TRUE)), variable) %>%   # mean and SE of original value
    ungroup() %>% 
    left_join(symbols_d, by = "spp") %>%                                            # merge with a post-hoc table 
    transmute(spp, value = paste0(round(M, 2), "(", round(SE, 2), ")", .group)) %>% # concatenate mean, se and symbols
    spread(spp, value) %>% 
    mutate('F' = fval, P = pval)                                                    # add P and F value for spp
  
  
  return(smmry_tbl)
}