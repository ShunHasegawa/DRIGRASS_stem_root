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
