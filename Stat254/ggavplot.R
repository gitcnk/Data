## Written by : Dason Kurkiewicz (github user: Dasonk)
## Date : Dec 2019

## My modifications:
## Date : March 28 2021
## 1. The lm smooth need to be y ~ x - 1 (we should not have the intercept)
## 2. Changed the lables of x and y axes.
## 3. Added code to include the title with the number of variables in the model.
## 4. Added a flag to switch on/off the partial regression line



avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile();
  png(filename = ff);
  OUT <- car::avPlots(MODEL, ...);
  dev.off()
  unlink(ff);
  OUT; }

ggAVPLOTS  <- function(MODEL, YLAB = NULL, LINE = 'No') {
  
  # Flag to enable the partial regression line
  mycolor <- ifelse(LINE == 'Yes',2 ,0.01)
  
  
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL);
  K       <- length(AVPLOTS);
  
  
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K);
  
  
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]]);
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'blue') + 
      geom_smooth(method = 'lm', se = FALSE, 
                  color = mycolor, formula = y ~ x - 1, linetype = 'dashed') +
      labs(title = 'Partial Correlation Plots',
           subtitle = paste('Model with' , K, 'variables', sep = ' ' ),
           x = paste0(names(DATA)[1], ' | linearly adjusted by others'),
           y = paste0(ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | linearly adjusted by others'), YLAB))
           ) }
  
  
  
  #Return output object
  GGPLOTS; }