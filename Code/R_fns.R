## Functions used

### Code education
classify_education <- function(x) {
  # Convert to lowercase for consistency
  x_lower <- tolower(x)
  # Highest degrees (High)
  if (grepl("master('s)?|mla|mba|ms|ma|phd|md|jd|juris doctor|doctorate|professional degree", x_lower)) {
    return("High")
  }
  # Bachelor's degrees (Medium)
  else if (grepl("bachelor|ba/bs|b.s.|b.a.", x_lower)) {
    return("Medium")
  }
  # High school or equivalent (Low)
  else if (grepl("high school|ged|some high school|kindergarten|8th grade|no schooling", x_lower)) {
    return("Low")
  }
  # Default: set as NA if not matched
  else {
    return(NA)
  }
} 

### Attrition functions

# ----------------------------------------------------------------------
# Function 1: skip_to_attrite()
# ----------------------------------------------------------------------

#This function takes a matrix of 0,1s in which 1 indicates missingness (NA) per respondent per question and removes `skippers`. Skippers are individuals who have a 0s in their row followed by 1s.

skip_to_attrite<-function(arg){
  n_col = length(arg)
  for (j in 1:n_col)
  {
    if (prod(arg[j:n_col])==1)
    {
      arg[j] = 1
    } 
    else 
    {
      arg[j] = 0
    }
  }
  return(arg)
}

# ----------------------------------------------------------------------
# Function 2: attrition()
# ----------------------------------------------------------------------

#Function to transform dataframe into an attrition dataframe. The attrition dataframe indicates, per variable, how many respondents attrited [note that this dataframe does not include `skippers`, i.e. respondents who skipped questions]. The dataframe also includes a variable that is the proportion of total N attrited, calculated as number of attrited respondents / number of respondents entering into the question.

#only works if you know the order of survey questions. 

attrition <- function(data)
{
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  
  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data<-apply(data,1,skip_to_attrite)
  data<-t(data) #transpose data
  
  data2<-data.frame(data)
  
  #transform into a long dataframe, such that the variable `attrited` is the number of missing observations per variable.
  data <- data.frame(colSums(data2))
  colnames(data) <- "attrited"
  
  #transform `attrited` to measure how many respondents attrited during each question, rather than how many missing values are in each question.
  attrite_2<-data$attrited
  num_dropped <- data[-1,] - data[-nrow(data),]
  data$attrited<- c(data[1,], num_dropped)
  data$attrite_2<-attrite_2
  
  #add variable `proportion` = number of attrited respondents / starting N
  data$proportion <- round(data$attrited/nrow(data2),2)
  #data$n_prev <- nrow(data2) - as.numeric(data$attrite_2)
  #data$n_prev <- Lag(data$n_prev, +1)
  #data$n_prev[1] <- nrow(data2)
  #data$proportion <-    round(data$attrited/data$n_prev,2)
  #data$n_prev <- NULL
  #data$attrite_2 <- NULL
  
  #add variable `questions` = the name of each variable in the original dataframe.
  data$questions <- rownames(data)
  rownames(data) <- c()
  
  #return dataframe
  return(data)
}

# ----------------------------------------------------------------------
# Function 3: plot_attrition()
# ----------------------------------------------------------------------

#Function that allows you to plot attrition in survey data.

#`data` must be data.frame. Note that this function works only if the order of variables = order of questions in the survey.

#`freq` is a logical argument that notes the Y axis of the attrition plot. Default is freq=TRUE, which is the frequency of attrited respondents. When freq=FALSE Y axis is the proportion of total N attrited, calculated number of attrited respondents / starting N
#`treatment` is a character of name(s) of question(s) in which treatments were administered. Marked in the plot with a red vertical line.

#`pre_treatment` is a character of name(s) of pre-treatment question(s). Marked in the plot with a green vertical line.

#`DV` is a character of name(s) of outcome question(s). Marked in the plot with a blue vertical line.

#`mediator` is a character of the name of the group of variables specified in `other_group_var`. Note that both `other_group` and `other_group_var` must be specified to use one of the functions.

plot_attrition <- function(data
                           ,freq = TRUE
                           ,treatment = NULL
                           ,pre_treatment = NULL
                           ,DV = NULL
                           ,mediator = NULL
                          ,treatment_color=NULL
                          ,pre_treatment_color=NULL
                          ,DV_color=NULL
                          ,mediator_color=NULL)
{ 
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  
  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if(class(freq)!="logical")
    stop("Freq must be logical. Default is freq=TRUE.")
  
  if(!is.null(treatment) & class(treatment)!="character")
    stop("Treatment must be character")
  
  if(!is.null(pre_treatment) & class(pre_treatment)!="character")
    stop("Pre_treatment must be character")
  
  if(!is.null(DV) & class(DV)!="character")
    stop("DV must be character")
  
  if(!is.null(mediator) & class(mediator)!="character")
    stop("mediator must be character")
  
  #set colors
  if(is.null(treatment_color)) treatment_color<-"firebrick"
  if(is.null(pre_treatment_color)) pre_treatment_color<-"goldenrod3"
  if(is.null(DV_color)) DV_color<-"royalblue3"
  if(is.null(mediator_color)) mediator_color<-"seagreen"
  
  
  #Begin by creating an attrition dataframe
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data<-apply(data,1,skip_to_attrite)
  data<-t(data) #transpose data
  
  data2<-data.frame(data)
  
  #transform into a long dataframe, such that the variable `attrited` is the number of missing observations per variable.
  data <- data.frame(colSums(data2))
  colnames(data) <- "attrited"
  
  #transform `attrited` to measure how many respondents attrited during each question, rather than how many missing values are in each question.
  attrite_2<-data$attrited
  num_dropped <- data[-1,] - data[-nrow(data),]
  data$attrited<- c(data[1,], num_dropped)
  data$attrite_2<-attrite_2
  
  #add variable `proportion` = number of attrited respondents / number of respondents entering into the question
  data$n_prev <- nrow(data2) - as.numeric(data$attrite_2)
  data$n_prev <- Lag(data$n_prev, +1)
  data$n_prev[1] <- nrow(data2)
  data$proportion <-   round(data$attrited/data$n_prev,2)
  data$n_prev <- NULL
  data$attrite_2 <- NULL
  
  #add variable `questions` = the name of each variable in the original dataframe.
  data$questions <- rownames(data)
  rownames(data) <- c()
  data$questions <- factor(data$questions, levels=data$questions)
  
  #Next, plot attrition
  #set colors for plots
  
  tmp_colors<-viridis(n=2,alpha=0.6,begin=0.25,end=1,direction=1,option="D")
  
  #create figure for if treatment is not NULL and freq = TRUE
  if(!freq){
  p <- ggplot(data, aes(x=questions,y=proportion)) + 
    geom_bar(stat = 'identity') +
    geom_histogram(color="#e9ecef", alpha=0.6, stat = 'identity') +
    scale_fill_manual(values=tmp_colors) 
}else{
  p <- ggplot(data,aes(questions,attrited)) + 
    geom_histogram(color="#e9ecef", alpha=0.6, stat = 'identity') +
    scale_fill_manual(values=tmp_colors) 
}
  
  #vlines
  if(freq){line_data<-data$attrited
    }else{line_data<-data$proportion}
  
  #add vline for treatment, only if it isn't null   
  if(!is.null(treatment)){
    p <- p + geom_vline(data= data.frame(type="Treatment", col="Treatment", treatment = treatment),
                        aes(colour=col, xintercept = which(data$questions%in%treatment)), 
                        size = 0.7, show.legend = TRUE)
    }
  #add vline for pre_treatment, only if it isn't null   
  if(!is.null(pre_treatment)){
    p <- p + geom_vline(data= data.frame(type="Pre-Treatment", col="Pre-Treatment", pre_treatment = pre_treatment),
                        aes(colour=col, xintercept = which(data$questions%in%pre_treatment)), 
                        size = 0.7, show.legend = TRUE)}
  #add vline for DV, only if it isn't null   
  if(!is.null(DV)){
    p <- p + geom_vline(data= data.frame(type="Outcome", col="Outcome", DV = DV),
                        aes(colour=col, xintercept = which(data$questions%in%DV)), 
                        size = 0.7, show.legend = TRUE)} 
  
  #add vline for mediator, only if it isn't null
  if(!is.null(mediator)) {
    p <- p + geom_vline(data= data.frame(type="Happy Mediator", col="Happy Mediator", mediator = mediator),
                        aes(colour=col, xintercept = which(data$questions%in%mediator)), #other_group_var), 
                        size = 0.7, show.legend = TRUE)
  } 
  
  #delete gray background  
  if(!freq){title_y<-"Proportion of respondents attrited"}else{title_y<-"Respondents attrited"}
  p <- p + theme(panel.grid.major = element_blank()
                 ,panel.grid.minor = element_blank()
                 ,panel.background = element_blank()
                 ,axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
    scale_colour_manual(name="Legend" ,breaks = c("Treatment","Pre-Treatment","Outcome","Happy Mediator")
                              ,labels = c("Treatment","Pre-Treatment","Outcome","Happy Mediator")
                              ,values = unname(c(treatment_color,pre_treatment_color,DV_color,mediator_color))) +
    labs(x = "Survey Questions") + #titles
    labs(y = title_y)
  
  return(p)
}


### Manski bound functions
compute_manski_bounds <- function(df, a = 0, b = 1) {
  # Inputs: df with columns Z (0/1) and Y (may be NA)
  # Returns: list with point estimates for bounds and components
  require(stats)
  # sample sizes
  n <- nrow(df)
  n1 <- sum(df$Z == 1)
  n0 <- sum(df$Z == 0)
  
  # observed response indicators
  R <- ifelse(is.na(df$Y), 0, 1)
  
  # response rates by arm
  rz1 <- mean(R[df$Z == 1])
  rz0 <- mean(R[df$Z == 0])
  
  # observed means among responders in each arm (NA if no responders)
  y1_obs <- if (sum(df$Z == 1 & R == 1) > 0) mean(df$Y[df$Z == 1 & R == 1]) else NA
  y0_obs <- if (sum(df$Z == 0 & R == 1) > 0) mean(df$Y[df$Z == 0 & R == 1]) else NA
  
  # bounds for mean potential outcomes
  mu1_lower <- rz1 * y1_obs + (1 - rz1) * a
  mu1_upper <- rz1 * y1_obs + (1 - rz1) * b
  mu0_lower <- rz0 * y0_obs + (1 - rz0) * a
  mu0_upper <- rz0 * y0_obs + (1 - rz0) * b
  
  # Manski bounds for ATE
  ate_lower <- mu1_lower - mu0_upper
  ate_upper <- mu1_upper - mu0_lower
  
  res <- list(
    n = n, n1 = n1, n0 = n0,
    rz1 = rz1, rz0 = rz0,
    y1_obs = y1_obs, y0_obs = y0_obs,
    mu1_lower = mu1_lower, mu1_upper = mu1_upper,
    mu0_lower = mu0_lower, mu0_upper = mu0_upper,
    ate_lower = ate_lower, ate_upper = ate_upper
  )
  return(res)
}
# Bootstrap for sampling uncertainty of bound endpoints
bootstrap_manski <- function(df, a = 0, b = 1, Rboot = 2000, seed = 1234) {
  set.seed(seed)
  n <- nrow(df)
  boot_ates <- matrix(NA, nrow = Rboot, ncol = 2)
  colnames(boot_ates) <- c("ate_lower", "ate_upper")
  
  for (i in 1:Rboot) {
    ids <- sample.int(n, size = n, replace = TRUE)
    dfb <- df[ids, , drop = FALSE]
    out <- compute_manski_bounds(dfb, a = a, b = b)
    boot_ates[i, "ate_lower"] <- out$ate_lower
    boot_ates[i, "ate_upper"] <- out$ate_upper
  }
  
  # point estimates from original sample
  orig <- compute_manski_bounds(df, a = a, b = b)
  
  # Construct percentile bootstrap CIs for each bound endpoint
  ci_lower <- quantile(boot_ates[, "ate_lower"], probs = c(0.025, 0.975), na.rm = TRUE)
  ci_upper <- quantile(boot_ates[, "ate_upper"], probs = c(0.025, 0.975), na.rm = TRUE)
  
  list(
    point = list(ate_lower = orig$ate_lower, ate_upper = orig$ate_upper),
    boot_samples = boot_ates,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

