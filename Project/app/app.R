######################### BEGIN OF SCRIPT #################################

library(shiny)
library(plyr)

############## RANDOMFOREST: RANDOM FOREST ######################

# Initiate library
library('randomForest'); library("pROC")

# Define function:
Random.Forest <- function(
  all = all,
  cutoff = .9, 
  num.tree = 10,
  num.try = sqrt(ncol(all)),
  cutoff.coefficient = 1,
  SV.cutoff = 1:10
) {
  
  #all = all
  #cutoff = .8
  #num.tree = 10,
  #num.try = sqrt(ncol(all))#,
  #cutoff.coefficient = 1
  #SV.cutoff = 1:10
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- randomForest(
    x = as.matrix(train.x),
    y = as.factor(train.y),
    xtest = as.matrix(test.x),
    ytest = as.factor(test.y),
    ntree = num.tree,
    mtry = num.try
  )
  sum <- summary(model)
  
  # Extract imporance
  feature.and.score <- data.frame(model$importance)
  feature.score <- feature.and.score[order(feature.and.score, decreasing = TRUE), ]
  feature.order <- rownames(feature.and.score)[order(feature.and.score, decreasing = TRUE)]
  new.feature.and.score <- data.frame(cbind(feature.order, feature.score))
  head(new.feature.and.score)
  #SV.cutoff = 1:5
  selected.variable <- feature.order[SV.cutoff]
  selected.variable
  
  # Make prediction on training:
  preds.train <- model$predicted
  preds.train[is.na(preds.train) == TRUE] <- 0
  #preds.mean.train <- mean(preds.train)
  #preds.train <- ifelse(preds.train > preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2]); tab.train
  percent.train <- sum(diag(tab.train))/sum(tab.train); percent.train
  
  # ROC
  actuals <- train.y
  scores <- as.numeric(preds.train)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc; auc.train
  
  # Make prediction on testing:
  #preds.binary <- model$test$predicted # colMeans(model$yhat.test)
  preds.probability <- model$test$votes[,2]
  preds.mean <- mean(preds.probability)
  preds.binary <- ifelse(preds.probability > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds.binary,test.y)
  ); dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table); percent
  
  # ROC
  actuals <- test.y
  scores <- preds.binary 
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc; auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, model$test$votes[,2])
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = model,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      Important.Variables = selected.variable,
      y.hat = preds.binary,
      y.truth = test.y,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob
      #AUC.Plot = plot(
      #  1 - spec, sens, type = "l", col = "red", 
      #  ylab = "Sensitivity", xlab = "1 - Specificity")
    )
  )
} # End of function

###################### iRF: ITERATIVE RANDOM FOREST ############################

# Library
library("iRF"); library("pROC")

# Define function:
iter.Random.Forest <- function(
  all = all,
  cutoff = .9,
  num.tree = 5,
  num.iter = 5
) {
  
  #all = all
  #cutoff = .8
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- iRF(
    x = as.matrix.data.frame(train.x),
    y = as.factor(train.y),
    xtest = as.matrix.data.frame(test.x),
    ytest = as.factor(test.y),
    n.iter = num.iter,
    ntree = num.tree,
    verbose = FALSE
  )
  sum <- model$rf.list[[num.iter]]
  
  # Make prediction on training:
  preds.train <- sum$predicted  # colMeans(model$yhat.train)
  preds.train[is.na(preds.train) == TRUE] <- 0
  #preds.mean.train <- mean(preds.train)
  #preds.train <- ifelse(preds.train > preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2]); tab.train
  percent.train <- sum(diag(tab.train))/sum(tab.train); percent.train
  
  # ROC
  actuals <- train.y
  scores <- as.numeric(preds.train)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc
  
  # Make prediction on testing:
  preds <- sum$test$predicted # colMeans(model$yhat.test)
  #preds.mean <- mean(preds)
  #preds <- ifelse(preds > preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  ); dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table); percent
  
  # ROC
  actuals <- test.y
  scores <- sum$test$votes[,2] # colMeans(model$yhat.test)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, sum$test$votes[,2])
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      y.hat = preds,
      y.truth = test.y,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1, 
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob
      #AUC.Plot = plot(
      #  1 - spec, sens, type = "l", col = "red", 
      #  ylab = "Sensitivity", xlab = "1 - Specificity")
    )
  )
} # End of function

######################## BART CLASSIFICATION: BAYESIAN ADDITIVE REGRESSION TREE ####################

# Library
library("BayesTree"); library("pROC")

# Define function:
bayesian.additive.regression.tree <- function(
  all = all,
  cutoff = .9,
  num.tree = 5,
  num.cut = 100,
  cutoff.coefficient = 1
) {
  
  #all = all
  #cutoff = 0.9
  #cutoff.coefficient = 1
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- bart(
    x.train = train.x,
    y.train = train.y,
    x.test  = test.x,
    verbose = FALSE,
    ntree = num.tree,
    numcut = num.cut)
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train.prob <- colMeans(model$yhat.train)
  preds.mean.train <- mean(preds.train.prob)
  preds.train <- ifelse(preds.train.prob > cutoff.coefficient*preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2]); tab.train
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- as.numeric(preds.train.prob)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc
  
  # Make prediction on testing:
  preds.prob <- colMeans(model$yhat.test)
  preds.mean <- mean(preds.prob)
  preds <- ifelse(preds.prob > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  )
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- colMeans(model$yhat.test)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, colMeans(model$yhat.test))
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      y.hat = preds,
      y.truth = test.y,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob
      #AUC.Plot = plot(
      #  1 - spec, sens, type = "l", col = "red", 
      #  ylab = "Sensitivity", xlab = "1 - Specificity")
    )
  )
} # End of function

################# BEGIN: Discrete ISCORE FAST ##########################

# Package
library(knitr)

# Define iscore function and BDA
# Using discrete function
discrete.vs <- function(
  all = all,
  cut_off = 0.9,
  num.initial.set = 8,
  how.many.rounds = 30,
  num.top.rows = 10,
  seed = 1
) {
  
  # Define function: Influence Measure (used to be GTD)
  influence.measure <- function(x.case,x.ctrl){ 
    var.num<-dim(x.case)[2]
    if(is.null(dim(x.case))) {
      nd<-length(x.case)
      nc<-length(x.ctrl)
      var.num  = 1
      cells.num = 3
    }else{
      nd<-dim(x.case)[1]
      nc<-dim(x.ctrl)[1]
      cells.num<-3^var.num
    }
    cell.case <- rep(0,cells.num)
    cell.ctrl <- rep(0,cells.num)
    if(var.num > 1){
      digits<-3^seq(0,var.num-1,1)
      ind.case<- as.vector(as.matrix(x.case) %*% digits)
      ind.ctrl<- as.vector(as.matrix(x.ctrl) %*% digits)
      uni.case<- sort(unique(ind.case)) 
      uni.ctrl<- sort(unique(ind.ctrl)) 
      for(i in 1:length(uni.case)) cell.case[uni.case[i]+1] <- sum(ind.case==uni.case[i])
      for(i in 1:length(uni.ctrl)) cell.ctrl[uni.ctrl[i]+1] <- sum(ind.ctrl==uni.ctrl[i])
    }else{
      uni.case<- sort(unique(x.case)) 
      uni.ctrl<- sort(unique(x.ctrl))
      for(i in 1:length(uni.case)) cell.case[uni.case[i]+1] <- sum(x.case==uni.case[i])
      for(i in 1:length(uni.ctrl)) cell.ctrl[uni.ctrl[i]+1] <- sum(x.ctrl==uni.ctrl[i])
    }
    y.bar<-nd/(nd+nc)
    y.se<-sd(rep(c(1,0),c(nd,nc)))
    prop<-ifelse((cell.case/(cell.case+cell.ctrl))=="NaN",0,cell.case/(cell.case+cell.ctrl))
    joint.score <- sum((prop - y.bar)^2 * (cell.case+cell.ctrl)^2)/(sum(cell.case+cell.ctrl)*y.se^2)
    return(joint.score)
  }
  
  # Define function: BDA
  Backward.Dropping.Algo <- function(case,ctrl,var.set){
    use.ind<-var.set                                                   
    allset<-NULL
    allscore<-NULL
    start<-use.ind
    allscore<-influence.measure(case[,start],ctrl[,start]) 
    for(i in length(use.ind):2){
      tmp.index<-use.ind[1:i]
      tmp.score<-c()
      for(ii in 1:length(tmp.index)){
        ind<-setdiff(tmp.index,tmp.index[ii])
        tmp.score<-c(tmp.score,influence.measure(case[,ind],ctrl[,ind]))
      }    
      max.ind<-which.max(tmp.score)
      allscore<-c(allscore,tmp.score[max.ind])
      
      use.ind<-setdiff(use.ind,tmp.index[max.ind])
      allset<-c(allset,tmp.index[max.ind])
    }
    allset<-c(allset,use.ind)
    return(list(back.score=allscore,drop.ind=allset))
  }
  
  # Define iscore function
  score <-  function(
    x.case = case,
    x.ctrl = ctrl,
    k = num.initial.set, 
    LC = ncol(case)
  ){
    sel <- sort(sample(1:(LC),k))
    temp <- Backward.Dropping.Algo(x.case,x.ctrl, sel)
    c(
      paste(sort(temp$drop.ind[which.max(temp$back.score):k]),collapse="_"),
      max(temp$back.score)
    )
  }
  
  # Compute iscore
  set.seed(seed)
  
  # Split:
  cutoff <- cut_off
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Redefine to fit for original iscore
  x_pop <- train.x
  y_pop <- train.y
  case <- x_pop[which(y_pop==1),]
  ctrl <- x_pop[which(y_pop==0),]
  rm(list=c("x_pop","y_pop"))
  
  # Compute iscore for each draw
  result <- t(replicate(how.many.rounds, score(x.case = case, x.ctrl = ctrl)))
  
  # Sort according to iscore
  all.result <- result[order(as.numeric(result[,2]), decreasing = TRUE),]
  all.variables <- all.result
  all.var <- NULL
  for (which.row in 1:nrow(all.variables)) {
    #which.row = 4
    selected.variable <- all.variables[which.row, ]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    if (length(selected.variable) <= 2) {
      all.var <- rbind(all.var)
    } else {
      all.var <- rbind(all.var, all.variables[which.row, ])
    }
  } # End of loop
  all.var.new <- all.var
  colnames(all.var.new) <- c("Top Module", "Measure")
  
  # output
  all.result <- all.var.new; all.result <- unique(all.result)
  num.top.rows <- ifelse(num.top.rows <= nrow(all.result), num.top.rows, nrow(all.result))
  top.result <- all.result[1:num.top.rows, ]
  
  # Output
  return(list(
    All.BDA.Modules = all.result,
    Top.BDA.Modules = top.result,
    Top.BDA.Modules.Pretty = kable(top.result)
  )) # Finished output
} # End of function

# Library
library(gtools)

# Define function to construct discrete variable
disc_var <- function(
  all = all,
  selected.variable = selected.variable
) {
  # Now we put variables together, **variable.cluster**
  # and we create assign matrix, **variable.cluster.assign**
  variable.cluster <- data.frame(
    #cbind(all[,c(as.character(selected.variable))]) # My Iscore Function
    all[,c(selected.variable)] # Tian Iscore Function
  )
  count.summary <- plyr::count(apply(variable.cluster, 1, paste0, collapse="_"))
  variable.cluster.collapse <- cbind(apply(variable.cluster, 1, paste0, collapse="_"))
  
  # Create new variable, a new cluster, based on selected variables
  new.variable <- matrix(0,nrow=nrow(variable.cluster),ncol=1)
  for (i in 1:nrow(count.summary)) {
    new.variable[
      variable.cluster.collapse == count.summary[i,1],
      ] <- count.summary$freq[i]/sum(count.summary$freq)
  }; colnames(new.variable) <- paste0(selected.variable, collapse = "___")
  
  # Output
  return(new.variable)
} # End of function

# Now
# Let us code a function to create modules based on top nth BDA result.
# input: an index, i.e. 1, 2, 3, ..., n such that n < B 
#        and B is the total number of rounds of BDA conducted 
# output: the module, i.e. a newly defined variable, based on 
#         the selected nth BDA result
new.module <- function(
  all = all,
  which.row = 1,
  Result = Result
){
  # Selected Variable
  selected.variable <- Result$All.BDA.Modules[which.row, 1]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- sort(unique(as.numeric(selected.variable)))
  selected.variable
  
  # Names of Selected Variable(s)
  names.of.selected <- colnames(all)[-1][c(selected.variable)]
  new.var <- disc_var(all, names.of.selected)
  
  # Output
  return(new.var)
} # End of function

################## GLM: LOGISTIC ####################################

# Library
library(pROC)

# Define function:
logistic <- function(
  all = all,
  cutoff = .67,
  fam = binomial,
  cutoff.coefficient = 1) {
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- data.frame(train[,-1]); colnames(train.x) <- colnames(train)[-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- data.frame(test[,-1]); dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  # GLM or # LM
  model <- glm(
    train.y ~.,
    data = train.x,
    family = fam
    # gaussian
    # binomial
    # quasibinomial
  )
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train.prob <- predict(model, train.x)
  preds.mean.train <- mean(preds.train.prob)
  preds.train <- ifelse(preds.train.prob > cutoff.coefficient*preds.mean.train, 1, 0)
  table.train <- as.matrix(cbind(preds.train,train.y))
  tab.train <- table(table.train[,1], table.train[,2])
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- preds.train.prob
  roc_obj.train <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj.train$auc
  
  # Make prediction on testing:
  colnames(test.x) <- colnames(train.x)
  preds.prob <- predict(model, test.x) # nrow(test.x)
  preds.mean <- mean(preds.prob)
  preds <- ifelse(preds.prob > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  )
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- preds.prob
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, predict(model, test.x))
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      #Train.ROC = plot(roc_obj.train),
      y.hat = preds,
      y.truth = test.y,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1#,
      #Test.ROC = plot(roc_obj)
    )
  )
} # End of function

######################### DEFINE: UI #################################

# Define UI:
ui <- fluidPage(
  titlePanel("Script for IScore with Balanced Case/Control and Performance with Tree-Based Competitors"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Sample Size:", 100),
      numericInput("b", "Number of times to run backward dropping:", 500),
      numericInput("m", "Number of modules to report:", 5),
      numericInput("r", "Number of bootstrap samples for bias:", 20),
      numericInput("s", "Seed:", 1),
      br(),
      sliderInput("p1", label = h5("Variables in module 1:"), min = 1, 
                  max = 50, value = c(1, 2)),
      sliderInput("p2", label = h5("Variables in module 2:"), min = 1, 
                  max = 50, value = c(2, 4)),
      br(),
      submitButton("Run Script")
    ),
    
    mainPanel(
      h4("Model:"),
      br(),
      uiOutput('formula'),
      br(),
      br(),
      br(),
      h4("Top Sets after Backward Dropping:"),
      br(),
      tableOutput("view"),
      br(),
      br(),
      br(),
      h4("Performance of IScore and Competitors:"),
      br(),
      tableOutput("performance"),
      br(),
      br(),
      br(),
      h6("The script takes a long time. Please load the page and go grab a cup of coffee!")
    )
  ),
  
  tags$head(
    conditionalPanel(condition="input.goButton > 0 | $('html').hasClass('shiny-busy')",
                     tags$div(style="float:right; padding-right:100px; padding-top:100px; color:black;
                              background-color:white; font-family:arial; font-size:18 px",
                              "Calculating... Please wait...")
    ))
  )


######################### DEFINE: SERVER #################################

# Define Server:
server = function(input, output) {
  
  ################ DEFINE: fomula #################
  output$formula <- renderUI({
    withMathJax(helpText(  
      paste(
        "\\begin{equation} y =
        \\left\\{
        \\begin{array}{lcl}",
        paste("x_{",input$p1[1]:input$p1[2],collapse="}+",sep=""),"} & \\text{ (mod 2)} \\\\",
        paste("x_{",input$p2[1]:input$p2[2],collapse="}+",sep=""), "} & \\text{ (mod 2)}
        \\end{array}
        \\right.
        \\end{equation}
        ",sep="")
      ))
    })
  
  output$range <- renderPrint({ input$p1 })
  
  ################ DEFINE: VIEW #################
  # Show the first "n" observations
  output$view <- renderTable({
    
    GTD <- function(x.case,x.ctrl){ 
      var.num<-dim(x.case)[2]
      if(is.null(dim(x.case))) {
        nd<-length(x.case)
        nc<-length(x.ctrl)
        var.num  = 1
        cells.num = 3
      }else{
        nd<-dim(x.case)[1]
        nc<-dim(x.ctrl)[1]
        cells.num<-3^var.num
      }
      cell.case <- rep(0,cells.num)
      cell.ctrl <- rep(0,cells.num)
      if(var.num > 1){                  
        digits<-3^seq(0,var.num-1,1)
        ind.case<- as.vector(as.matrix(x.case) %*% digits)
        ind.ctrl<- as.vector(as.matrix(x.ctrl) %*% digits)
        uni.case<- sort(unique(ind.case)) 
        uni.ctrl<- sort(unique(ind.ctrl)) 
        for(i in 1:length(uni.case)) cell.case[uni.case[i]+1] <- sum(ind.case==uni.case[i])
        for(i in 1:length(uni.ctrl)) cell.ctrl[uni.ctrl[i]+1] <- sum(ind.ctrl==uni.ctrl[i])
      }else{
        uni.case<- sort(unique(x.case)) 
        uni.ctrl<- sort(unique(x.ctrl))
        for(i in 1:length(uni.case)) cell.case[uni.case[i]+1] <- sum(x.case==uni.case[i])
        for(i in 1:length(uni.ctrl)) cell.ctrl[uni.ctrl[i]+1] <- sum(x.ctrl==uni.ctrl[i])
      }
      y.bar<-nd/(nd+nc)
      y.se<-sd(rep(c(1,0),c(nd,nc)))
      prop<-ifelse((cell.case/(cell.case+cell.ctrl))=="NaN",0,cell.case/(cell.case+cell.ctrl))
      joint.score <- sum((prop - y.bar)^2 * (cell.case+cell.ctrl)^2)/(sum(cell.case+cell.ctrl)*y.se^2)
      return( joint.score)
    }
    
    BGTA <- function(case,ctrl,var.set){
      use.ind<-var.set                                                   
      allset<-NULL
      allscore<-NULL
      start<-use.ind
      allscore<-GTD(case[,start],ctrl[,start]) 
      for(i in length(use.ind):2){
        tmp.index<-use.ind[1:i]
        tmp.score<-c()
        for(ii in 1:length(tmp.index)){
          ind<-setdiff(tmp.index,tmp.index[ii])
          tmp.score<-c(tmp.score,GTD(case[,ind],ctrl[,ind]))
        }    
        max.ind<-which.max(tmp.score)
        allscore<-c(allscore,tmp.score[max.ind])
        
        use.ind<-setdiff(use.ind,tmp.index[max.ind])
        allset<-c(allset,tmp.index[max.ind])
      }
      allset<-c(allset,use.ind)
      return(list(back.score=allscore,drop.ind=allset))
    }
    
    score <-  function(x.case=case,x.ctrl=ctrl,k = 8,LC = ncol(case)){
      sel <- sort(sample(1:(LC),k))
      temp <- BGTA(x.case,x.ctrl, sel)
      c(
        paste(sort(temp$drop.ind[which.max(temp$back.score):k]),collapse="_"),
        max(temp$back.score)
      )
    }
    
    n <- input$n # sample size
    m <- input$m # number to report
    b <- input$b #number of bdrop
    s <- input$s #seed number
    r <- input$r #number of bootstrap samples
    p <- 1e3 #population size
    set.seed(s)
    
    
    #    n <- 100
    #    m <- 5
    #    b <- 100
    #    p <- 1e5
    #    s <- 1    
    #   input <- list(p1 = c(1,2),p2=c(2,3,4))
    
    x_pop <- sapply(1:50,function(x) rbinom(p,1,.5))
    colnames(x_pop) <- paste("X",1:50,sep="")
    y_pop <- sapply(1:p, function(i) ifelse(rbinom(1,1,.5)==0,
                                            (sum(x_pop[i,input$p1[1]:input$p1[2]])) %% 2,
                                            (sum(x_pop[i,input$p2[1]:input$p2[2]])) %% 2))
    case <- x_pop[which(y_pop==1)[1:(n/2)],]
    ctrl <- x_pop[which(y_pop==0)[1:(n/2)],]
    
    rm(list=c("x_pop","y_pop"))
    
    result <- t(replicate(b,score(x.case=case,x.ctrl=ctrl)))
    result <- result[!duplicated(result),]
    result <- result[order(as.numeric(result[,2]),decreasing = TRUE),][1:m,]
    result[,2] <- round(as.numeric(result[,2]),2)
    
    iscore_n <- as.numeric(result[,2])/n
    lower_bound <- .5+.25*sqrt(8*as.numeric(result[,2])/n)
    
    upper <- function(x){
      index <- as.numeric(unlist(strsplit(x,"_")))
      if(length(index)==1){
        n_d <- length(case[,index])
        n_c <- length(ctrl[,index])
        f_d <- table(case[,index])/n_d
        f_c <- table(ctrl[,index])/n_c
        upper_bound <- .5 + .25*sum(abs(f_d - f_c))
      } else{
        n_d <- nrow(case[,index])
        n_c <- nrow(ctrl[,index])
        partitions <- apply(
          expand.grid(data.frame(matrix(rep(c(0,1), length(index)),ncol=length(index)))),
          1, paste,collapse="_")
        f_d <- data.frame(table(apply(case[,index],1,paste,collapse="_"))/n_d)
        f_c <- data.frame(table(apply(ctrl[,index],1,paste,collapse="_"))/n_c)
        f <- join_all(list(data.frame(Var1=partitions),f_d,f_c),by="Var1")
        f[is.na(f)] <- 0
        upper_bound <- .5 + .25*sum(abs(f[,2]-f[,3]))
      }
      upper_bound
    }
    
    upper_bound <- unlist(lapply(result[,1],upper))
    
    bstrap_sample <- function(x){
      index <- as.numeric(unlist(strsplit(x,"_")))
      partitions <- apply(expand.grid(data.frame(matrix(rep(c(0,1),
                                                            length(index)),ncol=length(index)))),1,paste,collapse="_")
      if(length(index)==1){
        n_d <- length(case[,index])
        n_c <- length(ctrl[,index])
        f_d <- table(case[,index])/n_d
        f_c <- table(ctrl[,index])/n_c
        f <- apply(cbind(partitions,f_d,f_c),2,as.numeric)
      } else{   
        n_d <- nrow(case[,index])
        n_c <- nrow(ctrl[,index])
        f_d <- data.frame(table(apply(case[,index],1,paste,collapse="_"))/n_d)
        f_c <- data.frame(table(apply(ctrl[,index],1,paste,collapse="_"))/n_c)
        f <- join_all(list(data.frame(Var1=partitions),f_d,f_c),by="Var1")
        f[is.na(f)] <- 0
      }
      bsample_d <- data.frame(Var1 = partitions)
      for(i in 1:r) bsample_d <- join(bsample_d,data.frame(table(sample(partitions,n_d,prob=f[,2],replace=TRUE))/n_d),by="Var1")
      bsample_c <- data.frame(Var1 = partitions)
      for(i in 1:r) bsample_c <- join(bsample_c,data.frame(table(sample(partitions,n_c,prob=f[,3],replace=TRUE))/n_c),by="Var1")
      bsample_d[is.na(bsample_d)] <- 0; bsample_c[is.na(bsample_c)] <- 0
      bsample <- .5+.25*colSums(abs(bsample_d[,-1]-bsample_c[,-1]))
      mean(bsample)
    } 
    bstrap <- unlist(lapply(result[,1],bstrap_sample))
    
    result <- cbind(result,
                    iscore_n,
                    round(lower_bound,4),
                    round(upper_bound,4),
                    abs(round(upper_bound-bstrap,4)))
    colnames(result) <- c("Module","IScore","IScore/n","Lower Bound","Upper Bound",
                          "Bootstrap Bias")
    result
  })
  
  ################ DEFINE: performance #################
  # Show the performance
  output$performance <- renderTable({
    
    n <- input$n # sample size
    m <- input$m # number to report
    b <- input$b #number of bdrop
    s <- input$s #seed number
    r <- input$r #number of bootstrap samples
    p <- 1e3 #population size
    set.seed(s)  
    
    
    #    n <- 100
    #    m <- 5
    #    b <- 100
    #    p <- 1e5
    #    s <- 1    
    #   input <- list(p1 = c(1,2),p2=c(2,3,4))
    
    x_pop <- sapply(1:50,function(x) rbinom((n+p),1,.5))
    colnames(x_pop) <- paste("X",1:50,sep="")
    y_pop <- sapply(1:(n+p), function(i) ifelse(rbinom(1,1,.5)==0,
                                                (sum(x_pop[i,input$p1[1]:input$p1[2]])) %% 2,
                                                (sum(x_pop[i,input$p2[1]:input$p2[2]])) %% 2))
    all = data.frame(cbind(y_pop, x_pop))
    
    # Run
    model.result.RF <- Random.Forest(
      all = all,
      cutoff = n/(p+n),
      num.tree = 50,
      num.try = sqrt(ncol(all)),
      cutoff.coefficient = 1,
      SV.cutoff = 1:10
    )
    
    # Run
    model.result.iRF <- iter.Random.Forest(
      all = all,
      cutoff = n/(p+n),
      num.tree = 20,
      num.iter = 5
    )
    
    # Run
    model.result.BART <- bayesian.additive.regression.tree(
      all = all,
      cutoff = n/(p+n),
      num.tree = 5,
      num.cut = 2,
      cutoff.coefficient = 1
    )
    
    # Run ISCORE
    x = all[, -1]
    y = all[, 1]
    cut_off = n/(p+n)
    num.initial.set = 7
    how.many.rounds = b
    num.top.rows = 1
    seed = 1
    
    # Define data
    all <- data.frame(cbind(y, x))
    
    # Run
    start_time <- Sys.time()
    Result <- discrete.vs(
      all = all,
      cut_off = cut_off,
      num.initial.set = num.initial.set,
      how.many.rounds = how.many.rounds,
      num.top.rows = num.top.rows,
      seed = seed
    ); end_time <- Sys.time(); end_time - start_time
    
    # Create New Data
    # From here, the script is working on updated data:
    # Updated data means the explanatory variables are the local means
    # calculated from the important modules, e.g. modules with the highest I-score.
    all.new <- data.frame(cbind(as.numeric(as.character(all[, 1]))))
    for (i in 1:num.top.rows) {
      # Selected Variable
      selected.variable <- Result$All.BDA.Modules[i, 1]
      selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
      selected.variable <- sort(unique(as.numeric(selected.variable)))
      selected.variable
      all.new <- data.frame(cbind(
        all.new,
        new.module(all, i, Result = Result),
        all[, -1][, c(selected.variable)]))
    } # End of loop
    
    # Update
    all <- all.new; colnames(all)[1] <- "label"; head(all)
    
    # Prediction:
    # Now we can start doing predictions.
    # Note:
    # Be aware that this point the data is always named *all*
    # and this data always has the first column to be response
    # and the rest of the columsn to be covariates.
    # Run
    model.result.IScore <- logistic(
      all = data.frame(all),
      cutoff = cut_off,
      fam = binomial,
      cutoff.coefficient = 1)
    
    # Output
    performance <- cbind(
      "Test Accuracy",
      model.result.RF$Testing.Accuracy,
      model.result.iRF$Testing.Accuracy,
      model.result.BART$Testing.Accuracy,
      model.result.IScore$Testing.Accuracy
    )
    colnames(performance) <- c("Algorithms", "RF", "iRF", "BART", "IScore")
    rownames(performance) <- c("Test Set Accuracy")
    performance
  })
  }

# Run App
shinyApp(ui = ui, server = server)

######################### END OF SCRIPT #################################