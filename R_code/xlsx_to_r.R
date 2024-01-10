# title:    excel_to_r
# purpose:  Reads in reporting event in xlsx format and produces analysis results dataset
# Author:   Malan Bosman
# Date:     27JUL2023


# libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(splitstackshape)

# Read in ARS xlsx content ----------------------------------------------------

ReportingEvent <- read_excel("ARSFILE.xlsx", 
                             sheet = "ReportingEvent")
ReferenceDocuments <- read_excel("ARSFILE.xlsx", 
                                 sheet = "ReferenceDocuments")
Categorizations <- read_excel("ARSFILE.xlsx", 
                              sheet = "Categorizations")
ListOfPlannedAnalyses <- read_excel("ARSFILE.xlsx", 
                                    sheet = "ListOfPlannedAnalyses")
ListOfPlannedOutputs <- read_excel("ARSFILE.xlsx", 
                                   sheet = "ListOfPlannedOutputs")
GlobalDisplaySections <- read_excel("ARSFILE.xlsx", 
                                    sheet = "GlobalDisplaySections")
Outputs <- read_excel("ARSFILE.xlsx", 
                      sheet = "Outputs")
Displays <- read_excel("ARSFILE.xlsx", 
                       sheet = "Displays")
OutputProgrammingCode <- read_excel("ARSFILE.xlsx", 
                                    sheet = "OutputProgrammingCode")
OutputCodeParameters <- read_excel("ARSFILE.xlsx", 
                                   sheet = "OutputCodeParameters")
OutputDocumentRefs <- read_excel("ARSFILE.xlsx", 
                                 sheet = "OutputDocumentRefs")
DataSubsets <- read_excel("ARSFILE.xlsx", 
                          sheet = "DataSubsets")
AnalysisSets <- read_excel("ARSFILE.xlsx", 
                           sheet = "AnalysisSets")
AnalysisGroupings <- read_excel("ARSFILE.xlsx", 
                                sheet = "AnalysisGroupings")
Analyses <- read_excel("ARSFILE.xlsx", 
                       sheet = "Analyses")
AnalysisProgrammingCode <- read_excel("ARSFILE.xlsx", 
                                      sheet = "AnalysisProgrammingCode")
AnalysisCodeParameters <- read_excel("ARSFILE.xlsx", 
                                     sheet = "AnalysisCodeParameters")
AnalysisDocumentRefs <- read_excel("ARSFILE.xlsx", 
                                   sheet = "AnalysisDocumentRefs")
AnalysisMethods <- read_excel("ARSFILE.xlsx", 
                              sheet = "AnalysisMethods")
AnalysisMethodCodeTemplate <- read_excel("ARSFILE.xlsx", 
                                         sheet = "AnalysisMethodCodeTemplate")
AnalysisMethodCodeParameters <- read_excel("ARSFILE.xlsx", 
                                           sheet = "AnalysisMethodCodeParameters")
AnalysisMethodDocumentRefs <- read_excel("ARSFILE.xlsx", 
                                         sheet = "AnalysisMethodDocumentRefs")
TerminologyExtensions <- read_excel("ARSFILE.xlsx", 
                                    sheet = "TerminologyExtensions")

#AnalysisResults <- read_excel("Common Safety Displays_upd.xlsx",
 #                                        sheet = "AnalysisResults")


# Read in ADaM datasets ---------------------------------------------------

ADSL <- read_csv("ADaM datasets/Original_csv/ADSL.csv")
ADAE <- read_csv("ADaM datasets/Original_csv/ADAE.csv") %>% 
  rename(TRT01A = TRTA)
ADVS <- read_csv("ADaM datasets/Original_csv/ADVS.csv") %>% 
  rename(TRT01A = TRTA)

# ListOfPlannedOutputs ----------------------------------------------------

# fill Lopa down
Lopa <- ListOfPlannedAnalyses %>% 
  fill(outputId)

# loop through outputs, 
Lopo <- ListOfPlannedOutputs

df_2 = data.frame()

# for (i in 1:1) {
for (i in 1:nrow(Lopo)) {
  Output = Lopo[i,]$outputId
  
  Anas <- Lopa %>% 
    filter(outputId == Output & !is.na(analysisId))
  
  # loop through individual analyses
  for (j in 1:nrow(Anas)) {
  # for (j in 1:10) {
    Anas_j <- Anas[j, ]$analysisId
    Anas_name <- Anas[j, ]$name
    
    Anas_s <- Analyses %>% 
      filter(id == Anas_j)
    Anas_name <- Anas_s$name
    
    ana_setId <- Anas_s$analysisSetId
    
    ############################ get relevant analysis Set 
    An_SetFunc <- function(an_set) {
      filt <- AnalysisSets %>% 
        filter(id == an_set)
      
      adam <- filt %>% 
        select(condition_dataset) %>% 
        as.character()
      
      filt_var <- filt %>% 
        select(condition_variable)
      
      val <- filt %>% 
        select(condition_value)
      
      logi <- filt %>% 
        select(condition_comparator)
      
      if(as.character(logi) == "EQ") {
        df1 <- get(adam) %>%
          filter(!!as.name(as.character(filt_var)) == as.character(val))
      }
      
      else if (as.character(logi) == "NE") {
        df1 <- get(adam) %>%
          filter(!!as.name(as.character(filt_var)) != as.character(val))
      }
      assign("ana_set", df1, envir = .GlobalEnv)
    }
    An_SetFunc(ana_setId)
    
    # Grouping ----------------------------------------------------------------
    
    ############################ group and apply method
    adam <- Anas_s %>%
      select(dataset) %>%
      unique() %>%
      as.character()
    
    methodid <- Anas_s$method_id
    vari <- Anas_s$variable
    dataset <- Anas_s$dataset
    
    # grouping:
    groupid1 <- Anas_s$groupingId1
    resultsByGroup1 <- Anas_s$resultsByGroup1
    group1_Position <- Anas_s$group1_Position
    
    
    filt1 <- AnalysisGroupings %>%
      filter(id == groupid1)
    
    group_var1 <- filt1 %>%
      # select(group_condition_variable) %>%
      select(groupingVariable) %>%
      unique()
    
    groupid2 <- Anas_s$groupingId2
    resultsByGroup2 <- Anas_s$resultsByGroup2
    group2_Position <- Anas_s$group2_Position
    
    filt2 <- AnalysisGroupings %>%
      filter(id == groupid2)
    
    group_var2 <- filt2 %>%
      # select(group_condition_variable) %>%
      select(groupingVariable) %>%
      unique()
    
    groupid3 <- Anas_s$groupingId3
    resultsByGroup3 <- Anas_s$resultsByGroup3
    group3_Position <- Anas_s$group3_Position
    
    
    filt3 <- AnalysisGroupings %>%
      filter(id == groupid3)
    
    group_var3 <- filt3 %>%
      # select(group_condition_variable) %>%
      select(groupingVariable) %>%
      unique()
    
    df_or <- get(adam)
    
    
    # data subset -------------------------------------------------------------
    
    if(!is.na(Anas_s$dataSubsetId)){
      subsetid <- Anas_s$dataSubsetId
      
      subsetrule <- DataSubsets %>% 
        filter(id == subsetid)
      # filter(id == "Dss02_Related_TEAE")
      # filter(id == "Dss01_TEAE")
      
      for (m in 1:max(subsetrule$level)) {
        
        if(max(subsetrule$level > 1)){
          lev = subsetrule %>% 
            filter(level %in% c(m, m+1))
        } else{
          lev = subsetrule %>% 
            filter(level == m)
        }
        
        # if(max(lev$order == 1)){
        #       condition_var <- lev$condition_variable
        #       condition_val <- lev$condition_value
        #       condition_com <- lev$condition_comparator
        # 
        #       if(condition_com == "EQ"){
        #         f_df <- df_or %>%
        #           filter(!!as.name(condition_var) == as.character(condition_val))
        #       }
        # } 
        # else {
        
        # for (n in 1:max(lev$order)) {
        for (n in 1:nrow(lev)) {
          
          ord1 = lev[n, ] %>% 
            cSplit("condition_value", "|")
          
          compound <- ord1$compoundExpression_logicalOperator
          if(!is.na(compound)){
            assign(paste('oper',m, sep=''), compound)
            o = 1
          } else {
            # assign(paste('var',m, "_", n, sep=''), ord1$condition_variable)
            # assign(paste('val1',m, "_", n, sep=''), ord1$condition_value_1)
            # assign(paste('val2',m, "_", n, sep=''), ord1$condition_value_2)
            # assign(paste('vcomp',m, "_", n, sep=''), ord1$condition_comparator)
            
            var = ord1$condition_variable
            val1 = ord1$condition_value_1
            val2 = ord1$condition_value_2
            vac = ord1$condition_comparator
            
            if(vac == "EQ"){
              f_vac = "=="
              assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "'",val1,"'"))
            } else if(vac == "IN"){
              f_vac = "%in%"
              assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "c('",val1, "', '",val2, "')"))
            }
          }
        }
        if(!(m ==1 & n == 1 & is.na(compound))){
          # if(!is.na(get(paste('oper',o, sep='')))){
          if(get(paste('oper',o, sep='')) == "AND") {
            f_df <- df_or %>%
              filter(eval(parse(text=get(paste("fexp", o,2, sep = "_")))) & 
                       eval(parse(text=get(paste("fexp", o,3, sep = "_")))))
          } else if(get(paste('oper',o, sep='')) == "AND"){
            f_df <- df_or %>%
              filter(eval(parse(text=get(paste("fexp", o,2, sep = "_")))) | 
                       eval(parse(text=get(paste("fexp", o,3, sep = "_")))))
          }
        } else {
          f_df <- df_or %>%
            filter(eval(parse(text=get(paste("fexp", 1,1, sep = "_")))))
        }
        # }
      }
    } else {
      f_df <- df_or
    }
    
    # don't use ---------------------------------------------------------------
    
    
    #         lev = subsetrule %>% 
    #           filter(level == m+1)
    #         
    #         for (o in 1:max(lev$order)) {
    #           ord2 = lev[o, ]
    #           
    #           # call result "X"
    #           assign(paste('var',m+1, "_", o, sep=''), ord2$condition_variable)
    #           assign(paste('val',m+1, "_", o, sep=''), ord2$condition_value)
    #           assign(paste('vcomp',m+1, "_", o, sep=''), ord2$condition_value)
    #           
    #           condition_var <- ord2$condition_variable
    #           condition_val <- ord2$condition_value
    #           condition_com <- ord2$condition_comparator
    #           
    #           
    #           
    #         }
    #         
    #       }
    #     }
    #       }
    #       
    #     }
    # 
    #   
    #   
    #   compound <- subsetrule[1,]$compoundExpression_logicalOperator
    #   
    #   if(is.na(compound)){
    #     condition_var <- subsetrule$condition_variable
    #     condition_val <- subsetrule$condition_value
    #     condition_com <- subsetrule$condition_comparator
    #     
    #     if(condition_com == "EQ"){
    #       f_df <- df_or %>% 
    #         filter(!!as.name(condition_var) == as.character(condition_val))
    #     }
    #    else {
    #     f_df <- df_or
    #    }
    #   }  else{
    #     
    #   } 
    # }
    #   
    
    
    
    if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
      num_grp <- 1
      if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
        num_grp <- 2
        if(resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
          num_grp <- 3
          df1 <- f_df %>%
            group_by(!!as.name(as.character(group_var1)),
                     !!as.name(as.character(group_var2)),
                     !!as.name(as.character(group_var3)))
        }
        else{
          df1 <- f_df %>%
            group_by(!!as.name(as.character(group_var1)),
                     !!as.name(as.character(group_var2)))
        }
      }
      else {
        df1 <- f_df %>%
          group_by(!!as.name(as.character(group_var1)))
      }
    }
    else  {
      df1 <- f_df
    }
    
    #Method: 
    meths <- AnalysisMethods %>% 
      filter(id == methodid) 
    
    df <- data.frame()
    
    ############################ calculate according to various methods
    for(k in 1:nrow(meths)){
      # for(k in 1:2){
      
      row = meths[k,]
      
      # Methods -----------------------------------------------------------------
      
      
      methodid = row$id
      label = row$operation_label
      pattern <- row$operation_resultPattern
      
      # if(method == "Mth01_CattVar_Count_ByGrp") {
      
      if(row$operation_id == "Mth01_CatVar_Count_ByGrp_1_n"){
        
        op_id = row$operation_id
        
        res <- df1 %>%
          summarise(res = n()) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
        
      }
      
      # if(method == "Mth02_ContVar_Summ_ByGrp") {
      
      if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_1_n"){
        
        op_id = row$operation_id
        
        res <- df1 %>%
          summarise(res = n()) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
        
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_2_Mean"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = mean(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_3_SD"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = sd(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_4_Median"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = median(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_5_Q1"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = quantile(!!as.name(vari), c(.25), na.rm = TRUE)) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_6_Q3"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = quantile(!!as.name(vari), c(.75), na.rm = TRUE)) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_7_Min"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = min(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth02_ContVar_Summ_ByGrp_8_Max"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = max(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      # if(method == "Mth04_ContVar_Comp_Anova") {
      else if(row$operation_id == "Mth04_ContVar_Comp_Anova_1_pval"){
        op_id = row$operation_id
        
        fm <- as.formula(paste(vari, "~", as.character(group_var1)))
        model <- lm(fm, data = df1)
        
        lmp <- function (model) {
          if (class(model) != "lm") stop("Not an object of class 'lm' ")
          f <- summary(model)$fstatistic
          p <- pf(f[1],f[2],f[3],lower.tail=F)
          attributes(p) <- NULL
          p
          
          res <- data.frame(Group1 = "", 
                            Group2 = "",
                            Group3 = "",
                            res = p,
                            method = methodid,
                            operation = op_id,
                            output = Output,
                            label = label,
                            pattern = pattern)
          assign("res", res, envir = .GlobalEnv)
        }
        
        lmp(model)
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      else if(row$operation_id == "Mth01_CatVar_Summ_ByGrp_1_n"){
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = n_distinct(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }
      
      # pct operation -----------------------------------------------------------
      
      
      
      else if(row$operation_id == "Mth01_CatVar_Summ_ByGrp_2_pct"){
        
        df_check = df_2
        NUM = Anas_s$referencedAnalysisOperations_analysisId1
        DEN = Anas_s$referencedAnalysisOperations_analysisId2
        
        NUM_op = row$operation_referencedResultRelationships1_operationId
        DEN_op = row$operation_referencedResultRelationships2_operationId
        
        op_id = row$operation_id
        res <- df1 %>%
          summarise(res = n_distinct(!!as.name(vari))) %>% 
          mutate(method = methodid,
                 operation = op_id,
                 output = Output,
                 label = label,
                 pattern = pattern
          )
        
        if(num_grp == 1) {
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1))) %>% 
            mutate(Group2 = "",
                   Group3 = "")
        }
        else if(num_grp == 2){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2))
            ) %>% 
            mutate(Group3 = "")
        }
        else if(num_grp == 3){
          res <- res %>% 
            rename(Group1 = !!as.name(as.character(group_var1)),
                   Group2 = !!as.name(as.character(group_var2)),
                   Group3 = !!as.name(as.character(group_var3))
            ) 
        }
        
        num_data <- res %>% 
          rename(NUM = res)
        
        # den_data <- df_2 %>% 
        #   filter(analysis == DEN) %>% 
        #   rename(DEN = res) 
        den_data <- df_2 %>% 
          filter(operation == DEN_op, 
                 output == Output) %>% 
          rename(DEN = res) 
        
        all_data <- left_join(num_data,
                              den_data %>% select(Group1, DEN),
                              by = c("Group1")
        ) %>% 
          mutate(res = NUM / DEN * 100) %>% 
          select(-NUM, -DEN)
        
        # call result "Xi"
        assign(paste('X', k, sep=''), all_data)
      }
      
      # if(method == "Mth04_ContVar_Comp_Anova") {
      else if(row$operation_id == "Mth03_CatVar_Comp_PChiSq_1_pval"){
        op_id = row$operation_id
        
        tab <- table(ADSL[, c(as.character(group_var1), as.character(group_var2))])
        p <- chisq.test(tab)$p.value
        
        res <- data.frame(Group1 = "", 
                          Group2 = "",
                          Group3 = "",
                          res = p,
                          method = methodid,
                          operation = op_id,
                          output = Output,
                          label = label,
                          pattern = pattern)
        
        # call result "Xi"
        assign(paste('X', k, sep=''), res)
      }

# Fisher ------------------------------------------------------------------
      # if(method == "Mth04_ContVar_Comp_Anova") {
      else if(row$operation_id == "Mth03_CatVar_Comp_FishEx_1_pval"){
        op_id = row$operation_id

        if(Anas_j ==  "An07_01_TEAE_Comp_ByTrt_PlacLow"){
          tab1 <- ADSL %>% 
            filter(TRT01A %in% c("Placebo", "Xanomeline Low Dose")) %>% 
            group_by(TRT01A) %>% 
            summarise(cnt = n())
          
          tab2 <- f_df %>% 
            select(USUBJID, TRT01A) %>% 
            unique() %>% 
            group_by(TRT01A) %>% 
            summarise(cnt_2 = n())
          
          tabf <- left_join(tab1, tab2, 
                            by = "TRT01A") %>% 
            mutate(cnt_1 = cnt - cnt_2) %>% 
            select(-cnt, -TRT01A)
          
          p <- fisher.test(tabf)$p.value
          
          res <- data.frame(Group1 = "",
                            Group2 = "",
                            Group3 = "",
                            res = p,
                            method = methodid,
                            operation = op_id,
                            output = Output,
                            label = label,
                            pattern = pattern)
          
          # call result "Xi"
          assign(paste('X', k, sep=''), res)
        }
        
        else if(Anas_j ==  "An07_01_TEAE_Comp_ByTrt_PlacHigh"){
          tab1 <- ADSL %>% 
            filter(TRT01A %in% c("Placebo", "Xanomeline High Dose")) %>% 
            group_by(TRT01A) %>% 
            summarise(cnt = n())
          
          tab2 <- f_df %>% 
            select(USUBJID, TRT01A) %>% 
            unique() %>% 
            group_by(TRT01A) %>% 
            summarise(cnt_2 = n())
          
          tabf <- left_join(tab1, tab2, 
                            by = "TRT01A") %>% 
            mutate(cnt_1 = cnt - cnt_2) %>% 
            select(-cnt, -TRT01A)
          
          p <- fisher.test(tabf)$p.value
          
          res <- data.frame(Group1 = "",
                            Group2 = "",
                            Group3 = "",
                            res = p,
                            method = methodid,
                            operation = op_id,
                            output = Output,
                            label = label,
                            pattern = pattern)
          
          # call result "Xi"
          assign(paste('X', k, sep=''), res)
        }
        
        else if(Anas_j ==  "An07_09_Soc_Comp_ByTrt_PlacLow"){
          tab1 <- ADSL %>%
            filter(TRT01A %in% c("Placebo", "Xanomeline Low Dose")) %>%
            group_by(TRT01A) %>%
            summarise(cnt = n())
          
          uniq_list <- f_df %>% 
            select(AESOC) %>% 
            unique()
          
          fish_df <- data.frame()
          
          for(q in 1:nrow(uniq_list)){
            
            item <- uniq_list[q,]
            
            tab2 <- f_df %>%
              filter(AESOC == as.character(item)) %>%
              select(USUBJID, TRT01A) %>%
              unique() %>%
              group_by(TRT01A) %>%
              summarise(cnt_2 = n()) 
            
            tabf <- left_join(tab1, tab2,
                              by = "TRT01A") %>%
              replace_na(list(cnt_2 = 0)) %>% 
              mutate(cnt_1 = cnt - cnt_2) %>%
              select(-cnt, -TRT01A)
            
            p <- fisher.test(tabf)$p.value
            
            dat <- data.frame(Group1 = "",
                              Group2 = as.character(item),
                              Group3 = "",
                              res = p,
                              method = methodid,
                              operation = op_id,
                              output = Output,
                              label = label,
                              pattern = pattern)
            
            fish_df <- rbind(fish_df, dat)
            
            assign(paste('X', k, sep=''), fish_df)
            
          }
        }
        
        else if(Anas_j ==  "An07_09_Soc_Comp_ByTrt_PlacHigh"){
          tab1 <- ADSL %>%
            filter(TRT01A %in% c("Placebo", "Xanomeline High Dose")) %>%
            group_by(TRT01A) %>%
            summarise(cnt = n())
          
          uniq_list <- f_df %>% 
            select(AESOC) %>% 
            unique()
          
          fish_df <- data.frame()
          
          for(q in 1:nrow(uniq_list)){
            
            item <- uniq_list[q,]
            
            tab2 <- f_df %>%
              filter(AESOC == as.character(item)) %>%
              select(USUBJID, TRT01A) %>%
              unique() %>%
              group_by(TRT01A) %>%
              summarise(cnt_2 = n()) 
            
            tabf <- left_join(tab1, tab2,
                              by = "TRT01A") %>%
              replace_na(list(cnt_2 = 0)) %>% 
              mutate(cnt_1 = cnt - cnt_2) %>%
              select(-cnt, -TRT01A)
            
            p <- fisher.test(tabf)$p.value
            
            dat <- data.frame(Group1 = "",
                              Group2 = as.character(item),
                              Group3 = "",
                              res = p,
                              method = methodid,
                              operation = op_id,
                              output = Output,
                              label = label,
                              pattern = pattern)
            
            fish_df <- rbind(fish_df, dat)
            
            assign(paste('X', k, sep=''), fish_df)
            
          }
        }
        
        else if(Anas_j ==  "An07_10_SocPt_Comp_ByTrt_PlacLow"){
          tab1 <- ADSL %>%
            filter(TRT01A %in% c("Placebo", "Xanomeline Low Dose")) %>%
            group_by(TRT01A) %>%
            summarise(cnt = n())
          
          uniq_list <- f_df %>% 
            select(AESOC, AEDECOD) %>% 
            unique()
          
          fish_df_PT <- data.frame()
          
          for(q in 1:nrow(uniq_list)){
            
            item <- uniq_list[q,"AEDECOD"]
            soc <- uniq_list[q,"AESOC"]
            
            tab2 <- f_df %>%
              filter(AEDECOD == as.character(item)) %>%
              select(USUBJID, TRT01A) %>%
              unique() %>%
              group_by(TRT01A) %>%
              summarise(cnt_2 = n()) 
            
            tabf <- left_join(tab1, tab2,
                              by = "TRT01A") %>%
              replace_na(list(cnt_2 = 0)) %>% 
              mutate(cnt_1 = cnt - cnt_2) %>%
              select(-cnt, -TRT01A)
            
            p <- fisher.test(tabf)$p.value
            
            dat <- data.frame(Group1 = "",
                              Group2 = as.character(soc),
                              Group3 = as.character(item),
                              res = p,
                              method = methodid,
                              operation = op_id,
                              output = Output,
                              label = label,
                              pattern = pattern)
            
            fish_df_PT <- rbind(fish_df_PT, dat)
            
            assign(paste('X', k, sep=''), fish_df_PT)
            
          }
        }
        
        else if(Anas_j ==  "An07_10_SocPt_Comp_ByTrt_PlacHigh"){
          tab1 <- ADSL %>%
            filter(TRT01A %in% c("Placebo", "Xanomeline High Dose")) %>%
            group_by(TRT01A) %>%
            summarise(cnt = n())
          
          uniq_list <- f_df %>% 
            select(AESOC, AEDECOD) %>% 
            unique()
          
          fish_df_PT <- data.frame()
          
          for(q in 1:nrow(uniq_list)){
            
            item <- uniq_list[q,"AEDECOD"]
            soc <- uniq_list[q,"AESOC"]
            
            tab2 <- f_df %>%
              filter(AEDECOD == as.character(item)) %>%
              select(USUBJID, TRT01A) %>%
              unique() %>%
              group_by(TRT01A) %>%
              summarise(cnt_2 = n()) 
            
            tabf <- left_join(tab1, tab2,
                              by = "TRT01A") %>%
              replace_na(list(cnt_2 = 0)) %>% 
              mutate(cnt_1 = cnt - cnt_2) %>%
              select(-cnt, -TRT01A)
            
            p <- fisher.test(tabf)$p.value
            
            dat <- data.frame(Group1 = "",
                              Group2 = as.character(soc),
                              Group3 = as.character(item),
                              res = p,
                              method = methodid,
                              operation = op_id,
                              output = Output,
                              label = label,
                              pattern = pattern)
            
            fish_df_PT <- rbind(fish_df_PT, dat)
            
            assign(paste('X', k, sep=''), fish_df_PT)
            
          }
        }
        
      }
      
      # combine with previous operations
      df <- rbind(df, get(paste('X', k, sep='')))

    }
    
    # add analysis and assign globally based on
    df_1 <- df %>% 
      mutate(analysis = Anas_j,
             name = Anas_name)
    assign(paste("res", j, sep=''), df_1, envir = .GlobalEnv)
    # }
    
    # combine with previous analyses
    df_2 <- rbind(df_2, get(paste('res', j, sep='')))
  }
}

# apply pattern
disp <- df_2 %>% 
  mutate(dec = ifelse(grepl("X.X", df_2$pattern, ), 
                      str_count(substr(df_2$pattern, str_locate(df_2$pattern, "X.X")[, 1]+2, nchar(df_2$pattern)), "X"), 0)) %>% 
  rowwise() %>% 
  mutate(rnd = round(res, dec)) %>% 
  as_tibble() %>% 
  mutate(disp = ifelse(grepl('\\(N=', df_2$pattern),
                       paste0("(N=", rnd, ")"), 
                       ifelse(grepl("\\(", df_2$pattern), 
                              paste0("(", rnd, ")"),
                              as.character(rnd))))
write.csv(disp, "disp_fin.csv")

