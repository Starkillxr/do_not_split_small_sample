library(tidyverse)

# Table 1: Model performance and stability of LASSO
#          according to the sample size and the level of task difficulty
df <- bind_rows(mutate(read_csv("GvM_full_search.csv"), Task="GvM_full"),
                mutate(read_csv("GvM_under_search.csv"), Task="GvM_under"),
                mutate(read_csv("MEN_full_search.csv"), Task="MEN_full"),
                mutate(read_csv("MEN_under_search.csv"), Task="MEN_under")) %>% 
    mutate(test_error = CV_AUC - Test_AUC) %>% 
    select(-X1) 

T1 <- df %>% 
    group_by(Task) %>% 
    summarize(AUC_mean = mean(Test_AUC),
              AUC_sd = sd(Test_AUC),
              Diff_mean = mean(abs(test_error)),
              Diff_sd = sd(abs(test_error))) %>% 
    mutate(performance = paste0(round(AUC_mean, 3), " (+/-", round(AUC_sd, 3), ")"),
           stability = paste0(round(Diff_mean, 3), " (+/-", round(Diff_sd, 3), ")")) %>% 
    select(Task, performance, stability)
View(T1)


# Table 3: Comparison of four methods to estimate model performance 
#          in meningioma grading task
get_subtable <- function(dataset){
    dataset %>% 
        mutate(diff = CV_AUC - Test_AUC,
               rsd = CV_AUC_SD/CV_AUC) %>% 
        group_by(Method) %>% 
        summarise(CV_AUC_mean = round(mean(CV_AUC), 3),
                  CV_AUC_lowCI = round(mean(CV_AUC) - 1.96*sd(CV_AUC)/3, 3),
                  CV_AUC_highCI = round(mean(CV_AUC) + 1.96*sd(CV_AUC)/3, 3),
                  CV_RSD_mean = round(mean(rsd), 3),
                  CV_RSD_lowCI = round(mean(rsd) - 1.96*sd(rsd)/3, 3),
                  CV_RSD_highCI = round(mean(rsd) + 1.96*sd(rsd)/3, 3),
                  Test_AUC_mean = round(mean(Test_AUC), 3),
                  Test_AUC_lowCI = round(mean(Test_AUC) - 1.96*sd(Test_AUC)/3, 3),
                  Test_AUC_highCI = round(mean(Test_AUC) + 1.96*sd(Test_AUC)/3, 3),
                  Diff_mean = round(mean(diff), 3), 
                  Diff_lowCI = round(mean(diff) - 1.96*sd(diff)/3, 3),
                  Diff_highCI = round(mean(diff) + 1.96*sd(diff)/3, 3)) 
}
MEN_moderate <- get_subtable(read_csv("compare_methods_MEN_moderate.csv")) %>% 
    mutate(Mismatch = "moderate")
MEN_extreme <- get_subtable(read_csv("compare_methods_MEN_extreme.csv")) %>% 
    mutate(Mismatch = "extreme")

result_table <- bind_rows(MEN_moderate, MEN_extreme) %>% 
    mutate(CV_AUC = paste0(CV_AUC_mean, " (", 
                           CV_AUC_lowCI, "-", CV_AUC_highCI, ")"),
           CV_RSD = paste0(CV_RSD_mean, " (", 
                           CV_RSD_lowCI, "-", CV_RSD_highCI, ")"),
           Test_AUC = paste0(Test_AUC_mean, " (", 
                             Test_AUC_lowCI, "-", Test_AUC_highCI, ")"),
           Test_error = paste0(Diff_mean, " (", 
                               Diff_lowCI, "-", Diff_highCI, ")")) %>% 
    select(Mismatch, Method, CV_AUC, CV_RSD, Test_AUC, Test_error)
View(result_table)