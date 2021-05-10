library(tidyverse)

# Fig. 2: CV-AUC and test error % according to the No. of features ----------

# load and merge data
df <- bind_rows(mutate(read_csv("GvM_full_by_nfeat.csv"), Task="GvM_full"),
                mutate(read_csv("GvM_under_by_nfeat.csv"), Task="GvM_half"),
                mutate(read_csv("MEN_full_by_nfeat.csv"), Task="MEN_full"), 
                mutate(read_csv("MEN_under_by_nfeat.csv"), Task="MEN_half")) %>% 
    mutate(test_error = abs(CV_AUC - Test_AUC)/CV_AUC*100) %>% 
    select(-X1)

# create a dataframe for plotting
df_plot <- df %>% 
    group_by(Task, Num_features) %>% 
    summarise(AUC_mean=mean(CV_AUC), AUC_SD=sd(CV_AUC), 
              Test_mean=mean(Test_AUC), Test_SD=sd(Test_AUC), 
              diff_mean=mean(test_error), diff_SD=sd(test_error), 
              diff_min=min(test_error), diff_max=max(test_error))

# Mean CV-AUC averaged over 1,000 trials according to the number of features
ggplot(df_plot %>% filter((Task=="GvM_full" | Task=="MEN_full")),
       aes(x=Num_features, y=AUC_mean, group=Task)) +
    geom_line(data=filter(df, Task=="GvM_full"), 
              aes(x=Num_features, y=CV_AUC, group=Random_state), 
              size=1, alpha=0.3, color='darkorange3') +
    geom_line(data=filter(df, Task=="MEN_full"), 
              aes(x=Num_features, y=CV_AUC, group=Random_state), 
              size=1, alpha=0.3, color="dodgerblue3") +
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=AUC_mean - 1.96*AUC_SD/sqrt(1000), 
                      ymax=AUC_mean + 1.96*AUC_SD/sqrt(1000)), 
                  lty=1, size=0.3, width=0.5) +
    labs(x="Number of features", y="Mean CV AUC in the training set") +
    ylim(c(0.35, 1)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")

ggplot(df_plot %>% filter((Task=="GvM_half" | Task=="MEN_half")),
       aes(x=Num_features, y=AUC_mean, group=Task)) +
    geom_line(data=filter(df, Task=="GvM_half"), 
              aes(x=Num_features, y=CV_AUC, group=Random_state), 
              size=1, alpha=0.3, color='darkorange1') +
    geom_line(data=filter(df, Task=="MEN_half"), 
              aes(x=Num_features, y=CV_AUC, group=Random_state), 
              size=1, alpha=0.3, color="dodgerblue") +
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=AUC_mean - 1.96*AUC_SD/sqrt(1000), 
                      ymax=AUC_mean + 1.96*AUC_SD/sqrt(1000)), 
                  lty=1, size=0.3, width=0.5) +
    labs(x="Number of features", y="Mean CV AUC in the training set") +
    ylim(c(0.35, 1)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")


# Mean Test-AUC averaged over 1,000 trials according to the number of features
ggplot(df_plot %>% filter((Task=="GvM_full" | Task=="MEN_full")),
       aes(x=Num_features, y=Test_mean, group=Task)) +
    geom_line(data=filter(df, Task=="GvM_full"), 
              aes(x=Num_features, y=Test_AUC, group=Random_state), 
              size=1, alpha=0.2, color='darkorange3') +
    geom_line(data=filter(df, Task=="MEN_full"), 
              aes(x=Num_features, y=Test_AUC, group=Random_state), 
              size=1, alpha=0.2, color="dodgerblue3") +
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=Test_mean - 1.96*Test_SD/sqrt(1000), 
                      ymax=Test_mean + 1.96*Test_SD/sqrt(1000)), 
                  lty=1, size=0.3, width=0.5) +
    labs(x="Number of features", y="AUC in the test set") +
    ylim(c(0.35, 1)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")

ggplot(df_plot %>% filter((Task=="GvM_half" | Task=="MEN_half")),
       aes(x=Num_features, y=Test_mean, group=Task)) +
    geom_line(data=filter(df, Task=="GvM_half"), 
              aes(x=Num_features, y=Test_AUC, group=Random_state), 
              size=1, alpha=0.2, color='darkorange1') +
    geom_line(data=filter(df, Task=="MEN_half"), 
              aes(x=Num_features, y=Test_AUC, group=Random_state), 
              size=1, alpha=0.2, color="dodgerblue") +
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=Test_mean - 1.96*Test_SD/sqrt(1000), 
                      ymax=Test_mean + 1.96*Test_SD/sqrt(1000)), 
                  lty=1, size=0.3, width=0.5) +
    labs(x="Number of features", y="AUC in the test set") +
    ylim(c(0.35, 1)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")

# 3) AUC difference averaged over 1,000 trials according to the number of features
ggplot(df_plot, aes(x=Num_features, y=diff_mean)) +
    geom_line(data=filter(df_plot, Task=="GvM_full"), 
              aes(x=Num_features, y=diff_mean), 
              size=1, color='darkorange3') +
    geom_errorbar(data=filter(df_plot, Task=="GvM_full"),
                  aes(ymin=diff_mean - 1.96*diff_mean/sqrt(1000), 
                      ymax=diff_mean + 1.96*diff_mean/sqrt(1000)), 
                  lty=1, color='darkorange3', size=0.3, width=0.5) +    
    geom_line(data=filter(df_plot, Task=="MEN_full"), 
              aes(x=Num_features, y=diff_mean), 
              size=1, color="dodgerblue3") +
    geom_errorbar(data=filter(df_plot, Task=="MEN_full"),
                  aes(ymin=diff_mean - 1.96*diff_mean/sqrt(1000), 
                      ymax=diff_mean + 1.96*diff_mean/sqrt(1000)), 
                  lty=1, color='dodgerblue3', size=0.3, width=0.5) +   
    labs(x="Number of features", 
         y="Averaged AUC difference between CV and test (%)") +
    ylim(c(3, 15)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")

ggplot(df_plot, aes(x=Num_features, y=diff_mean)) +
    geom_line(data=filter(df_plot, Task=="GvM_half"), 
              aes(x=Num_features, y=diff_mean), 
              size=1, color='darkorange1') +
    geom_errorbar(data=filter(df_plot, Task=="GvM_half"),
                  aes(ymin=diff_mean - 1.96*diff_mean/sqrt(1000), 
                      ymax=diff_mean + 1.96*diff_mean/sqrt(1000)), 
                  lty=1, color='darkorange1', size=0.3, width=0.5) +    
    geom_line(data=filter(df_plot, Task=="MEN_half"), 
              aes(x=Num_features, y=diff_mean), 
              size=1, color="dodgerblue") +
    geom_errorbar(data=filter(df_plot, Task=="MEN_half"),
                  aes(ymin=diff_mean - 1.96*diff_mean/sqrt(1000), 
                      ymax=diff_mean + 1.96*diff_mean/sqrt(1000)), 
                  lty=1, color='dodgerblue', size=0.3, width=0.5) +   
    labs(x="Number of features", 
         y="Averaged AUC difference between CV and test (%)") +
    ylim(c(3, 15)) +
    theme_bw() +
    theme(axis.title.x = element_text(size=40), 
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40), 
          axis.text.y = element_text(size=30),
          legend.position = "NONE")



# Fig. 3: Mean AUC and AUC difference by task difficulty and sample size -------

# load and merge data
df <- bind_rows(mutate(read_csv("GvM_full_search.csv"), Task="GBM vs Met"),
                mutate(read_csv("GvM_under_search.csv"), Task="GBM vs Met, undersampled"),
                mutate(read_csv("MEN_full_search.csv"), Task="Meningioma"),
                mutate(read_csv("MEN_under_search.csv"), Task="Meningioma, undersampled")) %>% 
    mutate(test_error = CV_AUC - Test_AUC) %>% 
    select(-X1) 

# plotting
ggplot(data=df, aes(x=CV_AUC, y=test_error, color=Task)) +
    geom_point(aes(color=Task, shape=Task), alpha=0.5, size=6) +
    theme_bw() +
    scale_shape_manual(values=c(19, 20, 15, 18)) +
    labs(x="Mean AUC", y="AUC difference between CV and testing") +
    theme(axis.title.x = element_text(size=40),
          axis.text.x = element_text(size=30), 
          axis.title.y = element_text(size=40),
          axis.text.y = element_text(size=30), 
          legend.position=c(0.2, 0.9), 
          legend.title=element_text(size=35, face="bold"),
          legend.text=element_text(size=28))

# Fig. 4: Visual explanation of discrepant model performances -------------
df <- read_csv("SVC_results.csv") %>%  
    filter(Random_state %in% c(518, 602, 563)) %>% 
    mutate(CV_lowCI = CV_AUC -1.96*CV_AUC_SD/2,
           CV_highCI = CV_AUC +1.96*CV_AUC_SD/2,
           Test_lowCI = Test_AUC_lCI,
           Test_highCI = Test_AUC_uCI) %>% 
    select(Random_state, 
           CV_AUC, CV_lowCI, CV_highCI,
           Test_AUC, Test_lowCI, Test_highCI) %>% 
    pivot_longer(cols = CV_AUC:Test_highCI, names_to = 'Set_Var') %>% 
    separate(Set_Var, sep = "_", into = c("Set", "Var")) %>% 
    pivot_wider(names_from = Var, values_from = value)
df$Random_state <- factor(df$Random_state, labels = c("563", "602", "518"), 
                          levels= c(563, 602, 518))
df$Set <- factor(df$Set, labels = c("Test", "CV"), levels= c("Test", "CV"))

ggplot(df, aes(x=Random_state, y=AUC, Set=Set)) +
    geom_point(aes(color=Set, shape=Set), position=position_dodge(width=0.4),
               size=8) +
    geom_errorbar(aes(ymin=lowCI, ymax=highCI, color=Set, lty=Set), 
                  position=position_dodge(width=0.4), width=0.2, size=1.5) +
    theme_bw() +
    labs(x = "Trial No. (Random state)", y = "AUC") +
    theme(axis.title.x = element_text(size=25), 
          axis.text.x = element_text(size=20), 
          axis.title.y = element_text(size=25), 
          axis.text.y = element_text(size=20), 
          legend.position=c(0.1, 0.1), 
          legend.title=element_text(size=25, face="bold"),
          legend.text=element_text(size=20)) +
    coord_flip()

