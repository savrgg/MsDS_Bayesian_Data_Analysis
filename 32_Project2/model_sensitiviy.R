library(readxl)
library(stringr)

mod2 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "modelo2") %>% 
  data.frame()
mod3 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "modelo3") %>% 
  data.frame()

df_plot <- rbind(mod2 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
                   setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
                   mutate(model = "mod2"),
      mod3 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
        setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
        mutate(model = "mod3")) %>% 
  mutate(node = str_replace(node, "tau\\[", ""),
         node = str_replace(node, "\\]", "")) %>% 
  mutate(node = as.numeric(node))
      

df_plot %>% ggplot(aes(x = node, y = mean_mod, group = model, color = model)) +
  geom_point() + 
  scale_y_log10() +
  #scale_x_continuous(breaks=seq(1, 413, 10))+
  geom_linerange(aes(ymin = p2_mod, ymax = p97_mod), alpha = .3) + 
  labs(title = "Sensitivity analysis for model 2 and model 3", 
       y = "log(value of tau)",
       x = "Tau (413 coefficients)")+
  theme_bw()+
  coord_flip() 
  
  
## second part sigma


mod2 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "modelo2_sigma") %>% 
  data.frame()
mod3 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "modelo3_sigma") %>% 
  data.frame()

df_plot <- rbind(mod2 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
                   setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
                   mutate(model = "mod2"),
                 mod3 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
                   setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
                   mutate(model = "mod3")) %>% 
  mutate(node = str_replace(node, "sigma\\[", ""),
         node = str_replace(node, "\\]", "")) %>% 
  mutate(node = as.numeric(node))


df_plot %>% ggplot(aes(x = node, y = mean_mod, group = model, color = model)) +
  geom_point() + 
  scale_y_log10() +
  #scale_x_continuous(breaks=seq(1, 413, 10))+
  geom_linerange(aes(ymin = p2_mod, ymax = p97_mod), alpha = .3) + 
  labs(title = "Sensitivity analysis for model 2 and model 3", 
       y = "log(value of sigma)",
       x = "sigma (413 coefficients)")+
  theme_bw()+
  coord_flip() 




# last part
mod4 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "model4") %>% 
  data.frame()
mod5 <- readxl::read_excel("sensitivityAnalysis.xlsx", sheet = "model5") %>% 
  data.frame()

df_plot <- rbind(mod4 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
                   setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
                   mutate(model = "mod4"),
                 mod5 %>% dplyr::select(X.node, X.mean, X2.5000000000000001E.2,  X0.97499999999999998) %>% 
                   setNames(c("node", "mean_mod", "p2_mod", "p97_mod")) %>% 
                   mutate(model = "mod5")) %>% 
  mutate(node = str_replace(node, "sigma\\[", ""),
         node = str_replace(node, "\\]", "")) %>% 
  mutate(node = as.numeric(node)) %>% 
  mutate(node = ifelse(model == "mod5", node, node+.5))


df_plot %>% ggplot(aes(x = node, y = mean_mod, group = model, color = model)) +
  geom_point() + 
  scale_y_log10() +
  #scale_x_continuous(breaks=seq(1, 413, 10))+
  geom_linerange(aes(ymin = p2_mod, ymax = p97_mod), alpha = .3) + 
  labs(title = "Sensitivity analysis for model 4 and model 5", 
       y = "log(value of sigma)",
       x = "sigma (47 coefficients)")+
  theme_bw()+
  coord_flip() 


xx <- read.csv(file = "ExtraDataForR_Report2(1)/DataProteinsMissingNA.csv", header = F)
xx[xx<5]<- NA

apply(xx, 2, function(x){
  sum(is.na(x))
})

xx_df <- apply(xx, 1, function(x){
  c(mean(log(x), na.rm = T), sd(log(x), na.rm = T))
}) %>% t() %>% data.frame() %>% mutate(ind = 414:460)
  # ggplot(aes(x = as.numeric(ind), y = X1, ymax = X1+1.96*X2, ymin = X1-1.96*X2))+
  # geom_point(alpha = .5)+
  # geom_linerange(alpha = .5) +
  # geom_hline(yintercept = log(5), color = "red") +
  # labs(title = "Assuming that each ln(X_gr) has a normal distribution:", x = "protein", y = "normal distribution" ) +
  # theme_bw()



xx2 <- read.csv(file = "ExtraDataForR_Report2(1)/DataProteins413.csv", header = F)

apply(xx2, 1, function(x){
  c(mean(log(x), na.rm = T), sd(log(x), na.rm = T))
}) %>% t() %>% data.frame() %>% mutate(ind = row.names(.)) %>% 
  ggplot(aes(x = as.numeric(ind), y = X1, ymax = X1+1.96*X2, ymin = X1-1.96*X2))+
  geom_point(alpha = .5)+
  geom_linerange(alpha = .5) +
  geom_hline(yintercept = log(5), color = "red") +
  labs(title = "Assuming that each ln(X_gr) has a normal distribution:", x = "protein", y = "normal distribution" ) +
  theme_bw() +
  geom_point(data = xx_df, aes(x = ind, y = X1), color = "blue", alpha = .5) +
  geom_linerange(data = xx_df, aes(ymin = X1-1.96*X2 , ymax = X1+1.96*X2), alpha = .5, color = "blue")
