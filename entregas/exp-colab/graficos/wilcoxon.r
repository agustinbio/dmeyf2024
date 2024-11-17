rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readr)

# Define the root folder containing the subfolders
root_folder <- "/mnt/storage/work/dmeyf/exp-colab/resultados/"

# List all subfolders (only 1 level deep)
subfolders <- list.dirs(root_folder, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store dataframes
data_list <- list()

# Loop over each subfolder
for (subfolder in subfolders) {
  # Define the path to the ganancias_log.txt file
  file_path <- file.path(subfolder, "ganancias_log.txt")
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the CSV file into a dataframe
    temp_df <- read_delim(file_path, show_col_types = FALSE)
    temp_df <- head(temp_df, -1)
    
    # Add a new column 'group' with the name of the subfolder (excluding root path)
    temp_df$grupo <- basename(subfolder)
    
    # Append the dataframe to the list
    data_list[[length(data_list) + 1]] <- temp_df
  }
}

# Combine all dataframes into a single dataframe
df_resultados <- bind_rows(data_list)

desired_order <- c("Sin lags", "1", "2", "3", "1,2","1,2,3", "1,2,3,4",
                   "1,2,3,4,5","1,2,3,4,5,6","1,12","1,2,12","1,2,3,6,12",
                   "1,2,3,4,5,6,12","1,6","1,6,12","1,3,6,9,12")

df_resultados$grupo <- factor(df_resultados$grupo, levels = desired_order)


test_wilcoxon <- pairwise.wilcox.test(df_resultados$ganancia, df_resultados$grupo, p.adjust.method="bonferroni")


# Convert the matrix into a dataframe
library(tidyr)
library(dplyr)
library(ggpubr)
library(rstatix)

pvalue_to_asterisks <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("ns")  # 'ns' for not significant
  }
}

step_increase = 0.02

# Create a dataframe from the matrix
df_p_values <- as.data.frame(as.table(test_wilcoxon$p.value)) %>%
  filter(!is.na(Freq)) %>%
  rename(group1 = Var1, group2 = Var2, p = Freq) %>%
  mutate(
    # Apply p-value to asterisk conversion
    significance = sapply(p, pvalue_to_asterisks)
  ) %>%
  add_y_position(formula = ganancia ~ grupo, data = df_resultados, step.increase = step_increase)


library(ggplot2)

filtrar_resultados <- function(df_resultados, df_p_values, selected_groups)
{
df_resultados_filtered <- df_resultados %>% filter(grupo %in% selected_groups)
df_p_values_filtered <- df_p_values %>%
  filter(group1 %in% selected_groups & group2 %in% selected_groups)
return(list(df_resultados = df_resultados_filtered, df_p_values = df_p_values_filtered))
}

selected_groups1 <- c("Sin lags", "1", "1,2","1,2,3", "1,2,3,4","1,2,3,4,5","1,2,3,4,5,6")
selected_groups2 <- c("1","2","3","1,2")
selected_groups3 <- c("1,2","1,12","1,2,12","1,2,3,6,12", "1,2,3,4,5,6,12","1,6","1,6,12","1,3,6,9,12")
res_filtrados1 <- filtrar_resultados(df_resultados,df_p_values, selected_groups1)
res_filtrados2 <- filtrar_resultados(df_resultados,df_p_values, selected_groups2)
res_filtrados3 <- filtrar_resultados(df_resultados,df_p_values, selected_groups3)

graficar_resultados <- function(res_filtrados){
grafico <- ggplot(data=res_filtrados$df_resultados,
            aes(x=grupo, y=ganancia))+
  stat_boxplot(geom = "errorbar")+geom_boxplot(outlier.shape = NA)+
  theme_bw()+
  geom_jitter(color = "red", width = 0.1, size = 2, alpha = 0.6) +  # Add original data points in red
  stat_pvalue_manual(res_filtrados$df_p_values, label = "significance", step.increase = step_increase)
return(grafico)
}

grafico1 <- graficar_resultados(res_filtrados1)
grafico2 <- graficar_resultados(res_filtrados2)
grafico3 <- graficar_resultados(res_filtrados3)