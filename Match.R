# 加载必要的包
library(MatchIt)
library(cobalt)
library(dplyr)
library(ggplot2)

# 创建必要的目录
dir.create("C:/MyProject/Myproject/code/LOS/newLOS/DATA/multimatch", recursive = TRUE, showWarnings = FALSE)
dir.create("C:/MyProject/Myproject/code/LOS/newLOS/figures", recursive = TRUE, showWarnings = FALSE)

# 读取数据
data <- read.csv("C:/MyProject/Myproject/code/LOS/data/datatest.csv")

# 定义用于匹配的变量
matching_vars <- c("gender", "age", "height", "weight", "WBC", "RBC", "Hgb", 
                   "PLT", "HCT", "PT", "INR", "APTT", "TT", "Fib", 
                   "DDimer", "FDP", "Wells_score_on_admission")

# 定义匹配方法
methods <- c("genetic", "nearest_caliper", "optimal_exact", "mahalanobis")

# 定义要添加的其他变量
additional_vars <- c("LOS", "IPR", "CER", "DIR", "DIC", "CLE","day_3","day_5","day_7","Days1_symptom_score")
columns_to_add <- c("Alprostadil", "Buqi.Tongluo.Capsules", "Diosmin", "Enoxaparin", 
                    "Furosemide", "Guhong.injection", "Nadroparin.Calcium", 
                    "Sodium.Aescinate", "Urokinase", "Warfarin", 
                    "Elevate.lower.limbs.to.30.degrees", "Local.immobilization", 
                    "BXpowder", "SbOintment")

# 循环进行不同方法的匹配
for(method in methods) {
  # 设置随机种子
  set.seed(1234)
  
  # 根据不同方法设置匹配参数
  if(method == "genetic") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "genetic",
                     pop.size = 200,
                     max.generations = 10,
                     wait.generations = 4,
                     ratio = 1)
  } else if(method == "nearest_caliper") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.2,
                     ratio = 1)
  } else if(method == "optimal_exact") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "optimal",
                     exact = c("gender"),
                     distance = "glm",
                     ratio = 1)
  } else if(method == "mahalanobis") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "nearest",
                     distance = "mahalanobis",
                     ratio = 1)
  }
  
  # 获取匹配后的数据
  matched_data <- match.data(m.out)
  
  # 选择需要的变量
  selected_vars <- c(matching_vars, "CombineAnti", additional_vars)
  matched_data <- matched_data[, selected_vars]
  
  # 添加额外的列
  data_to_add <- data[rownames(matched_data), columns_to_add, drop = FALSE]
  matched_data <- cbind(matched_data, data_to_add)
  
  # 生成love plot
  love_plot <- love.plot(m.out,
                         stats = "mean.diffs",
                         threshold = 0.1,
                         var.order = "unadjusted",
                         binary = "std",
                         abs = TRUE) +
    theme_bw() +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 10))) +
    labs(title = paste("Standardized Mean Differences -", method),
         x = "Absolute Standardized Mean Differences")
  
  # 保存love plot
  ggsave(paste0("C:/MyProject/Myproject/code/LOS/newLOS/figures/love_plot_", method, ".png"),
         plot = love_plot, width = 8, height = 6)
  
  # 保存匹配后的数据
  write.csv(matched_data, 
            paste0("C:/MyProject/Myproject/code/LOS/newLOS/DATA/multimatch/matched_data_", 
                   method, ".csv"),
            row.names = FALSE)
  
  # 输出匹配结果摘要
  cat("\nMatching Method:", method, "\n")
  print(summary(m.out))
  
  # 平衡性评估
  bal.tab <- bal.tab(m.out, un = TRUE, m.threshold = 0.1)
  print(bal.tab)
  
}



# ------------------------END------------------------#



# 加载必要的包
library(MatchIt)
library(cobalt)
library(dplyr)
library(ggplot2)

# 创建必要的目录
dir.create("C:/MyProject/Myproject/code/LOS/newLOS/DATA/multimatch", recursive = TRUE, showWarnings = FALSE)
dir.create("C:/MyProject/Myproject/code/LOS/newLOS/figures", recursive = TRUE, showWarnings = FALSE)

# 读取数据
data <- read.csv("C:/MyProject/Myproject/code/LOS/data/datatest.csv")

# 定义用于匹配的变量
matching_vars <- c("gender", "age", "height", "weight", "WBC", "RBC", "Hgb", 
                   "PLT", "HCT", "PT", "INR", "APTT", "TT", "Fib", 
                   "DDimer", "FDP", "Wells_score_on_admission")

# 定义匹配方法
methods <- c("genetic", "nearest_caliper", "optimal_exact", "mahalanobis")

# 定义要添加的其他变量
additional_vars <- c("LOS", "IPR", "CER", "DIR", "DIC", "CLE","day_3","day_5","day_7","Days1_symptom_score")
columns_to_add <- c("Alprostadil", "Buqi.Tongluo.Capsules", "Diosmin", "Enoxaparin", 
                    "Furosemide", "Guhong.injection", "Nadroparin.Calcium", 
                    "Sodium.Aescinate", "Urokinase", "Warfarin", 
                    "Elevate.lower.limbs.to.30.degrees", "Local.immobilization", 
                    "BXpowder", "SbOintment")

# 循环进行不同方法的匹配
for(method in methods) {
  # 设置随机种子
  set.seed(1234)
  
  # 根据不同方法设置匹配参数
  if(method == "genetic") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "genetic",
                     pop.size = 200,
                     max.generations = 10,
                     wait.generations = 4,
                     ratio = 1)
  } else if(method == "nearest_caliper") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.2,
                     ratio = 1)
  } else if(method == "optimal_exact") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "optimal",
                     exact = c("gender"),
                     distance = "glm",
                     ratio = 1)
  } else if(method == "mahalanobis") {
    m.out <- matchit(CombineAnti ~ gender + age + height + weight + WBC + RBC + 
                       Hgb + PLT + HCT + PT + INR + APTT + TT + Fib + DDimer + 
                       FDP + Wells_score_on_admission,
                     data = data,
                     method = "nearest",
                     distance = "mahalanobis",
                     ratio = 1)
  }
  
  # 获取匹配后的数据并重命名subclass为pair_id
  matched_data <- match.data(m.out, include.s.weights = FALSE) %>%
    rename(pair_id = subclass)
  
  # 选择需要的变量（包括pair_id）
  selected_vars <- c("pair_id", matching_vars, "CombineAnti", additional_vars)
  matched_data <- matched_data[, selected_vars]
  
  # 添加额外的列
  data_to_add <- data[rownames(matched_data), columns_to_add, drop = FALSE]
  matched_data <- cbind(matched_data, data_to_add)
  
  # 生成love plot
  love_plot <- love.plot(m.out,
                         stats = "mean.diffs",
                         threshold = 0.1,
                         var.order = "unadjusted",
                         binary = "std",
                         abs = TRUE) +
    theme_bw() +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 10))) +
    labs(title = paste("Standardized Mean Differences -", method),
         x = "Absolute Standardized Mean Differences")
  
  # 保存love plot
  ggsave(paste0("C:/MyProject/Myproject/code/LOS/newLOS/figures/love_plot_", method, ".png"),
         plot = love_plot, width = 8, height = 6)
  
  # 保存匹配后的数据
  write.csv(matched_data, 
            paste0("C:/MyProject/Myproject/code/LOS/newLOS/DATA/multimatch/matched_data_", 
                   method, ".csv"),
            row.names = FALSE)
  
  # 输出匹配结果摘要
  cat("\nMatching Method:", method, "\n")
  print(summary(m.out))
  
  # 平衡性评估
  bal.tab <- bal.tab(m.out, un = TRUE, m.threshold = 0.1)
  print(bal.tab)
}