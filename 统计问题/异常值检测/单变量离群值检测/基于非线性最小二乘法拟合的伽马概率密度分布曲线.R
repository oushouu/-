library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(nls2)

# 正态分布拟合vs伽马分布拟合vs手动调整伽马分布拟合
df1.2 %>%
  ggplot(aes(`計上～支払期限`)) +
  geom_histogram(aes(y =..density..), binwidth = 16, color = "white") +
  geom_line(aes(y = dnorm(`計上～支払期限`, mean(`計上～支払期限`), sd(`計上～支払期限`))), color = "red")

df1.2 %>% 
  ggplot(aes(`計上～支払期限`)) +
  geom_histogram(aes(y =..density..), binwidth = 16, color = "white") +
  geom_line(aes(y = dgamma(`計上～支払期限`, shape = alpha, rate = rate)), color = "red")

adj_rate <- 0.11
adj_shape <- adj_rate*(mean)

df1.2 %>% 
  ggplot(aes(`計上～支払期限`)) +
  geom_histogram(aes(y =..density..), binwidth = 16, color = "white") +
  geom_line(aes(y = dgamma(`計上～支払期限`, shape = adj_shape, rate = adj_rate)), color = "red")


# 第一步
## 日期数据直方图显示概率密度
df1.2 %>% 
  ggplot(aes(`計上～支払期限`)) +
  geom_histogram(aes(y =..density..), binwidth = 16, fill = "light blue")+
  stat_bin(aes(y=..density.., label=..density..), binwidth = 16, geom="text", hjust=1.8)+
  coord_flip()

## 日期数据按照上述直方图的binwidth分组并计算各组概率密度
density1.2 <- df1.2 %>% 
  mutate(interval_id = (`計上～支払期限`+7)%/%16) %>% 
  group_by(interval_id) %>% 
  summarise(count = n()) %>% 
  mutate(middle_interval = 16*interval_id, 
         density = count/nrow(df1.2)/16)
##density = count/sum(count)/binwidth



# 第二步
## 非线性最下二乘法拟合 
nls1.2 <- nls(# 公式：ガンマ分布密度関数
  formula = density ~ dgamma(middle_interval, shape, rate),
  
  # データ：元データから計算された各ノードと確率密度データ
  data = density1.2,
  
  # スタート：最初にフィードするパラメタ
  start = list(shape = adj_shape,
               rate = adj_rate)

## 概率密度预测值赋值
nls1.2_pred <- predict(nls1.2, density1.2)


## 拟合结果如下：
# Nonlinear regression model
# model: density ~ dgamma(middle_interval, shape, rate)
# data: density1.2
# shape   rate 
# 6.8583 0.1338 
# residual sum-of-squares: 2.152e-05
# 
# Number of iterations to convergence: 5 
# Achieved convergence tolerance: 8.566e-07


# 第三步
## 实际概率密度与模型预测密度对比
density1.2 %>% mutate(gamma_pred = nls1.2_pred) %T>% print() %>% 
  ggplot(aes())+
  geom_col(aes(x = middle_interval, y = density)) +
  geom_line(aes(x = middle_interval, y = gamma_pred), color = "#00BFC4")


# 第四步
## 根据第二部得到的最优参数结果画极大似然伽马密度分布曲线
df1.2 %>% 
  ggplot(aes(`計上～支払期限`)) +
  geom_histogram(aes(y =..density..), binwidth = 16, color = "white") +
  geom_line(aes(y = dgamma(`計上～支払期限`, shape = 6.8583 , rate = 0.1338)), color = "#F8766D")  


# 第五步

## 计算极大似然伽马分布的上限2.5%处阈值
qgamma(p = 0.975, 
       shape = 6.8583 , 
       rate = 0.1338)

# [1] 96.14495


## 定义阈值外的定义域并绘图将此部分上色
outlier_fill <- function(x) {
  y <- dgamma(x, shape = 6.8583 , rate = 0.1338)
  y[x < 96] <- NA
  return(y)
}

df1.2 %>% 
  ggplot(aes(`計上～支払期限`)) +
  # geom_histogram(aes(y =..density..), binwidth = 16, color = "white") +
  geom_line(aes(y = dgamma(`計上～支払期限`, shape = 6.8583 , rate = 0.1338)), color = "#F8766D") +
  geom_vline(aes(xintercept = qgamma(p = 0.975, shape = 6.8583 , rate = 0.1338))) +
  geom_text(aes(x= 90, label="上限2.5％閾値：96.14", y=0.01), colour="black", angle=90, vjust = 1.5, size=5) +
  stat_function(fun=outlier_fill, geom="area", fill="#ff2b00", alpha=1)


# 第六步
## 点散图绘制，阈值外标红
df1.2 %>% 
  mutate(外れ値 = ifelse(`計上～支払期限` <= qgamma(p = 0.975, shape = 6.8583 , rate = 0.1338), 
                      FALSE, TRUE)) %>% 
  ggplot(aes(record_date, `計上～支払期限`, color = 外れ値)) +
  geom_jitter(alpha = .5) +
  scale_color_manual(values=c("#00BFC4","#F8766D"))+
  labs(y= "計上～支払期限", x = "計上日")
