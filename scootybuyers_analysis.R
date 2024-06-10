
install.packages("ggvis")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plm")

library('ggvis')
library('tidyverse')
library('ggplot2')
library('plm')


scooty_buyers = read.csv('D:/RProject/scooty_buyers.csv', header=T, na.strings='')
head(scooty_buyers)
--------------------------------------------
class(scooty_buyers)
---------------------------------------------
str(scooty_buyers)
-----------------------------------------------
summary(scooty_buyers)
------------------------------------------------
levels(scooty_buyers$Gender)
----------------------------------------------
scooty_buyers$Marital.Status <- as.factor(scooty_buyers$Marital.Status)
scooty_buyers$Gender <- as.factor(scooty_buyers$Gender)
scooty_buyers$Home.Owner <- as.factor(scooty_buyers$Home.Owner)
scooty_buyers$Purchased.scooty <- as.factor(scooty_buyers$Purchased.scooty)
--------------------------------------------------
str(scooty_buyers)
------------------------------------------------
colSums(is.na(scooty_buyers))
----------------------------------------------------
summary(scooty_buyers)
----------------------------------------------
hist(scooty_buyers$Income)
----------------------------------------------
hist(scooty_buyers$Children, breaks = 20)
------------------------------------------------
hist(scooty_buyers$Cars, breaks = 15)
-------------------------------------------------
hist(scooty_buyers$Age)
---------------------------------------------
median(na.omit((scooty_buyers$Income)))
median(na.omit((scooty_buyers$Age)))
----------------------------------------------
scooty_buyers_clean <- scooty_buyers
colSums(is.na(scooty_buyers_clean))
-------------------------------------------------
# Income replaced with Median
scooty_buyers_clean$Income[is.na(scooty_buyers_clean$Income)] <- 
  median(na.omit((scooty_buyers$Income)))

# Age replaced with Median
scooty_buyers_clean$Age[is.na(scooty_buyers_clean$Age)] <- 
  median(na.omit((scooty_buyers$Age)))

colSums(is.na(scooty_buyers_clean))
------------------------------------------------

get_mode <- function(x) {                 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
-----------------------------------------------
# Marital Status replaced with Mode
scooty_buyers_clean$Marital.Status[is.na(scooty_buyers_clean$Marital.Status)] <- 
  get_mode(scooty_buyers$Marital.Status)

# Gender replaced with Mode
scooty_buyers_clean$Gender[is.na(scooty_buyers_clean$Gender)] <- 
  get_mode(scooty_buyers$Gender)

# Children replaced with Mode
scooty_buyers_clean$Children[is.na(scooty_buyers_clean$Children)] <- 
  get_mode(scooty_buyers$Children)

# Home Owner replaced with Mode
bike_buyers_clean$Home.Owner[is.na(bike_buyers_clean$Home.Owner)] <- 
  get_mode(bike_buyers$Home.Owner)

colSums(is.na(bike_buyers_clean))

---------------------------------------------------

# Cars replaced with Mean
scooty_buyers_clean$Cars[is.na(scooty_buyers_clean$Cars)] <- 
  mean(scooty_buyers$Cars, na.rm = TRUE)

colSums(is.na(scooty_buyers_clean))
------------------------------------------------------
write.csv(scooty_buyers_clean,"scooty_buyers_clean.csv", quote = FALSE, row.names = TRUE)
------------------------------------------------------
scooty_buyers <- scooty_buyers_clean
-------------------------------------------------------

counts <- table(scooty_buyers$Cars, scooty_buyers$Gender)
barplot(counts, main = '',
        xlab="Number of Gears",
        legend = rownames(counts))
----------------------------------------------------

plot(scooty_buyers$Income, type= "p", col="blue")
---------------------------------------------------

ggplot(scooty_buyers, aes(x = Age)) +
  geom_histogram()
---------------------------------------------------

plot(density(scooty_buyers$Income), main='Income Density Spread', col="red")
------------------------------------------------
ggplot(scooty_buyers,
       aes(y = Age, x = Gender)) +
  geom_point()
-----------------------------------------------
ggplot(scooty_buyers,
       aes(y = Age, x = Income)) +
  geom_point()
-------------------------------------------------

p3 <- ggplot(scooty_buyers,
             aes(x = Age,
                 y = Income)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))
p4 <- p3 + geom_point(aes(color = Age),data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAABa0lEQVR4Xu2XPU7DMBiGzcICxB1AHABuQM/ADSpEHBauwIjgBHAHBiA/GxILDBQxBBGxgBCoghEVJgZY6g41/QKp8Eeomtiqg+RHepf40+snstq6hFgslmKcx8lWM07a/QjFtKEL9ysBhTf3LcF5V6gCHdClVRLeWodcBnRBJ96nNHA0uoFOvA9AA7ZDBJnAz4cyZkFRC7x9EjUm8dqfjFvwOydzUWMar+diSFBQn11PHa7O45lfGBP8yiMNVhbwnIRhQcgrjdwlPDvgp+DxXa9wMnI2LpL3fpaxW0pFBCHc8ZmL/STBvYs3KZnEsOcaBSG9Wsg2/q8gPr5RolGQOyHzJLkKCX7QUT4kuij8NeOv1fHsALOC3pMTeYt4TsKYYKV/6kL3dPZofQav52JA8KCy1y0ncHfLXFj1Xvm76ZX/Be9TGviDc/vQSotV6XAuoOssTrbxPkpAIbw1HI1inpuXV5u432KxWCrGJ9oMoN6nUQB6AAAAAElFTkSuQmCC
                      alpha = 0.5,
                      size = 1.5,
                      position = position_jitter(width = 0.25, height = 0))
p4 +
  scale_x_discrete(name="Income") +
  scale_color_continuous(name="", low = "blue", high = "red")

------------------------------------------------

p5 <- ggplot(scooty_buyers, aes(x = Age, y = Occupation))
p5 + geom_line(aes(color = Age))

-------------------------------------------------
(p5 <- p5 + geom_line() +
    facet_wrap(~Gender, ncol = 10))
--------------------------------------------------

boxplot(scooty_buyers$Income, main = 'Income Boxplot')
boxplot(scooty_buyers[,c(1,4)], main='Multiple Box plots')
---------------------------------------------------

OutVals = boxplot(Scooty_buyers$Income)$out
print(OutVals)

which(scooty_buyers$Income %in% OutVals)

x = scooty_buyers$Income [!(scooty_buyers$Income %in% OutVals) ]
boxplot(x)
-----------------------------------------------------
summary(scooty_buyers)


------------------------------------------------------

columns_to_correlate <- c("Income", "Children", "Age", "Cars")

# Subset the data based on the selected columns
subset_data <- scooty_buyers[, columns_to_correlate]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)

# Print correlation matrix
print(correlation_matrix)
