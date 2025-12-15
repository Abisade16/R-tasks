#Importing library
library(tidyverse)
library(readxl)

#importing the australia soybean dataset
data2 <- read_csv("australia.soybean.csv")

#basic scatterplot of oil and protein
ggplot(data2,aes(x=oil,y=protein))+
  geom_point()

#scatterplot of oil and protein with layers
ggplot(data2, aes(x = oil, y = protein)) +
  labs(
    title= "Relationship between Oil and Protein",
    subtitle="Oil(x) and Protein(y)",
    caption="Data:: Australia Soybean",
    x= 'Oil',
    y="Protein"
  )+
  geom_point(shape=17)

#Scatterplot plot of oil and protein by location
s <- ggplot(data2, aes(x = oil, y = protein, color=loc)) +
  scale_color_discrete(name="Location")+ #correcting the legend's name
  geom_point()
s <-s+ #setting the labels
  labs( 
    title= "Relationship between Oil and Protein Based on Location",
    x= 'Oil',
    y="Protein"
  )
s + facet_grid(. ~ loc) #divides plot into panels

#importing the earlywhitecorn dataset
data3 <- read_csv("ars.earlywhitecorn96.csv")

#scatterplot of earht and yield by location
p <- ggplot(data3, aes(x=earht, y= yield, color=loc))+
  scale_color_discrete(name="Location")+ #correcting the legend's name
  geom_point()
p <- p+ #setting the labels
  labs(
    title="Relationship between Earht and Yield by location",
    subtitle="Scatterplot of Earht and Yield",
    caption ="Data:: Earlywhitecorn",
    x="Earht",
    y="Yield"
  )
p

#histogram and density plot of yield by location
y <- ggplot(data3,aes(x=yield,y=..density..))+
  geom_histogram(bins=30)+
  geom_density(color="skyblue", linewidth=1)+
  facet_wrap(.~loc,scales="free") #divides plot into panels of different scales
y <- y+ #setting the labels
  labs(
    title = "Histogram and Density of Yield by Location",
    x = "Yield",
    y = "Density"
    ) 
y+theme_minimal() #setting the plot's theme

#boxplot of yield by location
l <- ggplot(data3,aes(x=yield, y=loc, fill=loc))+
  geom_boxplot()+
  labs(#setting the labels
    title="Boxplot of Yield by Location",
    x="Yield",
    y=""
    )
l+guides(fill="none") #creates no legend

#importing iris dataset
data4 <- read_excel("iris.xlsx")

#summary table of variables by Species
iris_summary_all <- data4 |>
  group_by(Species)|>
  summarise(
    nSPecies=n(),
    across(where(is.numeric), list(
    Mean = mean,
    SD = sd,
    Var = var
  )))

#Scatterplot of Sepal Legnth and Petal Length by Species
b <- ggplot(data4, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")#Fitting a regression line for each Species
b <- b+
  labs( #setting the labels
    title="Relationship between Sepal Length and Petal Length by Species",
    x="Sepal Length",
    y="Petal Length"
  )
b+theme_light()
ggsave(filename="SL and PL relationship.png")

#Boxplot of Sepal Width by Species
d <- ggplot(data4, aes(y=Sepal.Width, x=Species, fill=Species))+
  geom_boxplot()+
  labs( #setting labels
    title="Boxplot of Sepal Width by Species",
    y="Sepal Width",
    x="Species"
  )
d+guides(fill="none") #creates no legend

#importing the dataset
Barley_Multi_Environment_Trial <- read_csv("steptoe.morex.pheno.csv")

#converting dataframe to tibble
regression <- as_tibble(Barley_Multi_Environment_Trial)

#choosing the most linear relationship
regression |>
  ggplot(aes(x=height,y=yield))+
  geom_point()+
  geom_smooth()

#simple regression model
srm <- lm(formula = yield~ height, data = regression)
summary(srm)

#multiple regression model
mrm <- lm(formula = yield~ height+malt, data=regression)
summary(mrm)
