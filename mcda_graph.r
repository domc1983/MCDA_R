library(ggvis)
library(dplyr)

switcher <- 0 #0 for benefit, 1 for cost benefit

#At the moment, the data must have headers: Name, Cost, C1, C2, C3, C4
MyData <- read.csv("MCDA_Input.csv", header = TRUE, sep = ",")

#At the moment, the weights for the hard-coded 4 criteria C1 - C4 are entered here:
weights <- c(0.25, 0.25, 0.25, 0.25)

#Add a column for the weighted sum (benefit score) and the Cost Benefit (Benefit per unit cost)
MyData <- MyData %>% 
  mutate(Benefit = MyData$C1 * weights[1] + MyData$C2 * weights[2] + MyData$C3 * weights[3] + MyData$C4 * weights[4],
         CostBenefit = Benefit / MyData$Cost
         )

#Add a column for a ranking when ordered by Benefit Score.
MyData <- MyData %>% arrange(desc(Benefit)) %>% mutate(RankByBenefit = rank(desc(Benefit)), cumul_benefit = cumsum(Benefit), cumul_cost_b = cumsum(Cost))

#Add a column for a ranking when ordered by Cost Benefit.
MyData <- MyData %>% arrange(desc(CostBenefit)) %>% mutate(RankByCostBenefit = rank(desc(CostBenefit)), cumul_costbenefit = cumsum(CostBenefit), cumul_cost_cb = cumsum(Cost))

#Choose which graph to show (defined by switcher variable)
if(switcher==0){
  MyData %>% arrange(RankByBenefit) %>% ggvis(~cumul_cost_b, ~cumul_benefit) %>% layer_text(text:= ~Name, dx := 10, dy := -5) %>% layer_points(fill := "red") %>% layer_lines()
} else if(switcher==1){
  MyData %>% arrange(RankByCostBenefit) %>% ggvis(~cumul_cost_cb, ~cumul_costbenefit) %>% layer_text(text:= ~Name, dx := 10, dy := -5) %>% layer_points(fill := "blue") %>% layer_lines()
} else {
  print("Switcher was neither 0 - Benefit Graph - or 1 - Cost Benefit graph")
}
