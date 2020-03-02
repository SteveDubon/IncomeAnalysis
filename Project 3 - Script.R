# ANOVA Test
m1 = aov(d1$appxincome~d1$sex*d1$educ)
summary(m1)

# Effect Size
library(lsr)
etaSquared(m1)

# Levene's test
leveneTest(d1$appxincome ~ d1$sex*d1$educ, center=median)

# Graphing Values (Intervals)
eduGender = c("Female", "Male","fCollege", "fHigh", "fNoHigh", "mCollege", "mHigh", "mNoHigh")
avgIncome = c(83626.01, 89501.19, 93428.43, 59837.84, 68465.91, 91071.43, 90390.95, 71791.67)
lowerInt = c(76326.01, 82901.66, 84628.43, 46837.84, 33465.91, 83671.43, 76390.95, 41791.67)
upperInt = c(90925.01, 96101.66, 102228.43, 72837.84, 103465.91, 98471.43, 104390.95, 101791.67)
d.ci = data.frame(Color = c("d","e","f","g","h","i", "j", "k"))
avgIncome = c(83626.01, 89501.19, 93428.43, 59837.84, 68465.91, 91071.43, 90390.95, 71791.67)
ci.upper = c(90925.01, 96101.66, 102228.43, 72837.84, 103465.91, 98471.43, 104390.95, 101791.67)
ci.lower = c(76326.01, 82901.66, 84628.43, 46837.84, 33465.91, 83671.43, 76390.95, 41791.67)
color = c("(F) Female", "(M) Male","College (F)", "HS Grad (F)", "No HS (F)", "College (M)", "HS Grad (M)", "No HS (M)")
ggplot(d.ci,aes(x = color,y = avgIncome, fill = color))+
  ggtitle("Expected Income By Gender and Education")+                                            # Title 
  xlab("Education Level") + ylab("Average Income (approx.)")+                                    # Axis names
  theme(plot.title = element_text(lineheight=.8, size = 23, face="bold"),                        # Labels styles
        axis.title.x = element_text(color = "black", size = 14, face = "bold"),       
        axis.title.y = element_text(color = "black", size = 14, face = "bold"))+
  geom_bar(stat = "identity", color = "black")+
  geom_errorbar(aes(ymin=ci.lower,ymax=ci.upper),width=.3)+
  scale_fill_brewer(palette="Paired")

# Boxplot
t = boxplot(mCollege$appxincome, fCollege$appxincome, mHigh$appxincome, fHigh$appxincome, mNoHigh$appxincome, fNoHigh$appxincome,
              main = "Income by Education and Gender", ylab = "Average Income", xlab = "Education by Gender", 
              col = (c("light blue", "pink")),
              names = (c("College (M)", "College (F)", "HS (M)", "HS (F)", "No HS (M)", "No HS(F)")))

# Interaction Plot
interaction.plot(x.factor     = d1$sex,
                 trace.factor = d1$educ, 
                 response     = d1$appxincome, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")


# Model Comparison
library("lmSupport", lib.loc="~/R/win-library/3.4")
modelCompare(ModelA = lmAll, ModelC = lmBudsFrost)
