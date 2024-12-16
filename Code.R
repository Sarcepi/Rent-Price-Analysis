library(dplyr)
library(caret)
library(ModelMatrixModel)
library(mgcv)
library(ggplot2)
library(sf)
library(car)
library(corrplot)
library(MASS)
library(RcmdrMisc)
library(randomForest)
library(gbm)
library(mpae)
library(knitr)
library(pdp)
library(gridExtra)
library(glmnet)


data<-read.csv2("/Users/aa/Downloads/apartment_data (2).csv",sep=",",header=T)
madrid<-data[251:nrow(data),]
summary(madrid)
glimpse(madrid)

madrid$Price<-as.numeric(gsub("\\.","",madrid$Price))
madrid<-madrid%>%
  filter(Zona!="Capital")
madrid <- madrid %>%
  mutate(Zona = ifelse(Zona %in% c("Barrio de Salamanca", "Salamanca"), "Salamanca", Zona)) %>%
  mutate(Zona = ifelse(Zona %in% c("Blas", "San Blas"), "San Blas - Canillejas", Zona)) %>%
  mutate(Zona = ifelse(Zona %in% c("Vallecas","Villa de Vallecas"), "Villa de Vallecas", Zona)) %>%
  mutate(Zona = ifelse(Zona %in% c("Lineal", "Ciudad Lineal"), "Ciudad Lineal", Zona)) %>%
  mutate(Zona = ifelse(Zona %in% c("Moncloa - Aravaca", "Aravaca"), "Moncloa - Aravaca", Zona)) %>%
  filter(Bathrooms < 10) %>%
  filter(Square.Meters > 10) %>%
  mutate(Zona = as.factor(Zona))

madrid<-na.omit(madrid)  
summary(madrid)
glimpse(madrid )


ggplot(madrid, aes(x = Price)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Price",
       x = "Price",
       y = "Frequency") +
  theme_classic()

ggplot(madrid, aes(x = Square.Meters)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Square.Meters",
       x = "Price",
       y = "Frequency") +
  theme_classic()



ggplot(madrid, aes(x = `Square.Meters`, y = Price,color = Zona)) +
  geom_point() +
  labs(title = "Price vs. Square Meters",
       x = "Square.Meters",
       y = "Price") +
  theme_classic()

par(mfrow=c(1,2))
ggplot(madrid) +
  aes(x = "", y = Price) +
  geom_boxplot(fill = "blue") +
  theme_minimal()
ggplot(madrid) +
  aes(x = "", y = Square.Meters) +
  geom_boxplot(fill = "blue") +
  theme_minimal()





par(mfrow=c(1,2))
ggplot(madrid) +
  aes(x = "", y = Bathrooms) +
  geom_boxplot(fill = "blue") +
  theme_minimal()
ggplot(madrid) +
  aes(x = "", y = Rooms) +
  geom_boxplot(fill = "blue") +
  theme_minimal()+
  ggtitle("Rooms")


top_prices_indices <- order(madrid$Price, decreasing = TRUE)[1:3]
madrid$Price[top_prices_indices]
madrid <- madrid[-top_prices_indices, ]
top_prices_indices2 <- order(madrid$Square.Meters, decreasing = TRUE)[1:3]
madrid$Square.Meters[top_prices_indices2]
madrid <- madrid[-top_prices_indices2, ]

madrid_subset <- madrid[, !names(madrid) %in% "Zona"]
plot(madrid_subset)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = madrid[, c("Square.Meters","Rooms","Bathrooms")], 
            y = madrid$Price, 
            plot = "scatter", 
            layout = c(3, 1))



theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = madrid[, c("Square.Meters","Rooms","Bathrooms")], 
            y = log(madrid$Price), 
            plot = "scatter",type = c("p", "smooth"),
            span = 1, 
            layout = c(3, 1))



madrid_map <- st_read("/Users/aa/Downloads/Distritos/Distritos.shp")
madrid_map$Zona<-madrid_map$NOMBRE
unique(madrid$Zona)
madrid <- madrid[madrid$Zona != "Capital", ]

apartment_summary <- madrid %>%
  group_by(Zona) %>%
  summarise(apartments = n())

madrid_map <- madrid_map %>%
  left_join(apartment_summary, by = "Zona")

madrid_map$apartments[is.na(madrid_map$apartments)] <- 0

ggplot(data = madrid_map) +
  geom_sf(aes(fill = apartments)) +  # Fill districts based on the 'apartments' count
  scale_fill_gradientn(
    colors = c("lightblue", "green", "yellow", "red"),
    values = scales::rescale(c(0, max(madrid_map$apartments)/4, max(madrid_map$apartments)/2, max(madrid_map$apartments))),
    na.value = "grey"
  ) +
  labs(
    title = "Apartments by District in Madrid",
    fill = "Number of Apartments"
  ) +
  theme_minimal()

zona_table <- table(madrid$Zona)

# Display the table using knitr's kable() function
kable(zona_table, caption = "Frequency Table of Zona")



set.seed(1)
nobs <- nrow(madrid)
itrain <- sample(nobs, 0.8 * nobs)
train <- madrid[itrain, ]
test <- madrid[-itrain, ]
train$Price <- log(train$Price)
test$Price <- log(test$Price)
dim(train)
dim(test)



linear_model<-lm(Price~Square.Meters+Rooms+Bathrooms+Zona,data=train)
summary(linear_model)
res<-linear_model$residuals
qqPlot(res)
shapiro.test(res)
madrid_subset <- madrid[, !names(madrid) %in% "Zona"]
mat_cor<-cor(madrid_subset)
corrplot(mat_cor,method = "ellipse")

car::vif(linear_model)
final_linear_model<-stepwise(linear_model)
model_with_interaction<-lm(Price~(Square.Meters*Bathrooms*Rooms)+Zona,data=train)

lm_prediction<-predict(model_with_interaction,newdata=test)
ab<-accuracy(lm_prediction,test$Price,na.rm = T,row_name = "Linear Regression" )

summary(model_with_interaction)
car::vif(model_with_interaction, type = 'predictor')
anova(final_linear_model,model_with_interaction)
accuracy <- function(pred, obs, na.rm = FALSE, 
                     tol = sqrt(.Machine$double.eps), 
                     row_name = NULL, 
                     results_df = NULL) {
  
  # Calculate errors
  err <- obs - pred 
  
  if(na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }
  
  # Calculate metrics
  metrics <- c(
    rmse = sqrt(mean(err^2)), 
    mae = mean(abs(err)), 
    r.squared = 1 - sum(err^2) / sum((obs - mean(obs))^2)
  )
  
  # Convert metrics to a data frame
  metrics_df <- as.data.frame(t(metrics))
  
  # Add row name if provided
  if (!is.null(row_name)) {
    rownames(metrics_df) <- row_name
  }
  
  # Append metrics to the results data frame if provided
  if (!is.null(results_df)) {
    results_df <- rbind(results_df, metrics_df)
  } else {
    results_df <- metrics_df
  }
  
  return(results_df)
}

obs<-test$Price
pred.plot(lm_prediction, obs, xlab = "Predicción", ylab = "Observado")
x_train<-(model.matrix(Price ~ ., data = train))[, -1]

y_train<- train$Price

x_test<-(model.matrix(Price ~ ., data = test))[, -1]
y_test<-test$Price



set.seed(1)
cv.ridge <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv.ridge)
cv.ridge$lambda.1se


coef(cv.ridge)
pred <- predict(cv.ridge, newx = x_test) # s = "lambda.1se"
ridge<-accuracy(pred, y_test,row_name = "Ridge regression");ridge
performance<-rbind(ab,ridge)

cv.lasso<- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv.lasso)
cv.lasso$lambda.1se
coef(cv.lasso)
pred <- predict(cv.lasso, newx = x_test)
lasso<-accuracy(pred, y_test,row_name = "Lasso");lasso
performance<-rbind(performance,lasso)

caret.glmnet <- train(x_train, y_train, method = "glmnet", 
                      preProc = c("zv", "center", "scale"), tuneLength = 10,
                      trControl = trainControl(method = "cv", number = 10))
caret.glmnet

ggplot(caret.glmnet, highlight = TRUE)

pred <- predict(caret.glmnet, newdata = x_test)
elastic<-accuracy(pred, y_test,row_name = "Elastic Net")
performance<-rbind(performance,elastic)


rf<-randomForest(Price~.,data=train);rf
plot(rf, main = "Random Forest")
varImpPlot(rf)
pdp1 <- partial(rf, "Square.Meters")
p1 <- plotPartial(pdp1)
pdp2 <- partial(rf, c("Bathrooms"))
p2 <- plotPartial(pdp2)
pdp3 <- partial(rf, c("Rooms"))
p3 <- plotPartial(pdp3)
grid.arrange(p1, p2,p3, ncol = 3)

pred <- predict(rf, newdata = test)
rf<-accuracy(pred, obs,row_name = "Random Forest");rf
performance<-rbind(performance,rf)

gam <- gam(
  Price ~ s(Square.Meters,Bathrooms,Rooms)+ Zona, 
  data = train, 
  family = gaussian()
)
summary(gam)

vis.gam(gam, view = c("Square.Meters", "Bathrooms"), plot.type = "persp", 
        theta = 30, phi = 30, 
        main = "3D Interaction of Square Meters and Bathrooms",
        color = "heat", zlab = "Price")
vis.gam(gam, view = c("Square.Meters", "Bathrooms"), plot.type = "contour", 
        color = "topo", main = "2D Contour Plot of Square Meters and Bathrooms")


pred <- predict(gam, newdata = test)
g<-accuracy(pred, obs,row_name = "GAM");g
performance<-rbind(performance,g)

pred.plot(pred, obs, xlab = "Predicción", ylab = "Observado")

trControl <- trainControl(method = "cv", number = 5)

caret.xgb <- train(Price ~ ., method = "xgbTree", data = train,
                   trControl = trControl, verbosity = 0)
caret.xgb

pred <- predict(caret.xgb, newdata = test)
xgb<-accuracy(pred, obs,row_name = "XGB");xgb
performance<-rbind(performance,xgb)

kable(performance,main="Models Performance")


