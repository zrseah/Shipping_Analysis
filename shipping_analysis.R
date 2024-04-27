#load data 
shippingdf <- read.csv("~/Case Studies/Shipping Case Study/Shipping_Analysis/shipping.csv")

#import library
library('stringr')
library('dplyr')
library('ggplot2')
library('magrittr')
library('patchwork')

#check structure of data 
str(shippingdf)

#data cleaning 

##change all column names to lower case
colnames(shippingdf) <- tolower(colnames(shippingdf))

##rename column names
colnames(shippingdf)[colnames(shippingdf) == "reached.on.time_y.n"] <- "punctuality"

##remove trailing spaces
shippingdf <- shippingdf %>%
  mutate_at(vars(warehouse_block, mode_of_shipment, product_importance, gender), trimws)

##check for duplicates
sum(duplicated(shippingdf))

##check for NAs
sum(is.na(shippingdf))

##capitalise the first letter of all entries in product importance 
shippingdf$product_importance <- str_to_title(shippingdf$product_importance)

#analysis 

# Gender breakdown
gender <- tibble(
  F = c(
    round(sum(shippingdf$gender == "F"),0),
    sum(shippingdf$gender == "F") / (sum(shippingdf$gender == "M") + sum(shippingdf$gender == "F")) * 100
  ),
  M = c(
    round(sum(shippingdf$gender == "M"),0),
    sum(shippingdf$gender == "M") / (sum(shippingdf$gender == "M") + sum(shippingdf$gender == "F")) * 100
  )
)

shipmentmode <- tibble(
  Flight = sum(shippingdf$mode_of_shipment == "Flight"),
  Road = sum(shippingdf$mode_of_shipment == "Road"),
  Ship = sum(shippingdf$mode_of_shipment == "Ship") 
)

whblock <- tibble(
  A = sum(shippingdf$warehouse_block == "A"),
  B = sum(shippingdf$warehouse_block == "B"),
  C = sum(shippingdf$warehouse_block == "C"),
  D = sum(shippingdf$warehouse_block == "D"),
  F = sum(shippingdf$warehouse_block == "F")
)

shipmentcolour <- c("pink1", "slategray1", "lightcyan1")
ggplot(shippingdf, aes(x = warehouse_block, fill = mode_of_shipment)) + 
  geom_bar(colour = "white", size = 0.5) +
  scale_fill_manual(values = shipmentcolour) +
  labs(x = "Warehouse Block", y = "Count", fill = "Shipment Mode") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))


##Customer rating 
customerrating <- tibble(
  '1' = sum(shippingdf$customer_rating == "1"),
  '2' = sum(shippingdf$customer_rating == "2"),
  '3' = sum(shippingdf$customer_rating == "3"),
  '4' = sum(shippingdf$customer_rating == "4"),
  '5' = sum(shippingdf$customer_rating == "5"),
)

ratings <- summary(shippingdf$customer_rating)

ratingswh <- aggregate(customer_rating ~ warehouse_block, shippingdf, function(x) round(mean(x), 2))
p2 <- ggplot(ratingswh, aes(x = warehouse_block, y = sprintf("%.2f", customer_rating))) +
  geom_point(colour = warehousecolours, size = 4) +
  scale_colour_manual(values = warehousecolours) +
  labs(x = "Warehouse Block", y = "Average Customer Rating") +
  theme(plot.background = element_rect(fill = "gray10"),
        panel.background = element_rect(fill = "grey1"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

ratingsshipment <- aggregate(customer_rating ~ mode_of_shipment, shippingdf, function(x) round(mean(x), 2))
p3 <-ggplot(ratingsshipment, aes(x = mode_of_shipment, y = sprintf("%.2f", customer_rating))) + 
  geom_point(colour = shipmentcolour, size = 4) +
  scale_colour_manual(values = shipmentcolour) +
  labs(x = "Mode of Shipment", y = "Average Customer Rating") +
  theme(plot.background = element_rect(fill = "gray10"),
        panel.background = element_rect(fill = "grey1"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12)) 

combined1 <- p2 + p3 + plot_layout(ncol = 2)

## Product punctuality and importance

punctualityrating <- aggregate(customer_rating ~ punctuality, shippingdf, function(x) round(mean(x),2))

punctuality <- tibble(
  On_time = sum(shippingdf$punctuality == "0"),
  Late = sum(shippingdf$punctuality == "1")
)

productimportance <- tibble(
  Low = sum(shippingdf$product_importance == "Low"), 
  Medium = sum(shippingdf$product_importance == "Medium"),
  High = sum(shippingdf$product_importance == "High"),
)

warehousepunctuality <-  shippingdf %>%  
  group_by(warehouse_block, punctuality) %>%  
  summarise(count = n()) %>%  
  ungroup %>%  
  group_by(warehouse_block) %>% 
  mutate(percentage = round((count/sum(count))* 100, 2))

punctualitycolour <- c("honeydew1", "indianred")
p4 <- ggplot(shippingdf, aes(x = warehouse_block, fill = as.character(punctuality))) +
  geom_bar(colour = "white", size = 0.5) +
  scale_fill_manual(values = punctualitycolour, labels = c("On Time", "Late")) +
  labs(x = "Warehouse Block", y = "Count", fill = "Punctuality") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

importancecolour <- c("lemonchiffon1","rosybrown1","indianred1")
p5 <- ggplot(shippingdf, aes(x = warehouse_block, fill = product_importance)) + 
  scale_fill_manual(values = importancecolour) +
  geom_bar(colour = "white", size = 0.5) +
  labs(x = "Warehouse Block", y = "Count", fill = "Product Importance")+
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

combined2 <- p4 + p5 + plot_layout(ncol = 2)

shipmentpunctuality <- shippingdf %>% 
  group_by(mode_of_shipment, punctuality) %>%  
  summarise(count = n()) %>%  
  ungroup() %>% 
  group_by(mode_of_shipment) %>% 
  mutate(percentage = round(count/sum(count)*100,2))

productorder <- c("Low", "Medium", "High")
shippingdf$product_importance <- factor(shippingdf$product_importance, levels = productorder)
productimportanceshippment <- shippingdf %>%  
  group_by(mode_of_shipment, product_importance) %>% 
  summarise(count = n()) %>%  
  ungroup() %>%  
  group_by(mode_of_shipment) %>% 
  mutate(percentage = round((count / sum(count)) * 100, 2))

p6 <- ggplot(shippingdf, aes(x = mode_of_shipment, fill = as.character(punctuality))) +
  geom_bar(colour = "white", size = 0.5) +
  scale_fill_manual(values = punctualitycolour, labels = c("On Time", "Late")) +
  labs(x = "Mode of Shipment", y = "Count", fill = "Punctuality") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

p7 <- ggplot(shippingdf, aes(x = mode_of_shipment, fill = product_importance)) +
  scale_fill_manual(values = importancecolour) +
  geom_bar(colour = "white", size = 0.5) +
  labs(x = "Mode of Shipment", y = "Count", fill = "Product Importance") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

combined3 <- p6 + p7 + plot_layout(ncol = 2)

productimportancepunctuality <- shippingdf %>%  
  group_by(product_importance, punctuality) %>%  
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(product_importance) %>% 
  mutate(percent = round(count/sum(count)*100, 2))

ggplot(shippingdf, aes(x = product_importance, fill = as.character(punctuality))) +
  geom_bar(colour = "white", size = 0.5) +
  scale_fill_manual(values = punctualitycolour, labels = c("On Time", "Late")) +
  labs(x  = "Product Importance", y = "Count", fill = "Punctuality") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

## Customer Calls
calls <- summary(shippingdf$customer_care_calls)

totalcalls <- aggregate(customer_care_calls ~ warehouse_block, data = shippingdf, FUN = sum)
warehousecolours <- c("cadetblue2", "honeydew3", "lightsteelblue1", "mistyrose1", "thistle2")
ggplot(totalcalls, aes(x = warehouse_block, y = customer_care_calls, fill = warehousecolours)) + 
  geom_bar(stat = "identity", colour = "white", size = 0.5) +
  scale_fill_manual(values = warehousecolours) +
  labs(x = "Warehouse Block", y = "Count of Customer Care Calls") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12)) + 
  guides(fill = FALSE)

totalcallsshipment <- aggregate(customer_care_calls ~ mode_of_shipment, data = shippingdf, FUN =sum)
ggplot(totalcallsshipment, aes(x = mode_of_shipment, y = customer_care_calls, fill = shipmentcolour)) + 
  geom_bar(stat = "identity", colour = "white", size = 0.5) +
  scale_fill_manual(values = shipmentcolour) +
  labs(x = "Warehouse Block", y = "Count") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12)) + 
  guides(fill = FALSE)
  
## Product costs 
productcosts <- summary(shippingdf$cost_of_the_product)

breaks <- c(0, 99, 199, 299, Inf)
labels <- c("$99 & Below", "$100 - $199", "$200 - $299", "$300 & Above")
shippingdf$cost_of_the_product_range <- cut(shippingdf$cost_of_the_product, breaks = breaks, labels = labels, include.lowest = TRUE)
pricerangecolour <- c("lightcyan1", "lavender", "lightpink1", "lemonchiffon1")
ggplot(shippingdf, aes(x = mode_of_shipment, fill = cost_of_the_product_range)) +
  geom_bar(colour = "white", size = 0.5) +
  scale_fill_manual(values = pricerangecolour) +
  labs(x = "Mode of Shipment", y = "Count", fill = "Product Price Range") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

# Discount offered
discount <- summary(shippingdf$discount_offered)

breaks2 <- c(0, 5, 10, 20, 30, 40, 50, 60 , 70)
discountlabels <- c("5% & Below", "6% to 10%", "11% to 20%", "21% to 30%", 
                    "31% to 40%", "41% to 50%", "51% to 60%", "61% to 70%")
shippingdf$discount_offered_range <- cut(shippingdf$discount_offered, breaks = breaks2, labels = discountlabels, include.lowest  = TRUE)
discountcolours <- c("cornsilk", "azure3", "slategray4","mistyrose2", "paleturquoise2", "lavenderblush", "thistle2", "grey")
ggplot(shippingdf, aes(x = discount_offered_range)) + 
  geom_bar(colour = "white", size = 0.5, fill = discountcolours) +
  scale_fill_manual(values = discountcolours) +
  labs(x  = "Discount", y = "Count") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

discountrating <- aggregate(customer_rating ~ discount_offered_range, shippingdf, function(x) round(mean(x),2))
ggplot(discountrating, aes(x = discount_offered_range, y = customer_rating)) + 
  geom_point(colour = discountcolours, size = 4) +
  scale_colour_manual(values = discountcolours) + 
  labs(x = "Discount", y = "Average Customer Rating") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12))

# Weight 
weight <- summary(shippingdf$weight_in_gms)

breaks3 <- c(0, 999, 1999, 2999, 3999, 4999, 5999, 6999, Inf)
weightlabels <- c("999g & Below", "1000g to 1999g", "2000g to 2999g", "3000g to 3999g", "4000g to 4999g", 
                  "5000g to 5999g", "6000g to 6999g", "7000g & Above")
shippingdf$weight_range <- cut(shippingdf$weight_in_gms, breaks = breaks3, labels = weightlabels, include.lowest = TRUE)
ggplot(shippingdf, aes(x = weight_range, fill = mode_of_shipment)) +
  geom_bar(colour = "white", size = 0.5) + 
  scale_fill_manual(values = shipmentcolour) +
  labs(x = "Weight Range", fill = "Mode of Shipment") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey1"),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(family = "Arial", color = "white", size = 12),
        axis.text = element_text(family = "Arial", color = "white", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# prior purchases 
priorpurchases <- summary(shippingdf$prior_purchases)