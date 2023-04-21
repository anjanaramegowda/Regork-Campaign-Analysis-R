library(completejourney)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(zoo)
library(tidyverse)
library(lubridate)

# Required data frames from the completejourney dataset
transactions_df <- get_transactions()
promotions_df <- get_promotions()
products_df <- products

campaigns_df <- campaigns %>%
  transform(campaign_id = as.numeric(campaign_id))

coupons_df <- coupons %>%
  transform(campaign_id = as.numeric(campaign_id)) 

campaign_descriptions_df <- campaign_descriptions %>%
  transform(campaign_id = as.numeric(campaign_id)) 

camp_prod_tsv <- coupons_df %>%
  select(campaign_id, product_id) %>%
  inner_join(campaign_descriptions_df, by = "campaign_id") %>%
  select(campaign_id, product_id, campaign_type) %>%
  inner_join(transactions_df, by = "product_id") %>%
  mutate(transaction_month = lubridate::month(transaction_timestamp, label = TRUE)
         , month_num = lubridate::month(transaction_timestamp))%>%
  select(campaign_type, campaign_id, product_id, transaction_month, month_num, sales_value) %>%
  group_by(campaign_id, campaign_type, month_num, transaction_month) %>%
  summarise(total_sales = sum(sales_value)) %>%
  arrange(campaign_type, campaign_id)

#Households Targeted per campaign
camp_hh_targeted <- campaigns %>%
  group_by(campaign_id) %>%
  summarise(hh_targeted = n_distinct(household_id)) %>%
  arrange(campaign_id) %>%
  inner_join(campaign_descriptions, by = "campaign_id") %>%
  transform(campaign_id = as.numeric(campaign_id)) %>%
  select(campaign_type, campaign_id, hh_targeted) %>%
  group_by(campaign_type)

camp_hh_redeemed <- campaigns %>%
  select(campaign_id) %>%
  distinct() %>%
  left_join(coupon_redemptions, by = c("campaign_id")) %>%
  group_by(campaign_id) %>%
  summarise(hh_redeemed = n_distinct(household_id)) %>%
  arrange(campaign_id) %>%
  inner_join(campaign_descriptions, by = "campaign_id") %>%
  transform(campaign_id = as.numeric(campaign_id)) %>%
  select(campaign_id, campaign_type, hh_redeemed)


camp_perf_df <- camp_hh_targeted %>%
  inner_join(camp_hh_redeemed, by = "campaign_id") %>%
  arrange(campaign_id) %>%
  mutate(redemption_rate = (hh_redeemed/hh_targeted)*100) %>%
  rename(campaign_type = campaign_type.x) %>%
  select(campaign_id, campaign_type, hh_targeted, hh_redeemed, redemption_rate) %>%
  arrange(desc(redemption_rate))

max_camp_df <- campaigns %>%
  select(campaign_id) %>%
  transform(campaign_id = as.numeric(campaign_id)) %>%
  summarise(cnt = max(campaign_id))
maximum_campaign_id <- max_camp_df$cnt

overall_camp_perf <- camp_perf_df %>%
  ggplot(aes(y =redemption_rate, x = campaign_id, color = campaign_type)) +
  geom_point(size =2) +
  scale_color_manual(values = c('#F8766E',
                                '#00BA38',
                                '#619CFF'))+
  scale_x_continuous(breaks = seq(0,maximum_campaign_id+1,2)) +
  facet_wrap(~ campaign_type) +
  ylab('Coupon Redemption Rate') +
  xlab('Campaign Id') +
  labs(
    title = "Overall Campaign Performance",
    subtitle = "Coupons redemtion rate is coupon redemption for targeted households",
    caption = "") +
  theme_light()+
  theme(axis.text.x = element_text(angle=360, vjust=1, hjust=1))

# Type B Campaign Activity vs Sales Generated

campaign_months <-c(3,4,5,6,7,10,11,12)
type_b_sales <-campaign_descriptions %>%
  inner_join(coupons, by="campaign_id") %>%
  filter(campaign_type== 'Type B') %>%
  inner_join(transactions_df, by="product_id") %>%
  mutate(month=month(transaction_timestamp),
         Camp_there =case_when(
           month %in% campaign_months ~ "Yes",
           TRUE ~ "No")) %>%
  group_by(month,Camp_there)%>%
  summarize(sum= sum(sales_value))
type_b_sales %>%
  ggplot()+
  geom_line(aes(x=month, y=sum))+
  geom_point(aes(x=month, y=sum, group = Camp_there,color= Camp_there),size=2)+
  scale_y_continuous(labels=scales::dollar_format())+
  xlab("Months") +
  ylab("Mean Sales") +
  labs(title= "Impact of Campaigns on Sales - Type B")+
  scale_x_discrete(limits = c(1:12)) +
  guides(col=guide_legend("Campaigns?"))

# Impact Of Campaign Duration On Redemption Rate (RR)

campaign_duration_rr <- campaign_descriptions %>%
  transform(campaign_id=as.numeric(campaign_id)) %>%
  inner_join(camp_perf_df, by= "campaign_id")%>%
  mutate(
    campaign_duration = end_date - start_date,
    campaign_duration = case_when(
      campaign_duration %in% 30:40 ~ "30 to 40",
      campaign_duration %in% 41:50 ~ "40 to 50",
      campaign_duration %in% 51:60 ~ "50 to 60",
      campaign_duration %in% 51:161 ~ "Over 60")) %>%
  group_by(campaign_duration)%>%
  summarise(mean_pf=mean(redemption_rate))
print(campaign_duration_rr)

# Coupon Redemption Of Household – By Income Range

demo_income <-demographics %>%
  left_join(coupon_redemptions, by="household_id")%>%
  mutate(redemption= case_when(
    is.na(redemption_date) ~ 'Never Used a promotion',
    TRUE ~'Used a Promotion'
  )) %>%
  select(age,income,redemption) %>%
  group_by(income,redemption)%>%
  summarize(hh_count=n())

demo_income_plot <- demo_income %>%
  ggplot(aes(reorder(income, hh_count),hh_count, fill=redemption)) +
  geom_bar(position="stack", space = 1,stat="identity", alpha=0.7) +
  #scale_x_discrete(guide = guide_axis(n.dodge=2))+
  coord_flip()+
  xlab("Income Range") +
  ylab("Number of Households")           +
  guides(col=guide_legend("Coupon Redemption?"))           +
  labs(title= " Households Redeeming Promotions by Income Range")

# Coupon Redemption Of Household – By Age

demo_age <-demographics %>%
  left_join(coupon_redemptions, by="household_id")%>%
  mutate(redemption= case_when(
    is.na(redemption_date) ~ 'Never Used a promotion',
    TRUE ~'Used a Promotion'
  )) %>%
  select(age,income,redemption) %>%
  group_by(age,redemption)%>%
  summarize(hh_count=n())

demo_age %>%
  ggplot(aes(reorder(age,hh_count),hh_count,fill=redemption)) +
  geom_bar(position="stack", stat="identity",alpha=0.7) +
  coord_flip()           +
  xlab("Age Group") +
  ylab("Number of Households")           +
  guides(col=guide_legend("Coupon Redemption?"))           +
  labs(title= "Number of Households Redeeming Promotions by Age")

# Campaign total sales value info with start date and end date

camp_info_df <- campaign_descriptions_df %>%
  select(campaign_id, campaign_type, start_date, end_date) %>%
  mutate(start_month = lubridate::month(start_date), end_month = lubridate::month(end_date)
         , start_month_nm = lubridate::month(start_date, label = TRUE), 
         end_month_nm = lubridate::month(end_date, label = TRUE))

camp_prod_tsv <- camp_prod_tsv %>%
  inner_join(camp_info_df, by = c("campaign_id","campaign_type")) %>%
  select(campaign_id, campaign_type, transaction_month, total_sales, start_month, end_month, start_month_nm,
         end_month_nm, month_num) 

# minimum and maximum total sales value for campaigns 

min_max_sales <- camp_prod_tsv %>%
  group_by(campaign_id) %>%
  summarize(max_sales = max(total_sales), min_sales = min(total_sales))

camp_prod_tsv <- camp_prod_tsv %>%
  inner_join(min_max_sales, by = "campaign_id")

# Type - A campaigns trend wrt total sales value between start and end date

type_a_campaigns <- campaign_descriptions %>% filter(campaign_type == "Type A") %>% select(campaign_id)

for(i in type_a_campaigns$campaign_id){
  p <-camp_prod_tsv %>%
    filter(campaign_id == i) %>%
    ggplot() +
    geom_line(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id)) +
    geom_point(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id),size = 2) +
    #geom_point(aes(transaction_month, camp_month),size = 5) +
    # geom_line(aes(transaction_month, camp_month)) +
    geom_rect(aes(NULL, NULL, xmin = start_month_nm, xmax = end_month_nm, ymin = min_sales, ymax = max_sales), fill = factor(1), alpha = 0.01) +
    theme_light() +
    theme(legend.position = 'right') +
    labs(title = "Sales By Month Per Campaign",
         x = "Months",
         y = "Total Sales"
    )
  new_name <- paste0("plot_",i)
  assign(new_name, p)
  rm(p)
}

plot_18
grid.arrange(plot_18, plot_27, plot_8, plot_13, layout_matrix = rbind(c(1,2),c(3,4)))

# Type - B campaigns trend wrt total sales value between start and end date

type_b_campaigns <- campaign_descriptions_df %>% filter(campaign_type == "Type B") %>% filter(start_date > '2017-01-01' & end_date <= '2017-12-31') %>% select(campaign_id)

for(i in type_b_campaigns$campaign_id){
  p <-camp_prod_tsv %>%
    filter(campaign_id == i) %>%
    ggplot() +
    geom_line(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id)) +
    geom_point(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id),size = 2) +
    #geom_point(aes(transaction_month, camp_month),size = 5) +
    # geom_line(aes(transaction_month, camp_month)) +
    geom_rect(aes(NULL, NULL, xmin = start_month_nm, xmax = end_month_nm, ymin = min_sales, ymax = max_sales), fill = factor(1), alpha = 0.01) +
    theme_light() +
    theme(legend.position = 'right') +
    labs(title = "Sales By Month Per Campaign",
         x = "Months",
         y = "Total Sales"
    )
  new_name <- paste0("plot_typeb_",i)
  assign(new_name, p)
  rm(p)
}

grid.arrange(plot_typeb_1, plot_typeb_2, plot_typeb_4, plot_typeb_5,
             plot_typeb_7, plot_typeb_9, plot_typeb_10, plot_typeb_11,
             plot_typeb_12, plot_typeb_16, plot_typeb_17, plot_typeb_19,
             layout_matrix = rbind(c(1,2,3),c(4,5,6),c(7,8,9),c(10,11,12)))

# Type - C campaigns trend wrt total sales value between start and end date

type_c_campaigns <- campaign_descriptions_df %>% filter(campaign_type == "Type C") %>% filter(start_date > '2017-01-01' & end_date <= '2017-12-31') %>% select(campaign_id)

for(i in type_c_campaigns$campaign_id){
  p <-camp_prod_tsv %>%
    filter(campaign_id == i) %>%
    ggplot() +
    geom_line(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id)) +
    geom_point(aes(transaction_month, total_sales, group = campaign_id, colour = campaign_id),size = 2) +
    #geom_point(aes(transaction_month, camp_month),size = 5) +
    # geom_line(aes(transaction_month, camp_month)) +
    geom_rect(aes(NULL, NULL, xmin = start_month_nm, xmax = end_month_nm, ymin = min_sales, ymax = max_sales), fill = factor(1), alpha = 0.01) +
    theme_light() +
    theme(legend.position = 'right') +
    labs(title = "Sales By Month Per Campaign",
         x = "Months",
         y = "Total Sales"
    )
  new_name <- paste0("plot_typec_",i)
  assign(new_name, p)
  rm(p)
}

grid.arrange(plot_typec_3, plot_typec_6, plot_typec_14,
             layout_matrix = rbind(c(1,2,3)))
