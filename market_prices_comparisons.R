


library(tidyverse)
library(MyFunction)

nodes<-c('TH_NP15_GEN-APND','TH_SP15_GEN-APND','DLAP_PGAE-APND','DLAP_SCE-APND','DLAP_SDGE-APND')
startdate<-'2023-11-01'
enddate<-'2023-11-30'


#download_prices<-MyFunction::get_caiso_prices(nodes=nodes)
#download_prices %>% write.csv('C:/Users/ryanl/OneDrive/Documents/R/R Projects/caiso_prices_202311_2.csv')


price_df<- read_csv('C:/Users/ryanl/OneDrive/Documents/R/R Projects/caiso_prices_202311_2.csv')

price_df<-price_df%>%rename_all(tolower)

price_df%>%distinct(data_item)

price_df%>%
  filter(data_item=='LMP_PRC')%>%
  filter(as.Date(end_datetime)==ymd('2023-11-15'))%>%
  ggplot(aes(x=end_datetime,y=value,color=market))+
  geom_line()+
  facet_grid(rows=vars(resource_name))+
  labs(x='Datetime',y='Price ($/MWh)',color='Market')+
  ggtitle('CAISO Market Prices')+
  scale_x_datetime(date_breaks = '1 hour')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

cal<-create_calendar(startdate,enddate)

price_df%>%
