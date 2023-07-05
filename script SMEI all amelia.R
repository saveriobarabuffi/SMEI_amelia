rm(list=ls())
set.seed(1)
library(tidyverse)
library(readxl)
library(writexl)
library(naniar)
library(Amelia)
library(haven)
library(parallel)

df <- read_xlsx("C:\\Users\\lalon\\OneDrive\\Desktop\\postdoc\\SMEI\\diff\\dataset\\all\\SMEI_all_amelia.xlsx")[-c(3,4,12,13)]

nopat <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(pat_stock))) %>% 
  filter(count==9)

noint<- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(intangible))) %>% 
  filter(count==9)

noemp <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(employees))) %>% 
  filter(count==9)

noprod <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(productionvalue))) %>% 
  filter(count==9)

nolong <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(longtermdebt))) %>% 
  filter(count==9)

nodebt <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(debt))) %>% 
  filter(count==9)

norev <- df %>%
  group_by(BVDid) %>%
  mutate(count = sum(is.na(revenues))) %>% 
  filter(count==9)


df <- df %>% 
  filter(!BVDid %in% nopat$BVDid) %>% 
  filter(!BVDid %in% noint$BVDid) %>% 
  filter(!BVDid %in% noemp$BVDid) %>%  
  filter(!BVDid %in% noprod$BVDid) %>% 
  filter(!BVDid %in% nolong$BVDid) %>% 
  filter(!BVDid %in% nodebt$BVDid) %>% 
  filter(!BVDid %in% norev$BVDid) %>% 
mutate(pat_stock=asinh(pat_stock)) %>% 
  mutate(intangible=asinh(intangible)) %>% 
  mutate(employees=asinh(employees)) %>% 
  mutate(productionvalue=asinh(productionvalue)) %>% 
  mutate(longtermdebt=asinh(longtermdebt)) %>% 
  mutate(edebtployees=asinh(debt)) %>% 
  mutate(revenues=asinh(revenues)) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(BVDid=as.factor(BVDid))

writexl::write_xlsx(df,"C:\\Users\\lalon\\OneDrive\\Desktop\\postdoc\\SMEI\\diff\\dataset\\all\\SMEI_all_totalpropmatch.xlsx")


Total.out <- amelia(df, 
                    ts="year", cs="BVDid",
                    intercs=TRUE,
                    polytime=1, splinetime=1,
                    tolerance = 0.0001,
                    empri = 0.01*nrow(df),
                    max.resample = 10,
                    boot.type = "ordinary",
                    m=5)

impTotal1 <-Total.out$imputations$imp1
impTotal2 <-Total.out$imputations$imp2
impTotal3 <-Total.out$imputations$imp3
impTotal4 <-Total.out$imputations$imp4
impTotal5 <-Total.out$imputations$imp5
impTotal6 <-Total.out$imputations$imp6
impTotal7 <-Total.out$imputations$imp7
impTotal8 <-Total.out$imputations$imp8
impTotal9 <-Total.out$imputations$imp9
impTotal10 <-Total.out$imputations$imp10