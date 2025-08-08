# Registration of 'USDA-ARS Vera': A new public hop cultivar with tropical, stone-fruit aroma and powdery mildew resistance
# Last Updated: August 7, 2025

# install packages
library(dplyr)
library(ggplot2)
library(emmeans)
library(ggpubr)
library(tidyr)
library(ggResidpanel)
library(Hmisc)
library(lme4)
library(multcomp)

# set working directory for the location of the data files 
setwd("/users/kayla.altendorf/OneDrive - USDA/Documents/2025/W1108-333 Vera Release/Data/")

# files that should be in this directory
# Chemistry.xlsx
# Yield.xlsx
# Downy Mildew Controlled Environment.xlsx
# Downy Mildew on Farm - Mt. Angel, OR 2025.xlsx
# PM_Cas.csv
# PM_v6.csv 
# PM_200.csv
# PM_204.csv
# Historical Hop Yields.xlsx
# Historical Hop Yields 2022-2024.xlsx

#### hop chemistry analyses #### 
# read in data
chemistry <- read_excel("./Chemistry.xlsx", na = c("NA")) 

# set year and rep as factors
chemistry$rep <- as.factor(chemistry$rep)
chemistry$year <- as.factor(chemistry$year)

# set order of varieties
neworder <- c("Vera", "Cascade", "Zeus")
chemistry <- arrange(transform(chemistry, genotype=factor(genotype,levels=neworder)), genotype)

# select only needed columns
chemistry <- chemistry %>% dplyr::select(genotype, source, HPLC_AA, HPLC_BA, Pct_CoH, TotalOil, b_Pinene, Myrcene, Linalool, Caryophyllene, Farnesene, Humulene, Geraniol, Unidentified)

# make long
chemistry <- chemistry %>% pivot_longer(3:14, values_to = c("value"), names_to = c("trait"))

# set an order of the traits
traits <- c("HPLC_AA", "HPLC_BA", "Pct_CoH", "TotalOil", "b_Pinene", "Myrcene", "Linalool", 
            "Caryophyllene", "Farnesene", "Humulene", "Geraniol", "Unidentified")

chemistry <- dplyr::arrange(transform(chemistry, trait=factor(trait,levels=traits)), trait)

# separate out into experiment station and on farm data 
chemistry_exp <- chemistry %>% filter(source == "USDA Breeding Program") # experiment station
chemistry_farm <- chemistry %>% filter(source != "USDA Breeding Program") # on-farm

# note comparisons for stat_compare_means and set p-value thresholds
my_comparisons <- list(c("Zeus", "Vera"), c("Cascade", "Vera"))
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", ""))

# plot chemistry results from experiment station 
ggplot(chemistry_exp, aes(x = genotype, y = value, fill = genotype)) +
  geom_boxplot() + 
  facet_wrap(~trait, scales = "free_y") + 
  stat_compare_means(comparisons = my_comparisons, paired = FALSE, method = "t.test", symnum = symnum.args, vjust = 1.25, size = 4, color = "gray39") +
  theme_bw() + 
  xlab("Cultivar") + 
  ylab("Percentage (%)") +
  labs(fill = "Cultivar") + 
  scale_fill_manual(values = c("#CC79A7", "#009E73", "#E69F00"), name = "Cultivar") 
  

# on-farm data
# determine if the experiment station values and the on-farm values are different using t.tests
# specify that the experiment station data be of Vera only (not including commercial checks Zeus and Cascade)
chemistry_exp_vera <- chemistry_exp %>% filter(genotype == "Vera") # experiment station

# iterate through traits, add results from t.tests to a list 
t.test.result <- list() 

for (i in 1:length(traits)) {
  t.test.result[[i]] <- t.test(chemistry_farm[chemistry_farm$trait == traits[i],]$value, 
                        chemistry_exp_vera[chemistry_exp_vera$trait == traits[i],]$value, paired = FALSE)
  
}
# result: significant iterations = 1 (HPLC_AA) and 9 (Farnesene) 


#### experiment station yield #### 
# read in data - this includes on farm and experiment station data
yield <- read_excel("./Yield.xlsx", na = c(NA, "."))

# calculate yield data
yield <- yield %>% mutate(predicted_yield_kg = (wet_yield_kg * dry_matter) * 1.1, # assuming a 10% moisture after drying
                          predicted_yield_kg_per_string = predicted_yield_kg / strings, # strings harvested per plot
                          predicted_yield_kg_per_acre = ((889*2) * predicted_yield_kg_per_string), # strings per acre
                          predicted_yield_kg_per_hectare = predicted_yield_kg_per_acre *  2.47105381) # converting to kg/ha

# filter out the experiment station data, and make year and rep factors
exp_yield <- yield %>% filter(trial_type == "Experiment Station") %>% mutate(year = as.factor(year), 
                                                                             rep = as.factor(rep))
# linear model for yield
# repeated measures - as in, plots were not randomized between years 1 and 2 due to the crop being perennial
lm <- lmer(predicted_yield_kg_per_hectare ~ genotype*year + (1|genotype:rep) + (1|rep), data = exp_yield)
resid_panel(lm)
lm_an <- anova(lm)
lm_em <- emmeans(lm, ~ genotype | year, type = "response") # calculate emmeans
lm_em_cld <- data.frame(cld(lm_em, Letters = c(letters), adjust = "none")) # with a compact letter display for comparisons

# order the varieties (pre-determined above)
lm_em_cld <- arrange(transform(lm_em_cld,
                             genotype=factor(genotype,levels=neworder)), genotype)

# ggplot for yield, faceted by year using emmeans
ggplot(lm_em_cld, aes(genotype, emmean, fill = genotype)) +
  facet_grid(~year) + 
  geom_bar(stat = "identity") + 
  theme_bw() +
  labs(y = "Yield (kg/ha)",
       x = "Cultivar", fill = "Genotype") + 
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2,
                position=position_dodge(.9)) + 
  geom_text(lm_em_cld, mapping = aes(x = genotype, y = emmean+400, label = .group), size = 6, inherit.aes = FALSE) + 
  geom_text(lm_em_cld, mapping = aes(x = genotype, y = (3750), label = round(emmean, digits = 0)), size = 4, inherit.aes = FALSE) +
  scale_fill_manual(values = c("#CC79A7", "#009E73", "#E69F00")) + 
  scale_y_continuous(limits = c(0, 3750), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500)) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = font_size), 
        axis.text.y = element_text(size = font_size), 
        axis.title.y = element_text(size = font_size), 
        plot.title = element_text(size = font_size, hjust = 0.5), 
        axis.title.x = element_text(size = font_size), 
        strip.text.x = element_text(size = font_size))


#### downy mildew ####
# controlled environment screening 
# read in data
dm <- read_excel("./Downy Mildew Controlled Environment.xlsx", skip = 1)
colnames(dm)[8] <- "percent_chlorosis"

# calculate emmeans - sqrt transformation improved qq plot 
dm_lm <- lmer(sqrt(percent_chlorosis) ~ Cultivar + (1|Plant) + (1|Node) + (1|Leaf), data = dm)
resid_panel(dm_lm)
dm_an <- anova(dm_lm)
dm_em <- emmeans(dm_lm, ~ Cultivar, type = "response")
dm_cld <- data.frame(cld(dm_em,  adjust = "none", Letters = c(letters))) 

# set the order of cultivars
neworder <- c("Vera", "Nugget", "Pacific Gem")
dm_cld <- arrange(transform(dm_cld, Cultivar=factor(Cultivar,levels=neworder)), Cultivar)

# plot
ggplot(dm_cld, aes(Cultivar, response, fill = Cultivar)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=.2,
                position=position_dodge(.9)) + 
  theme_bw() + 
  geom_text(dm_cld, mapping = aes(x = Cultivar, y = response+SE+5, label = `.group`), size = 7, inherit.aes = FALSE) + 
  labs(y = "Chlorosis (%)",
       x = "Cultivar", fill = "Cultivar") + 
  theme(legend.text = element_text(size = 20), 
        legend.position = "none",
        plot.subtitle = element_text(size = 15), 
        legend.title = element_text(size = font_size), 
        axis.text.y = element_text(size = font_size), 
        axis.text.x = element_text(size = font_size), 
        axis.title.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size), 
        plot.title = element_text(size = font_size, hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("#CC79A7", "#0072B2", "#E69F00")) #1200 x 900

# field screening
# Mt. Angel Oregon in 2025 with a Cascade check 
dm_field <- read_excel("./Downy Mildew on Farm - Mt. Angel, OR 2025.xlsx")
# filter out cascade and vera
dm_field <- dm_field %>% filter(Genotype %in% c("Cascade", "W1108-333"))

# APRIL rating 
cas <- dm_field %>% filter(Genotype == "Cascade") %>% mutate(DM_April15 = as.numeric(`DM score April 15`))
ver <- dm_field %>% filter(Genotype == "W1108-333") %>% mutate(DM_April15  = as.numeric(`DM score April 15`))

t.test(cas$DM_April15, ver$DM_April15)

# JUNE rating
cas <- dm_field %>% filter(Genotype == "Cascade") %>% mutate(DM_June2 = as.numeric(`DM score June 2`))
ver <- dm_field %>% filter(Genotype == "W1108-333") %>% mutate(DM_June2  = as.numeric(`DM score June 2`))

t.test(cas$DM_June2, ver$DM_June2)

#### powdery mildew ####
# these values were already calculated in a previous analysis - see "Seven publicly available hop genotypes with multi-race powdery mildew resistance"
# in the Journal of Plant Registrations
cas <- read.csv("./PM_Cas.csv") %>% filter(Identifier %in% c("Cascade", "Symphony", "W1108_333")) %>% mutate(mildew = "HPM 1084")
v6 <- read.csv("./PM_v6.csv") %>% filter(Identifier %in% c("Nugget", "Symphony", "W1108_333")) %>% mutate(mildew = "HPM 609")
pm200 <- read.csv("./PM_200.csv") %>% filter(Identifier %in% c("Nugget", "Symphony", "W1108_333", "Zenith")) %>% mutate(mildew = "HPM 200")
pm204 <- read.csv("./PM_204.csv") %>% filter(Identifier %in% c("Nugget", "Symphony", "W1108_333", "Zenith", "Target")) %>% mutate(mildew = "HPM 204")

# bind them together
pm <- rbind(cas, v6, pm200, pm204)

# rename W1108-333 to Vera
pm$Identifier <-  gsub('W1108_333', 'Vera', pm$Identifier)

# set the order or races, then genotypes
neworder <- c("HPM 609", "HPM 1084", "HPM 200", "HPM 204")
pm <- arrange(transform(pm, mildew=factor(mildew,levels=neworder)), mildew)
neworder <- c("Target", "Zenith", "Cascade", "Nugget", "Symphony", "Vera")
pm <- arrange(transform(pm, Identifier=factor(Identifier,levels=neworder)), Identifier)

# create a data frame of virulences
virulence <- data.frame(mildew = unique(pm$mildew), 
                        virulence = c("Vb, V1, V2, V3, V5", 
                                      "Vb, V1, V3", 
                                      "Cascade-adapted; Vb, V3, V5", 
                                      "Vb, V3, V4, V5, V6"))

# left join to pm data 
pm_virulence <- pm %>% left_join(virulence, by = "mildew")

# plot
ggplot(pm_virulence, aes(Median, Identifier)) + 
  geom_point()  + 
  facet_wrap(~mildew + virulence, nrow = 1, scales = "free_x") + 
  geom_pointrange(aes(xmin = Percentile.lower, xmax = Percentile.upper)) + 
  scale_color_manual(values = c("darkgray", "black")) +
  ylab("Cultivar") +
  xlab("Median Rating / Colony Count") + 
  theme_bw() + 
  theme(legend.text = element_text(size = 20), 
        legend.position = "none",
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size = 12, color = "black"), 
        plot.subtitle = element_text(size = 15), 
        legend.title = element_text(size = font_size), 
        axis.text.y = element_text(size = font_size), 
        axis.text.x = element_text(size = font_size), 
        axis.title.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size), 
        plot.title = element_text(size = font_size, hjust = 0.5, face = "bold"))



#### hop box cone morphology analysis #####
# this data was collected from the experiment station trials in Prosser, WA
# using the HopBox: https://doi.org/10.1002/ppj2.20080

# read in data
hopbox <- read.csv("./HopBox.csv")

# select only the traits we need
hopbox <- hopbox %>% dplyr::select(Variety, area, length, width, openness)

# rename with units
colnames(hopbox)[2:5] <- c("Area (cm^2)", "Length (cm)", "Width (cm)", "Openness")

# make long
hopbox <- hopbox %>% pivot_longer(2:5, values_to = c("value"), names_to = c("trait"))

# order
neworder <- c("Vera", "Cascade", "Zeus")
hopbox <- arrange(transform(hopbox, Variety=factor(Variety,levels=neworder)), Variety)

# set comparisons
my_comparisons <- list(c("Zeus", "Vera"), c("Cascade", "Vera"))
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", ""))

# plot
ggplot(hopbox, aes(x = Variety, y = value, fill = Variety)) +
  geom_boxplot() + 
  facet_wrap(~trait, scales = "free_y") + 
  stat_compare_means(comparisons = my_comparisons, paired = FALSE, method = "t.test", symnum = symnum.args, vjust = 1.25, size = 4, color = "gray39") +
  theme_bw() + 
  xlab("Cultivar") +
  ylab("Value")+ 
  scale_fill_manual(values = c("#CC79A7", "#009E73", "#E69F00"), name = "Variety") + 
  theme(legend.text = element_text(size = 20), 
        legend.position = "none",
        plot.subtitle = element_text(size = 15), 
        legend.title = element_text(size = font_size), 
        strip.text.x =  element_text(size = font_size), 
        axis.text.y = element_text(size = font_size), 
        axis.text.x = element_text(size = font_size), 
        axis.title.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size), 
        plot.title = element_text(size = font_size, hjust = 0.5, face = "bold"))


#### determining the percentage of hop acreage that is proprietary and public ####
nass <- read_excel("./Historical Hop Yields 2022-2024.xlsx", na = c(NA, ""))

nass_sum23 <- nass %>% 
  group_by(Year, Proprietary) %>% summarise(sum(Acreage, na.rm = T)) %>% 
  filter(Year == 2023) %>% 
  mutate(total = sum(`sum(Acreage, na.rm = T)`), 
         percentage = `sum(Acreage, na.rm = T)`/ total)

nass_sum24 <- nass %>% group_by(Year, Proprietary) %>% summarise(sum(Acreage, na.rm = T)) %>% 
  filter(Year == 2024) %>%
  mutate(total = sum(`sum(Acreage, na.rm = T)`), 
         percentage = `sum(Acreage, na.rm = T)`/ total)


# how does this differ from the past
acre_yield <- read.csv("./Historical Hop Yields.csv", na.strings = c("", ".", "NA"))
by_type <- acre_yield %>% group_by(Year, Release) %>% summarise(acreage = sum(Acreage, na.rm = T))
by_year <- acre_yield %>% group_by(Year) %>% summarise(acreage_total = sum(Acreage, na.rm = T))

full_dat <- left_join(by_type, by_year, by = "Year") %>% mutate(proportion_of_acreage = acreage / acreage_total)


#### on farm yields ####
# taking out aroma hop averages across states
aroma_hop_averages <- nass %>% filter(`Market Class` == "Aroma") %>% group_by(State, Year) %>% summarise(mean_yield = mean(Yield, na.rm = T))
# these values were added to the spreadsheet for yield

# filter out on-farm data
on_farm <- yield %>% filter(trial_type == "On-Farm") %>% mutate(year = as.factor(year)) %>% filter(genotype %in% c("Vera", "Average"))

# set the order of varieties
neworder <- c("Vera", "Average")
on_farm <- arrange(transform(on_farm,
                             genotype=factor(genotype,levels=neworder)) ,genotype)

# convert to kgs/ha for publication purposes
on_farm <- on_farm %>% mutate(kgs_ha = lbs_acre *1.12085)

font_size = 15

# figure 
ggplot(on_farm, aes(x=genotype, y=kgs_ha, fill = genotype))+
  geom_bar(stat='identity')+
  facet_grid(vars(year), vars(state)) + 
  scale_fill_manual(values = c("#CC79A7", "#56B4E9")) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x= element_text(size = font_size), 
        legend.title = element_text(size = font_size), 
        strip.text.x = element_text(size = font_size), 
        strip.text.y = element_text(size = font_size), 
        axis.text.y = element_text(size = font_size), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        plot.title = element_text(size = font_size, hjust = 0.5, face = "bold"))+ 
  ylab("Yield (kg/ha)") + 
  xlab("Cultivar") + 
  geom_text(on_farm, mapping = aes(x = genotype, y = (kgs_ha+200), label = round(kgs_ha, digits = 0)), size = 4, inherit.aes = FALSE) 



