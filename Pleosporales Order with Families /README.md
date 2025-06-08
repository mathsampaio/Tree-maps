---
title: "Pleosporales_Order_with_Families"
output: html_document
date: "2025-05-23"
---

```{r}
library(treemapify)
library(ggplot2)
library(readxl)
library(MetBrewer)
library(dplyr)
library(purrr)
library(forcats)
library(tibble)
```

#### Once we have a tree map plot with all the orders from the Dothideomycetes class, we should focus in each orders to explore more of the genomes from each families. In that way, in the final annalysis, we can estimate how many genomes we have per order.

## PLEOSPORALES ORDER
Here, we're going to focus in the families of the Pleosporales order (n = 901 assemblies)

### FAMILIES

#### For that, let's read the dataset from NCBI
```{r}
## Importing a specific sheet from the dataset
P_FAMILIES <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Families_Filtered")

## In this dataset, we are working with all the families in this order
```

#### Plotting all the famlies within de Pleosporales order
```{r}
P_FAMILIES_plot <- ggplot(P_FAMILIES, aes(area = Assemblies, fill = Family,
               label = paste(Assemblies, Family, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Hokusai2",30)) + labs(title = "Number of assemblies from families in the Pleosporales order 
available in the NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

## Número de genomas montados disponíveis no NCBI das famílias da ordem Pleosporales
P_FAMILIES_plot
```

#### Now we can choose to filter these data... so we can visualize all the famlies with more than 30 assemblies.

#### For that, we must filter this dataset, as follows
```{r}
## Filtering all the values above 30
P_FAMILIES_31 <- P_FAMILIES %>% 
  filter(Assemblies > 30)

## Now we are working with all the taxonomic groups (Orders and unspecified genus) with more than 30 assemblies 
```


#### Plotting all the famlies within de Pleosporales order with more than 30 assemblies
```{r}
P_FAMILIES_plot_31 <- ggplot(P_FAMILIES_31, aes(area = Assemblies, fill = Family,
               label = paste(Family, Assemblies, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Hokusai2",5)) + labs(title = "Famlies from the Pleosporales order with more than 30 assemblies from NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

## Famílias da ordem Pleosporales com mais de 30 genomas montados disponíveis no NCBI
P_FAMILIES_plot_31
```

![P_FAMILIES_plot_31](https://github.com/user-attachments/assets/3829c502-2ead-4a36-958a-f0b28d976710)

```{r}
png("P_FAMILIES_plot_31.png", width = 644)
  P_FAMILIES_plot_31
dev.off()
```

#### And then, we can plot how many genus with genomes we have within this families

```{r}

## Importing a specific sheet from the dataset, cleaning it (choosing which colummns and erasing all the empity cells) and filtering all the genus with more than 0 genomes from each family

Pleosporaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Pleosporaceae")

  Pleosporaceae_clean <- Pleosporaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Pleosporaceae_clean_1 <- Pleosporaceae_clean %>% 
    filter(Number_of_assemblies > 0)

Phaeosphaeriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Phaeosphaeriaceae")

  Phaeosphaeriaceae_clean <- Phaeosphaeriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Phaeosphaeriaceae_clean_1 <- Phaeosphaeriaceae_clean %>% 
    filter(Number_of_assemblies > 0)

Didymellaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Didymellaceae")

  Didymellaceae_clean <- Didymellaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Didymellaceae_clean_1 <- Didymellaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Corynesporascaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Corynesporascaceae")

  Corynesporascaceae_clean <- Corynesporascaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Corynesporascaceae_clean_1 <- Corynesporascaceae_clean %>% 
    filter(Number_of_assemblies > 0)    

Didymosphaeriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Didymosphaeriaceae")

  Didymosphaeriaceae_clean <- Didymosphaeriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Didymosphaeriaceae_clean_1 <- Didymosphaeriaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
    
## Listing the families and the number of genus with available assemblies
lista_de_familias_31 <- list(Pleosporaceae = Pleosporaceae_clean_1, Phaeosphaeriaceae = Phaeosphaeriaceae_clean_1, Didymellaceae = Didymellaceae_clean_1, Corynesporascaceae = Corynesporascaceae_clean_1, Didymosphaeriaceae = Didymosphaeriaceae_clean_1)

## And now, we're going to create a unified tibble with the families and the number of genus with available assemblies
resumo_familias_31 <- imap_dfr(lista_de_familias_31, ~ {
  tibble(
    Families = .y,
    Number_of_genus = n_distinct(.x$Genus)
  )
})

```

#### Plotting the number of genus of all the famlies within de Pleosporales order with more than 30 assemblies
```{r}

resumo_familias_31_plot <- ggplot(resumo_familias_31, aes(
  y = Number_of_genus,
  x = fct_reorder(Families, Number_of_genus),
  fill = Families
)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  coord_flip() + 
  theme_light() +
  scale_fill_manual(values = met.brewer("Hokusai2", n = length(unique(resumo_familias_31$Families)))) +
  labs(
    title = "Number of genus with available assemblies",
    x = "Families",  # Corrige o nome do eixo X
    y = "Number of genus"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 0.8 / 2
  ) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  scale_y_continuous(breaks = seq(0, 13, by = 2), expand = expansion(mult = 0.026)) +
  guides(fill = guide_legend(reverse = TRUE))

resumo_familias_31_plot
```

![resumo_familias_31_plot](https://github.com/user-attachments/assets/ffb26565-ff91-442a-913c-39a988fb3620)


```{r}
png("resumo_familias_31_plot.png", width = 544)
  resumo_familias_31_plot
dev.off()
```





#### Now we can choose to filter these data... so we can visualize all the famlies with less than 30 assemblies.

#### For that, we must filter this dataset, as follows
```{r}
## Filtering all the values under 30
P_FAMILIES_30 <- P_FAMILIES %>% 
  filter(Assemblies < 30)

## Now we are working with all the taxonomic groups (Orders and unspecified genus) with less than 30 assemblies 
```


#### Plotting all the famlies within de Pleosporales order with more than 30 assemblies
```{r}
P_FAMILIES_plot_30 <- ggplot(P_FAMILIES_30, aes(area = Assemblies, fill = Family,
               label = paste(Family, Assemblies, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Hokusai2",24)) + labs(title = "Famlies from the Pleosporales order with less than 30 assemblies from NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

## Famílias da ordem Pleosporales com menos de 30 genomas montados disponíveis no NCBI
P_FAMILIES_plot_30
```

![P_FAMILIES_plot_30](https://github.com/user-attachments/assets/76cf42ec-0fc7-4933-a5a4-c8066b419ce1)


```{r}
png("P_FAMILIES_plot_30.png", width = 944)
  P_FAMILIES_plot_30
dev.off()
```

```{r}

## Importing a specific sheet from the dataset, cleaning it (choosing which colummns and erasing all the empity cells) and filtering all the genus with more than 0 genomes from each family

Leptosphaeriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Leptosphaeriaceae")

  Leptosphaeriaceae_clean <- Leptosphaeriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Leptosphaeriaceae_clean_1 <- Leptosphaeriaceae_clean %>% 
    filter(Number_of_assemblies > 0)

Melanommataceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Melanommataceae")

  Melanommataceae_clean <- Melanommataceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Melanommataceae_clean_1 <- Melanommataceae_clean %>% 
    filter(Number_of_assemblies > 0)

Cucurbitariaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Cucurbitariaceae")

  Cucurbitariaceae_clean <- Cucurbitariaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Cucurbitariaceae_clean_1 <- Cucurbitariaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Sporormiaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Sporormiaceae")

  Sporormiaceae_clean <- Sporormiaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Sporormiaceae_clean_1 <- Sporormiaceae_clean %>% 
    filter(Number_of_assemblies > 0)    

Coniothyriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Coniothyriaceae")

  Coniothyriaceae_clean <- Coniothyriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Coniothyriaceae_clean_1 <- Coniothyriaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Periconiaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Periconiaceae")

  Periconiaceae_clean <- Periconiaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Periconiaceae_clean_1 <- Periconiaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Massarinaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Massarinaceae")

  Massarinaceae_clean <- Massarinaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Massarinaceae_clean_1 <- Massarinaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Pseudopyrenochaetaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Pseudopyrenochaetaceae")

  Pseudopyrenochaetaceae_clean <- Pseudopyrenochaetaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Pseudopyrenochaetaceae_clean_1 <- Pseudopyrenochaetaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Shiraiaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Shiraiaceae")

  Shiraiaceae_clean <- Shiraiaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Shiraiaceae_clean_1 <- Shiraiaceae_clean %>% 
    filter(Number_of_assemblies > 0)

Lentitheciaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Lentitheciaceae")

  Lentitheciaceae_clean <- Lentitheciaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Lentitheciaceae_clean_1 <- Lentitheciaceae_clean %>% 
    filter(Number_of_assemblies > 0)
  
Lindgomycetaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Lindgomycetaceae")

  Lindgomycetaceae_clean <- Lindgomycetaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Lindgomycetaceae_clean_1 <- Lindgomycetaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Trematosphaeriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Trematosphaeriaceae")

  Trematosphaeriaceae_clean <- Trematosphaeriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Trematosphaeriaceae_clean_1 <- Trematosphaeriaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Thyridariaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Thyridariaceae")

  Thyridariaceae_clean <- Thyridariaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Thyridariaceae_clean_1 <- Thyridariaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Lophiostomataceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Lophiostomataceae")

  Lophiostomataceae_clean <- Lophiostomataceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Lophiostomataceae_clean_1 <- Lophiostomataceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Pleomassariaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Pleomassariaceae")

  Pleomassariaceae_clean <- Pleomassariaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Pleomassariaceae_clean_1 <- Pleomassariaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Diademaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Diademaceae")

  Diademaceae_clean <- Diademaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Diademaceae_clean_1 <- Diademaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Delitschiaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Delitschiaceae")

  Delitschiaceae_clean <- Delitschiaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Delitschiaceae_clean_1 <- Delitschiaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Dothidotthiaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Dothidotthiaceae")

  Dothidotthiaceae_clean <- Dothidotthiaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Dothidotthiaceae_clean_1 <- Dothidotthiaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Tetraplosphaeriaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Tetraplosphaeriaceae")

  Tetraplosphaeriaceae_clean <- Tetraplosphaeriaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Tetraplosphaeriaceae_clean_1 <- Tetraplosphaeriaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Amniculicolaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Amniculicolaceae")

  Amniculicolaceae_clean <- Amniculicolaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Amniculicolaceae_clean_1 <- Amniculicolaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Lophiotremataceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Lophiotremataceae")

  Lophiotremataceae_clean <- Lophiotremataceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Lophiotremataceae_clean_1 <- Lophiotremataceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Acrocalymmaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Acrocalymmaceae")

  Acrocalymmaceae_clean <- Acrocalymmaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Acrocalymmaceae_clean_1 <- Acrocalymmaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Torulaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Torulaceae")

  Torulaceae_clean <- Torulaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Torulaceae_clean_1 <- Torulaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
Nigrogranaceae <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Nigrogranaceae")

  Nigrogranaceae_clean <- Nigrogranaceae %>%
  select(Genus, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Nigrogranaceae_clean_1 <- Nigrogranaceae_clean %>% 
    filter(Number_of_assemblies > 0)
    
## Listing the families and the number of genus with available assemblies
lista_de_familias_30 <- list(Leptosphaeriaceae = Leptosphaeriaceae_clean_1, Melanommataceae = Melanommataceae_clean_1, Cucurbitariaceae = Cucurbitariaceae_clean_1, Sporormiaceae = Sporormiaceae_clean_1, Periconiaceae = Periconiaceae_clean_1, Coniothyriaceae = Coniothyriaceae_clean_1, Pseudopyrenochaetaceae = Pseudopyrenochaetaceae_clean_1, Massarinaceae = Massarinaceae_clean_1, Trematosphaeriaceae = Trematosphaeriaceae_clean_1, Thyridariaceae = Thyridariaceae_clean_1, Shiraiaceae = Shiraiaceae_clean_1, Lindgomycetaceae = Lindgomycetaceae_clean_1, Lentitheciaceae = Lentitheciaceae_clean_1, Torulaceae = Torulaceae_clean_1, Tetraplosphaeriaceae = Tetraplosphaeriaceae_clean_1, Pleomassariaceae = Pleomassariaceae_clean_1, Nigrogranaceae = Nigrogranaceae_clean_1, Lophiotremataceae = Lophiotremataceae_clean_1, Lophiostomataceae = Lophiostomataceae_clean_1, Dothidotthiaceae = Dothidotthiaceae_clean_1, Diademaceae = Diademaceae_clean_1, Delitschiaceae = Delitschiaceae_clean_1, Amniculicolaceae = Amniculicolaceae_clean_1, Acrocalymmaceae = Acrocalymmaceae_clean_1)

## And now, we're going to create a unified tibble with the families and the number of genus with available assemblies
resumo_familias_30 <- imap_dfr(lista_de_familias_30, ~ {
  tibble(
    Families = .y,
    Number_of_genus = n_distinct(.x$Genus)
  )
})

```


#### Plotting the number of genus of all the famlies within de Pleosporales order with less than 30 assemblies
```{r}

resumo_familias_30_plot <- ggplot(resumo_familias_30, aes(
  y = Number_of_genus,
  x = fct_reorder(Families, Number_of_genus),
  fill = Families
)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  coord_flip() + 
  theme_light() +
  scale_fill_manual(values = met.brewer("Hokusai2", n = length(unique(resumo_familias_30$Families)))) +
  labs(
    title = "Number of genus with available assemblies",
    x = "Families",  
    y = "Number of genus"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 0.8 / 2
  ) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  scale_y_continuous(breaks = seq(0, 13, by = 2), expand = expansion(mult = 0.026)) +
  guides(fill = guide_legend(reverse = TRUE))

resumo_familias_30_plot
```

![resumo_familias_30_plot](https://github.com/user-attachments/assets/c52947d4-ddcf-4319-9274-65efa7fff487)



```{r}
png("resumo_familias_30_plot.png", width = 1044)
  resumo_familias_30_plot
dev.off()
```

### GENERA WITHIN FAMILIES

#### For that, let's read the dataset from NCBI
```{r}
## Importing a specific sheet from the dataset
P_GENERA <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Genus_Filtered")

## In this dataset, we are working with all the families in this order
```

#### Plotting all the famlies within de Pleosporales order
```{r}
P_GENERA_plot <- ggplot(P_GENERA, aes(area = Assemblies, fill = Genus,
               label = paste(Assemblies, Genus, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Hokusai2",8)) + labs(title = "Number of assemblies from genera within the Pleosporales order 
available in the NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

## Número de genomas montados disponíveis no NCBI dos gêneros dentro da ordem Pleosporales
P_GENERA_plot
```

![P_GENERA_plot](https://github.com/user-attachments/assets/371ac967-c567-4c0a-9d20-a2fca4acb0b3)


```{r}
png("P_GENERA_plot.png", width = 944)
  P_GENERA_plot
dev.off()
```

```{r}
## Importing a specific sheet from the dataset, cleaning it (choosing which colummns and erasing all the empity cells) and filtering all the species with more than 0 genomes from each genera

Genus_W_IN_order <- readxl::read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = "Genus_W_IN_order")

  Genus_W_IN_order_clean <- Genus_W_IN_order %>%
  select(Species, Number_of_assemblies) %>%
  filter_all(all_vars(. != "N/A" & !is.na(.)))

    Genus_W_IN_order_clean_1 <- Genus_W_IN_order_clean %>% 
    filter(Number_of_assemblies > 0)

```

#### Plotting the number of assemblies of all the genera within de Pleosporales order
```{r}

Genus_W_IN_order_clean_1_plot <- ggplot(Genus_W_IN_order_clean_1, aes(
  y = Number_of_assemblies,
  x = fct_reorder(Species, Number_of_assemblies),
  fill = Species
)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  coord_flip() + 
  theme_light() +
  scale_fill_manual(values = met.brewer("Hokusai2", n = length(unique(Genus_W_IN_order_clean_1$Species)))) +
  labs(
    title = "Number of genus with available 
    assemblies",
    x = "Species",  
    y = "Number of assemblies"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 0.8 / 2
  ) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  scale_y_continuous(breaks = seq(0, 6, by = 2), expand = expansion(mult = 0.026)) +
  guides(fill = guide_legend(reverse = TRUE))

Genus_W_IN_order_clean_1_plot
```


![Genus_W_IN_order_clean_1_plot](https://github.com/user-attachments/assets/abbcc47a-339f-4cbf-aa4c-bc85480dbf19)


```{r}
png("Genus_W_IN_order_clean_1_plot.png", width = 644)
  Genus_W_IN_order_clean_1_plot
dev.off()
```

```{r}
# Making one single table with all genera from Pleosporales order with available assemblie
All_genera <- bind_rows(Pleosporaceae_clean_1, Phaeosphaeriaceae_clean_1, Didymellaceae_clean_1, Corynesporascaceae_clean_1, Didymosphaeriaceae_clean_1, Leptosphaeriaceae_clean_1, Melanommataceae_clean_1,Cucurbitariaceae_clean_1, Sporormiaceae_clean_1, Periconiaceae_clean_1, Coniothyriaceae_clean_1, Pseudopyrenochaetaceae_clean_1, Massarinaceae_clean_1, Trematosphaeriaceae_clean_1, Thyridariaceae_clean_1, Shiraiaceae_clean_1, Lindgomycetaceae_clean_1, Lentitheciaceae_clean_1, Torulaceae_clean_1, Tetraplosphaeriaceae_clean_1, Pleomassariaceae_clean_1, Nigrogranaceae_clean_1, Lophiotremataceae_clean_1, Lophiostomataceae_clean_1, Dothidotthiaceae_clean_1, Diademaceae_clean_1, Delitschiaceae_clean_1, Amniculicolaceae_clean_1,  Acrocalymmaceae_clean_1, Genus_W_IN_order_clean_1 )

All_genera
# Total, minus 1...
```


### HERE IS A BETTER, AUTOMATIZED, CLEAN AND NOT DUMB WAY TO GENERATE THE SAME TABLE FROM THE NUMBER OF GENUS WITH GENOMES FROM EACH FAMILY!!!

```{r}
library(readxl)
library(dplyr)
library(purrr)
library(tibble)

# Lista de planilhas (nomes das famílias) a serem lidas
familias <- c("Pleosporaceae", "Phaeosphaeriaceae", "Didymellaceae", "Corynesporascaceae", "Didymosphaeriaceae")

# Função para ler, limpar e filtrar os dados de uma planilha
processar_familia <- function(familia) {
  read_xlsx("Pleosporales_Order_with_Families.xlsx", sheet = familia) %>%
    select(Genus, Number_of_assemblies) %>%
    filter_all(all_vars(. != "N/A" & !is.na(.))) %>%
    filter(Number_of_assemblies > 0)
}

# Aplicar a função a todas as famílias e armazenar em uma lista
AAAlista_de_familias_31 <- set_names(map(familias, processar_familia), familias)

# Criar a tabela resumo
AAAresumo_familias_31 <- imap_dfr(AAAlista_de_familias_31, ~ {
  tibble(
    Families = .y,
    Number_of_genus = n_distinct(.x$Genus)
  )
})

```

