
# Tree map plot of Dothideomycetes fungi order

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

####The ideia of this script is to group all the available assemblies in NCBI database from the Dothideomycetes class, so I can have an ideia of how many genomes I'll have to filter in the next steps

#### First, it's necessary to import all de gattered dataset from NCBI database to the Script
```{r}
## Importing the dataset
D_ORDERS <- readxl::read_xlsx("25-05-23_Orders_from_Dothideomycetes.xlsx")

## In this dataset, we are working with all the taxonomic groups (Orders and unspecified genus) 
```

#### Plotting all the taxonomic groups within de Dothideomycetes class 
```{r}
D_ORDERS_plot <- ggplot(D_ORDERS, aes(area = Assemblies, fill = Group_name,
               label = paste(Group_name, Assemblies, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Hokusai2",125)) + labs(title = "Number of assemblies in the NCBI database per orders from the Dothideomycetes class")+  theme(plot.title = element_text(hjust = 0.5))

##Número de genomas montados disponíveis no NCBI por ordens da classe Dothideomycetes
D_ORDERS_plot
```

![D_ORDERS_plot](https://github.com/user-attachments/assets/64410565-7a89-414e-a9da-6f9c790072ef)

### Saving the figures

```{r}
tiff("D_ORDERS_plot.tiff", width = 604)
  D_ORDERS_plot
dev.off()
```

```{r}
png("D_ORDERS_plot.png", width = 604)
  D_ORDERS_plot
dev.off()
```

```{r}
pdf("D_ORDERS_plot.pdf")
 D_ORDERS_plot
dev.off()
```

```{r}
svg("D_ORDERS_plot.svg")
 D_ORDERS_plot
dev.off()
```



#### Now we can choose to filter these data... so we can visualize all the groups with more than 50 assemblies.

#### For that, we must filter this dataset, as follows
```{r}
## Filtering all the values above 50
D_ORDERS_50 <- D_ORDERS %>% 
  filter(Assemblies > 50)

## Now we are working with all the taxonomic groups (Orders and unspecified genus) with more than 50 assemblies 
```

#### Plotting all the taxonomic groups within de Dothideomycetes class with more than 50 assemblies
```{r}
D_ORDERS_plot_50 <- ggplot(D_ORDERS_50, aes(area = Assemblies, fill = Group_name,
               label = paste(Group_name, Assemblies, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Veronese",5)) + labs(title = "Orders from the Dothideomycetes class with more than 50 assemblies from NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

##Ordens da classe Dothideomycetes com mais de 50 genomas montados disponíveis no NCBI
D_ORDERS_plot_50
```

![D_ORDERS_plot_50](https://github.com/user-attachments/assets/b10851f7-0555-436a-994d-f24bdd98e3c6)

### Saving the figures

```{r}
tiff("D_ORDERS_plot_50.tiff", width = 604)
  D_ORDERS_plot_50
dev.off()
```

```{r}
png("D_ORDERS_plot_50.png", width = 604)
  D_ORDERS_plot_50
dev.off()
```

```{r}
pdf("D_ORDERS_plot_50.pdf")
 D_ORDERS_plot_50
dev.off()
```

```{r}
svg("D_ORDERS_plot_50.svg")
 D_ORDERS_plot_50
dev.off()
```



#### Now we can choose to filter these data to visualize all the groups with less than 50 assemblies

#### For that, we must filter this dataset, as follows
```{r}
## Filtering all the values below 50
D_ORDERS_49 <- D_ORDERS %>% 
  filter(Assemblies < 50)

## Now we are working with all the taxonomic groups (Orders and unspecified genus) with less than 50 assemblies 
```

#### Plotting all the taxonomic groups within de Dothideomycetes class with less than 50 assemblies
```{r}
D_ORDERS_plot_49 <- ggplot(D_ORDERS_49, aes(area = Assemblies, fill = Group_name,
               label = paste(Group_name, Assemblies, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") + scale_fill_manual(values=met.brewer("Tiepolo",125)) + labs(title = "Orders from the Dothideomycetes class with less than 50 assemblies from NCBI database")+  theme(plot.title = element_text(hjust = 0.5))

##Ordens da classe Dothideomycetes com menos de 50 genomas montados disponíveis no NCBI
D_ORDERS_plot_49
```
![D_ORDERS_plot_49](https://github.com/user-attachments/assets/bb6ac3e7-8412-4727-827c-6ec459fd75e4)


### Saving the figures

```{r}
tiff("D_ORDERS_plot_49.tiff", width = 604)
  D_ORDERS_plot_49
dev.off()
```

```{r}
png("D_ORDERS_plot_49.png", width = 604)
  D_ORDERS_plot_49
dev.off()
```

```{r}
pdf("D_ORDERS_plot_49.pdf")
 D_ORDERS_plot_49
dev.off()
```

```{r}
svg("D_ORDERS_plot_49.svg")
 D_ORDERS_plot_49
dev.off()
```


