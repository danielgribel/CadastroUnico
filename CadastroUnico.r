library(ggplot2)
library(psych)
library(repr)
library(gridExtra)
library(corrplot)
library(viridis)
library(data.table)
library(caret)
library(Hmisc)

# Load the dataset
df <- read.csv(file = 'data/base_familia_2018.csv', header = TRUE, sep = ';')

# Print the number of rows and columns
nrow(df)
ncol(df)

# Get an overview of the dataset
head(df)

# Get the number of NAs in each column
print(data.frame(colSums(is.na(df))))

# Treat the 'cod_escoa_sanitario_domic_fam' column
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 1] <- 'Rede coletora'
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 2] <- 'Fossa septica'
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 3] <- 'Fossa rudimentar'
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 4] <- 'Vala a ceu aberto'
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 5] <- 'Rio, lago ou mar'

# Convert 'Outro' and 'NA' to 'others'
df$cod_escoa_sanitario_domic_fam[is.na(df$cod_escoa_sanitario_domic_fam)] <- 'Outro'
df$cod_escoa_sanitario_domic_fam[df$cod_escoa_sanitario_domic_fam == 6] <- 'Outro'

# Display 'cod_escoa_sanitario_domic_fam'
print(data.frame(table(df$cod_escoa_sanitario_domic_fam)), row.names = FALSE)

# The 'ind_familia_quilombola_fam' column presents 'NA' for samples classified as indigena.
# As very few samples are marked as quilombola, we join it with indigena, creating the column 'indigena_quilombola'
df$ind_familia_quilombola_fam[is.na(df$ind_familia_quilombola_fam)] <- 1
names(df)[names(df) == 'ind_familia_quilombola_fam'] <- 'indigena_quilombola'

# Code convention: convert values with 2 (meaning no presence) to 0
df$cod_local_domic_fam[df$cod_local_domic_fam == 2] <- 0
df$cod_familia_indigena_fam[df$cod_familia_indigena_fam == 2] <- 0
df$cod_banheiro_domic_fam[df$cod_banheiro_domic_fam == 2] <- 0
df$cod_agua_canalizada_fam[df$cod_agua_canalizada_fam == 2] <- 0
df$indigena_quilombola[df$indigena_quilombola == 2] <- 0

# Code convention: convert estrato values -- (0) Rural, (1) Urban
df$estrato[df$estrato == 1] <- 0
df$estrato[df$estrato == 2] <- 1

# Code convention: convert classf values -- (0) Other, (1) RM ou RIDE
df$classf[df$classf == 3] <- 0
df$classf[df$classf == 2] <- 1

# Get uninformative columns or columns with many NA entries
drop <- c(
    'cd_ibge',
    'dat_cadastramento_fam',
    'dat_alteracao_fam',
    'dat_atualizacao_familia',
    'nom_estab_assist_saude_fam',
    'cod_eas_fam',
    'nom_centro_assist_fam',
    'cod_centro_assist_fam',
    'ind_parc_mds_fam',
    'peso.fam',
    'cod_familia_indigena_fam',
    'ind_familia_quilombola_fam',
    'id_familia'
)

# Remove columns
df <- df[, !(names(df) %in% drop)]

# We have some samples with NA entries. As for these samples the NAs occur in most columns and
# the number os samples with NA are not significant, we remove these rows
df <- na.omit(df)

# Treating 'cod_material_piso_fam'
df$cod_material_piso_fam[df$cod_material_piso_fam == 2] <- 'Cimento'
df$cod_material_piso_fam[df$cod_material_piso_fam == 5] <- 'Ceramica, lajota ou pedra'

# As the 'Madeira' class has few samples, we join 'Madeira aproveitada' and 'Madeira aparelhada'
df$cod_material_piso_fam[df$cod_material_piso_fam == 3] <- 'Madeira'
df$cod_material_piso_fam[df$cod_material_piso_fam == 4] <- 'Madeira'

# As 'Terra', 'Carpete', and 'Outro' has few samples, we agreggate them in 'Others'
df$cod_material_piso_fam[df$cod_material_piso_fam == 1] <- 'Outro'
df$cod_material_piso_fam[df$cod_material_piso_fam == 6] <- 'Outro'
df$cod_material_piso_fam[df$cod_material_piso_fam == 7] <- 'Outro'

# Display 'cod_material_piso_fam'
print(data.frame(table(df$cod_material_piso_fam)), row.names = FALSE)

# Treating 'cod_material_domic_fam'
df$cod_material_domic_fam[df$cod_material_domic_fam == 1] <- 'Alvenaria (com revestimento)'
df$cod_material_domic_fam[df$cod_material_domic_fam == 2] <- 'Alvenaria (sem revestimento)'

# Joining 'Madeira aproveitada' and 'Madeira aparelhada'
df$cod_material_domic_fam[df$cod_material_domic_fam == 3] <- 'Madeira'
df$cod_material_domic_fam[df$cod_material_domic_fam == 6] <- 'Madeira'

# Joining 'Taipa revestida' and 'Taipa nao revestida'
df$cod_material_domic_fam[df$cod_material_domic_fam == 4] <- 'Taipa'
df$cod_material_domic_fam[df$cod_material_domic_fam == 5] <- 'Taipa'

# Joining 'Palha', and 'Outro'
df$cod_material_domic_fam[df$cod_material_domic_fam == 7] <- 'Outro'
df$cod_material_domic_fam[df$cod_material_domic_fam == 8] <- 'Outro'

# Display 'cod_material_domic_fam'
print(data.frame(table(df$cod_material_domic_fam)), row.names = FALSE)

# Treating 'cod_abaste_agua_domic_fam'
df$cod_abaste_agua_domic_fam[df$cod_abaste_agua_domic_fam == 1] <- 'Rede de distribuicao'
df$cod_abaste_agua_domic_fam[df$cod_abaste_agua_domic_fam == 2] <- 'Poco ou nascente'
df$cod_abaste_agua_domic_fam[df$cod_abaste_agua_domic_fam == 3] <- 'Cisterna'
df$cod_abaste_agua_domic_fam[df$cod_abaste_agua_domic_fam == 4] <- 'Outro'

# Display 'cod_abaste_agua_domic_fam'
print(data.frame(table(df$cod_abaste_agua_domic_fam)), row.names = FALSE)

# Display 'cod_especie_domic_fam'
print(data.frame(table(df$cod_especie_domic_fam)), row.names = FALSE)

# After removing the NA entries, the cod_especie_domic_fam column has only one value, so we remove it
df <- df[, !(names(df) %in% c('cod_especie_domic_fam'))]

# Treating 'cod_destino_lixo_domic_fam'
df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 1] <- 'Coletado'
df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 2] <- 'Coletado'

df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 3] <- 'Queimado ou enterrado'

df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 4] <- 'Descartado na natureza'
df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 5] <- 'Descartado na natureza'

df$cod_destino_lixo_domic_fam[df$cod_destino_lixo_domic_fam == 6] <- 'Outro'

# Display 'cod_destino_lixo_domic_fam'
print(data.frame(table(df$cod_destino_lixo_domic_fam)), row.names = FALSE)

# Treating 'cod_iluminacao_domic_fam'

# We join 'Eletrica com medidor proprio', 'Eletrica com medidor comunitario', and 'Eletrica sem medidor' in one class
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 1] <- 1
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 2] <- 1
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 3] <- 1

# We join 'Oleo/Querosene/Gas', 'Vela', and 'Outro' in one class
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 4] <- 0
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 5] <- 0
df$cod_iluminacao_domic_fam[df$cod_iluminacao_domic_fam == 6] <- 0

# Display 'cod_iluminacao_domic_fam', which is now a binary variable -- (0) no eletricity, (1) has eletricity
print(data.frame(table(df$cod_iluminacao_domic_fam)), row.names = FALSE)

# Treating 'cod_calcamento_domic_fam'

# We join 'Parcial' and 'Total'
df$cod_calcamento_domic_fam[df$cod_calcamento_domic_fam == 2] <- 1

# And mark 'Nao existe' as 0
df$cod_calcamento_domic_fam[df$cod_calcamento_domic_fam == 3] <- 0

# Display 'cod_calcamento_domic_fam', which is now a binary variable -- (0) no calcamento, (1) has calcamento
print(data.frame(table(df$cod_calcamento_domic_fam)), row.names = FALSE)

# Define a customized theme to be shared by our plots
mytheme <- theme(text = element_text(size = 20))

# Define the income intervals. We consider intervals from 0 to 1000 (stepping by 100), and a last interval >= 1000
inter <- seq(0, 1000, by = 100)

# Arrays storing the number of families in each income segment, for each category (all, non BF, BF)
freq_renda_all <- rep(0, length(inter))
freq_renda_bf0 <- rep(0, length(inter))
freq_renda_bf1 <- rep(0, length(inter))

# Count the number of families in each income segment
for(i in seq(1, length(inter) - 1)) {
    str <- inter[i]
    end <- inter[i+1]
    freq_renda_all[i] <- nrow(df[df$vlr_renda_media_fam >= str & df$vlr_renda_media_fam < end,])
    freq_renda_bf0[i] <- nrow(df[df$marc_pbf == 0 & df$vlr_renda_media_fam >= str & df$vlr_renda_media_fam < end,])
    freq_renda_bf1[i] <- nrow(df[df$marc_pbf == 1 & df$vlr_renda_media_fam >= str & df$vlr_renda_media_fam < end,])
}

# Add the last income segment (>= 1000) and counts the number of families
freq_renda_all[length(inter)] <- nrow(df[df$vlr_renda_media_fam >= end,])
freq_renda_bf0[length(inter)] <- nrow(df[df$marc_pbf == 0 & df$vlr_renda_media_fam >= end,])
freq_renda_bf1[length(inter)] <- nrow(df[df$marc_pbf == 1 & df$vlr_renda_media_fam >= end,])

# Calculate the frequencies in terms of percentage
perc_renda_all <- 100*round(freq_renda_all/sum(freq_renda_all), 3)
perc_renda_all <- paste(perc_renda_all, '%', sep = '')

perc_renda_bf0 <- 100*round(freq_renda_bf0/sum(freq_renda_bf0), 3)
perc_renda_bf0 <- paste(perc_renda_bf0, '%', sep = '')

perc_renda_bf1 <- 100*round(freq_renda_bf1/sum(freq_renda_bf1), 3)
perc_renda_bf1 <- paste(perc_renda_bf1, '%', sep = '')

# Define the intervals as strings (to place in the x-axis of the plots)
intervals <- c('< 100', '100 a 200', '200 a 300', '300 a 400', '400 a 500',
               '500 a 600', '600 a 700', '700 a 800', '800 a 900', '900 a 1000', '>= 1000')
intervals <- factor(intervals, levels = intervals)

# Build the data frames for each category
data_histogram_all <- data.frame(salary_range = factor(intervals), freq = freq_renda_all, perc = perc_renda_all)
data_histogram_bf0 <- data.frame(salary_range = factor(intervals), freq = freq_renda_bf0, perc = perc_renda_bf0)
data_histogram_bf1 <- data.frame(salary_range = factor(intervals), freq = freq_renda_bf1, perc = perc_renda_bf1)

# Vertical bound of the plot
max_y <- 2500000
break_y <- 500000

# Plot of income distribution for the entire population
plt_renda_all <- ggplot(data_histogram_all, aes(salary_range, freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme + 
    ylab("# Familias") +
    xlab("Renda media per capita (total)") +
    geom_text(aes(label = perc_renda_all), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Plot of income distribution for non BF families
plt_renda_bf0 <- ggplot(data_histogram_bf0, aes(salary_range, freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme + 
    ylab("") +
    xlab("Renda media per capita (nao BF)") +
    geom_text(aes(label = perc_renda_bf0), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Plot of income distribution for BF families
plt_renda_bf1 <- ggplot(data_histogram_bf1, aes(salary_range, freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme + 
    ylab("") +
    xlab("Renda media per capita (BF)") +
    geom_text(aes(label = perc_renda_bf1), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Display plots side-by-side
options(repr.plot.width = 16.5, repr.plot.height = 6)
grid.arrange(plt_renda_all, plt_renda_bf0, plt_renda_bf1, ncol = 3)

# Plot of income distribution according to Local do domicilio (Rural vs Urbano)
plt_local <- ggplot(df, aes(x = factor(cod_local_domic_fam, levels = 0:1, labels = c('Rural', 'Urbano')),
                            y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    mytheme +
    ylab("Renda media per capita") +
    xlab("Local do domicilio")

# Plot of income distribution according to Estrato (GM1 vs GM2)
# GM1 corresponds to municipios (counties) with 100 to 5k families, whereas GM2 has > 5k municipios
plt_estrato <- ggplot(df, aes(x = factor(estrato, levels = 0:1, labels = c('GM1', 'GM2')),
                              y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Estrato")

# Plot of income distribution according to the BF program (non BF families vs BF families)
plt_bf <- ggplot(df, aes(x = factor(marc_pbf, levels = 0:1, labels = c('Nao', 'Sim')),
                         y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Beneficiario do BF")

# Plot of income distribution among Indigena/Quilombola and non Indigena/Quilombola
plt_ind_qui <- ggplot(df, aes(x = factor(indigena_quilombola, levels = 0:1, labels = c('Nao', 'Sim')),
                         y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Indigena ou Quilombola")

# Display plots side-by-side
options(repr.plot.width = 16, repr.plot.height = 5)
grid.arrange(plt_local, plt_estrato, plt_bf, plt_ind_qui, ncol = 4)

# We define a threshold for filtering the number of members, as it approaches zero for large values 
lim_qtd <- 10

# Get the number of family members in each group (all, non BF, and BF)
df_pessoas_all <- data.frame(table(df[df$qtde_pessoas <= lim_qtd, ]$qtde_pessoas))
df_pessoas_bf0 <- data.frame(table(df[df$qtde_pessoas <= lim_qtd & df$marc_pbf == 0, ]$qtde_pessoas))
df_pessoas_bf1 <- data.frame(table(df[df$qtde_pessoas <= lim_qtd & df$marc_pbf == 1, ]$qtde_pessoas))

# Calculate the number of family members in terms of percentage
perc_pessoas_all <- 100*round(df_pessoas_all$Freq/sum(df_pessoas_all$Freq), 3)
perc_pessoas_all <- paste(perc_pessoas_all, '%', sep = '')

perc_pessoas_bf0 <- 100*round(df_pessoas_bf0$Freq/sum(df_pessoas_bf0$Freq), 3)
perc_pessoas_bf0 <- paste(perc_pessoas_bf0, '%', sep = '')

perc_pessoas_bf1 <- 100*round(df_pessoas_bf1$Freq/sum(df_pessoas_bf1$Freq), 3)
perc_pessoas_bf1 <- paste(perc_pessoas_bf1, '%', sep = '')

# Vertical bound of the plot
max_y <- 1500000
break_y <- 500000

# Plot of the number of family members (entire population)
plt_pessoas_all <- ggplot(df_pessoas_all, aes(x = factor(Var1), y = Freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme +
    ylab("# Familias") +
    xlab("# Pessoas no domicilio (total)") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    geom_text(aes(label = perc_pessoas_all), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Plot of the number of family members (non BF families)
plt_pessoas_bf0 <- ggplot(df_pessoas_bf0, aes(x = factor(Var1), y = Freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    theme_bw() + 
    mytheme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("") +
    xlab("# Pessoas no domicilio (nao BF)") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    geom_text(aes(label = perc_pessoas_bf0), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Plot of the number of family members (BF families)
plt_pessoas_bf1 <- ggplot(df_pessoas_bf1, aes(x = factor(Var1), y = Freq)) +
    geom_bar(fill = 'grey80', stat = 'identity') +
    theme_bw() + 
    mytheme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("") +
    xlab("# Pessoas no domicilio (BF)") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    geom_text(aes(label = perc_pessoas_bf1), hjust = -0.1, vjust = 0.2, size = 5, angle = 90)

# Display plots side-by-side
options(repr.plot.width = 16, repr.plot.height = 5)
grid.arrange(plt_pessoas_all, plt_pessoas_bf0, plt_pessoas_bf1, ncol = 3)

# Vertical bound of the plot
max_y_renda <- 3000
break_y <- 500

# Plot of average income distribution as a function of the family size (entire population)
plt_renda_pessoas_all <- ggplot(df, aes(x = factor(qtde_pessoas), y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme +
    ylab("Renda media per capita") +
    xlab("# Pessoas no domicilio (total)") +
    scale_y_continuous(limits = c(0, max_y_renda), breaks = seq(0, max_y_renda, break_y))

# Plot of average income distribution as a function of the family size (non BF families)
plt_renda_pessoas_bf0 <- ggplot(df[df$marc_pbf == 0,], aes(x = factor(qtde_pessoas), y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme +
    ylab("") +
    xlab("# Pessoas no domicilio (nao BF)") +
    scale_y_continuous(limits = c(0, max_y_renda), breaks = seq(0, max_y_renda, break_y))

# Plot of average income distribution as a function of the family size (BF families)
plt_renda_pessoas_bf1 <- ggplot(df[df$marc_pbf == 1,], aes(x = factor(qtde_pessoas), y = vlr_renda_media_fam)) +
    geom_boxplot(outlier.shape = 1) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    mytheme +
    ylab("") +
    xlab("# Pessoas no domicilio (BF)") +
    scale_y_continuous(limits = c(0, max_y_renda), breaks = seq(0, max_y_renda, break_y))

# Display plots side-by-side
options(repr.plot.width = 16, repr.plot.height = 6)
grid.arrange(plt_renda_pessoas_all, plt_renda_pessoas_bf0, plt_renda_pessoas_bf1, ncol = 3)

# Type of material on the floor
freq_material_piso <- data.frame(table(df$cod_material_piso_fam))

# Type of material on the wall
freq_material_parede <- data.frame(table(df$cod_material_domic_fam))

# Form of water supply
freq_abastecimento_agua <- data.frame(table(df$cod_abaste_agua_domic_fam))

# Form of garbage collection
freq_destino_lixo <- data.frame(table(df$cod_destino_lixo_domic_fam))

# Lighting type
freq_iluminacao <- data.frame(table(df$cod_iluminacao_domic_fam))

# Form of sanitary outflow
freq_escoa_sanitario <- data.frame(table(df$cod_escoa_sanitario_domic_fam))

# Vertical bound of the plot
max_y_material <- 4000000
break_y <- 1000000

# Plot of the distribution of families according to the type of material on the floor
plt_material_piso <- ggplot(freq_material_piso, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.65, 0.8),
        legend.background = element_rect(fill = "transparent", size = 0.3, linetype = "solid")) +
    scale_fill_viridis_d(option = "magma") +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("# Familias") +
    labs(fill = "Material do piso") +
    scale_y_continuous(limits = c(0, max_y_material), breaks = seq(0, max_y_material, break_y))

# Plot of the distribution of families according to the type of material on the wall
plt_material_parede <- ggplot(freq_material_parede, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.62, 0.78),
        legend.background = element_rect(fill = "transparent", size = 0.3, linetype = "solid")) +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("") +
    labs(fill = "Material das paredes") +
    scale_y_continuous(limits = c(0, max_y_material), breaks = seq(0, max_y_material, break_y)) +
    scale_fill_viridis_d(option = "magma")

# Display plots side-by-side
options(repr.plot.width = 12, repr.plot.height = 5)
grid.arrange(plt_material_piso, plt_material_parede, ncol = 2)

# Vertical bound of the plot
max_y <- 5000000
break_y <- 1000000

# Plot of the distribution of families according to the form of water supply
plt_abastecimento_agua <- ggplot(freq_abastecimento_agua, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.65, 0.8),
        legend.background = element_rect(fill = "transparent", size = 0.3, linetype = "solid")) +
    scale_fill_viridis_d(option = "magma") +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("# Familias") +
    labs(fill = "Abastecimento de agua") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y))

# Plot of the distribution of families according to the form of garbage collection
plt_destino_lixo <- ggplot(freq_destino_lixo, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.65, 0.8),
        legend.background = element_rect(fill = "transparent", size = 0.2, linetype = "solid")) +
    scale_fill_viridis_d(option = "magma") +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("# Familias") +
    labs(fill = "Destino do lixo") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y))

# Plot of the distribution of families according to eletricity supply (yes or no)
plt_iluminacao <- ggplot(freq_iluminacao, aes(
                    x = reorder(Var1, -Freq),
                    y = Freq,
                    fill = factor(reorder(Var1, Freq), levels = 0:1, labels = c('Outro', 'Eletrica')))) +
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.85),
        legend.background = element_rect(fill = "transparent", size = 0.2, linetype = "solid")) +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("") +
    labs(fill = "Iluminacao") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y))

# Plot of the distribution of families according to the form of sanitary outflow
plt_escoa_sanitario <- ggplot(freq_escoa_sanitario, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) +
    geom_bar(stat = "identity") +
    theme_bw() +
    mytheme +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.7, 0.75),
        legend.background = element_rect(fill = "transparent", size = 0.2, linetype = "solid")) +
    scale_fill_viridis_d(option = "magma") +
    guides(fill = guide_legend(title.hjust = 1, label.position = "left", label.hjust = 1)) +
    ylab("") +
    labs(fill = "Escoamento sanitario") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y))

# Display plots in a 2x2 grid
options(repr.plot.width = 12, repr.plot.height = 5)
grid.arrange(plt_abastecimento_agua, plt_iluminacao, ncol = 2)
grid.arrange(plt_destino_lixo, plt_escoa_sanitario, ncol = 2)

# Dataframe for water supply, discriminated by BF indicator
df_agua <- rbind(
    cbind(data.frame(table(df[df$marc_pbf == 0, ]$cod_agua_canalizada_fam)), bf = rep(0, 2)),
    cbind(data.frame(table(df[df$marc_pbf == 1, ]$cod_agua_canalizada_fam)), bf = rep(1, 2))
)

# Dataframe for bathroom existence, discriminated by BF indicator
df_banheiro <- rbind(
    cbind(data.frame(table(df[df$marc_pbf == 0, ]$cod_banheiro_domic_fam)), bf = rep(0, 2)),
    cbind(data.frame(table(df[df$marc_pbf == 1, ]$cod_banheiro_domic_fam)), bf = rep(1, 2))
)

# Dataframe for sewage collection, discriminated by BF indicator
df_rede_esg <- rbind(
    cbind(data.frame(table(df[df$marc_pbf == 0, ]$cod_escoa_sanitario_domic_fam == "Rede coletora")), bf = rep(0, 2)),
    cbind(data.frame(table(df[df$marc_pbf == 1, ]$cod_escoa_sanitario_domic_fam == "Rede coletora")), bf = rep(1, 2))
)

# Dataframe for garbage collection, discriminated by BF indicator
df_lixo <- rbind(
    cbind(data.frame(table(df[df$marc_pbf == 0, ]$cod_destino_lixo_domic_fam == "Coletado")), bf = rep(0, 2)),
    cbind(data.frame(table(df[df$marc_pbf == 1, ]$cod_destino_lixo_domic_fam == "Coletado")), bf = rep(1, 2))
)

# Redefine column names
colnames(df_agua) <- c("agua", "freq", "bf")
colnames(df_banheiro) <- c("banheiro", "freq", "bf")
colnames(df_rede_esg) <- c("coleta", "freq", "bf")
colnames(df_lixo) <- c("coleta", "freq", "bf")

# Water supply, in terms of percentage
percs_agua <- c(
    100*round(df_agua[df_agua$bf == 0, ]$freq/sum(df_agua[df_agua$bf == 0, ]$freq), 3),
    100*round(df_agua[df_agua$bf == 1, ]$freq/sum(df_agua[df_agua$bf == 1, ]$freq), 3)
)

# Bathroom existence, in terms of percentage
percs_banheiro <- c(
    100*round(df_banheiro[df_banheiro$bf == 0, ]$freq/sum(df_banheiro[df_banheiro$bf == 0, ]$freq), 3),
    100*round(df_banheiro[df_banheiro$bf == 1, ]$freq/sum(df_banheiro[df_banheiro$bf == 1, ]$freq), 3)
)

# Garbage collection, in terms of percentage
percs_lixo <- c(
    100*round(df_lixo[df_lixo$bf == 0, ]$freq/sum(df_lixo[df_lixo$bf == 0, ]$freq), 3),
    100*round(df_lixo[df_lixo$bf == 1, ]$freq/sum(df_lixo[df_lixo$bf == 1, ]$freq), 3)
)

# SewageScollection, in terms of percentage
percs_rede_esg <- c(
    100*round(df_rede_esg[df_rede_esg$bf == 0, ]$freq/sum(df_rede_esg[df_rede_esg$bf == 0, ]$freq), 3),
    100*round(df_rede_esg[df_rede_esg$bf == 1, ]$freq/sum(df_rede_esg[df_rede_esg$bf == 1, ]$freq), 3)
)

# Add '%' symbol
percs_agua <- paste(percs_agua, '%', sep = '')
percs_banheiro <- paste(percs_banheiro, '%', sep = '')
percs_lixo <- paste(percs_lixo, '%', sep = '')
percs_rede_esg <- paste(percs_rede_esg, '%', sep = '')

# Vertical bound
max_y <- 2500000
break_y <- 1000000

# Bars colors
colors <- c("grey80", "grey40")

# Plot: water supply
plt_agua <- ggplot(df_agua, aes(
        x = factor(bf, levels = 0:1, labels = c('Nao', 'Sim')),
        y = freq,
        fill = factor(agua, levels = 0:1, labels = c('Nao', 'Sim')))) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw() + 
    mytheme +
    ylab("# Familias") +
    xlab("Beneficiarios do BF") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = percs_agua), position = position_dodge(width = 0.9), vjust = -0.2, size = 5) +
    labs(fill = "Agua encanada") + 
    theme(legend.position = "top", legend.title = element_text(size = 15), legend.text = element_text(size = 15))

# Plot: existence of bathroom
plt_banheiro <- ggplot(df_banheiro, aes(
        x = factor(bf, levels = 0:1, labels = c('Nao', 'Sim')),
        y = freq,
        fill = factor(banheiro, levels = 0:1, labels = c('Nao', 'Sim')))) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Beneficiarios do BF") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = percs_banheiro), position = position_dodge(width = 0.9), vjust = -0.2, size = 5) +
    labs(fill = "Banheiro") + 
    theme(legend.position = "top", legend.title = element_text(size = 15), legend.text = element_text(size = 15))

# Plot: garbage collection
plt_lixo <- ggplot(df_lixo, aes(
        x = factor(bf, levels = 0:1, labels = c('Nao', 'Sim')),
        y = freq,
        fill = factor(coleta, levels = c(FALSE, TRUE), labels = c('Nao', 'Sim')))) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Beneficiarios do BF") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = percs_lixo), position = position_dodge(width = 0.9), vjust = -0.2, size = 5) +
    labs(fill = "Coleta lixo") + 
    theme(legend.position = "top", legend.title = element_text(size = 15), legend.text = element_text(size = 15))

# Plot: sewage collection
plt_rede_esg <- ggplot(df_rede_esg, aes(
        x = factor(bf, levels = 0:1, labels = c('Nao', 'Sim')),
        y = freq,
        fill = factor(coleta, levels = c(FALSE, TRUE), labels = c('Nao', 'Sim')))) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw() + 
    mytheme +
    ylab("") +
    xlab("Beneficiarios do BF") +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = percs_rede_esg), position = position_dodge(width = 0.9), vjust = -0.2, size = 5) +
    labs(fill = "Coleta esgoto") + 
    theme(legend.position = "top", legend.title = element_text(size = 15), legend.text = element_text(size = 15))

# Display plots side-by-side
options(repr.plot.width = 16.5, repr.plot.height = 5)
grid.arrange(plt_agua, plt_banheiro, plt_lixo, plt_rede_esg, ncol = 4)

# One-hot encoding
dummy <- dummyVars(" ~ .", data = df)

# Due to memory limitations, we randomly sample 1M rows (typically does not affect the correlations outcome)
set.seed(1645)
df_sampled <- df[sample(nrow(df), 1000000), ]
df_sampled <- data.frame(predict(dummy, newdata = df_sampled))

# Overview of the derived dataset
head(df_sampled)
nrow(df_sampled)
ncol(df_sampled)

# Flatten the coorelation matrix, generating a table representation
flatCorMatrix <- function(cormat, pmat) {
    M <- ncol(cormat)
    u <- 1
    rows_idx <- rep(0, M*(M-1)/2)
    cols_idx <- rep(0, M*(M-1)/2)
    corr <- rep(0, M*(M-1)/2)
    pval <- rep(0, M*(M-1)/2)
    
    for (i in seq(1, M-1)) {
        for (j in seq(i+1, M)) {
            rows_idx[u] <- i
            cols_idx[u] <- j
            corr[u] <- cormat[i, j]
            pval[u] <- pmat[i, j]
            u <- u+1
        }
    }
    data.frame(
        row = rownames(cormat)[row(cormat)][rows_idx],
        column = rownames(cormat)[row(cormat)][cols_idx],
        cor = corr,
        p = pval
    )
}

# Filter the columns we want to drop from the correlation matrix
# cor_features <- cor(newdata[, !(names(newdata) %in% c("id_familia"))])
cor_features <- cor(df_sampled)

# Pearson correlatin matrix
cor_matrix <- rcorr(as.matrix(df_sampled))

# Get the correlations and p-values in a table format
df_cor <- flatCorMatrix(cor_matrix$r, cor_matrix$P)

# Display the 20 largest correlations
df_cor[order(-df_cor$cor),][1:10, ]

# Display the 20 lowest correlations
df_cor[order(df_cor$cor),][1:10, ]
