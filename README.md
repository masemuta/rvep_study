# rvep_study

R scripts to generate figures from the present study

# Como a safra do pinhão afeta a evasão escolar? Um estudo de caso no Município de Clevelândia, Paraná – Brasil

The R script `R/rbep_study.R` can be run to reproduce the results of the following study:

**______, _., ______, _._., ______, _. &, _________, _._.**. 2025. Como a safra do pinhão afeta a evasão escolar? Um estudo de caso no Município de Clevelândia, Paraná – Brasil

To run the script, you can either execute the `rbep_study.sh` shellscript file or run the R script under a R GUI such as RStudio.

## Data

Data can be found in the `data` folder. The `study_dataset` folder includes the `.csv` file with raw occurrence data from SIDRA's IBGE open access data available at <https://sidra.ibge.gov.br/tabela/289>.

## Results

All the results are saved in the `resultados` folder.

<img alt="Série temporal (2010-2023) de colheita e comércio de pinhão entre os principais Estados produtores" src="resultados/pinhão_prod_valor_juntos2.png" width="1000">

Figure 1: **Série temporal (2010-2023) de colheita e comércio de pinhão entre os principais Estados produtores**.

<img alt="Evasão escolar em Clevelândia - PR entre 2019 e 2024" src="resultados/evasão_cleve_pinhão_motivos2.png" width="1000">

Figure 2: **Evasão escolar em Clevelândia - PR entre 2019 e 2024**.

## License

Data and R script are available under the GNU General Public License version 3 (see `LICENSE` file).
