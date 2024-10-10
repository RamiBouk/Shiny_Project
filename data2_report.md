---
title: "Report on Data 1"
output: html_document
---



# Analyse détaillée du nouveau jeu de données

#### 1. Dimensions du jeu de données, valeurs manquantes et attributs constants
- **Dimensions** : Le jeu de données contient 41 188 lignes et 21 colonnes.
- **Valeurs manquantes** : Certaines colonnes contiennent des valeurs manquantes :
  - `job` : 330 valeurs manquantes
  - `marital` : 80 valeurs manquantes
  - `education` : 1 731 valeurs manquantes
  - `default` : 8 597 valeurs manquantes
  - `housing` et `loan` : 990 valeurs manquantes chacune

  Ces valeurs manquantes doivent être traitées avant de construire un modèle prédictif, car elles pourraient introduire du biais ou réduire la précision du modèle.
- **Attributs constants** : Aucun attribut constant n'a été identifié, ce qui signifie qu'aucune variable n'est triviale et toutes peuvent potentiellement apporter des informations.


```
## 41188 21
```

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="out5a8481331e68642d" style="width:100%;height:auto;"></div><!--/html_preserve-->

#### 2. Proportion des individus ayant répondu "yes" ou "no"
Le graphique en bars indique une proportion déséquilibrée entre les réponses :
- **No** : La majorité des individus, environ 88.7 %, ont répondu "no".
- **Yes** : Environ 11.3 % des individus ont répondu "yes".

Ce déséquilibre de classe suggère qu'il faudra peut-être utiliser des techniques pour gérer les données déséquilibrées, comme le sur-échantillonnage de la classe minoritaire ou le sous-échantillonnage de la classe majoritaire, afin d'améliorer les performances des modèles de prédiction.


![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

**La plupart des caractéristiques sont catégorielles, à l'exception des suivantes :**

- `age`
- `duration`
- `euribor3m`
- `pdays`
- `cons.price.idx`
- `cons.conf.idx`
- `nr.employed`
- `campaign`



- Considérons uniquement les variables les **plus** et **moins importantes**: 

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="outf06d226d5deedcaa" style="width:100%;height:auto;"></div><!--/html_preserve-->

#### Top 3 
![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-3.png)
#### Top 3 rates 
![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-3.png)

## 3. Analyse des variables catégorielles
Les graphiques de répartition montrent que certaines variables catégorielles présentent des différences significatives dans les proportions de "yes" et "no" :
- **emp.var.rate**, **month**, et **poutcome** montrent des écarts clairs entre les proportions de "yes" et "no", ce qui indique qu'elles sont probablement des prédicteurs importants pour déterminer la réponse cible.

### 3. Analyse des variables numériques 
<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="outd4b70be79d863bc9" style="width:100%;height:auto;"></div><!--/html_preserve-->
#### 3 variables with least 'count'
![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-2.png)![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-3.png)

## 5. Matrice de corrélation des attributs
L'analyse de la matrice de corrélation met en évidence les points suivants :
- Les variables telles que **euribor3m**, **nr.employed**, et **emp.var.rate** présentent des corrélations significatives entre elles ainsi qu'avec la variable cible, ce qui peut indiquer qu'elles sont étroitement liées au comportement observé dans les données.
- Les faibles corrélations directes pour certaines autres variables suggèrent qu'elles ont moins d'impact isolé sur la variable cible ou qu'elles pourraient interagir de manière plus complexe avec d'autres variables.

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

## B- Conclusions globales
- **Variables influentes** : Les variables comme **duration**, **euribor3m**, **nr.employed**, et certaines variables catégorielles comme **emp.var.rates**, **month**, et **poutcome** semblent jouer un rôle clé dans la prédiction de la réponse cible "yes". Elles devraient être prioritaires dans l'élaboration d'un modèle de prédiction.
- **Déséquilibre de classe** : Le déséquilibre de la classe cible (majorité de "no") devra être traité avec des techniques adaptées pour améliorer les performances du modèle.
- **Stratégies d'amélioration** : Des techniques d'ingénierie des caractéristiques, de sélection des variables et de gestion des valeurs manquantes seront essentielles pour obtenir des résultats optimaux lors de la construction du modèle prédictif.

Ces conclusions fournissent une base solide pour aborder la modélisation prédictive de ce jeu de données en utilisant les variables et stratégies identifiées.


# Prediction de Churn

### Métriques

<table class="table table table-striped table-hover table-condensed table-responsive" style="font-size: 14px; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Prediction Results: Performance Metrics for Different Data Balancing Techniques</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(76, 175, 80, 255) !important;"> Data.Approach </th>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(76, 175, 80, 255) !important;"> Model </th>
   <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(76, 175, 80, 255) !important;"> ROC.Default </th>
   <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(76, 175, 80, 255) !important;"> ROC.Grid </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4"> No Balancing </td>
   <td style="text-align:left;"> DT </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> LG </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM_RBF </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4"> Undersampling </td>
   <td style="text-align:left;"> DT </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> LG </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM_RBF </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4"> Oversampling </td>
   <td style="text-align:left;"> DT </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.87 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> LG </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.89 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> SVM_RBF </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
</tbody>
</table>
### Observations

- **No Balancing:** 
  - The SVM model shows the highest performance with **ROC Default** at 0.93, although its **ROC Grid** slightly decreases to 0.92.
  - LG and SVM_RBF models have relatively similar performance, but SVM_RBF shows slightly better results in the **ROC Grid**.

- **Undersampling:**
  - The SVM model maintains its strong performance with both **ROC Default** and **ROC Grid** at 0.93, indicating its stability in undersampled data.
  - LG shows a slight improvement in **ROC Grid** compared to the default, suggesting that grid tuning enhances its predictive capabilities.

- **Oversampling:**
  - SVM_RBF model stands out with the best performance, achieving **ROC Default** of 0.94 and **ROC Grid** of 0.96, which makes it the most suitable model in oversampled conditions.
  - Decision Tree (DT) shows significant improvement in **ROC Grid** with oversampling compared to no balancing, indicating that it benefits from the increased dataset size.

### General Observations

- **SVM Models Performance:** SVM and SVM_RBF consistently outperform other models across all data balancing techniques, with SVM_RBF showing particularly strong results in oversampling.
- **Data Balancing Impact:** Both undersampling and oversampling techniques generally improve the performance of the models, with oversampling showing the most consistent increase in **ROC Grid** values.
- **Model Stability:** Decision Tree models have lower ROC scores compared to SVM models but show significant improvement when tuning is applied and when data balancing is used.

These findings suggest that **SVM models** (especially SVM_RBF) offer the best predictive performance, particularly when the data is balanced using oversampling. Balancing techniques are crucial in enhancing model accuracy, especially in scenarios with imbalanced datasets.
