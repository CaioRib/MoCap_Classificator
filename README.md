# MoCap_Classificator
Projeto Final para a disciplina SCC0275 - Introdução à Ciência de Dados
Alunos:
- Alexandre Norcia Medeiros         	- nUSP 10295583
- Caio Abreu de Oliveira Ribeiro	    - nUSP 10262839
- Daniel Penna Chaves Bertazzo        - nUSP 10349581
- Vinicius Torres Dutra Maia da Costa	- nUSP 10262781

# Descrição:
Classificação entre 5 posições de mão analisando sensores de movimento em uma luva de captura de movimento do dataset 
[MoCap Hand Postures](https://archive.ics.uci.edu/ml/datasets/MoCap+Hand+Postures), encontrado no repositório UCI

# Etapas:
- Análise e pré-processamento dos dados brutos
- Definição dos modelos de classificação
- Otimização de hiperparâmetros (tuning)
- K-Fold Cross Validation
- Comparação da acurácia dos modelos

# Modelos:
- K-nearest neighbors (KNN)
- Naive Bayes
- Support-Vector Machine (SVM)
- Multilayer Perceptron (MLP)
- Random Forest

# Otimização dos hiperparâmetros:
- Extrair os valores dos parâmetros de cada método, visando a melhor performance possível
- Cálculo do erro médio variando os parâmetros, por meio do k-fold cross validation
- Valor de K no modelo KNN
- Kernel function no modelo SVM
- Tamanho da camada escondida e eta (decaimento dos pesos) no modelo MLP
- Número de árvores no modelo Random Forest

# Resultados:
<img src="https://i.imgur.com/EWns5qR.png" width="1109" height="172" title="Results">






