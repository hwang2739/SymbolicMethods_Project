# SymbolicMethods_Project
In this project, we aim to build a research interest summarization tool using LDA. The 'R' packages 'RISMed' and 'textmineR' were used.

Process Flow
1. Abstract Extraction from PubMed for author ('RISMed' package)
2. MeSH dictionary creation: MeSH term extraction and aggregation from all abstracts
3. Topic Identification: Unsupervised Topic Modelling with MeSH terms as input (LDA modeling)
4. Reverse Mapping to MeSH Dictionary (fuzzy string matching, mapping the bigrams and trigrams back to the MeSH dictionary)

Innovation
1. Use of MeSH terms for research interests summarization
2. Labelling of LDA algorithm outputs using standard concepts (MeSH term reverse mapping)
