# SymbolicMethods_Project
In this project, we aim to build a research interest summarization tool using LDA. The 'R' packages 'RISMed' and 'textmineR' were used.
#Process Flow
1. Author name is the input and the package 'RISMed' is used to extract the abstracts published by the author on PubMed. 
2. A Document-Term matrix is constructed with each 'document' being a vector a MeSH terms extracted from a given abstract.
3.Each vector of MeSH terms (for a given abstract) is fed as a document to the LDA model.
4. Two approaches have been taken to assign topic names:
    1. By Using the bi- and tri-grams outputted by the LDA model
    2. By using the pre-generated topic labels 
5. Fuzzy string match conducted between bi-grams and tri-grams and the long list of all MeSH terms extracted from all abstracts associated with an author to reveal the main topics the research interest corresponds to.


