import pandas as pd 
from gensim.models.keyedvectors import KeyedVectors
import os
DATASET_PATH = '../csv/'
EMBEDDING_PATH = '../embeddings/embeddings-l-model.vec'
DATASETS = ['devel', 'train', 'test']
LEXICO = ['lexico_neg_lemas_grande', 'lexico_pos_lemas_grande', 'stop_words_esp_anasent']


def get_datasets(datasets = DATASETS, dataset_path=DATASET_PATH) -> list[pd.DataFrame]:
    print("Loading datasets...")
    res = []
    for dataset in datasets:
        res.append(pd.read_csv(os.path.join(dataset_path, dataset + '.csv')))
    print("Datasets loaded")
    return res

def get_lexico(lexico = LEXICO, datalexico_path=DATASET_PATH):
    lexicos = []
    for lex in lexico:
        lexicos.append(pd.read_csv(os.path.join(datalexico_path, lex + '.csv')))
    return lexicos

def get_embeddings(word_vector_path = EMBEDDING_PATH, limit=300000):
    print("Loading embeddings model...")
    wordvectors = KeyedVectors.load_word2vec_format(word_vector_path, limit=limit)
    print("Embeddings loaded")
    return wordvectors

