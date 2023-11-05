from typing import List
import math
import pandas as pd
from code.tokenizer import Tokenizer
import code.clasificacion as clf
import code.representacion as rep
import code.carga as carga
from sklearn.metrics import classification_report
from sklearn.metrics import f1_score
import code.preprocessing as pre
import numpy as np
import time
import itertools
from tqdm.auto import tqdm

TweetText = List[str]
Tweet = object

def print_testing_dataset(classifier: clf.Classifier, word_rep: rep.WordMapper, preprocessing: pre.Preprocess):
    print("\nTesting classifier:", classifier.__class__.__name__, "\nParameters:", "{" + classifier.get_metaparameters() + "}",
            "\nWord representation:", word_rep.__class__.__name__, "\nPreprocessing:", preprocessing.__class__.__name__)

def split_dataset(corpus: pd.DataFrame):
    X_train = list(corpus['Tweet'])
    Y_train = list(corpus['Class'])
    return X_train, Y_train

def test_classifer(classifier: clf.Classifier, X_train, Y_train, X_test, Y_test, pbar = None):
    if pbar is None:
       print(f"Training classifier")
    else:
        pbar.set_postfix({'state': 'Training'}, refresh=True)
    train_st = time.time()
    classifier.train(X_train, Y_train)
    train_et = time.time()

    if pbar is None:
        print(f"Classifier trained in {train_et - train_st} seconds")
        print(f"Testing classifier")
    else:
        pbar.set_postfix({'state': 'Trained'}, refresh=True)
        pbar.set_postfix({'state': 'Testing'}, refresh=True)

    test_st = time.time()
    Y_pred = classifier.classify(X_test)
    test_et = time.time()

    if pbar is None:
        print(f"Classifier tested in {test_et - test_st} seconds")
    else:
        pbar.set_postfix({'state': 'Tested'}, refresh=True)    
    return Y_test, Y_pred, train_et - train_st, test_et - test_st
    

def test_classifers(classifiers: list[clf.Classifier],
         word_reps: list[rep.WordMapper],
         preprocessing_modules: list[pre.Preprocess],
         train_corpus: pd.DataFrame,
         measure_corpus: pd.DataFrame):
    
    summ = []
    
    X_train, Y_train = split_dataset(train_corpus)
    X_test, Y_test = split_dataset(measure_corpus)
    try:
        with tqdm(total=len(preprocessing_modules), position=0, leave=False) as pbar1:
            for preprocessing in preprocessing_modules:
                pbar1.set_description(f"Preprocesser: {preprocessing.__class__.__name__}")
                #print(f"\nExecuting tests for preprocessing: {preprocessing.__class__.__name__}")
                pbar1.set_postfix({'state': 'Preprocessing'}, refresh=True)
                X_train_prep = preprocessing.preprocess_tweets(X_train)
                X_test_prep = preprocessing.preprocess_tweets(X_test)
                pbar1.set_postfix({'state': 'Preprocessed'}, refresh=True)

                with tqdm(total=len(word_reps), position=1, leave=False) as pbar2:
                    for word_rep in word_reps:

                        word_rep.fit(X_train_prep)
                        pbar2.set_description(f"Wordrep: {word_rep.__class__.__name__}")
                        pbar2.set_postfix({'state': 'Applying'}, refresh=True)
                        X_train_rep = word_rep.tweets_to_vec(X_train_prep)
                        X_test_rep = word_rep.tweets_to_vec(X_test_prep)
                        pbar2.set_postfix({'state': 'Applied'}, refresh=True)

                        #print(f"\nExecuting tests for word representation: {word_rep.__class__.__name__}")
                        with tqdm(total=len(classifiers), desc='Inner Loop', position=2, leave=False) as pbar3:
                            for classifier in classifiers:
                                pbar3.set_description(f"Classifier: {classifier.__class__.__name__}")
                                #print_testing_dataset(classifier, word_rep, preprocessing)
                                Y_test, Y_pred, train_time, test_time = test_classifer(classifier, X_train_rep, Y_train, X_test_rep, Y_test, pbar3)
                                f1 = f1_score(Y_test, Y_pred, average='macro')
                                #print(f"F1 Score: {f1}")
                                summ.append([classifier.__class__.__name__,'{' + classifier.get_metaparameters() + '}', word_rep.__class__.__name__, 
                                            preprocessing.__class__.__name__, f1, train_time, test_time])
                                pbar3.update()
                        pbar2.update()
                pbar1.update()
    except KeyboardInterrupt:
        print("Manual interruption")
                
    summarize = pd.DataFrame(columns=['Classifier', 'Metaparameters', 'Word Representation', 'Preprocessing', 'F1 Score',
                                       'Train Time', 'Test time'], data=summ)
    return summarize

def test_single_classifer(classifier: clf.Classifier,
         word_rep: rep.WordMapper,
         preprocessing: pre.Preprocess,
         train_corpus: pd.DataFrame,
         measure_corpus: pd.DataFrame):
   
    print_testing_dataset(classifier, word_rep, preprocessing)
    X_train, Y_train = split_dataset(train_corpus)
    X_test, Y_test = split_dataset(measure_corpus)

    X_train_prep = preprocessing.preprocess_tweets(X_train)
    X_test_prep = preprocessing.preprocess_tweets(X_test)

    word_rep.fit(X_train_prep)

    X_train_rep = word_rep.tweets_to_vec(X_train_prep)
    X_test_rep = word_rep.tweets_to_vec(X_test_prep)
    Y_test, Y_pred, train_time, test_time = test_classifer(classifier, X_train_rep, Y_train, X_test_rep, Y_test)
    report = classification_report(Y_test, Y_pred)
    return f1_score(Y_test, Y_pred, average='macro'), report, train_time, test_time


def get_classifiers(parameters_dict: dict, Classifier):
    if len(parameters_dict) == 0:
        return []
    parameters = list(map(lambda x: list(zip(parameters_dict.keys(), x)), itertools.product(*parameters_dict.values())))
    classifiers = [Classifier(x) for x in parameters]
    return classifiers
    
def dump_results(results, filename, time_id, path='../results'):
    results.to_csv(f"{path}/{time_id}_{filename}_results.csv", index=False)
    print
    print('Successful run')
    print(f"Results saved in {path}/{time_id}_{filename}_results.csv")
    print('Top 3 results:')
    results = results.sort_values(by=["F1 Score"], ascending=False)
    print(results.head(3).to_string(index=False))    

def get_mean_tweet_lenght(df: pd.DataFrame):
    preprocessor = pre.AllPreprocess()
    tweets = list(df['Tweet'])
    tokenizer = Tokenizer('sklearn')
    tweets = preprocessor.preprocess_tweets(tweets)
    tweets = list(map(lambda x: tokenizer.tokenize(x), tweets))
    tweensl = list(map(lambda x: len(x), tweets))
    return math.floor(np.average(tweensl))

if __name__ == "__main__":
    devel, train, test = carga.get_datasets()

    wordvectors = carga.get_embeddings()
    preprocessing_methods = [pre.DummyPreprocess(), pre.AllPreprocess(), pre.StopWordsPreprocess(), pre.Tarea1Preprocess()]

    mean_tweet_lenght = get_mean_tweet_lenght(train)

    standard_bow = rep.StandardBowMapper()
    mean_embedding = rep.MeanEmbeddingMapper(wordvectors)
    array_embedding = rep.ArrayEmbeddingMapper(mean_tweet_lenght, wordvectors)
    count_bow = rep.CountStandardBowMapper()
    count_mean_embedding = rep.CountMeanEmbeddingMapper(wordvectors)
    count_array_embedding = rep.CountArrayEmbeddingMapper(mean_tweet_lenght, wordvectors)

    svm_parameters = {'c':[1, 2], 'kernel':['rbf', 'poly']}
    knn_parameters ={'n_neighbors':[5, 3, 7, 10], 'weights':['uniform', 'distance'], 'p':[1, 2]}
    mlp_embedding_parameters = {'max_iter':[100, 200, 400], 'hidden_layer_sizes':[(50,), (100,), (50, 50), (100, 100)],
                                'activation':['relu', 'logistic'], 'random_state':[1]}
    mlp_bow_parameters = {'max_iter':[400], 'hidden_layer_sizes':[(100,), (50, 50)], 'activation':['relu', 'logistic'], 'random_state':[1]}
    lstm_parameters_embedding = {'sequence_length': [mean_tweet_lenght], 'embedding_length': [300], 'lstm_layers': [[(50,'tanh','sigmoid')]],
                            'dense_layer': [[(50,None)], [(50,'relu')], [(100, 'relu')], [(100,'relu'), (100,'relu'), (100,'relu')]], 'seed': [1]}
    lstm_parameters_count_embedding = {'sequence_length': [mean_tweet_lenght], 'embedding_length': [303], 'lstm_layers': [[(50,'tanh','sigmoid')]],
                            'dense_layer': [[(50,None)], [(50,'relu')], [(100, 'relu')], [(100,'relu'), (100,'relu'), (100,'relu')]], 'seed': [1]}







    time_id = int(time.time())
    classifiers_svm = get_classifiers(svm_parameters, clf.SVMClassifier)
    results_classifiers_svm_bow = test_classifers(classifiers_svm, [standard_bow, count_bow], preprocessing_methods, train, devel)
    dump_results(results_classifiers_svm_bow, "svm_bow", time_id)

    classifiers_knn = get_classifiers(knn_parameters, clf.KNNClassifier)
    results_classifiers_knn_bow = test_classifers(classifiers_knn, [standard_bow, count_bow], preprocessing_methods, train, devel)
    dump_results(results_classifiers_knn_bow, "knn_bow", time_id)

    mlp_for_bow_classifiers = get_classifiers(mlp_bow_parameters, clf.MLPClassifier)  
    results_mlp_for_bow = test_classifers(mlp_for_bow_classifiers, [standard_bow, count_bow], preprocessing_methods, train, devel)
    dump_results(results_mlp_for_bow, "mlp_for_bow", time_id)

    classifiers_svm = get_classifiers(svm_parameters, clf.SVMClassifier)
    results_classifiers_svm_emb = test_classifers(classifiers_svm, [mean_embedding, count_mean_embedding], preprocessing_methods, train, devel)
    dump_results(results_classifiers_svm_emb, "svm_emb", time_id)

    classifiers_knn = get_classifiers(knn_parameters, clf.KNNClassifier)
    results_classifiers_knn_emb = test_classifers(classifiers_knn, [mean_embedding, count_mean_embedding], preprocessing_methods, train, devel)
    dump_results(results_classifiers_knn_emb, "knn_emb", time_id)

    mlp_for_embedding_classifiers = get_classifiers(mlp_embedding_parameters, clf.MLPClassifier)
    results_mlp_for_emb = test_classifers(mlp_for_embedding_classifiers, [mean_embedding, count_mean_embedding], preprocessing_methods, train, devel)
    dump_results(results_mlp_for_emb, "mlp_for_embedding", time_id)

    lstm_classifiers = get_classifiers(lstm_parameters_embedding, clf.LSTMClassifier)
    results_lstm_sl_300 = test_classifers(lstm_classifiers, [array_embedding], preprocessing_methods, train, devel)
    dump_results(results_lstm_sl_300, "lstm_sl_300", time_id)

    lstm_classifiers_count_embedding = get_classifiers(lstm_parameters_count_embedding, clf.LSTMClassifier)
    results_lstm_sl_303 = test_classifers(lstm_classifiers_count_embedding, [count_array_embedding], preprocessing_methods, train, devel)
    dump_results(results_lstm_sl_303, "lstm_sl_303", time_id)