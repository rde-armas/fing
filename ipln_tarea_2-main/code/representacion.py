from sklearn.feature_extraction.text import CountVectorizer
from gensim.models.keyedvectors import KeyedVectors
import numpy as np
import abc
from code.tokenizer import Tokenizer
from nltk.stem import WordNetLemmatizer
from code.carga import get_lexico
import scipy

class WordMapper(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, tokenizer: Tokenizer, LEXICO_PATH: str = './csv/'):
        self.tokenizer = tokenizer
        self.lemmatizer = WordNetLemmatizer()
        self._lemmas_loaded = False
        self._load_lemmas(LEXICO_PATH)

    @abc.abstractmethod
    def tweets_to_vec(self, tweets: list[str]):
        pass
    
    def fit(self, tweets: list[str]):
        pass

    def _load_lemmas(self, LEXICO_PATH):
        if not self._lemmas_loaded:
            self.pos_lemmas = set(get_lexico(datalexico_path=LEXICO_PATH)[1]['Word'].values)
            self.neg_lemmas = set(get_lexico(datalexico_path=LEXICO_PATH)[0]['Word'].values)
        self._lemmas_loaded = True

    def _count_positive_negative_lemmas(self, tweet):
        tokenized = self.tokenizer.tokenize(tweet)
        positive = len(list(filter(lambda x: self.lemmatizer.lemmatize(x).lower() in self.pos_lemmas, tokenized)))
        negative = len(list(filter(lambda x: self.lemmatizer.lemmatize(x).lower() in self.neg_lemmas, tokenized)))
        total = len(tokenized)
        return [positive, negative, total]


############################################# BOW #############################################
class BowMapper(WordMapper, metaclass=abc.ABCMeta):
    pass

class StandardBowMapper(BowMapper):
    def __init__(self, tokenizer = Tokenizer('sklearn')) -> None:
        super().__init__(tokenizer)

    def tweets_to_vec(self, tweets: list[str]):
        return self.vectorizer.transform(tweets)
    
    def fit(self, tweets: list[str]):
        self.vectorizer = CountVectorizer(max_features=300000)
        self.vectorizer.fit(tweets)


class CountStandardBowMapper(StandardBowMapper):
    def __init__(self, tokenizer=Tokenizer('sklearn')) -> None:
        super().__init__(tokenizer)
    
    def tweet_to_vec(self, tweet: str):
        rep = super().tweet_to_vec(tweet)
        pos, neg, tot = self._count_positive_negative_lemmas(tweet)
        concat = scipy.sparse.hstack([rep, pos, neg, tot])
        return concat

    def tweets_to_vec(self, tweets: list[str]):
        rep = super().tweets_to_vec(tweets)
        x = np.array([self._count_positive_negative_lemmas(tweet) for tweet in tweets])
        sparse_x = scipy.sparse.coo_matrix(x)
        return scipy.sparse.hstack([rep, sparse_x])
        
    
############################################# EMBEDDING #############################################
class EmbeddingMapper(WordMapper, metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def embedding_len(self) -> int:
        pass
    
    @abc.abstractmethod
    def word_to_vec(self, word: str):
        pass

class Word2VecEmbeddingMapper(EmbeddingMapper):

    def __init__(self, tokenizer: Tokenizer, wordvectors) -> None:
        super().__init__(tokenizer)
        self.wordvectors = wordvectors
        self.vector_len = self.wordvectors.vector_size
        self._w2vec_vectorizer = np.vectorize(self.word_to_vec)
        
    def word_to_vec(self, word: str):
        if word in self.wordvectors:
            return self.wordvectors.get_vector(word)
        else:
            return None # TODO: VER SI AGREGAR to.
        
class MeanEmbeddingMapper(Word2VecEmbeddingMapper):

    def __init__(self, wordvectors,  tokenizer: Tokenizer = Tokenizer('sklearn')) -> None:
        super().__init__(tokenizer, wordvectors)

    def word_to_vec(self, word: str):
        return super().word_to_vec(word)

    def tweet_to_vec(self, tweet: str):
        words = self.tokenizer.tokenize(tweet)
        val = []
        for word in words:
            vec = self.word_to_vec(word)
            if vec is not None:
                val.append(vec)

        val = np.array(val)
        if len(val) == 0:
            return np.zeros(self.vector_len)
        
        return np.mean(val, axis=0)

    def tweets_to_vec(self, tweets: list[str]):
        vec = []
        for x in tweets:
            vec.append(self.tweet_to_vec(x))
 
        return np.array(vec)

    def embedding_len(self) -> int:
        return self.vector_len

class CountMeanEmbeddingMapper(MeanEmbeddingMapper):
    def __init__(self, wordvectors, tokenizer: Tokenizer = Tokenizer('sklearn')) -> None:
        super().__init__(wordvectors, tokenizer)
    
    def tweet_to_vec(self, tweet: str):
        rep = super().tweet_to_vec(tweet)
        pos, neg, total = self._count_positive_negative_lemmas(tweet)
        return np.concatenate((rep, [pos, neg, total]), axis=None)

class ArrayEmbeddingMapper(Word2VecEmbeddingMapper):
    def __init__(self, array_len: int, wordvectors, tokenizer: Tokenizer = Tokenizer('sklearn')) -> None:
        self.array_len = array_len
        super().__init__(tokenizer, wordvectors)

    def word_to_vec(self, word: str):
        return super().word_to_vec(word)

    def tweet_to_vec(self, tweet: str):
        tweet = tweet.lower() # TODO: move to preprocessing
        words = self.tokenizer.tokenize(tweet)
        val = []
        for word in words:
            vec = self.word_to_vec(word)
            if vec is not None:
                val.append(vec)
        val_len = len(val)
        # add if len == 0
        if val_len < self.array_len:
            for _ in range(0, self.array_len - val_len):
                val.append(np.zeros(self.vector_len))
        elif self.array_len < val_len:
            val[self.array_len - 1] = np.mean(val[self.array_len - 1:], axis=0)
            val = val[:self.array_len]
        val = np.array(val)        
        return val

    def tweets_to_vec(self, tweets: list[str]):
        vec = []
        for x in tweets:
            vec.append(self.tweet_to_vec(x))
 
        return np.array(vec)

    def embedding_len(self) -> int:
        return self.vector_len

class CountArrayEmbeddingMapper(ArrayEmbeddingMapper):
    def __init__(self, array_len: int, wordvectors, tokenizer: Tokenizer = Tokenizer('sklearn')) -> None:
        super().__init__(array_len, wordvectors, tokenizer)
    
    def tweet_to_vec(self, tweet: str):
        rep = super().tweet_to_vec(tweet)
        pos, neg, tot = self._count_positive_negative_lemmas(tweet)
        for i in [pos, neg, tot]:
            rep = np.pad(rep, ((0,0), (0,1)), 'constant', constant_values=(i))
        
        return rep