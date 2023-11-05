from sklearn.feature_extraction.text import CountVectorizer
import nltk
import spacy
from gensim.utils import simple_preprocess
from keras.preprocessing.text import text_to_word_sequence

class Tokenizer:
    def __init__(self, library_type) -> None:
        if library_type == 'nltk':
            nltk.download('punkt', quiet=True)  # Download the Punkt tokenizer
            self._tokenize = nltk.word_tokenize
        elif library_type == 'spacy':
            self.nlp = spacy.load('en_core_web_sm')
            self._tokenize = self.spacy_tokenize
        elif library_type == 'gensim':
            self._tokenize = simple_preprocess
        elif library_type == 'keras':
            self._tokenize = text_to_word_sequence
        elif library_type == 'sklearn':
            vectorizer = CountVectorizer()
            self._tokenize = vectorizer.build_tokenizer()
        else:
            raise ValueError(f"Unknown library type: {library_type}")

    def spacy_tokenize(self, text):
        return [token.text for token in self.nlp(text)]
        
    def tokenize(self, text):
        return self._tokenize(text)