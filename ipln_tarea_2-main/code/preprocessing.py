import abc
import re
import os
from nltk.stem import WordNetLemmatizer
import code.tokenizer as tokenizer
import csv
from code.carga import get_lexico

ABBREVIATIONS_DIR = "../csv/abbreviations/"
LEXICO_PATH = './csv/'

tknzr = tokenizer.Tokenizer('sklearn')
lemmatizer = WordNetLemmatizer()

def set_abbr_dir(path: str):
    global ABBREVIATIONS_DIR
    ABBREVIATIONS_DIR = path

def set_lexico_path(path: str):
    global LEXICO_PATH
    LEXICO_PATH = path

class Preprocess(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def preprocess_tweet(self, tweet: str):
        pass

    def preprocess_tweets(self, tweets: list[str]):
        res = []
        for x in tweets:
            res.append(self.preprocess_tweet(x))
        return res

class MetaPreprocess(Preprocess):
    _hooks = []
    _built = False
    def append_preproccesser(self, x: Preprocess):
        self._hooks.append(x)
    
    def build(self):
        self._built = True

    def preprocess_tweet(self, tweet: str):
        if not self._built:
            raise RuntimeError("Preprecessor not built! Run self.build() before preprocessing")
        y = tweet
        for x in self._hooks:
            y = x.preprocess_tweet(y)
        return y

    def preprocess_tweets(self, tweets: list[str]):
        if not self._built:
            raise RuntimeError("Preprecessor not built! Run self.build() before preprocessing")
        return super().preprocess_tweets(tweets)

class StopWordsPreprocess(Preprocess):
    stopwords = []
    
    def __init__(self) -> None:
        super().__init__()
        StopWordsPreprocess.__load_stopwords()

    def __load_stopwords():
        if StopWordsPreprocess.stopwords != []:
            return
        
        StopWordsPreprocess.stopwords = list(get_lexico(datalexico_path=LEXICO_PATH)[2]['Word'].values)
        StopWordsPreprocess.stopwords = set(map(str.lower, StopWordsPreprocess.stopwords))

    def __tknz(self, tweet: str):
        return tknzr.tokenize(tweet)

    def __remove_stopwords(self, tweet: list[str]):
        return filter(lambda x: x.lower() not in StopWordsPreprocess.stopwords, tweet) 

    def preprocess_tweet(self, tweet: str):
        return (" ".join(self.__remove_stopwords(self.__tknz(tweet)))).lower()

class DummyPreprocess(Preprocess):
    def preprocess_tweet(self, tweet: str):
        return tweet.lower()
    
    def preprocess_tweets(self, tweets: list[str]):
        return list(map(lambda tweet: self.preprocess_tweet(tweet), tweets))

class Tarea1Preprocess(Preprocess):
    def replace_hashtags(self, text):
        return re.sub(r'(^|(?<=\s))#\w+', 'hashtag', text)

    def replace_emails(self, text):
        return re.sub(r'([A-Za-z0-9]+[.-_])*[A-Za-z0-9]+@[A-Za-z0-9-]+(\.[A-Z|a-z]{2,})+', 'email', text)

    def replace_mentions(self, text):
        return re.sub(r'(^|(?<=\s))@\w+', 'mención', text)

    def replace_http_https_urls(self, text):
        return re.sub(r'https?:\/\/(?:www\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b(?:[-a-zA-Z0-9()@:%_\+.~#?&\/=]*)', 'link', text)

    def replace_jaja(self, text):
        value = '|'.join([f'([{v}j]*(j{v}|{v}j)[{v}j]*\s*)+' for v in 'aeiou'])
        return re.sub(fr'(^|(?<=\s))({value})($|(?=\s))', 'risa', text)

    def replace_dates_and_fractions(self, text):
        return re.sub(r'[0-9]+(\/[0-9]+)+', 'fecha', text)

    def replace_percentages(self, text):
        return re.sub(r'[+-]?[0-9]+(\.)?[0-9]*%', 'porcentaje', text)

    def replace_numbers(self, text):
        return re.sub(r'\s+\d+\s*(,|\.|:|-)*\d*', 'número', text)

    def replace_hours(self, text):
        return re.sub(r'\s[0-9]{1,2}(:|\'|\.|\s)?[0-9]{0,2}\s?(am|pm|h)', 'hora', text)

    def read_abbreviations_map(self):
        csvs = os.listdir(ABBREVIATIONS_DIR)
        csvs = filter(lambda x: re.match(r'.*\.', x) is not None,csvs)
        csvs = map(lambda x: ABBREVIATIONS_DIR + x, csvs)
        abbreviations = []

        for file in csvs:
            with open(file, 'r', encoding="utf-8") as csv_file:
                data = list(csv.reader(csv_file))
                abbreviations = abbreviations + data
        return abbreviations

    def clean_abbreviations_from_map(self, text):
        abbreviations = self.read_abbreviations_map()
        for (abb, real) in abbreviations:
            re_str = r'(^|(?<=\s))' + abb + r'\.?($|(?=\s))'
            text = re.sub(re_str, real, text)
        return text

    def clean_punctuation(self, text):
        text = re.sub(r'[\.,:;*`]', '', text)
        symbols = ['¿', '¡', '!', '?','"','\'','(',')','[',']','{','}']
        for symbol in symbols:
            text = text.replace(symbol, f' {symbol} ')
        text = re.sub(r'\s+', ' ', text)
        text = text.strip()
        return text

    def replace_emoticons(self, text):
        text = re.sub(r'(?::|;|=)(?:-)?(?:\)|\(|D|P|d|p|O|o|0|3|\\|\/|\||\]|x|X|@|\\|\/|\||\]|x|X|@)', 'emoticon', text)
        emoticon_pattern = r'(?<=\s)(\:\w+\:|\<[\/\\]?3|[\(\)\\\D|\*\$][\-\^]?[\:\;\=]|[\:\;\=B8][\-\^]?[3DOPp\@\$\*\\\)\(\/\|])(?=\s|[\!\.\?]|$)'
        return re.sub(emoticon_pattern, 'emoticon', text)

    def replace_repeated_characters(self, text):
        return re.sub(r'([a-zA-Z])\1{2,}', r'\1', text)

    def cleanup_pipeline(self, text):
        text = text.lower()
        text = self.replace_hashtags(text)
        text = self.replace_emails(text)
        text = self.replace_mentions(text)
        text = self.replace_http_https_urls(text)
        text = self.replace_dates_and_fractions(text)
        text = self.replace_percentages(text)
        text = self.replace_hours(text)
        text = self.replace_numbers(text)
        text = self.replace_emoticons(text)
        text = self.clean_punctuation(text)
        text = self.clean_abbreviations_from_map(text)
        text = self.replace_jaja(text)
        return text

    def preprocess_tweet(self, tweet: str):
        return self.cleanup_pipeline(tweet)

    def preprocess_tweets(self, tweets: list[str]):
        return super().preprocess_tweets(tweets)

class AllPreprocess(Preprocess):
    def __init__(self) -> None:
        super().__init__()
        self.preprocessor = MetaPreprocess()
        self.preprocessor.append_preproccesser(Tarea1Preprocess())
        self.preprocessor.append_preproccesser(StopWordsPreprocess())
        self.preprocessor.build()
    
    
    def preprocess_tweet(self, tweet: str):    
        return self.preprocessor.preprocess_tweet(tweet)