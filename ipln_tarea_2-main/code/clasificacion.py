import abc
import enum
from sklearn import neural_network
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
import tensorflow as tf
import numpy as np
from dataclasses import dataclass

encoding = ['P', 'N', 'NONE']
one_hot_encoding = lambda x: np.array([1,0,0]) if x == 'P' else (np.array([0,1,0]) if x == 'N' else np.array([0,0,1]))

class classif(enum.Enum):
    P = 0
    N = 1
    NONE = 2

Metaparameters = list[(str, object)]

class Classifier(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, metaparams: Metaparameters):
        self.mp = metaparams
        pass

    @abc.abstractmethod
    def classify(self, tweets: list[str]) -> list(classif):
        pass

    @abc.abstractmethod
    def train(self, X_train, Y_train):
        pass

    def get_metaparameters(self):
        if self.mp is None or len(self.mp) == 0:
            return ''
        return ", ".join(map(lambda x: ": ".join(map(str, x)), self.mp))

class MLPClassifier(Classifier):
    def __init__(self, metaparams: Metaparameters) -> None:
        super().__init__(metaparams)
        max_iter = self.mp[0][1]
        hidden_layer_sizes = self.mp[1][1]
        activation = self.mp[2][1]
        seed = self.mp[3][1]
        self.classifier = neural_network.MLPClassifier(hidden_layer_sizes = hidden_layer_sizes, max_iter=max_iter, activation=activation, random_state=seed)

    def classify(self, tweets: list[str]) -> list[classif]:
        return self.classifier.predict(tweets)

    def train(self, X_train, Y_train):
        self.classifier.fit(X_train, Y_train)
    
class SVMClassifier(Classifier):
    def __init__(self, metaparams: Metaparameters) -> None:
        super().__init__(metaparams)
        c = self.mp[0][1]
        kernel = self.mp[1][1]
        self.classifier = SVC(C=c, kernel=kernel)

    def classify(self, tweets: list[str]) -> list[classif]:
        return self.classifier.predict(tweets)

    def train(self, X_train, Y_train):
        self.classifier.fit(X_train, Y_train)

class KNNClassifier(Classifier):
    def __init__(self, metaparams: Metaparameters) -> None:
        super().__init__(metaparams)
        nn = self.mp[0][1]
        weight = self.mp[1][1]
        metric_exponent = self.mp[2][1]
        self.classifier = KNeighborsClassifier(n_neighbors=nn, weights=weight, p=metric_exponent,n_jobs=-1)
    
    def classify(self, tweets: list[str]) -> list(classif):
        return self.classifier.predict(tweets)

    def train(self, X_train, Y_train):
        self.classifier.fit(X_train, Y_train)

class LSTMClassifier(Classifier):
    def __init__(self, metaparameters: Metaparameters) -> None:
        super().__init__(metaparameters)
        self.sequence_length = self.mp[0][1]
        self.embedding_length = self.mp[1][1]
        self.lstm_layers = self.mp[2][1]
        self.dense_layers = self.mp[3][1]
        self.seed = self.mp[4][1]

        tf.random.set_seed(self.seed)
        model = tf.keras.Sequential()
        model.add(tf.keras.layers.Input(shape=(self.sequence_length, self.embedding_length)))
        for l in self.lstm_layers:
            model.add(tf.keras.layers.LSTM(units=l[0],activation=l[1],recurrent_activation=l[2]))
        for l in self.dense_layers:
            model.add(tf.keras.layers.Dense(units=l[0], activation=l[1]))
        model.add(tf.keras.layers.Dense(3, activation='softmax'))
        model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
        self.classifier = model

    def train(self, X_train, Y_train):
        Y_train_encoded = np.array(list(map(one_hot_encoding, Y_train)))
        self.classifier.fit(X_train, Y_train_encoded, epochs=20, batch_size=32)

    def classify(self, tweets) -> list:
        return list(map(lambda x: encoding[x], np.argmax(self.classifier.predict(tweets), axis=-1)))

    def get_metaparameters(self):
        return f"sequence_length: {self.sequence_length}, embedding_length: {self.embedding_length}, lstm_layers: {self.lstm_layers}, dense_layers: {self.dense_layers}, seed: {self.seed}"