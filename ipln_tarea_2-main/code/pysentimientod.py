from pysentimiento import create_analyzer
from sklearn.metrics import classification_report, f1_score

def convert_res(x):
    if x == 'POS':
        return 'P'
    elif x == 'NEG':
        return 'N'
    elif x == 'NEU':
        return 'NONE'
    else:
        return x

def test_pysentimiento(X_test, Y_test):
    analyzer = create_analyzer(task="sentiment", lang="es")
    result_pysentimiento = list(map(lambda x: convert_res(x.output), analyzer.predict(X_test)))
    return f1_score(Y_test, result_pysentimiento, average='macro'), classification_report(Y_test, result_pysentimiento)