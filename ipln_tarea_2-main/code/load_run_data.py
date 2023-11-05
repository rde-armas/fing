import pandas as pd
import os
import re

def load_one_result(name: str, path):
    return pd.read_csv(f"{path}/{name}")

def get_latest_timestamp(path):
    ls = os.listdir(path)
    xs = []
    for l in ls:
        match = re.search(r"^(\d+).*", l)
        if match is not None:
            xs.append(match)

    if len(xs) == 0:
        raise Exception("No results found")
    
    max = 0
    for x in xs:
        id = int(x.group(1))
        if id > max:
            max = id
    val = list(filter(lambda x: x.startswith(str(max)), ls))
    return val

def get_names():
    return ["svm_knn", "mlp_for_embedding", "lstm"] #"mlp_for_bow"

def load_all(path):
    if path is None:
        return []
    names = get_latest_timestamp(path)
    res = []
    for name in names:
        res.append(load_one_result(name, path))
    return res

def select_top_n(df: pd.DataFrame,  n: int, by: str):
    return 

def concat_pandas(dfs: list[pd.DataFrame]):
    df = pd.DataFrame(columns=dfs[0].columns)
    df = pd.concat(dfs)
    return df

def print_top_n_by_classifier(df: pd.DataFrame, n: int):
    print(f"\nTop {n} per classifier:")
    for classifier in df['Classifier'].unique():
        print(f"\nClassifier: {classifier} ")
        new_df = df[df['Classifier'] == classifier].sort_values(by=['F1 Score'], ascending=False).head(n)
        print(new_df[['Classifier','Word Representation','Preprocessing','F1 Score','Train Time','Test time', 'Metaparameters']].to_string(index=False))

def print_top_n(df: pd.DataFrame, n: int):
    print(f"\nTop {n}:")
    df = df.sort_values(by=["F1 Score"], ascending=False)
    print(df.head(n)[['Classifier','Word Representation','Preprocessing','F1 Score','Train Time','Test time', 'Metaparameters']].to_string(index=False))    

def print_global_summary(global_top = 10,  dataframes: list[pd.DataFrame] = None, path = None):
    res = load_all(path)
    if dataframes is not None:
        res = res + dataframes
    res = concat_pandas(res)
    print_top_n(res, global_top)

def print_algorithm_summary(top_per_classifier = 2,  dataframes: list[pd.DataFrame] = None, path = None):
    res = load_all(path)
    if dataframes is not None:
        res = res + dataframes
    res = concat_pandas(res)
    print_top_n_by_classifier(res, top_per_classifier)

if __name__ == "__main__":
    print_global_summary(path='../results')
    print_algorithm_summary(path='../results')