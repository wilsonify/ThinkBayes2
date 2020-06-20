"""A standard machine learning task without much sacred magic."""
import pymongo as pymongo
from sacred import Experiment
from sacred.observers import MongoObserver
from sklearn import svm, datasets, model_selection

assert "mongo_client" in dir(pymongo)

ex = Experiment("svm")

mongo_user = "mongo_user"
mongo_pwd = "mongo_password"
mongo_host = "localhost"
mongo_port = 27017
mongo_db = "sacred"
mongo_url = f"mongodb://{mongo_user}:{mongo_pwd}@{mongo_host}:{mongo_port}/{mongo_db}?authSource=admin"
observer = MongoObserver(url=mongo_url)

ex.observers.append(observer)
ex.add_config(
    {"C": 1.0, "gamma": 0.7, "kernel": "rbf", "seed": 42, }
)


def get_model(C, gamma, kernel):
    return svm.SVC(C=C, kernel=kernel, gamma=gamma)


@ex.main  # Using main, command-line arguments will not be interpreted in any special way.
def run(_config):
    x_array, y_array = datasets.load_breast_cancer(return_X_y=True)
    x_array_train, X_test, y_train, y_test = model_selection.train_test_split(
        x_array, y_array, test_size=0.2
    )
    clf = get_model(
        _config["C"], _config["gamma"], _config["kernel"]
    )  # Parameters are passed explicitly.
    clf.fit(x_array_train, y_train)
    return clf.score(X_test, y_test)


if __name__ == "__main__":
    ex.run()
