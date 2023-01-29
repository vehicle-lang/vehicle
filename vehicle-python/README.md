# vehicle-python

To install the dependencies _and_ the development dependencies, run the following command from this directory:

```sh
poetry install --with dev
```

To run the loss function tests, run the following command from this directory:

```sh
poetry run tox
```

To run the training test, run the following command from this directory:

```sh
poetry run python tests/test.py
```
