import os
import pickle

def save_model(model: object, path: str, overwrite=False):
    """Save sklearn models using pickle"""

    if not os.path.exists(path):
        with open(path, 'wb') as file:
                pickle.dump(model, file)
        print("File saved successfully at " + os.path.abspath(path))
    else:
        print("File already exists at " + os.path.abspath(path))
        if overwrite==False:
            print("File not overwritten.")
        else:
            with open(path, 'wb') as file:
                pickle.dump(model, file)
            print("File overwritten successfully.")

def load_model(path: str):
    """Load sklearn models using pickle"""

    if not os.path.exists(path):
        print("File does not exist.")
    else: 
        with open(path, 'rb') as file:
            model = pickle.load(file)
        print("File loaded successfully.")
        return model
