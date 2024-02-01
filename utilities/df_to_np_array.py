import numpy as np
import pandas as pd

def df_to_np_array(data: pd.DataFrame, y_name: str, X_names=["all"]):
    """Split X and y into two numpy arrays (suitable for sklearn)

    Parameters:
      data: a pandas DataFrame;

      y_name: name of the response variable;
      
      X_name: name(s) of the predictor variable(s) (default "all"). 
    """

    if X_names[0]=="all":
        columns = data.columns
    else: 
        columns = X_names
    X, y = np.array(data[[col for col in columns if col != y_name]]), np.array(data[y_name])
    return X, y