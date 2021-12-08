## Libraries

import numpy as np
import pandas as pd
import sys

## Testing dataset:

X = np.array([[0, 0, 1, 0, 0], [1, 0, 0, 1, 1], [1, 0, 1, 0, 1], [0, 0, 1, 1, 0]])
df = pd.DataFrame(X).rename(columns={0:'target', 1:'A', 2:'B', 3:'C', 4:'D'})

# Activation functions:

def sigmoid(x):
    return 1/(1+np.exp(-x))
    
def d_sigmoid(x):
    return x*(1-x)

## Parameters (dictionary):

params = {'epoch': 50000,
         'learning_rate': 0.1,
         'n_hidden_layers':10,
         'n_output_layers':1}


params

## Algorithm main function:

def neural_network_python(data, target_name, params):
    
    ## Function inputs format sanity checks:
    
    if isinstance(df, pd.DataFrame):
        print("Sanity check 1: Data is a pandas dataframe.")
    else:
        sys.exit("Sanity check 1: Data is in incorrect format, please use pandas dataframe instead.")
    
    if isinstance(target_name, str):
        print("Sanity check 2: Target name is a string.")
    else:
        sys.exit("Sanity check 2: Target name is in incorrect format, please use string instead.")
    
    if isinstance(params, dict):
        print("Sanity check 3: Params is a dictionary.")
    else:
        sys.exit("Sanity check 3: Params is in incorrect format, please use dictionary instead.")
    
    ## Data format sanity check:
    
    if len([data[col].dtype for col in data.columns if data[col].dtype not in [int, float, np.int64, np.float64]])==0:
        print("Sanity check 4: Columns are in correct format types.")
    else:
        sys.exit("Sanity check 4: All Columns must be numeric types, please transform non numerical variables.")
    
    ## Data definition:
    
    X = np.array(data.drop(target_name, axis=1))
    y = np.array(data[target_name])
    
    ## Parameters:
    
    ep = params.get('epoch')
    alpha = params.get('learning_rate')
    iln = X.shape[1]
    hln = params.get('n_hidden_layers')
    oln = params.get('n_output_layers')
    
    ## Variables definition:
    
    losses = []

    # Input weights: Number of neurons at input layer * number of neurons at hidden layer:
    wih = np.random.uniform(size=(iln, hln))

    # Input bias: one by number of neurons at hidden layer:
    bih = np.random.uniform(size=(1, hln))

    # Output: Number of neurons at output layer  * number of neurons at hidden layer:
    woh = np.random.uniform(size=(hln, oln))

    # Output bias: one by number of neurons at hidden layer:
    boh = np.random.uniform(size=(1, oln))
    
    ## Main algorithm
    
    for epoch in range(ep):
        
        #Forward propagation:

        # Hidden layers activations:
        hl_lt = np.dot(X, wih) + bih
        hl_a = sigmoid(hl_lt)

        # Output activation
        ol_lt = np.dot(hl_a, woh) + boh
        output = sigmoid(ol_lt)

        #Back propagation:

        # Error:
        error = (np.square(y - output.T) / 2).T

        # Derivatives:
        sl_ol = d_sigmoid(output)
        sl_hl = d_sigmoid(hl_a)
        
        #Calculating rate of change in error:
        d_o = error*sl_ol
        e_hl = d_o.dot(woh.T)
        d_hl= e_hl * sl_hl
        
        #Updating weights and bias:
        
        woh += hl_a.T.dot(d_o)*alpha
        boh += np.sum(d_o, axis=0, keepdims=True)*alpha
        wih += X.T.dot(d_hl)*alpha
        bih += np.sum(d_hl, axis=0, keepdims=True)*alpha
        
        losses.append(np.average(error))
        
        if epoch % 1000 == 0:
            print("Error at epoch " + str(epoch) + " is " + str(np.average(error)))
        
    return output, losses


out, loss = neural_network_python(df, 'target', params)

print(df['target'])

print(out)




