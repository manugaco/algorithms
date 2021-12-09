## Neural Network from scratch in python language:

## Libraries

import numpy as np
import pandas as pd
import sys

## Testing dataset:

X = np.array([[0, 0, 1, 1], [1, 0, 1, 0], [0, 0, 0, 1]])
y = np.array([[1], [1], [0]])

# Activation functions:

def sigmoid(x):
    return 1/(1+np.exp(-x))
    
def d_sigmoid(x):
    return sigmoid(x)*(1-sigmoid(x))

## Parameters (dictionary):

params = {'epoch': 5000,
         'learning_rate': 0.1,
         'n_hidden_layers':3,
         'n_output_layers':1}

## Algorithm main function:

def nn_python(y, X, params):
    
    ## Function inputs format sanity checks:
    
    if isinstance(params, dict):
        print("Sanity check 1: Params is a dictionary.")
    else:
        sys.exit("Sanity check 1: Params is in incorrect format, please use dictionary instead.")

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


out, loss = nn_python(y, X, 'target', params)

print(out)




