import numpy as np
import matplotlib.pyplot as plt
from numpy import random


# X : initial data / K : number of expected clusters
def G(X, K):
    G = np.zeros((K, 2))
    for i in range(K):
        index = np.random.choice(len(X))
        G[i] = X[index]
    return G


X0_init = np.random.multivariate_normal((2, 2), 2*np.eye(2), 128)
X1_init = np.random.multivariate_normal((-4, -4), 6*np.eye(2), 128)

X = np.concatenate((X0_init, X1_init), axis=0)
random.shuffle(X)

G1 = G(X, 2)

def k_means(x, K, G, ite):
    for k in range(ite):
        Test = np.zeros(len(x))
        for i in range(len(x)):
            valeur = np.zeros(len(G))
            for j in range(K):
                valeur[j] = np.linalg.norm(x[i] - G[j])
            Test[i] = np.where(np.isclose(valeur, min(valeur)))[0]
        G = np.zeros((len(G), 2))
        Nk = np.zeros(len(G))
        for i in range(len(G)):
            for j in range(len(x)):
                if Test[j] == i:
                    Nk[i] += 1  # counting the number of elements of each class
                    G[i] += x[j]
        # print(Nk)
        for i in range(len(G)):
            G[i] = 1 / Nk[i] * G[i]
    return Test, G


# To see the results
# print(k_means(X, 2, G1, 1000))

fig = plt.figure(figsize=(15, 4))
# Plotting the 2 samples as constructed (left)
plt.subplot(121)
plt.scatter(X0_init[:, 0], X0_init[:, 1], label="Classe 1", c='red')
plt.scatter(X1_init[:, 0], X1_init[:, 1], label="Classe 2", c='blue')
plt.legend()

# Plotting the k-means classes
plt.subplot(122)
# Colouring the results
results = k_means(X, 2, G1, 1000)[0]
X0, X1 = np.zeros((1, 2)), np.zeros((1, 2))
for k in range(len(X)):
    if results[k] == float(0):
        X0 = np.vstack((X0, X[k, :]))
    else:
        X1 = np.vstack((X1, X[k, :]))

plt.scatter(X0[:, 0], X0[:, 1], c='violet')
plt.scatter(X1[:, 0], X1[:, 1], c='cyan')

plt.show()
