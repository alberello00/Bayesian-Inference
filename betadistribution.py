import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import beta

# Parameters for the beta distribution
alpha_values = [0.5, 1, 2, 5]
beta_values = [0.5, 1, 2, 5]

# Values between 0 and 1 for the x-axis
x = np.linspace(0, 1, 1000)

# Plotting the beta distributions for different parameter values
plt.figure(figsize=(12, 8))

for alpha, beta in zip(alpha_values, beta_values):
    plt.plot(x, beta.pdf(x, alpha, beta), label=f'Beta({alpha}, {beta})')

plt.title('Beta Distribution for Different Parameters')
plt.xlabel('x')
plt.ylabel('Probability Density Function (PDF)')
plt.legend()
plt.show()
