from causallearn.search.ConstraintBased.PC import pc, pc_alg
import pickle
import pandas as pd
import numpy as np
import networkx as nx
import statistics
from causallearn.utils.GraphUtils import GraphUtils
from causallearn.utils.PDAG2DAG import pdag2dag
from sklearn import base
from matplotlib import rc
import matplotlib.pyplot as plt
from scmtools.utils import get_dot_graph
from sklearn.neural_network import MLPRegressor
from sklearn.linear_model import LinearRegression
from sklearn.inspection import PartialDependenceDisplay
from scmtools.model import build_ground_truth_causal_model
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from scmtools.utils import copy_causal_model_with_frozen_mechanisms
from scmtools.augment import augment_causal_model_with_black_box, sample_augmented_counterfactuals
from causallearn.search.FCMBased import lingam
from dowhy import gcm
from cdp import CausalDependencePlotter, UncertainCausalDependencePlotter
from dagma.nonlinear import DagmaMLP, DagmaNonlinear
import torch
import matplotlib.image as mpimg
import matplotlib.pyplot as plt
import io
import seaborn as sns
from dowhy.gcm import model_evaluation
#first, processing the dataframe to remove unwanted variables
BIGDF = pd.read_csv("processed_game_data.csv")
df = BIGDF.loc[:, ~BIGDF.columns.str.contains('^Unnamed')]

varz = ["Flow","Unpleasantness-0to100","Immersion","NegativeAffect","Challenge","Tension","Competence","BFI-Extraversion", "PCS Helplessness", "FFMQ Total"]
df_var_of_int = df[varz]

#Fitting model for structure discovery
eq_model = DagmaMLP(dims=[len(df_var_of_int.columns),4,1], bias=True, dtype=torch.double)
model = DagmaNonlinear(eq_model, dtype=torch.double)
dfnp = df_var_of_int.to_numpy().astype(np.float64)
W_est = model.fit(dfnp, T=5, lr=0.0001, warm_iter=5e4, max_iter=8e4 , w_threshold=0.25)

dfnpreduced = df_var_of_int.to_numpy().astype(np.float64)

#Generating conditional dependence graph figure
newDF =  pd.DataFrame(W_est, index = df_var_of_int.columns, columns = df_var_of_int.columns)
plt.clf()
G =nx.DiGraph(newDF)
pyd = nx.nx_pydot.to_pydot(G)
tmp_png = pyd.create_png(f="png")
pyd.write_dot('output_graphviz.dot')
fp = io.BytesIO(tmp_png)
img = mpimg.imread(fp, format='png')
plt.figure(figsize = (10,10))
plt.imshow(img)
plt.axis('off')
plt.show()
plt.savefig("graph_dagma.svg")

#Building causal model
df = df_var_of_int
dag_graph = G
causal_model_g = gcm.InvertibleStructuralCausalModel(dag_graph)
gcm.auto.assign_causal_mechanisms(causal_model=causal_model_g, based_on=df)
gcm.fit(causal_model_g, df)
pickle.dump(causal_model_g, open("fitted_model_stored_v2.pickle","wb"))

#Drawing conditional dependence sampling plots
# define treatment and outcome
treatment_name = 'Flow'
outcome_name = 'Unpleasantness-0to100'
covariates_list = COLUMNS
covariates_list.remove(outcome_name)

black_box_X_train = df[covariates_list]
black_box_y_train = df[outcome_name]

explanatory_X_data = df[covariates_list]

plotter_bc = CausalDependencePlotter(
    black_box_X_train=black_box_X_train, 
    black_box_y_train=black_box_y_train,
    black_box_model=RandomForestRegressor(max_depth=10, random_state=0),
    fit_causal_model=causal_model_bc,
    explanatory_X_data=explanatory_X_data,
    outcome_name=outcome_name,
    prefit_black_box=False,
    average_curve_color=colors[0],
    individual_curve_color=colors[7]
)

%matplotlib inline
fig, axes = plt.subplots(nrows=2, ncols=4, sharex=False, sharey=False, figsize=(36, 10), dpi=300)
axes = axes.ravel()


treatment_one = 'Flow'
treatment_two = 'PCS Helplessness'
axes[0].set_xlabel(treatment_one)
axes[1].set_xlabel(treatment_one)
#axes[1].set_ylim([0.2,0.5])
#axes[5].set_ylim([0.2,0.5])
axes[2].set_xlabel(treatment_one)
axes[3].set_xlabel(treatment_one)


plotter_bc.plot_total_effect(treatment_var=treatment_one, axis=axes[0])
plotter_bc.plot_controlled_effect(
    treatment_var=treatment_one, 
    control_vars=('BFI-Extraversion',), 
    control_tuples=[(val/5,) for val in range(1, 5)], 
    combine_plots=True, 
    axis=axes[1]
)
plotter_bc.average_curve_color = colors[1]
plotter_bc.individual_curve_color = colors[8]
plotter_bc.plot_direct_effect(treatment_var=treatment_one, axis=axes[2])
plotter_bc.average_curve_color = colors[5]
plotter_bc.individual_curve_color = colors[2]
plotter_bc.plot_indirect_effect(treatment_var=treatment_one, axis=axes[3])
plotter_bc.average_curve_color = colors[0]
plotter_bc.individual_curve_color = colors[7]
plotter_bc.plot_total_effect(treatment_var=treatment_two, axis=axes[4])

axes[4].set_title('')
plotter_bc.plot_controlled_effect(
    treatment_var=treatment_two, 
    control_vars=('BFI-Extraversion',), 
    control_tuples=[(val/5,) for val in range(1, 5)], 
    combine_plots=True, 
    axis=axes[5]
)
axes[5].set_title('')
plotter_bc.average_curve_color = colors[1]
plotter_bc.individual_curve_color = colors[8]
plotter_bc.plot_direct_effect(treatment_var=treatment_two, axis=axes[6])
axes[6].set_title('')
plotter_bc.average_curve_color = colors[5]
plotter_bc.individual_curve_color = colors[2]
plotter_bc.plot_indirect_effect(treatment_var=treatment_two, axis=axes[7])
axes[7].set_title('')
axes[1].legend(title='BFI-Extraversion')
axes[5].legend(title='BFI-Extraversion')
# plt.yticks(ticks=[2.0, 4.0], labels=['Benign', 'Malignant'])
#sns.despine(top=True, right=True, left=False, bottom=False)
#plt.tight_layout()
plt.savefig("big_graph_analysis_v2.pdf")
plt.show()


#Calculating graph solidity
print(gcm.evaluate_causal_model(causal_model_bc, df, evaluate_invertibility_assumptions=False, evaluate_causal_structure=True))

#calculating correlation and mutual information
corr = df_var_of_int.corr()
from sklearn.feature_selection import mutual_info_regression
def custom_mi_reg(a, b):
    a = a.reshape(-1, 1)
    b = b.reshape(-1, 1)
    return  mutual_info_regression(a, b)[0] # should return a float value
    
mi = df_var_of_int.corr(method=custom_mi_reg) 
