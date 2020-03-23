# Abdelmalek Hajjam
# Data 608 - Spring 2020

# Question 2
# Are stewards (steward activity measured by the ‘steward’ variable) having an impact on the health of trees?
# In order to answer this question correlation between 'stewards' and 'health' should be correlated.

import pandas as pd
import numpy as np
import pandas as pd
import numpy as np
import json
import plotly.offline as py
import plotly.graph_objs as go
from plotly import tools
import dash
import dash_core_components as dcc
import dash_html_components as html

url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'
trees = pd.read_json(url)
trees_q1 = trees[['spc_common', 'status', 'boroname']]
trees_q1['spc_common'].fillna('Unknown', inplace=True)

# create columns that specify tree status
for status in set(trees_q1['status']):
    trees_q1[status] = np.where(trees_q1['status'] == status, 1, 0)

trees_q1 = pd.DataFrame(trees_q1.groupby(['boroname', 'spc_common']).sum())
trees_q1.head()

# find out boroughs
boroughs = list(set(trees['boroname']))

trace_list_q2 = []

# create plot titles
borough_list = list(map(lambda x: str(x), boroughs))

trees_q2 = trees[['spc_common', 'health', 'boroname', 'steward']]

trees_q2['spc_common'].fillna('Unknown', inplace=True)
trees_q2.dropna(inplace=True)
trees_q2[['steward', 'health']] = trees_q2[[
    'steward', 'health']].apply(lambda x: pd.factorize(x)[0])
trees_q2_cor = pd.DataFrame(trees_q2.groupby(
    ['boroname', 'spc_common']).corr())
fig_q2 = tools.make_subplots(rows=1, cols=len(
    boroughs), subplot_titles=tuple(borough_list))


boroughs = list(set(trees_q2['boroname']))
plants = list(set(trees_q2['spc_common']))

for borough in boroughs:
    trace = go.Bar(
        x=list(trees_q1.loc[borough].index),
        y=list(trees_q2_cor.loc[borough]['steward'][::2])
    )
    trace_list_q2 += [trace]

for i in range(len(boroughs)):
    fig_q2.append_trace(trace_list_q2[i], 1, i+1)


fig_q2['layout'].update(showlegend=False, height=500, width=1400,
                        title='Proportion of Trees in Good, Fair and Poor Conditions')


app = dash.Dash()

colors = {
    'background': '#ffffff',
    'text': '#111111'
}


app.layout = html.Div(style={'backgroundColor': colors['background']}, children=[
    html.H1(
        children='Question #2',
        style={
            'textAlign': 'center',
            'color': colors['text']
        }
    ),
    html.Div(children='Correlation between stewardsand health of trees', style={
        'textAlign': 'center',
        'color': colors['text']
    }),

    html.Div([
        dcc.Graph(figure=fig_q2, id='my-figure')
    ])
])


if __name__ == '__main__':
    app.run_server(debug=True)
