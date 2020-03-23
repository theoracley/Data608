import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import numpy as np
import pandas as pd
import numpy as np
import json
import colorlover as cl
import plotly.offline as py
import plotly.graph_objs as go
from plotly import tools
import dash
import dash_core_components as dcc
import dash_html_components as html



url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'
trees = pd.read_json(url)
trees_q1 = trees[['spc_common','health','boroname']]
trees_q1['spc_common'].fillna('Unknown',inplace = True)
trees_q1.dropna(inplace = True)

#identify different health conditions
statuses = list(set(trees_q1['health']))
print(statuses)

colors = ['rgb(49,130,189)','rgb(204,204,204)','rgba(222,45,38,0.8)']


#create columns that specify tree health conditions
for status in set(trees_q1['health']):
    trees_q1[status] = np.where(trees_q1['health']==status,1,0)
    
trees_q1 = pd.DataFrame(trees_q1.groupby(['boroname','spc_common']).sum())
trees_q1.head()

#find out boroughs
boroughs = list(set(trees['boroname']))

trees_q1['total'] = trees_q1.sum(axis=1)
for column in list(trees_q1.columns):
    trees_q1[column] = (trees_q1[column]/trees_q1['total'])*100
trees_q1.head()


#create list to store data for each borough
trace_list=[]

#create plot titles
borough_list = list(map(lambda x: str(x), boroughs))

#select number of columns
cols=len(boroughs)
#calculate number of rows
rows=1
fig = tools.make_subplots(rows=rows, cols=cols, subplot_titles=tuple(borough_list))

#iterate through boroughs
for borough in boroughs:
        for i in range(0,len(statuses)):
            trace = go.Bar(
            x = list(trees_q1.loc[borough].index),
            y = list(trees_q1.loc[borough][statuses[i]]),
            name = statuses[i],
            marker=dict(color=colors[i])
            )
            trace_list += [trace]



row_i = []
col_j = []
for i in range(1,rows+1):
    for j in range (1,cols+1):
        for n in range (1,4):
            row_i.append(i)
            col_j.append(j)

for i in range(0,len(trace_list)):        
     fig.append_trace(trace_list[i], row_i[i],col_j[i]) 
 
        
fig['layout'].update(showlegend=False,height=1000, width=900, title='Proportion of Trees in Good, Fair and Poor Conditions', barmode='stack')


app = dash.Dash()

colors = {
    'background': '#ffffff',
    'text': 'black'
}

app.layout = html.Div(style={'backgroundColor': colors['background']}, children=[
    html.H1(
        children='Question #1',
        style={
            'textAlign': 'center',
            'color': colors['text']
        }
    ),
    html.Div(children='Proportion of trees in Good, Fair and Poor conditions', style={
        'textAlign': 'center',
        'color': colors['text']
    }),

       
    html.Div([
        dcc.Graph(figure=fig, id='my-figure')])
    ])




if __name__ == '__main__':
    app.run_server(debug=True)