import networkx as nx
import random
import matplotlib.pyplot as plt
import typing
import numpy as np

def degree(DAG: nx.DiGraph):
    return typing.cast(typing.Iterator[tuple[int, int]], DAG.in_degree())

def generate_graph() -> nx.DiGraph:
    G=nx.gnp_random_graph(20,0.12,directed=True)
    DAG = nx.DiGraph([(u,v) for (u,v) in G.edges() if u<v])
    # add weights to graph vertices
    for v in DAG.nodes():
        DAG.nodes[v]['weight'] = random.randint(1,100)
    roots = [n for n, d in degree(DAG) if d==0]
    root = roots[0]
    for r in roots[1:]:
        DAG.add_edge(root,r)
    assert nx.is_directed_acyclic_graph(DAG)
    assert len([n for n, d in degree(DAG) if d==0]) == 1
    return DAG

def display_graph(DAG: nx.DiGraph, color):
    pos = nx.spring_layout(DAG, k=1.0, iterations=20)
    nx.draw_networkx_nodes(DAG, pos, node_color=color)
    nx.draw_networkx_edges(DAG, pos, edge_color=color)
    edge_labels = nx.get_node_attributes(DAG, 'weight')
    edge_labels = {k: f"{k}, {v}" for (k, v) in edge_labels.items()}
    nx.draw_networkx_labels(DAG, pos, labels=edge_labels)

# graph = generate_graph()
# THREADS = 4
# total_weight = sum(graph.nodes[n]['weight'] for n in graph.nodes())
# max_node_weight = total_weight / THREADS

# print(max_node_weight)

# display_graph(graph, 'green')

# changed = True
# iters = 0
# while changed:
#     changed = False
#     for v, u, *_ in graph.edges():
#         if not u in graph.nodes or not v in graph.nodes: continue
#         if graph.nodes[u]['weight'] + graph.nodes[v]['weight'] < max_node_weight and len(list(graph.predecessors(v))) == 1:
#             graph.nodes[u]['weight'] += graph.nodes[v]['weight']
#             graph = nx.contracted_nodes(graph, u, v, self_loops=False)
#             changed = True
#     iters += 1
# print(nx.is_directed_acyclic_graph(graph))
# print(iters)
# display_graph(graph, 'blue')

# plt.show()

def group_weight(DAG: nx.DiGraph, group: list[int]):
    return sum(DAG.nodes[n]['weight'] for n in group)

DAG = generate_graph()
THREADS = 8

rev = DAG.reverse()
assert nx.is_directed_acyclic_graph(rev)

for n, d in degree(rev):
    rev.nodes[n]['rc'] = d

frontier = [n for n, d in degree(rev) if d==0]
groups: list[list[int]] = []
total_weight = sum(rev.nodes[n]['weight'] for n in rev.nodes())
segment_max_weight = total_weight / THREADS * 1.2

while frontier:
    node = frontier.pop()

    group = rev.nodes[node].get('group')
    if not group:
        group = [node]
        rev.nodes[node]['group'] = group
        groups.append(group)
    for n in rev.successors(node):
        other_group = rev.nodes[n].get('group')
        if not other_group:
            if rev.nodes[n]['rc'] > 1:
                rev.nodes[n]['rc'] -= 1
                continue
            if len(list(rev.successors(n))) != 0 and group_weight(rev, group) + rev.nodes[n]['weight'] < segment_max_weight:
                group.append(n)
                rev.nodes[n]['group'] = group
                frontier.append(n)
                continue
        elif other_group == group:
            continue
        elif group_weight(rev, group) + group_weight(rev, other_group) < segment_max_weight:
            group.extend(other_group)
            groups.remove(other_group)
            for n in other_group: rev.nodes[n]['group'] = group
            continue
        frontier.append(n)

print(groups, segment_max_weight)



display_graph(DAG, 'green')

for group in groups:
    for (a, b) in zip(group, group[1:]):
        DAG.nodes[b]['weight'] += DAG.nodes[a]['weight']
        DAG = nx.contracted_nodes(DAG, b, a, self_loops=False)

print(nx.is_directed_acyclic_graph(DAG))
display_graph(DAG, 'blue')

plt.show()