import itertools
import math

side = 50
# -30 degrees and +90 degrees for x and y axial unit vectors
radian_x = - math.pi / 6
radian_y = math.pi / 2
# cartesian coordinates for the x and y axial unit vectors
cart_x = [side * math.cos(radian_x), side * math.sin(radian_x)]
cart_y = [side * math.cos(radian_y), side * math.sin(radian_y)]

def axial_to_cartesian(v):
    return (round(v[0] * cart_x[0] + v[1] * cart_y[0], 2),
            round(v[0] * cart_x[1] + v[1] * cart_y[1], 2))

def euclidean_distance(v1, v2):
    return math.sqrt((v1[0] - v2[0])*(v1[0] - v2[0]) + (v1[1] - v2[1])*(v1[1] - v2[1]))

# returns true if the second vertex is a positive cardinal direction jump from the first
def is_edge(v1, v2):
    return any([v2[1] == v1[1] and v2[0] == (v1[0] + 1),
               v2[1] == v1[1] + 1 and v2[0] == v1[0],
               v2[1] == v1[1] + 1 and v2[0] == v1[0] + 1])
def get_axial_vertices():
    vertices = ([(-5, i) for i in range(-4, 0)]
             + [(-4, i) for i in range(-5, 2)]
             + [(-3, i) for i in range(-5, 3)]
             + [(-2, i) for i in range(-5, 4)]
             + [(-1, i) for i in range(-5, 5)]
             + [(0, i) for i in range(-4, 5)]
             + [(1, i) for i in range(-4, 6)]
             + [(2, i) for i in range(-3, 6)]
             + [(3, i) for i in range(-2, 6)]
             + [(4, i) for i in range(-1, 6)]
             + [(5, i) for i in range(1, 5)])
    return vertices

def get_cartesian_vertices(vertices):
    return list(map(axial_to_cartesian, vertices))

def get_edges(cart_vertices):
    edges = []
    for v1, v2 in itertools.permutations(cart_vertices, 2):
        if euclidean_distance(v1, v2) <= side + 0.1:
            edges.append((v1, v2))
    return edges

def get_int_edges(axial_vertices):
    int_edges = []
    for v1, v2 in itertools.permutations(axial_vertices, 2):
        if is_edge(v1, v2):
            int_edges.append((v1, v2))
    return int_edges

if __name__=="__main__":
    axial_vertices = get_axial_vertices()
    cart_vertices = get_cartesian_vertices(axial_vertices)
    edges = get_edges(cart_vertices)
    int_edges = get_int_edges(axial_vertices)
