# Convert OBJ file to Erland code lines (to be used in Wings3D)

code = """%shedron() ->
  Verts = [%s],
  Faces = [%s],
  {Verts,Faces}.
""" # define Erlang code pattern for Wings3D

with open('Polyhedra.obj','r') as file: blocks = file.read().split('\n\n')

blocks = [block for block in blocks if block[0] != '#'] # remove comment blocks

offset = 1 # initialize offset used for face indices on each shape

for block in blocks: # loop over all blocks (= one block per shape)
  words = [line.split() for line in block.split('\n')] # split all block lines
  name = words[0][1] # shape name is the 2nd word of the first block line
  nv, nf = int(words[1][1]), int(words[1][4]) # nb of vertices, nb of faces
  cx, cy, cz = [float(words[1][n]) for n in (9,10,11)] # coords of centroid
  verts, faces = [], [] # initialize lists for vertices and faces
  for n in range(nv): # loop over 'v' lines
    v, x, y, z = words[n+2]
    # move coordinate to origin by subtracting centroid coordinates
    x, y, z = [round(float(v)-c, 8) for v,c in zip((x,y,z),(cx,cy,cz))]
    verts.append("{%s,%s,%s}" % (x, y, z)) # append current vertex to list
  for n in range(nf): # loop over 'f' lines
    # each index group should start from 0, so subtract current offset
    f, *index = words[n+nv+2]; index = [int(i)-offset for i in index] 
    faces.append(str(index).replace(' ','')) # append current face to list
  print(code % (name, ', '.join(verts), ', '.join(faces)))
  offset += nv # update offset according to number of vertices in current shape
