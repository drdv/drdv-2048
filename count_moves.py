#
# count number of moves in './scores/score-2048.json'
#

import json

with open('./scores/score-2048.json') as data_file:
    data = json.load(data_file)

for move in ['U', 'D', 'L', 'R']:
    print("move '{0:s}': {1:3d} times".format(move, data['moves'].count(move)))
