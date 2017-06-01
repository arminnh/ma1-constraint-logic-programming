# generates hashiwokakero boards from https://gamegix.com/hashi-o-kakero/game
# the board needs to be copies into input_board.txt
dim = 13
f = open('input_board.txt', 'r')
board = f.readlines()

board_list = []
for i in range(dim):
    row = []
    for j in range(dim):
        row.append(0)
    board_list.append(row)

b = str.split(board[0], '<')

x = 0
y = 0
skip = False
for item in b:

    splitted = item.split(' ')
    if '/div' in splitted[0] or len(splitted) == 0:
        continue

    if skip:
        skip = False
        continue

    #print(splitted)

    if 'div' == splitted[0]:
        board_list[x][y] = int(splitted[-1][-1])
        skip = True
        y += 1
        if y >= dim:
            y = 0
            x += 1
        if x == dim:
            break

    elif 'img' in splitted[0]:
        board_list[x][y] = 0

        y += 1
        if y >= dim:
            y = 0
            x += 1
        if x == dim:
            break
    #print("x {} y {}".format(x,y))
    #print(board_list[x])


print("ECLiPSe:\n")
print('board(XXX, [](')
for row in board_list:
    print('    []({}),'.format(row))
print(')).\n\n\n')

print("CHR:\n")
print('board(XXX, [')
for row in board_list:
    print('    {},'.format(row))
print(']).')
