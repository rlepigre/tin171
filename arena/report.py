#!/usr/bin/env python

personality = ["trivial_bot",
               "static_distance_bot",
               "iddfs_bot",
               "static_iddfs_bot",
               "minimax_bot",
               "parallel_static_bot",
               "evolved_distance_bot",
               "evolved_iddfs_bot",
               "parallel_euclidean_bot",
               "parallel_evolved_bot",
               "alphabeta_bot",
               "evolved_alphabeta_bot",
               "evolved3_distance_bot",
               "evolved3_iddfs_bot",
               "evolved3_alphabeta_bot",
               "parallel_evolved3_bot"]

stats = open("stats-2-players-2.txt").readlines()
stats = stats + open("stats-2-players.txt").readlines()
stats = stats + open("stats.txt").readlines()
stats = [ eval(x) for x in stats ]

def analyze(stats, p1, p2):
    if p1 == p2:
        return ' '

    players = {1: personality[p1], 2: personality[p2]}
    pstats = [x for x in stats if x['players'] == players]
    wins = 0
    total = 0
    for game in pstats:
        if not 'winner' in game: continue
        total += 1
        (player, nick) = game['winner']
        if player == 1:
            wins += 1

    if total == 0:
        return '-'

    ret = "%.02f" % (float(wins) / float(total))
    if ret[0] == '0':
        return ret[1:]
    return ret

def table(cstart, cend):
    print "\\begin{tabular}{ | c |",(" c " * (cend - cstart)),"| }"
    print "\\hline"

    print "".ljust(2),"&",
    for p2 in range(cstart, cend):
        print str(p2).center(4),
        if p2 < cend-1: print "&",
    print "\\\\ \n\\hline"

    for p1 in range(0,16):
        print str(p1).ljust(2),"&",
        for p2 in range(cstart, cend):
            result = analyze(stats, p1, p2)
            print result.rjust(4),
            if p2 < cend-1:
                print "&",
        print "\\\\"

    print "\\hline\n\\end{tabular}"

table(0, 7)
print
table(7, 16)
