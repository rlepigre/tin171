#ifndef __BOARD_H__
#define __BOARD_H__

/*
 * Prints the board.
 * buf: array containing the board.
 */
void print_board(const char* buf);

/*
 * Get the index of a board position.
 * line: the line number.
 * col: the column letter.
 * returns the position.
 */
int get_array_pos(int line, char col);

#endif

