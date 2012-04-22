#include <stdio.h>
#include "board.h"

/*
 * Prints the board.
 * buf: array containing the board.
 */
void print_board(const char* buf){
  int i, j;
  int offset = 16;
  int line = 0;
  char col = 'A';
  for(j = 0; j < 20; j++) putchar(' ');
  for(j = 0; j < 16; j++){
    printf("%c ", col);
    col++;
  }
  printf("%c\n", col);
  for(j = 0; j < 20; j++) putchar(' ');
  for(j = 0; j < 16; j++) printf("| ");
  printf("|\n");
  for(j = 0; j < 20; j++) putchar(' ');
  for(j = 0; j < 16; j++) printf("/ ");
  printf("/\n%2i ", line);
  for(i = 0; i < 289; i++){
    if(i % 17 == 0 && i != 0){
      line++;
      printf("\n%2i ", line);
    }
    if(i % 17 == 0){
      for(j = 0; j < offset; j++)
        if(j % 2 == 1)
          putchar(' ');
        else
          putchar('-');
      offset--;
    }
    switch(buf[i]){
      case ' ':
        putchar('o');
        break;
      case '#':
        if(i % 17 % 2 == 0)
          putchar('/');
        else
          putchar(' ');
        break;
      default:
        putchar(buf[i]);
        break;
    }
    if(i % 17 != 16) putchar(' ');
  }
  printf("\n ");
  for(j = 0; j < 17; j++) printf(" /");
  printf("\n");
}

/*
 * Get the index of a board position.
 * line: the line number.
 * col: the column letter.
 * returns the position.
 */
int get_array_pos(int line, char col){
  return line * 17 + (col - 'A');
}

