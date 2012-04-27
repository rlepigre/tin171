/****************************************************************************
 * Usage: ./dist < dis_vx.data > dis_vx                                     *
 * dis_vx is a data file, only characters '-' 'o' 'A'..'Z' are processed.   *
 * The first 17 * 17 of them are for the player 1, the next 17 * 17 for     *
 * player 2, ... until player 6.                                            *
 ****************************************************************************/
#include <stdio.h>

int main(int argc, char **argv){
  char c;
  int i;
  int counter;

  printf("    data=(None");
  for(i = 0; i < 6; i++){
    printf(",\n          (");

    counter = 0;
    while((counter < 17 * 17) && ((c = getchar()) != EOF)){
      if(c == '-' || c == 'o'){
        if(counter == 0)
          printf("-1");
        else
          printf(", -1");
        counter++;
      }else if(c >= 'A' && c <= 'Z'){
        if(counter == 0)
          printf("%i", c - 'A');
        else
          printf(", %i", c - 'A');
        counter++;
      }
    }

    printf(")");
  }

  printf(")\n");
  return 0;
}
