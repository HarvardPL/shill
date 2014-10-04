#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  while (true) {
    printf("looping!\n");
    sleep(1);
  }
}
