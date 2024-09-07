#include <stdio.h>
#include <stdlib.h>

#include "vm.h"

static void repl(void) {
  char line[1024];
  for(;;) {
    printf("> ");
    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }
    interpret(line);
  }
}

static char *readFile(const char *path) {
  FILE *fp = fopen(path, "rb");
  if (fp == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(fp, 0L, SEEK_END);
  size_t fileSize = ftell(fp);
  rewind(fp);

  char *source = (char *)malloc(fileSize + 1);
  if (source == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }
  size_t nRead = fread(source, sizeof(char), fileSize, fp);
  if (nRead < fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }
  source[nRead] = '\0';

  fclose(fp);
  return source;
}

static void runFile(const char *path) {
  char *source = readFile(path);
  InterpretResult result = interpret(source);
  free(source);
  if(result == INTERPRET_COMPILE_ERROR) exit(65);
  if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char *argv[]) {
  initVM();
  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: clox [path]\n");
    exit(64);
  }
  freeVM();
  return 0;
}
