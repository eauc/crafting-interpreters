#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include "memory.h"

void initValueArray(ValueArray *valueArray) {
  valueArray->count = 0;
  valueArray->capacity = 0;
  valueArray->values = NULL;
}

void freeValueArray(ValueArray *valueArray) {
  FREE_ARRAY(uint8_t, valueArray->values, valueArray->capacity);
  initValueArray(valueArray);
}

void writeValueArray(ValueArray *valueArray, Value value) {
  if (valueArray->capacity < valueArray->count + 1) {
    int oldCapacity = valueArray->capacity;
    valueArray->capacity = GROW_CAPACITY(valueArray->capacity);
    valueArray->values = GROW_ARRAY(Value, valueArray->values, oldCapacity, valueArray->capacity);
  }
  valueArray->values[valueArray->count] = value;
  valueArray->count++;
}

void printValue(Value value) {
  printf("%g", value);
}
