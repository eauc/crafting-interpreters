#include "value.h"
#include "memory.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>

bool valuesEqual(Value a, Value b) {
  if (a.type != b.type) {
    return false;
  }
  switch (a.type) {
  case VAL_BOOL:
    return AS_BOOL(a) == AS_BOOL(b);
  case VAL_NIL:
    return true;
  case VAL_NUMBER:
    return AS_NUMBER(a) == AS_NUMBER(b);
  case VAL_OBJ: {
    ObjString *aString = AS_STRING(a);
    ObjString *bString = AS_STRING(b);
    return aString->length == bString->length &&
           memcmp(aString->chars, bString->chars, aString->length) == 0;
  }
  default:
    return false;
  }
}

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
    valueArray->values = GROW_ARRAY(Value, valueArray->values, oldCapacity,
                                    valueArray->capacity);
  }
  valueArray->values[valueArray->count] = value;
  valueArray->count++;
}

void printValue(Value value) {
  switch (value.type) {
  case VAL_BOOL: {
    printf(AS_BOOL(value) ? "true" : "false");
    break;
  }
  case VAL_NIL: {
    printf("nil");
    break;
  }
  case VAL_NUMBER: {
    printf("%g", AS_NUMBER(value));
    break;
  }
  case VAL_OBJ: {
    printObject(value);
    break;
  }
  }
}
