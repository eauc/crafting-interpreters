#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

typedef enum {
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj *next;
};
void printObject(Value value);

struct ObjString {
  Obj obj;
  int length;
  char *chars;
  size_t hash;
};
ObjString *takeString(char *chars, size_t length);
ObjString *copyString(const char *chars, size_t length);

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_STRING(value) isObjType(value, OBJ_STRING)

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

#define AS_STRING(value) ((ObjString *)(AS_OBJ(value)))
#define AS_CSTRING(value) (AS_STRING(value)->chars)

#endif
