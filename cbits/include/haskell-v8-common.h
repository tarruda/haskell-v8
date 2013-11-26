#define V8TYPE_UNDEFINED 0
#define V8TYPE_NULL 1
#define V8TYPE_TRUE 2
#define V8TYPE_FALSE 3
#define V8TYPE_NUMBER 4
#define V8TYPE_STRING 5
#define V8TYPE_ARRAY 6
#define V8TYPE_OBJECT 7
#define V8TYPE_REGEXP 8
#define V8TYPE_DATE 9


typedef struct {
  int type;
  int thrown;
  void *value;
} v8_result;
