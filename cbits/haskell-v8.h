#ifndef __HASKELL_V8_
#define __HASKELL_V8_

#include <v8.h>

#include "./include/haskell-v8-common.h"

using namespace v8;


typedef struct {
  int should_dispose;
  int ref_count;
} isolate_data;


isolate_data * get_isolate_data(Isolate *i) {
  isolate_data *data;

  if ((data = (isolate_data *)i->GetData()) == NULL) {
    data = (isolate_data *)malloc(sizeof(isolate_data));
    data->ref_count = 0;
    data->should_dispose = 0;
    i->SetData(data);
  }

  return data;
}


template <class T> class V8Ref {
public:
  V8Ref(Isolate *i, Handle<T> h) : isolate(i) {
    // Increase the isolate refcount
    isolate_data *data = get_isolate_data(isolate);
    data->ref_count++;
    // Make a persistent reference to the object
    handle.Reset(i, h);
  }

  ~V8Ref() {
    isolate_data *data;

    {
      // lock and enter the isolate
      Locker locker(isolate);
      Isolate::Scope isolate_scope(isolate);
      // decrement the isolate refcount
      data = get_isolate_data(isolate);
      data->ref_count--;
    }

    // destroy the persistent reference to the object
    handle.Dispose();

    // If haskell already freed the isolate and this was the
    // last object holding a reference to it, dispose and free the isolate
    if (data->should_dispose == 1 && data->ref_count == 0) {
      isolate->Dispose();
    }
  }

  Persistent<T> handle;
  Isolate *isolate;
};


class V8Object {
public:
  V8Object(V8Ref<Value> *v, V8Ref<Context> *c) : value(v), context(c) { }

  ~V8Object() {
    delete value;
    // No need to delete the context as it will be done automatically
    // by haskell's garbage collector
  }

  V8Ref<Value> *value;
  V8Ref<Context> *context;
};


#ifdef __cplusplus
extern "C" {
#endif

extern void free_isolate(Isolate *);
extern void free_context(V8Ref<Context> *);
extern void free_value(V8Object *);
extern Isolate * new_isolate();
extern V8Ref<Context> * new_context(Isolate *);
extern type_value * eval_in_context(char *, V8Ref<Context> *);
char * v8_to_string(V8Object *);

#ifdef __cplusplus
}
#endif

#endif
